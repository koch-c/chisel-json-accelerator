import chisel3._
import chisel3.util._

/** PDA state used to calculate two ChannelFifo symbol transitions per cycle. */
class PdaState(pathLength: Int, maxDepth: Int) extends Bundle {
  private val stateWidth = math.max(
    1,
    log2Ceil(pathLength + 1)
  ) // Path length is the amount of keys in the jsonPath, in sensor.temperature it would be log2(2 + 1).Which in this case would return 2 because 3 values can be expressed on 2 bits.
  private val stackPointerWidth = math.max(
    1,
    log2Ceil(maxDepth + 1)
  ) // The width of the stackPointer has to hold up to the depth. Meaning, it would need maxDepth + 1 in bits.

  val stackPointer = UInt(stackPointerWidth.W) // Points to where in the stack
  val stack = Vec(
    maxDepth + 1,
    UInt(stateWidth.W)
  ) // The stack holding values corresponding to how far a key is matched.
  val projecting =
    Bool() // If we have found a colon and we project we're gonna find the value.
  val colonAddress = UInt(12.W) // The address of the current colon.
  val attributeId = UInt(8.W) // Field whose value is being captured.
  val found = Bool() // If any of the requested keys are found.
  val done = Bool() // Done is set when the outer most } or ] is found.
}

/** Pushdown automaton for configured key paths, using the interfaces from the
  * Lexer and attribute-loader branches. The Lexer recognizes key strings and
  * emits `JsonSymbol`s; this module only tracks path state and JSON nesting.
  */
class PushdownAutomaton(config: JsonPathConfig) extends Module {
  private val paths = config.paths
  private val maxDepth = config.maxDepth
  private val keyIds = config.keyIds
  private val keyIdWidth = math.max(1, log2Ceil(keyIds.size max 2))
  private val (edges, trieStateCount, terminalStates) = {
    val builtEdges = scala.collection.mutable.ArrayBuffer.empty[(Int, Int, Int)]
    var nextState = 1
    paths.foreach { candidate =>
      var parent = 0
      candidate.foreach { key =>
        builtEdges.find(edge =>
          edge._1 == parent && edge._2 == keyIds(key)
        ) match {
          case Some((_, _, child)) => parent = child
          case None                =>
            builtEdges += ((parent, keyIds(key), nextState))
            parent = nextState
            nextState += 1
        }
      }
    }
    val terminals = paths.map { candidate =>
      val terminal = candidate.foldLeft(0) { (parent, key) =>
        builtEdges
          .find(edge => edge._1 == parent && edge._2 == keyIds(key))
          .get
          ._3
      }
      terminal -> config.attributeIds(candidate)
    }
    (builtEdges.toSeq, nextState, terminals)
  }
  private val stateWidth = math.max(1, log2Ceil(trieStateCount))
  private val stackPointerWidth = math.max(1, log2Ceil(maxDepth + 1))

  val io = IO(new Bundle {
    // ChannelFifo emits batches of up to two ordered JSON symbols to the PDA.
    val in = Flipped(
      Decoupled(new ChannelBatch(new JsonSymbol(keyIds.size), width = 2))
    )
    // Matches use the exact descriptor consumed by AttributeLoader.
    val descriptor = Decoupled(new AttributeDescriptor)
    // End-of-record is signaled after the outermost container is popped.
    val endOfRecord = Output(Bool())
    val found = Output(Bool())
    val done = Output(Bool())
    val depth = Output(UInt(stackPointerWidth.W))
  })

  val state = RegInit(0.U.asTypeOf(new PdaState(trieStateCount - 1, maxDepth)))
  val pendingDescriptor = RegInit(false.B)
  val pendingDescriptorBits = Reg(new AttributeDescriptor)
  val pendingEndOfRecord = RegInit(false.B)

  /** Transition.
    *
    * @param current
    * @param symbol
    * @param symbolValid
    * @return
    */
  private def transition(
      current: PdaState,
      symbol: JsonSymbol,
      symbolValid: Bool
  ): (PdaState, Bool, UInt, UInt, UInt, Bool) = {
    val next = Wire(new PdaState(trieStateCount - 1, maxDepth))
    next := current
    val project = WireDefault(false.B)
    val projectAddress = WireDefault(0.U(12.W))
    val projectLength = WireDefault(0.U(13.W))
    val projectId = WireDefault(0.U(8.W))
    val endOfRecord = WireDefault(false.B)

    // A key transition is selected from TOS-1 and writes its target into TOS.
    val parentIndex =
      Mux(current.stackPointer === 0.U, 0.U, current.stackPointer - 1.U)
    (stackPointerWidth - 1, 0)
    val parentState = current.stack(parentIndex)
    val tosIndex = current.stackPointer(stackPointerWidth - 1, 0)
    val keyTransitions = edges.map { case (parent, keyId, _) =>
      (symbol.keyId === keyId.U(keyIdWidth.W)) && (parentState === parent.U(
        stateWidth.W
      ))
    }
    val hasTransition = keyTransitions.foldLeft(false.B)(_ || _)
    val targetState = Mux1H(keyTransitions, edges.map(_._3.U(stateWidth.W)))
    val terminalMatches = terminalStates.map { case (id, _) =>
      current.stack(tosIndex) === id.U(stateWidth.W)
    }
    val isTerminal = terminalMatches.reduce(_ || _)

    // When a valid symbol exists within the entries.
    when(symbolValid) {
      // if the symbol is either '{' or '[' and the stackpointer is not at the maxDepth. We can move the stackpointer and add a new entry of 0.
      when(
        symbol.kind === SymbolKind.ObjOpen || symbol.kind === SymbolKind.ArrOpen
      ) {
        when(current.stackPointer < maxDepth.U) {
          next.stackPointer := current.stackPointer + 1.U
          next.stack(
            (current.stackPointer + 1.U)(stackPointerWidth - 1, 0)
          ) := 0.U
        }
      }
        // When the symbol is a key and the stackpointer is further along that 0th index.
        .elsewhen(symbol.kind === SymbolKind.Key) {
          when(current.stackPointer =/= 0.U) {
            next.stack(tosIndex) := Mux(
              hasTransition,
              targetState,
              0.U
            ) // If theres a valid transition we set the top of the stack to targetState.
            // If theres a transition and the the targetState is one of the terminalStates we have found a match.
            when(
              hasTransition && terminalStates
                .map { case (id, _) => targetState === id.U(stateWidth.W) }
                .foldLeft(false.B)(_ || _)
            ) {
              next.found := true.B
            }
          }
        }
        // If the symbol is a colon and the it is terminal, we know a value has to be captured. Thus set projecting and colon address.
        .elsewhen(symbol.kind === SymbolKind.Colon) {
          when(isTerminal) {
            next.projecting := true.B
            next.colonAddress := symbol.addr
            next.attributeId := Mux1H(
              terminalMatches,
              terminalStates.map(_._2.U(8.W))
            )
          }
        }
        // If the symbol is a comma and we're currently looking in a value, it means we're done looking sp we can set the project flag. And calculate the value size.
        .elsewhen(symbol.kind === SymbolKind.Comma) {
          when(current.projecting) {
            project := true.B
            projectAddress := current.colonAddress
            projectLength := symbol.addr - current.colonAddress
            projectId := current.attributeId
            next.projecting := false.B
          }
          // If the stackpointer is not 0, we inject a zero in the stack.
          when(current.stackPointer =/= 0.U) {
            next.stack(tosIndex) := 0.U
          }
          // If the symbol is either '}' or ']' and we're currently projecting, we set the projecting falg false and project true and calculate value length.
        }
        .elsewhen(
          symbol.kind === SymbolKind.ObjClose || symbol.kind === SymbolKind.ArrClose
        ) {
          when(current.projecting) {
            project := true.B
            projectAddress := current.colonAddress
            projectLength := symbol.addr - current.colonAddress
            projectId := current.attributeId
            next.projecting := false.B
          }
          // If the stackpointer is progressed, we remove one from the stackpointer.
          // Unless the stackpoiter is already at 1 in which the program is done.
          when(current.stackPointer =/= 0.U) {
            next.stackPointer := current.stackPointer - 1.U
            when(current.stackPointer === 1.U) {
              next.done := true.B
              endOfRecord := true.B
            }
          }
        }
    }

    (next, project, projectAddress, projectLength, projectId, endOfRecord)
  }

  // Two concurrent symbols are processed. The valid entry, says within the entries are there a valid symbol.
  val valid0 = io.in.valid && io.in.bits.count > 0.U
  val valid1 = io.in.valid && io.in.bits.count > 1.U
  val (after0, project0, address0, length0, id0, eor0) =
    transition(state, io.in.bits.entries(0), valid0)
  val (after1, project1, address1, length1, id1, eor1) =
    transition(after0, io.in.bits.entries(1), valid1)

  // AttributeLoader accepts one descriptor per cycle. If both PDA stages
  // finish projections together, retain the second until the loader accepts it.
  val firstProject = project0 || project1
  val firstAddress = Mux(project0, address0, address1)
  val firstLength = Mux(project0, length0, length1)
  val firstId = Mux(project0, id0, id1)
  val hasSecondProject = project0 && project1

  io.descriptor.valid := pendingDescriptor || firstProject
  io.descriptor.bits.attributeId := Mux(
    pendingDescriptor,
    pendingDescriptorBits.attributeId,
    // Each lane finishes the capture held in its input state.
    Mux(project0, state.attributeId, after0.attributeId)
  )
  io.descriptor.bits.startAddress := Mux(
    pendingDescriptor,
    pendingDescriptorBits.startAddress,
    firstAddress
  )
  io.descriptor.bits.length := Mux(
    pendingDescriptor,
    pendingDescriptorBits.length,
    firstLength
  )

  // Do not consume another batch while a second descriptor is buffered. When
  // producing the first descriptor, accept input only if the loader accepts it.
  io.in.ready := !pendingDescriptor && (!firstProject || io.descriptor.ready)
  when(io.in.fire) {
    state := after1
    when(hasSecondProject) {
      pendingDescriptor := true.B
      pendingDescriptorBits.attributeId := after0.attributeId
      pendingDescriptorBits.startAddress := address1
      pendingDescriptorBits.length := length1
    }
  }
  when(io.descriptor.fire && pendingDescriptor) {
    pendingDescriptor := false.B
  }

  // Delay end-of-record until any second descriptor from that input batch has
  // been accepted by the loader.
  when(io.in.fire) {
    pendingEndOfRecord := (eor0 || eor1) && hasSecondProject
  }
  io.endOfRecord := (io.in.fire && (eor0 || eor1) && !hasSecondProject) ||
    (io.descriptor.fire && pendingDescriptor && pendingEndOfRecord)

  io.found := state.found
  io.done := state.done
  io.depth := state.stackPointer
}
