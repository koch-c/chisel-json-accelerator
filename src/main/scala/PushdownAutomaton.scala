import chisel3._
import chisel3.util._

/** PDA state used to calculate two ChannelFifo symbol transitions per cycle. */
class PdaState(pathLength: Int, maxDepth: Int) extends Bundle {
  private val stateWidth = math.max(1, log2Ceil(pathLength + 1))
  private val stackPointerWidth = math.max(1, log2Ceil(maxDepth + 1))

  // Slot 0 is the root state `$`; each open bracket pushes a new TOS slot.
  val stackPointer = UInt(stackPointerWidth.W)
  val stack = Vec(maxDepth + 1, UInt(stateWidth.W))
  val projecting = Bool()
  val colonAddress = UInt(12.W)
  val found = Bool()
  val done = Bool()
}

/**
  * Pushdown automaton for one fixed key path, using the interfaces from the
  * Lexer and attribute-loader branches. The Lexer recognizes key strings and
  * emits `JsonSymbol`s; this module only tracks path state and JSON nesting.
  */
class PushdownAutomaton(config: JsonPathConfig) extends Module {
  private val path = config.path
  private val maxDepth = config.maxDepth
  private val keyIds = config.keyIds
  private val keyIdWidth = math.max(1, log2Ceil(keyIds.size))
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

  val state = RegInit(0.U.asTypeOf(new PdaState(path.length, maxDepth)))
  val pendingDescriptor = RegInit(false.B)
  val pendingDescriptorBits = Reg(new AttributeDescriptor)
  val pendingEndOfRecord = RegInit(false.B)

  private def transition(
    current: PdaState,
    symbol: JsonSymbol,
    symbolValid: Bool
  ): (PdaState, Bool, UInt, UInt, Bool) = {
    val next = Wire(new PdaState(path.length, maxDepth))
    next := current
    val project = WireDefault(false.B)
    val projectAddress = WireDefault(0.U(12.W))
    val projectLength = WireDefault(0.U(13.W))
    val endOfRecord = WireDefault(false.B)

    // A key transition is selected from TOS-1 and writes its target into TOS.
    val parentIndex = Mux(current.stackPointer === 0.U, 0.U, current.stackPointer - 1.U)
      (stackPointerWidth - 1, 0)
    val parentState = current.stack(parentIndex)
    val tosIndex = current.stackPointer(stackPointerWidth - 1, 0)
    val keyTransitions = path.zipWithIndex.map { case (key, index) =>
      (symbol.keyId === keyIds(key).U(keyIdWidth.W)) && (parentState === index.U)
    }
    val hasTransition = keyTransitions.reduce(_ || _)
    val targetState = Mux1H(keyTransitions, path.indices.map(i => (i + 1).U))

    when(symbolValid) {
      when(symbol.kind === SymbolKind.ObjOpen || symbol.kind === SymbolKind.ArrOpen) {
        when(current.stackPointer < maxDepth.U) {
          next.stackPointer := current.stackPointer + 1.U
          next.stack((current.stackPointer + 1.U)(stackPointerWidth - 1, 0)) := 0.U
        }
      }.elsewhen(symbol.kind === SymbolKind.Key) {
        when(current.stackPointer =/= 0.U) {
          next.stack(tosIndex) := Mux(hasTransition, targetState, 0.U)
          when(hasTransition && targetState === path.length.U) {
            next.found := true.B
          }
        }
      }.elsewhen(symbol.kind === SymbolKind.Colon) {
        when(current.stack(tosIndex) === path.length.U) {
          // The paper defines the first colon after a terminal path as start.
          next.projecting := true.B
          next.colonAddress := symbol.addr
        }
      }.elsewhen(symbol.kind === SymbolKind.Comma) {
        when(current.projecting) {
          project := true.B
          projectAddress := current.colonAddress
          projectLength := symbol.addr - current.colonAddress
          next.projecting := false.B
        }
        when(current.stackPointer =/= 0.U) {
          next.stack(tosIndex) := 0.U
        }
      }.elsewhen(symbol.kind === SymbolKind.ObjClose || symbol.kind === SymbolKind.ArrClose) {
        when(current.projecting) {
          project := true.B
          projectAddress := current.colonAddress
          projectLength := symbol.addr - current.colonAddress
          next.projecting := false.B
        }
        when(current.stackPointer =/= 0.U) {
          next.stackPointer := current.stackPointer - 1.U
          when(current.stackPointer === 1.U) {
            next.done := true.B
            endOfRecord := true.B
          }
        }
      }
    }

    (next, project, projectAddress, projectLength, endOfRecord)
  }

  // A FIFO batch may contain one or two symbols. The second sees the first's
  // state updates, just like the two serial s_proc stages in the paper.
  val valid0 = io.in.valid && io.in.bits.count > 0.U
  val valid1 = io.in.valid && io.in.bits.count > 1.U
  val (after0, project0, address0, length0, eor0) =
    transition(state, io.in.bits.entries(0), valid0)
  val (after1, project1, address1, length1, eor1) =
    transition(after0, io.in.bits.entries(1), valid1)

  // AttributeLoader accepts one descriptor per cycle. If both PDA stages
  // finish projections together, retain the second until the loader accepts it.
  val firstProject = project0 || project1
  val firstAddress = Mux(project0, address0, address1)
  val firstLength = Mux(project0, length0, length1)
  val hasSecondProject = project0 && project1

  io.descriptor.valid := pendingDescriptor || firstProject
  io.descriptor.bits.attributeId := Mux(
    pendingDescriptor,
    pendingDescriptorBits.attributeId,
    config.attributeId.U(8.W)
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
      pendingDescriptorBits.attributeId := config.attributeId.U(8.W)
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
