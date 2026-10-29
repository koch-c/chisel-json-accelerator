# JSONPath Implementation Plan

We split today's `Parser` into a **lexer** and a **path matcher**, joined by one fixed interface. All four of us agree that interface on day 1, and after that everyone works in parallel against stubs and a software model, never against each other's unfinished code.

## 1. Scope

| Phase | Path syntax | Example | Hardware cost |
|---|---|---|---|
| 1 | Root + key steps | `$.system.firmware.version` | One "matched steps" counter, key comparator |
| 2 | Array index, wildcards | `$.system.sensors[1].value`, `$.system.sensors[*].name` | Element counter per open array |
| 3 | Path set at runtime over UART | first line sent = path, then the document | Path registers + small path-string FSM |
| Stretch | Recursive descent | `$..temperature` | Match-state stack, one entry per depth |
| Out of scope | Filters, slices, unions | `[?(@.price<10)]` | Needs buffering of already-streamed siblings |

Today's behaviour ("find `temperature` anywhere") is really `$..temperature`, so it comes back in the stretch phase. Until then the old `Parser` stays in the repo and on the board.

## 2. Architecture

```
UART RX ─► Streamer ─► JsonLexer ──JsonEvent──► PathMatcher ──selected bytes──► Output ─► UART TX
           (8 B words)  (owner A)                (owner B)  ▲                    (owner D)
                                                            │
                                         PathConfig ────────┘
                                 (compile time: JsonPath.parse, owner C
                                  runtime: PathLoader over UART, owner D)
```

- **JsonLexer** knows JSON syntax: strings, escapes, depth, and whether each byte is part of a key or a value. It knows nothing about paths.
- **PathMatcher** knows paths: it reads events and the `PathConfig` and marks the bytes that belong to a matching value. It never looks at quotes or escapes.
- Every link is `Decoupled`, so UART TX backpressure still stalls the whole chain, as it does today.
- Exact paths (phases 1–2) need no stack. Step `k` can only match at depth `k+1`, so one "steps matched so far" register plus a per-depth array index is enough. `..` is what forces a stack.

## 3. Day-1 contract (all four, ~2 hours, one PR)

These files go in `src/main/scala/jsonpath/` and are frozen once merged. Any later change is a PR that all four review.

```scala
/** One input byte, annotated by the lexer. */
class JsonEvent(maxDepth: Int) extends Bundle {
  val byte     = UInt(8.W)
  val depth    = UInt(log2Ceil(maxDepth + 1).W) // depth of the innermost open container
  val inArray  = Bool() // innermost open container is an array
  val keyStart = Bool() // opening quote of an object key
  val keyByte  = Bool() // a byte of the key text (quotes excluded, escapes left raw)
  val keyEnd   = Bool() // closing quote of an object key
  val valStart = Bool() // first byte of a value: digit, letter, '"', '{' or '['
  val valByte  = Bool() // byte belongs to a value, nested containers included
  val valDone  = Bool() // ',' '}' or ']' that ends the current value (not part of it)
  val elemNext = Bool() // ',' between array elements: the index goes up by one
}

object StepKind extends ChiselEnum { val Key, Index, AnyIndex = Value }

class PathStep(maxKeyLen: Int) extends Bundle {
  val kind   = StepKind()
  val key    = Vec(maxKeyLen, UInt(8.W))
  val keyLen = UInt(log2Ceil(maxKeyLen + 1).W)
  val index  = UInt(16.W)
}

class PathConfig(maxSteps: Int, maxKeyLen: Int) extends Bundle {
  val steps    = Vec(maxSteps, new PathStep(maxKeyLen))
  val numSteps = UInt(log2Ceil(maxSteps + 1).W)
}

/** A byte of a matched value. */
class MatchedByte extends Bundle {
  val byte  = UInt(8.W)
  val first = Bool() // first byte of a new match
  val last  = Bool() // last byte of that match
}
```

The same PR also holds:

1. Stub modules `JsonLexer` and `PathMatcher` with the real IO and outputs tied to zero, so `sbt test` compiles from day 1.
2. The Scala signature `JsonPath.parse(path: String): Seq[Step]`, with `Step` = `Key(name) | Index(n) | AnyIndex`, plus `JsonPath.toConfig`.
3. Hand-written expected events for the `{ "temperature": 25, "humidity": 60 }` example, so A and B can test before C's model exists.
4. Empty spec files: `JsonLexerSpec`, `PathMatcherSpec`, `JsonPathSpec`, `EndToEndSpec`.

## 4. Who owns what

| Owner | Workstream | Files | Tested against |
|---|---|---|---|
| A | **Lexer.** Move the string/escape/depth logic out of `Parser.scala`. Add a 1-bit-per-depth object/array stack and an "expecting key" flag, emit one `JsonEvent` per byte, keep 1 byte/cycle. Flag depth overflow and unbalanced brackets. | `jsonpath/JsonLexer.scala`, `JsonLexerSpec.scala` | Day-1 fixtures, then C's reference lexer on every document in `ParserTest` |
| B | **Path matcher.** Key comparison per step, matched-step counter, array index counters, `AnyIndex`. Emits `MatchedByte` with `first`/`last`. Later the `..` stack. | `jsonpath/PathMatcher.scala`, `PathMatcherSpec.scala` | Hand-built event sequences poked straight into the matcher, never through the lexer |
| C | **Software side and verification.** `JsonPath.parse` and `toConfig`. A test-only reference lexer that turns a string into the expected events, a reference JSONPath evaluator (golden model), and a random JSON + path generator. Owns `EndToEndSpec`. | `jsonpath/JsonPath.scala`, `test/.../JsonModel.scala`, `EndToEndSpec.scala` | Pure Scala unit tests, no hardware needed |
| D | **Integration and board.** Rewire `Accelerator` to Streamer → Lexer → Matcher. Output format for several matches. `PathLoader` that reads the path from UART and fills `PathConfig`. FPGA scripts, README, demo. | `Accelerator.scala`, `jsonpath/PathLoader.scala`, `fpga/*`, `AcceleratorTest.scala` | Stubs until M2, then the real modules |

C's reference lexer is what A and B test against, so C delivers it first, within 3 days. The hand-written fixtures in the contract PR cover the gap.

## 5. Milestones

1. **M0, day 1.** Contract PR merged, stubs compile, CI green.
2. **M1, each module alone.** Lexer events equal the model on all current test documents. Matcher passes phase-1 paths on hand-built events. `JsonPath.parse` and the golden model pass their unit tests.
3. **M2, first integration (owner D).** `$.system.firmware.version` returns `"1.2.3"` end to end in simulation. Old `Parser` stays as the board default.
4. **M3, phase 2.** Indices and `[*]` work. Several matches are reported over UART. `EndToEndSpec` runs about 1000 random document/path pairs against the golden model.
5. **M4, on the board.** Path sent over UART at runtime on the Nexys 4 DDR, demo and README updated. The new pipeline replaces `Parser` in `Accelerator`.
6. **Stretch.** `..` via a match stack (B), several paths in parallel (B + D), more than one byte per cycle in the lexer (A).

## 6. Working rules

- **One branch per person, small PRs into `main`.** CI already runs `sbt test` on every push, and nothing merges red.
- **Test against stubs and the model, never against a teammate's work in progress.** That is the whole point of the contract.
- **The contract is frozen.** Need a new field? Open a PR touching only `jsonpath/*Bundle*`, tag everyone, merge the same day.
- **D merges integration.** A, B and C don't edit `Accelerator.scala`; D doesn't edit the lexer or matcher internals.
- **One Kanban card per sub-task,** each with an owner and a "done = test X passes" line.
- **15-minute sync twice a week:** what merged, what's blocked, whether the contract needs to change.
- **Before the refactor starts,** commit or drop the uncommitted changes to `Parser.scala` and `ParserTest.scala` on `main`, so A starts from a clean base.

## 7. Open questions for the kickoff

- [ ] Size limits: `maxSteps` (8?), `maxKeyLen` (16?), `maxDepth` (32 or 16?). Check the LUT cost on the Artix-7.
- [ ] Output format for several matches. One value per line, then `FOUND <n>`?
- [ ] Runtime path over UART in this course, or compile-time paths only?
- [ ] Is matching escaped keys byte for byte, as today, good enough?
- [ ] Who takes the `..` stretch if B is still busy with phase 2?
