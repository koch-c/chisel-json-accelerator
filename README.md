# JAW - JSON Accelerator in Wires
This repo contains the Chisel code for a hardware-accelerated JSON parser that accepts [`JSONPath`](https://goessner.net/articles/JsonPath/) queries.

## Motivation

We wanted to create a hardware generator that could solve a problem we could all relate to. After a little bit of thinking we agreed to work on creating an accelerator that could parse JSON, since we had all worked with JSON many times before. While it might sound a bit boring to create an accelerated parser, it is nevertheless something that has a real use case. 

For example, on [Cloudflare Radar](https://radar.cloudflare.com/traffic) we can see that JSON constitutes a large share of HTTP responses on Cloudflare's servers worldwide. On the 17th of November, 2026, JSON constituted `34.7%` of all HTTP responses over the past 3 months. JSON far surpasses any other kind of MIME type in HTTP responses. 
[![Cloudflare Radar HTTP requests by response content type, showing JSON at 35.3%](images/cloudflare-radar.png)](https://radar.cloudflare.com/traffic)

Google have also investigated what operations large datacenters spend a lot of resources. In the paper [*Profiling a Warehouse-Scale Computer*](https://ieeexplore.ieee.org/document/7478434/) (Kanev et al., IEEE Micro 2016) found that **22–27% of CPU cycles** in Google's fleet were spent on infrastructure operations, including protocol-buffer management. The authors of the paper identify serialization and deserialization as promising hardware-acceleration targets.

## Design

Our design is based on the architecture introduced in [*SPEAR-JSON*](https://ieeexplore.ieee.org/document/10296352) (Hahn et al., FPL 2023). We chose to follow the design of the paper for a number of reasons:
- **It is a hardware generator**. This design is meant to support queries that run for a long period of time. Instead of accepting arbitrary `JSONPath` queries, the design takes the query as a parameter and generates dedicated HDL tailored to the query. 
- **It is minimal**. It strives to create an efficient and small implementation that does not use a lot of FPGA resources.
- **It is a part in a larger machinery**. The implementation is meant to be used in a larger data pipeline that enables the implementation to ingest, parse, and return results. In our design we connect the design to our computer using UART. 

The paper introduces a design that utilizes the sparsity of JSON to increase parsing throughput. Essentially it uses a `path encoder` to find relevant symbols in the JSON file, after which a `pushdown automata` matches the symbols with whatever `JSONPath` query the `FPGA` has been synthesized to parse. Consider the example below where the `JSONPath` query is: `$.(id|user.name)`

![SPEAR-JSON Figure 1 Parser flow](images/SPEAR-JSON-figure-1.png)
Figure 1 from the [*SPEAR-JSON*](https://ieeexplore.ieee.org/document/10296352) paper.

Instead of considering every character of the JSON equally, we use the `path encoder` to first find all the symbols of the JSON (the small gray boxes below the JSON string). Symbols are essentially just structural characters of JSON like `,`, `}`, `:`, etc. and keys in a JSON. These symbols are fed to the `pushdown automata`, and the automata compare them one at a time with the prefix of the query the FPGA has been synthesized to match. Every time there is a match between a symbol and a prefix in the query, the `pushdown automata` pushes the prefix to its stack. In this way the `pushdown automata` can keep track of how much of a query has been matched on some path.  

## Architecture

The overall architecture and flow of bits is shown in the mermaid diagram below:

```mermaid
---
config:
  layout: elk
  elk:
    mergeEdges: true
---
flowchart LR
    RX[UartRx] --> INPUT[InputFIFO]
    INPUT -->|8-byte words| LEXER[Parallel Lexer]
    LEXER -->|Up to 8 symbols per batch| FIFO[ChannelFifo]
    FIFO -->|Up to 2 symbols per cycle| PDA[PushdownAutomaton]
    PDA -->|Value Info| LOADER[AttributeLoader]
    INPUT --> BUFFER[InputBuffer]
    BUFFER -->|Original value bytes| LOADER
    LOADER --> TX[UartTx]
```

- `UartRx` receives the JSON bytes from our computer over UART and passes them to the `InputFIFO`.
- `InputFIFO` packs the incoming bytes into 8-byte words, adapting the byte-by-byte UART input to the wider input of the `Lexer`. The same words are also written to the `InputBuffer`, so we can retrieve the original values later.
- `Lexer` examines all eight bytes of a word in parallel and can accept one word per cycle when unstalled. It carries string and escape state across word boundaries, then emits structural characters and recognized keys as symbols with their byte positions. Each output batch contains up to eight symbols in input order; words with no symbols produce no batch.
- `ChannelFifo` buffers symbols, accepting batches of up to eight from the `Lexer` and delivering up to two symbols per cycle to the `PushdownAutomaton`. This absorbs bursts of symbols so the lexer only has to stall when the FIFO is full.
- `PushdownAutomaton` uses a stack to keep track of the current path through the JSON and compares it with the query the FPGA has been synthesized to match. When it finds a matching value, it sends the value's location to the `AttributeLoader`.
- `InputBuffer` stores the original JSON bytes while the symbols move through the parser. The `AttributeLoader` reads from this buffer to retrieve a matching value without having to reconstruct it from symbols.
- `AttributeLoader` uses the location from the `PushdownAutomaton` to read the matching value from the `InputBuffer`. It then passes the original value bytes to `UartTx`.
- `UartTx` sends the extracted value bytes back to our computer over UART, completing the flow.
<!-- TODO: Document the NumberParser and NumberSerializer output path. -->

### Parameterization
The hardware generator takes a `JsonPathConfig` with these parameters:

| Parameter | Explanation |
|---|---|
| `paths` | The `JSONPath` query the generated hardware matches. |
| `maxDepth` | Maximum JSON nesting the matcher tracks, including the root (default: 8). |
| `attributeId` | **WTF is this. We should remove this.** First byte of every numeric frame; shared by all paths. Not sent in `Raw` mode. |
| `valueType` | One output format for all paths: `Raw`, `Number`, `Int32`, or `Int64`. |

| `valueType` | Explanation |
|---|---|
| `Raw` | Original matched bytes, starting with the colon. |
| `Number` | Decimal number as a sign, magnitude, and base-10 exponent, e.g. negative `125 × 10⁻¹` for `-12.5`. |
| `Int32` | Signed 32-bit integer. |
| `Int64` | Signed 64-bit integer. |

JAW currently supports rooted object-key paths such as `$.user.name` and unions of explicit paths such as `$.(id | user.name)`, configured as `Seq(Seq("id"), Seq("user", "name"))`. Matches are emitted in input order. 

<!-- TODO, extend this section once code has been updated -->

### Top Level Modules
All the modules except the `UartTx/Rx` modules are wired together in the `Accelerator` module. The `AcceleratorTop` module wires the `Accelerator` module together with `UartTx/Rx`. The bulk of the integration test for the accelerator are written for the `Accelerator` module, while the test written for `AcceleratorTop` mostly test the Uart support. We have split up the modules like this, because it is slow to simulate Uart, and by isolating the Uart support to a different module, we can efficiently test the accelerator without interacting with Uart and without introducing unnecessary wires. 

## Agile Development
We of courses implemented JAW with an Agile mindset. Every Thursday after class we had our weekly standups, where we talked about what tickets we had completed, what was blocking us, and we decided what we wanted to work on for the next spring. We organized ourselves through GitHub Projects in a kanban board

![Kanban board on Github Projects](images/kanban-board.png)

### CI
We used GitHub Actions to facilitate continuous integration. We started with a simple action that ran all chisel tests:
```yaml
jobs:
  test:
    # Shortened
      - name: Run tests
        run: sbt test
```
But we later extended CI to also apply formatting checks and ensuring that the chisel code could compile to SystemVerilog:
```yaml
jobs:
  test:
    # Shortened
      - name: Check Scala formatting
        run: sbt "scalafmtCheckAll; scalafmtSbtCheck"

      - name: Run tests
        run: sbt test

      - name: Generate top-level Verilog
        run: |
          sbt "runMain GenerateAcceleratorTop"
          test -s generated/AcceleratorTop.sv
```

### Initial Design Process

We adopted the mindset, **build, deploy, repeat** and in the beginning of the project we pushed for an MVP. Our first MVP was very simple. It essentially streamed a simple JSON string stored on chip, and lid an LED depending on whether or not the JSON contained the key matching a basic `JSONPath` query. It accepted JSON strings like
```json
{ "temperature": 25, "humidity": 60 }
```
We very quickly designed, synthesized, implemented and ran the first MVP on the FPGA board. When we had accomplished the first *M*VP we moved on to a MVP 2. This MVP had UART support, so we could stream arbitrarily large JSON files to the FPGA board, and the board would reply back with value matching the configured `JSONPath` query. We also extended our support for more complex `JSONPath` queries such that we could also detect keys in nested objects like:

```json
{"system":{"board":"Nexys 4 DDR","location":"lab bench","firmware":{"version":"1.2.3","build":20261027},"sensors":[{"name":"humidity","value":60},{"name":"pressure","value":1013},{"temperature":"twenty-five degrees"}],"notes":"UART streaming JSON test with nested objects, an array, and a sufficiently long message to cross many eight-byte blocks"},"enabled":true}     
```

Once we had completed the MVP 2 and made it run on the FPGA board, we processed to implement the final version of JAW. 
<!-- TODO: describe how we first designed the first part followed by the two parsers and filtering logic -->

# AI and LLM Usage
We used LLM and coding harnesses to assist us in the development of JAW. We primarily focus on the architecture of JAW and created implementation plans. We then made agents create an implementation of our architecture plan, and we iterated over the generated code many times together with the agent, so the end result was efficient and was an implementation of what we required. When the agent was incapable of delivering, we implemented parts of the chisel code ourselves. We adopted this approach to try and learn the future of chip design, where agents increasingly will write the code for us, and the chip designers must be critic and validate the generated code.
