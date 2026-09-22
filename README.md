# Gemini Pitch

## 1. Hardware-Accelerated JSON Parsing / Validation Engine

- _The Problem:_ Modern microservices and cloud infrastructure pass massive volumes of JSON telemetry, API requests, and logs. CPUs spend immense amounts of cycles sequentially branching, checking quotes, and scanning strings to parse payloads.
- _The Accelerator:_ A parallel stream-based JSON parser that evaluates incoming byte streams simultaneously. It can identify structural tokens ({, }, [, ], :, ,) and extract specific key-value pairs on the fly without building heavy DOM trees in memory.
- _Why Chisel Fits:_ You can write a parameterized generator that takes the target key string length and maximum expected nesting depth as Scala constructor parameters, emitting optimized finite state machines (FSMs) and parallel comparator trees.

# MVP

A parameterized Chisel-generated streaming JSON parser that accepts a restricted JSON object, searches for one compile-time-selected key, and lights an LED when that key is found.

The JSON file used in the MVP will be

```json
{ "temperature": 25, "humidity": 60 }
```

## Post MVP features

- Multiple target keys
  - Generate parallel comparators for several keys.
  - Output one match bit per key.
- Configurable key storage
  - Store keys in registers or BRAM.
  - Change search keys at runtime without regenerating hardware.
- Nested JSON path matching
  - Generate hardware for paths such as: device.location.latitude
  - Match both keys and their nesting context.
- Parameterized parser depth
  - Generate depth counters and FSM states based on maxDepth.
  - Reject documents exceeding the configured depth.
- Parallel byte processing
  - Process 4, 8, or 16 input bytes per cycle.
  - Generate parallel structural-character detectors.
