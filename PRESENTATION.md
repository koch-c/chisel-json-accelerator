# Chisel JSON Accelerator

## The problem

JSON is used everywhere—from web APIs to cloud services and sensor data.

Processing large amounts of JSON takes time and computing resources because a general-purpose processor must inspect the data one character at a time.

## Our idea

Move part of the JSON-processing workload into specialized hardware.

Instead of parsing an entire document in software, the hardware reads the incoming data as a stream and looks directly for information we care about.

```text
JSON data  →  Hardware accelerator  →  Match found
```

## The MVP

Our first version focuses on one simple task: finding a specific key in a JSON object.

Example input:

```json
{ "temperature": 25, "humidity": 60 }
```

The hardware searches for `temperature`. When it finds the key, it activates an output such as an LED.

This small demonstration lets us prove the core idea before supporting more complex JSON documents.

## A hardware generator

The project is not limited to one fixed circuit. It is a generator that creates specialized hardware for a chosen JSON key.

```text
Selected key  →  Hardware generator  →  Specialized accelerator
```

For example, selecting `temperature` generates a circuit that searches for `temperature`. Selecting `humidity` generates a different circuit for that key.

This allows the same design to produce a family of accelerators for different use cases.

## The bigger vision

Future versions could:

- search for several keys at once;
- extract values directly from the data stream;
- understand nested JSON objects;
- process several characters in parallel; and
- filter data before it reaches the CPU.

## Key takeaway

This project explores how a common software task can become a specialized streaming hardware task.

The MVP is intentionally simple: detect one JSON key and show the result on an LED. Its purpose is to demonstrate the idea clearly and create a foundation for a faster and more capable JSON accelerator.
