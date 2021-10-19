# simdjson 

An Aeson-like interface over the [simdjson](https://github.com/simdjson/simdjson) C++ library.

## Overview

This library exposes a `FromJSON` typeclass that can be used to write decoders for JSON documents using the On Demand API from simdjson. From the simdjson On Demand design documentation:

> Good applications for the On Demand API might be:

> You are working from pre-existing large JSON files that have been vetted. You expect them to be well formed according to a known JSON dialect and to have a consistent layout. For example, you might be doing biomedical research or machine learning on top of static data dumps in JSON.

> Both the generation and the consumption of JSON data is within your system. Your team controls both the software that produces the JSON and the software the parses it, your team knows and control the hardware. Thus you can fully test your system.

> You are working with stable JSON APIs which have a consistent layout and JSON dialect.

With this in mind, `Data.SIMDJSON.FromJSON` instances can potentially decode Haskell types faster than traditional `Data.Aeson.FromJSON` instances, especially in cases where you only need to decode a subset of the document. 

## Usage

This library does _not_ offer a Haskell API over the entire simdjson On Demand API. It currently binds only to what is needed for writing `FromJSON` instances. If your Haskell type has an instance of `Data.SIMDJSON.FromJSON`, then you can decode a strict `ByteString` with `decode` and `decodeWith`. 

## Benchmarks
The benchmarks are testing full decoding of a large-ish (12 MB) JSON array of objects, and then a partial decoding of Twitter user objects to highlight the on-demand benefits.

![](bench.svg)

## Performance Tips

* Decode to `Text` instead of `String` wherever possible!
* You can improve performance by holding onto your own `SIMDJSONEnv` and using `decodeWith` instead of `decode`. This ensures the simdjson instances are allocated by the caller who can hold a reference to them, which prevents the garbage collector from running their finalizers. `decode` creates and destroys the simdjson instances every time it runs, which adds a performance penalty.
