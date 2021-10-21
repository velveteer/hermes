# Hermes 

A Haskell interface over the [simdjson](https://github.com/simdjson/simdjson) C++ library for decoding JSON documents. Hermes, messenger of the gods, was the great-grandfather of Jason, son of Aeson.

## Overview

This library exposes functions that can be used to write decoders for JSON documents using the simdjson On Demand API. From the simdjson On Demand design documentation:

> Good applications for the On Demand API might be:

> You are working from pre-existing large JSON files that have been vetted. You expect them to be well formed according to a known JSON dialect and to have a consistent layout. For example, you might be doing biomedical research or machine learning on top of static data dumps in JSON.

> Both the generation and the consumption of JSON data is within your system. Your team controls both the software that produces the JSON and the software the parses it, your team knows and control the hardware. Thus you can fully test your system.

> You are working with stable JSON APIs which have a consistent layout and JSON dialect.

With this in mind, `Data.Hermes` parsers can potentially decode Haskell types faster than traditional `Data.Aeson.FromJSON` instances, especially in cases where you only need to decode a subset of the document. This is because `Data.Aeson.FromJSON` must convert the entire document into a DOM, which means memory increases linearly with the input size. The On Demand API does not have this constraint because it iterates over the JSON string in memory without parsing the entire document up front. 

## Usage

This library does _not_ offer a Haskell API over the entire simdjson On Demand API. It currently binds only to what is needed for writing basic decoders. You can see the benchmarks for example usage. All parsers operate in `IO` and will re-throw errors from the underlying simdjson calls.

## Benchmarks
The benchmarks are testing full decoding of a large-ish (12 MB) JSON array of objects, and then a partial decoding of Twitter status objects to highlight the on-demand benefits.

![](./hermesbench/bench.svg)

## Performance Tips

* Decode to `Text` instead of `String` wherever possible!
* Decode to `Int` or `Double` instead of `Scientific` if you can.
* You can improve performance by holding onto your own `HermesEnv` and using `decodeWith` instead of `decode`. This ensures the simdjson instances are allocated by the caller who can hold a reference to them, allowing re-use and preventing the garbage collector from running their finalizers. `decode` creates and destroys the simdjson instances every time it runs, which adds a performance penalty.

## Limitations

Because the On Demand API uses a forward-only iterator (except for object fields), you must be mindful to not access values out of order. In other words, you should not hold onto a `Value` to parse later since the iterator may have already moved beyond it. 

Further work is coming to wrap the `simdjson::dom` API, which should allow walking the DOM in any order you want, but at the expense of parsing the entire document into a DOM. 

Because the On Demand API does not validate the entire document upon creating the iterator (besides UTF-8 validation and token scanning), it is possible to parse an invalid JSON document but not realize it until later. Keep this is mind if you need the entire document to be validated up front, in which case the simdjson DOM API is a better fit for you.

> The On Demand approach is less safe than DOM: we only validate the components of the JSON document that are used and it is possible to begin ingesting an invalid document only to find out later that the document is invalid. Are you fine ingesting a large JSON document that starts with well formed JSON but ends with invalid JSON content?
