<h1 align="left">
<img src="https://raw.githubusercontent.com/velveteer/hermes/master/wings.svg" height=25 width=50 />
hermes
</h1>
<p align="left">
<a href="https://github.com/velveteer/hermes/actions">
  <img src="https://img.shields.io/github/actions/workflow/status/velveteer/hermes/ci.yaml?branch=master&style=for-the-badge" alt="CI badge" />
</a>
<a href="https://hackage.haskell.org/package/hermes-json">
  <img src="https://img.shields.io/hackage/v/hermes-json?label=hackage&style=for-the-badge" alt="Hackage badge" />
</a>
</p>

A Haskell interface over the [simdjson](https://github.com/simdjson/simdjson) C++ library for decoding JSON documents. Hermes, messenger of the gods, was the maternal great-grandfather of Jason, son of Aeson.

## Overview

This library exposes functions that can be used to write decoders for JSON documents using the simdjson On Demand API. From the simdjson On Demand design documentation:

> Good applications for the On Demand API might be:

> You are working from pre-existing large JSON files that have been vetted. You expect them to be well formed according to a known JSON dialect and to have a consistent layout. For example, you might be doing biomedical research or machine learning on top of static data dumps in JSON.

> Both the generation and the consumption of JSON data is within your system. Your team controls both the software that produces the JSON and the software the parses it, your team knows and control the hardware. Thus you can fully test your system.

> You are working with stable JSON APIs which have a consistent layout and JSON dialect.

With this in mind, `Data.Hermes` parsers can decode Haskell types faster than traditional `Data.Aeson.FromJSON` instances, especially in cases where you only need to decode a subset of the document. This is because `Data.Aeson.FromJSON` converts the entire document into a `Data.Aeson.Value`, which means memory usage increases linearly with the input size. The `simdjson::ondemand` API does not have this constraint because it iterates over the JSON string in memory without constructing an intermediate tree. This means decoders are truly lazy and you only pay for what you use.

## Usage

This library does _not_ offer a Haskell API over the entire simdjson On Demand API. It currently binds only to what is needed for defining and running a `Decoder`. You can see the tests and benchmarks for example usage. `Decoder a` is a thin layer over IO that keeps some context around for better error messages. `simdjson::ondemand` exceptions will be caught and re-thrown with enough information to troubleshoot. In the worst case you may run into a segmentation fault that is not caught, which you are encouraged to report as a bug.

### Decoders

```haskell
personDecoder :: Value -> Decoder Person
personDecoder = withObject $ \obj ->
  Person
    <$> atKey "_id" text obj
    <*> atKey "index" int obj
    <*> atKey "guid" text obj
    <*> atKey "isActive" bool obj
    <*> atKey "balance" text obj
    <*> atKey "picture" (nullable text) obj
    <*> atKey "latitude" scientific obj

-- Decode a strict ByteString.
decodePersons :: ByteString -> Either HermesException [Person]
decodePersons = decodeEither $ list personDecoder
```

It looks a little like `Waargonaut.Decode.Decoder m`, just not as polymorphic. The interface is copied because it's elegant and does not rely on typeclasses. However, `hermes` does not give you a cursor to play with, the cursor is implied and is forward-only (except when accessing object fields). This limitation allows us to write very fast decoders.

### Exceptions

When decoding fails for a known reason, you will get a `Left HermesException` indicating if the error came from `simdjson` or from an internal `hermes` call. The exception contains a `DocumentError` record with some useful information, for example:
```haskell
*Main> decodeEither (withObject . atKey "hello" $ list text) "{ \"hello\": [\"world\", false] }"
Left (SIMDException (DocumentError {path = "/hello/1", errorMsg = "Error while getting value of type text. The JSON element does not have the requested type.", docLocation = "false] }", docDebug = "json_iterator [ depth : 3, structural : 'f', offset : 21', error : No error ]"}))
```

## Benchmarks
We benchmark the following operations using both `hermes-json` and `aeson` strict ByteString decoders:
* Decode an array of 1 million 3-element arrays of doubles
* Decode a very small object into a Map
* Full decoding of a large-ish (12 MB) JSON array of objects
* Partial decoding of Twitter status objects to highlight the on-demand benefits

### Specs

* GHC 9.2.1
* aeson-2.0.3.0 with text-2.0
* Intel Core i7-7500U @2.70GHz / 2x8GB RAM @LPDDR3

#### Non-threaded runtime

<!-- AUTO-GENERATED-CONTENT:START (BENCHES) -->
| Name                                                | Mean (ps)     | 2*Stdev (ps) | Allocated  | Copied    | Peak Memory |
| --------------------------------------------------- | ------------- | ------------ | ---------- | --------- | ----------- |
| All.1 Million 3-Arrays.Hermes [[Double]]            | 514149749400  | 23001383910  | 567060714  | 555767893 | 548405248   |
| All.1 Million 3-Arrays.Aeson [[Double]]             | 1909749532600 | 105016463882 | 9240071234 | 918470102 | 815792128   |
| All.Small Object to Map.Hermes Decode               | 1402062       | 89160        | 4311       | 143       | 815792128   |
| All.Small Object to Map.Aeson Lazy                  | 3149933       | 190718       | 20444      | 5         | 815792128   |
| All.Small Object to Map.Aeson Strict                | 2960649       | 197972       | 20455      | 3         | 815792128   |
| All.Full Persons Array.Ordered Keys.Hermes Decode   | 87606624200   | 882987914    | 131017990  | 61609312  | 815792128   |
| All.Full Persons Array.Ordered Keys.Aeson Lazy      | 412865060000  | 37200085522  | 1040817535 | 257682836 | 815792128   |
| All.Full Persons Array.Ordered Keys.Aeson Strict    | 294154477200  | 3203013536   | 1039500388 | 171613607 | 815792128   |
| All.Full Persons Array.Unordered Keys.Hermes Decode | 98984943000   | 3869859988   | 131482700  | 61507519  | 815792128   |
| All.Full Persons Array.Unordered Keys.Aeson Lazy    | 416241784400  | 31016972066  | 1040900869 | 257736671 | 815792128   |
| All.Full Persons Array.Unordered Keys.Aeson Strict  | 295604611200  | 13477999910  | 1040897500 | 171857398 | 815792128   |
| All.Partial Twitter.Hermes Decode                   | 380242857     | 21886584     | 331150     | 3106      | 815792128   |
| All.Partial Twitter.Aeson Lazy                      | 14210219600   | 1363261550   | 38167991   | 6912052   | 815792128   |
| All.Partial Twitter.Aeson Strict                    | 11107521750   | 697866752    | 38738747   | 4728197   | 815792128   |
|                                                     |
<!-- AUTO-GENERATED-CONTENT:END (BENCHES) -->

![](https://raw.githubusercontent.com/velveteer/hermes/master/hermesbench/bench.svg)

#### Threaded runtime

<!-- AUTO-GENERATED-CONTENT:START (BENCHES_THREADED) -->
| Name                                                | Mean (ps)     | 2*Stdev (ps) | Allocated  | Copied    | Peak Memory |
| --------------------------------------------------- | ------------- | ------------ | ---------- | --------- | ----------- |
| All.1 Million 3-Arrays.Hermes [[Double]]            | 541920265800  | 29342742108  | 567061829  | 555826856 | 548405248   |
| All.1 Million 3-Arrays.Aeson [[Double]]             | 1953230810200 | 126855715860 | 9240069761 | 919129263 | 815792128   |
| All.Small Object to Map.Hermes Decode               | 1472371       | 91562        | 4311       | 143       | 815792128   |
| All.Small Object to Map.Aeson Lazy                  | 3096479       | 193244       | 20444      | 6         | 815792128   |
| All.Small Object to Map.Aeson Strict                | 2986724       | 200024       | 20456      | 5         | 815792128   |
| All.Full Persons Array.Ordered Keys.Hermes Decode   | 90311085300   | 7526254196   | 130369345  | 60862744  | 815792128   |
| All.Full Persons Array.Ordered Keys.Aeson Lazy      | 422369991600  | 38053428096  | 1040823664 | 257776459 | 815792128   |
| All.Full Persons Array.Ordered Keys.Aeson Strict    | 300466422000  | 3072876168   | 1039500630 | 171704298 | 815792128   |
| All.Full Persons Array.Unordered Keys.Hermes Decode | 100871024200  | 2866865436   | 131482618  | 61527776  | 815792128   |
| All.Full Persons Array.Unordered Keys.Aeson Lazy    | 424049660600  | 33951808604  | 1040901105 | 258769347 | 815792128   |
| All.Full Persons Array.Unordered Keys.Aeson Strict  | 303529052800  | 7155692116   | 1040895670 | 172239231 | 815792128   |
| All.Partial Twitter.Hermes Decode                   | 385226798     | 22763366     | 331159     | 3125      | 815792128   |
| All.Partial Twitter.Aeson Lazy                      | 14504587600   | 979839172    | 38168119   | 6898312   | 815792128   |
| All.Partial Twitter.Aeson Strict                    | 11363703650   | 798733766    | 38738875   | 4741485   | 815792128   |
|                                                     |
<!-- AUTO-GENERATED-CONTENT:END (BENCHES_THREADED) -->

![](https://raw.githubusercontent.com/velveteer/hermes/master/hermesbench/bench_threaded.svg)

## Performance Tips

* Use `text` >= 2.0 to benefit from its UTF-8 implementation.
* Decode to `Text` instead of `String` wherever possible!
* Decode to `Int` or `Double` instead of `Scientific` if you can.
* Decode your object fields in order. Out of order field lookups will slightly degrade performance. If encoding with `aeson`, you can leverage `toEncoding` to enforce ordering.

## Limitations

Because the On Demand API uses a forward-only iterator (except for object fields), you must be mindful to not access values out of order. In other words, you should not hold onto a `Value` to parse later since the iterator may have already moved beyond it.

Because the On Demand API does not validate the entire document upon creating the iterator (besides UTF-8 validation and basic well-formed checks), it is possible to parse an invalid JSON document but not realize it until later. If you need the entire document to be validated up front then a DOM parser is a better fit for you.

> The On Demand approach is less safe than DOM: we only validate the components of the JSON document that are used and it is possible to begin ingesting an invalid document only to find out later that the document is invalid. Are you fine ingesting a large JSON document that starts with well formed JSON but ends with invalid JSON content?

This library currently cannot decode scalar documents, e.g. a single string, number, boolean, or null as a JSON document.

## Portability

Per the `simdjson` documentation:

> A recent compiler (LLVM clang6 or better, GNU GCC 7.4 or better, Xcode 11 or better) on a 64-bit (PPC, ARM or x64 Intel/AMD) POSIX systems such as macOS, freeBSD or Linux. We require that the compiler supports the C++11 standard or better.

However, this library relies on `std::string_view` without a shim, so C++17 or better is highly recommended.
