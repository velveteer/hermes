<p align="center">
<img src="https://raw.githubusercontent.com/velveteer/hermes/master/wings.svg" height=60 width=100 />
</p>
<h1 align="center">hermes</h1>
<p align="center">
<a href="https://github.com/velveteer/hermes/actions">
  <img src="https://img.shields.io/github/workflow/status/velveteer/hermes/CI?style=flat-square" alt="CI badge" />
</a>
<a href="https://hackage.haskell.org/package/hermes-json">
  <img src="https://img.shields.io/hackage/v/hermes-json?style=flat-square" alt="Hackage badge" />
</a>
</p>

A Haskell interface over the [simdjson](https://github.com/simdjson/simdjson) C++ library for decoding JSON documents. Hermes, messenger of the gods, was the maternal great-grandfather of Jason, son of Aeson. 

## Overview

This library exposes functions that can be used to write decoders for JSON documents using the simdjson On Demand API. From the simdjson On Demand design documentation:

> Good applications for the On Demand API might be:

> You are working from pre-existing large JSON files that have been vetted. You expect them to be well formed according to a known JSON dialect and to have a consistent layout. For example, you might be doing biomedical research or machine learning on top of static data dumps in JSON.

> Both the generation and the consumption of JSON data is within your system. Your team controls both the software that produces the JSON and the software the parses it, your team knows and control the hardware. Thus you can fully test your system.

> You are working with stable JSON APIs which have a consistent layout and JSON dialect.

With this in mind, `Data.Hermes` parsers can potentially decode Haskell types faster than traditional `Data.Aeson.FromJSON` instances, especially in cases where you only need to decode a subset of the document. This is because `Data.Aeson.FromJSON` converts the entire document into a `Data.Aeson.Value`, which means memory usage increases linearly with the input size. The `simdjson::ondemand` API does not have this constraint because it iterates over the JSON string in memory without constructing an intermediate tree. This means decoders are truly lazy and you only pay for what you use.

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

### Intel Core i7-7500U @2.70GHz / 2x8GB RAM @LPDDR3

#### Non-threaded runtime

<!-- AUTO-GENERATED-CONTENT:START (BENCHES) -->
| Name                                                | Mean (ps)     | 2*Stdev (ps) | Allocated  | Copied    | Peak Memory |
| --------------------------------------------------- | ------------- | ------------ | ---------- | --------- | ----------- |
| All.1 Million 3-Arrays.Hermes [[Double]]            | 515111634200  | 19332030346  | 567061009  | 555768600 | 548405248   |
| All.1 Million 3-Arrays.Aeson [[Double]]             | 1879357624000 | 109849580814 | 9240070406 | 918469928 | 815792128   |
| All.Small Object to Map.Hermes Decode               | 1453784       | 99664        | 4509       | 143       | 815792128   |
| All.Small Object to Map.Aeson Lazy                  | 2936816       | 184144       | 20403      | 1         | 815792128   |
| All.Small Object to Map.Aeson Strict                | 2954601       | 167788       | 20379      | 1         | 815792128   |
| All.Full Persons Array.Ordered Keys.Hermes Decode   | 106374445200  | 1634370706   | 150384445  | 77972337  | 815792128   |
| All.Full Persons Array.Ordered Keys.Aeson Lazy      | 449523758200  | 41027868974  | 1212515956 | 268045285 | 815792128   |
| All.Full Persons Array.Ordered Keys.Aeson Strict    | 353756693800  | 33058983332  | 1212278538 | 200617993 | 815792128   |
| All.Full Persons Array.Unordered Keys.Hermes Decode | 113706475400  | 3830951512   | 150322956  | 75438334  | 815792128   |
| All.Full Persons Array.Unordered Keys.Aeson Lazy    | 439971868800  | 7835893708   | 1213065712 | 264393596 | 815792128   |
| All.Full Persons Array.Unordered Keys.Aeson Strict  | 353582487600  | 34203690928  | 1212032626 | 200502732 | 815792128   |
| All.Partial Twitter.Hermes Decode                   | 448839050     | 22309802     | 398892     | 4643      | 815792128   |
| All.Partial Twitter.Aeson Lazy                      | 16644838050   | 1080972950   | 52866100   | 7541778   | 815792128   |
| All.Partial Twitter.Aeson Strict                    | 14025762400   | 793325836    | 53294115   | 5958334   | 815792128   |
|                                                     |
<!-- AUTO-GENERATED-CONTENT:END (BENCHES) --> 

![](https://raw.githubusercontent.com/velveteer/hermes/master/hermesbench/bench.svg)

#### Threaded runtime

<!-- AUTO-GENERATED-CONTENT:START (BENCHES_THREADED) -->
| Name                                                | Mean (ps)     | 2*Stdev (ps) | Allocated  | Copied    | Peak Memory |
| --------------------------------------------------- | ------------- | ------------ | ---------- | --------- | ----------- |
| All.1 Million 3-Arrays.Hermes [[Double]]            | 541886019300  | 28985190366  | 567061372  | 555825639 | 547356672   |
| All.1 Million 3-Arrays.Aeson [[Double]]             | 1948081269800 | 127560261526 | 9240069829 | 919131304 | 815792128   |
| All.Small Object to Map.Hermes Decode               | 1443894       | 82886        | 4262       | 144       | 815792128   |
| All.Small Object to Map.Aeson Lazy                  | 3010568       | 205200       | 20404      | 2         | 815792128   |
| All.Small Object to Map.Aeson Strict                | 3012037       | 99100        | 20405      | 2         | 815792128   |
| All.Full Persons Array.Ordered Keys.Hermes Decode   | 112708023300  | 5172458580   | 150387033  | 79494634  | 815792128   |
| All.Full Persons Array.Ordered Keys.Aeson Lazy      | 460293246300  | 2987469998   | 1212937927 | 264367279 | 815792128   |
| All.Full Persons Array.Ordered Keys.Aeson Strict    | 360104741850  | 6231191600   | 1212295571 | 192141024 | 815792128   |
| All.Full Persons Array.Unordered Keys.Hermes Decode | 118564635400  | 8355435606   | 150325103  | 76318688  | 815792128   |
| All.Full Persons Array.Unordered Keys.Aeson Lazy    | 476108313200  | 33933903066  | 1212647160 | 268222585 | 815792128   |
| All.Full Persons Array.Unordered Keys.Aeson Strict  | 363291220000  | 15069371922  | 1212036871 | 190951202 | 815792128   |
| All.Partial Twitter.Hermes Decode                   | 465476487     | 6501684      | 402833     | 4861      | 815792128   |
| All.Partial Twitter.Aeson Lazy                      | 17562410575   | 370107430    | 53230448   | 7657222   | 815792128   |
| All.Partial Twitter.Aeson Strict                    | 14433454400   | 1426192330   | 53290220   | 6050622   | 815792128   |
|                                                     |
<!-- AUTO-GENERATED-CONTENT:END (BENCHES_THREADED) --> 

![](https://raw.githubusercontent.com/velveteer/hermes/master/hermesbench/bench_threaded.svg)

## Performance Tips

* Decode to `Text` instead of `String` wherever possible!
* Decode to `Int` or `Double` instead of `Scientific` if you can.
* Decode your object fields in order. Out of order field lookups will slightly degrade performance. If encoding with `aeson`, you can leverage `toEncoding` to enforce ordering.
* You can improve performance by holding onto your own `HermesEnv`. `decodeEither` creates and destroys the simdjson instances every time it runs, which adds a performance penalty. Beware, do _not_ share a `HermesEnv` across multiple threads.

## Limitations

Because the On Demand API uses a forward-only iterator (except for object fields), you must be mindful to not access values out of order. In other words, you should not hold onto a `Value` to parse later since the iterator may have already moved beyond it. 

Because the On Demand API does not validate the entire document upon creating the iterator (besides UTF-8 validation and basic well-formed checks), it is possible to parse an invalid JSON document but not realize it until later. If you need the entire document to be validated up front then a DOM parser is a better fit for you.

> The On Demand approach is less safe than DOM: we only validate the components of the JSON document that are used and it is possible to begin ingesting an invalid document only to find out later that the document is invalid. Are you fine ingesting a large JSON document that starts with well formed JSON but ends with invalid JSON content?

This library currently cannot decode scalar documents, e.g. a single string, number, boolean, or null as a JSON document. 

## Portability

Per the `simdjson` documentation:

> A recent compiler (LLVM clang6 or better, GNU GCC 7.4 or better, Xcode 11 or better) on a 64-bit (PPC, ARM or x64 Intel/AMD) POSIX systems such as macOS, freeBSD or Linux. We require that the compiler supports the C++11 standard or better.

However, this library relies on `std::string_view` without a shim, so C++17 or better is highly recommended.
