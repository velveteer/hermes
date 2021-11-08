<p align="center">
<img src="./wings.svg" height=60 width=100 />
</p>
<h1 align="center">hermes</h1>
<p align="center">
<a href="https://github.com/velveteer/hermes/actions"></a>
  <img src="https://img.shields.io/github/workflow/status/velveteer/hermes/CI?style=flat-square" alt="CI badge" />
</a>

A Haskell interface over the [simdjson](https://github.com/simdjson/simdjson) C++ library for decoding JSON documents. Hermes, messenger of the gods, was the maternal great-grandfather of Jason, son of Aeson. 

## Overview

This library exposes functions that can be used to write decoders for JSON documents using the simdjson On Demand API. From the simdjson On Demand design documentation:

> Good applications for the On Demand API might be:

> You are working from pre-existing large JSON files that have been vetted. You expect them to be well formed according to a known JSON dialect and to have a consistent layout. For example, you might be doing biomedical research or machine learning on top of static data dumps in JSON.

> Both the generation and the consumption of JSON data is within your system. Your team controls both the software that produces the JSON and the software the parses it, your team knows and control the hardware. Thus you can fully test your system.

> You are working with stable JSON APIs which have a consistent layout and JSON dialect.

With this in mind, `Data.Hermes` parsers can potentially decode Haskell types faster than traditional `Data.Aeson.FromJSON` instances, especially in cases where you only need to decode a subset of the document. This is because `Data.Aeson.FromJSON` converts the entire document into a `Data.Aeson.Value`, which means memory usage increases linearly with the input size. The `simdjson::ondemand` API does not have this constraint because it iterates over the JSON string in memory without constructing any abstract representation.

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

When decoding fails for a known reason, you will get a `Left HermesException` indicating if the error came from `simdjson` or from an internal `hermes` call. The exception contains a `HError` record with some useful information, for example:
```haskell
*Main> decodeEither (withObject . atKey "hello" $ list text) "{ \"hello\": [\"world\", false] }" 
Left (SIMDException (HError {path = "/hello/1", errorMsg = "Error while getting value of type text. The JSON element does not have the requested type.", docLocation = "false] }", docDebug = "json_iterator [ depth : 3, structural : 'f', offset : 21', error : No error ]"}))
```

## Benchmarks
We benchmark decoding a very small object into a Map, full decoding of a large-ish (12 MB) JSON array of objects, and then a partial decoding of Twitter status objects to highlight the on-demand benefits. 

### Intel Core i7-7500U @2.70GHz / 2x8GB RAM @LPDDR3

#### Non-threaded runtime

<!-- AUTO-GENERATED-CONTENT:START (BENCHES) -->
| Name                                                | Mean (ps)    | 2*Stdev (ps) | Allocated  | Copied    | Peak Memory |
| --------------------------------------------------- | ------------ | ------------ | ---------- | --------- | ----------- |
| All.Small Map.Hermes Decode                         | 1430043      | 98284        | 4385       | 143       | 7340032     |
| All.Small Map.Aeson Lazy                            | 3074248      | 117324       | 20429      | 1         | 7340032     |
| All.Small Map.Aeson Strict                          | 3007853      | 203164       | 20379      | 1         | 7340032     |
| All.Full Persons Array.Ordered Keys.Hermes Decode   | 107214239200 | 2873378208   | 150299557  | 79289045  | 79691776    |
| All.Full Persons Array.Ordered Keys.Aeson Lazy      | 460601158600 | 42258394176  | 1212512116 | 268000746 | 175112192   |
| All.Full Persons Array.Ordered Keys.Aeson Strict    | 363472924200 | 32966648036  | 1212295943 | 200587190 | 175112192   |
| All.Full Persons Array.Unordered Keys.Hermes Decode | 115546909400 | 3407696936   | 150378564  | 76357368  | 175112192   |
| All.Full Persons Array.Unordered Keys.Aeson Lazy    | 460678491200 | 42588429760  | 1212648432 | 268072984 | 175112192   |
| All.Full Persons Array.Unordered Keys.Aeson Strict  | 362742103000 | 29580737556  | 1212031841 | 200459722 | 175112192   |
| All.Partial Twitter.Hermes Decode                   | 456638171    | 22093120     | 398486     | 4214      | 175112192   |
| All.Partial Twitter.Aeson Lazy                      | 17472130250  | 723106370    | 52866100   | 7515643   | 175112192   |
| All.Partial Twitter.Aeson Strict                    | 14744468100  | 678312562    | 53294115   | 5943878   | 175112192   |
|                                                     |
<!-- AUTO-GENERATED-CONTENT:END (BENCHES) --> 

![](./hermesbench/bench.svg)

#### Threaded runtime

<!-- AUTO-GENERATED-CONTENT:START (BENCHES_THREADED) -->
| Name                                                | Mean (ps)    | 2*Stdev (ps) | Allocated  | Copied    | Peak Memory |
| --------------------------------------------------- | ------------ | ------------ | ---------- | --------- | ----------- |
| All.Small Map.Hermes Decode                         | 1495947      | 85492        | 4386       | 144       | 7340032     |
| All.Small Map.Aeson Lazy                            | 3081292      | 164638       | 20429      | 2         | 7340032     |
| All.Small Map.Aeson Strict                          | 3017031      | 182234       | 20380      | 2         | 7340032     |
| All.Full Persons Array.Ordered Keys.Hermes Decode   | 110535954300 | 3332468454   | 150298146  | 79311040  | 79691776    |
| All.Full Persons Array.Ordered Keys.Aeson Lazy      | 469184100000 | 38276376594  | 1212511618 | 268113377 | 175112192   |
| All.Full Persons Array.Ordered Keys.Aeson Strict    | 367993488200 | 31137092744  | 1212294266 | 200696363 | 175112192   |
| All.Full Persons Array.Unordered Keys.Hermes Decode | 119124767800 | 4314384042   | 150378826  | 76383865  | 175112192   |
| All.Full Persons Array.Unordered Keys.Aeson Lazy    | 470064230200 | 43438819828  | 1212643670 | 268190706 | 175112192   |
| All.Full Persons Array.Unordered Keys.Aeson Strict  | 365673890800 | 32837572354  | 1212032368 | 200568710 | 175112192   |
| All.Partial Twitter.Hermes Decode                   | 463643534    | 44800558     | 383152     | 2666      | 175112192   |
| All.Partial Twitter.Aeson Lazy                      | 17639466900  | 1321877076   | 52866228   | 7542641   | 175112192   |
| All.Partial Twitter.Aeson Strict                    | 14577693200  | 1072991306   | 53294243   | 5959617   | 175112192   |
|                                                     |
<!-- AUTO-GENERATED-CONTENT:END (BENCHES_THREADED) --> 

![](./hermesbench/bench_threaded.svg)

## Performance Tips

* Decode to `Text` instead of `String` wherever possible!
* Decode to `Int` or `Double` instead of `Scientific` if you can.
* If you know the key ordering of the JSON then you can use `atOrderedKey` instead of `atKey`. This is faster but it cannot handle missing keys.
* You can improve performance by holding onto your own `HermesEnv`. `decodeEither` creates and destroys the simdjson instances every time it runs, which adds a performance penalty. Beware, do _not_ share a `HermesEnv` across multiple threads.

## Limitations

Because the On Demand API uses a forward-only iterator (except for object fields), you must be mindful to not access values out of order. In other words, you should not hold onto a `Value` to parse later since the iterator may have already moved beyond it. 

Further work is coming to wrap the `simdjson::dom` API, which should allow walking the DOM in any order you want, but at the expense of parsing the entire document into a DOM. 

Because the On Demand API does not validate the entire document upon creating the iterator (besides UTF-8 validation and basic well-formed checks), it is possible to parse an invalid JSON document but not realize it until later. If you need the entire document to be validated up front then a DOM parser is a better fit for you.

> The On Demand approach is less safe than DOM: we only validate the components of the JSON document that are used and it is possible to begin ingesting an invalid document only to find out later that the document is invalid. Are you fine ingesting a large JSON document that starts with well formed JSON but ends with invalid JSON content?

This library currently cannot decode scalar documents, e.g. a single string, number, boolean, or null as a JSON document. 

## Portability

Per the `simdjson` documentation:

> A recent compiler (LLVM clang6 or better, GNU GCC 7.4 or better, Xcode 11 or better) on a 64-bit (PPC, ARM or x64 Intel/AMD) POSIX systems such as macOS, freeBSD or Linux. We require that the compiler supports the C++11 standard or better.

However, this library relies on `std::string_view` without a shim, so C++17 or better is highly recommended.
