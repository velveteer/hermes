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
| All.Small Map.Hermes Decode                         | 1808326      | 97420        | 5371       | 145       | 7340032     |
| All.Small Map.Aeson Lazy                            | 3028265      | 199952       | 20403      | 1         | 7340032     |
| All.Small Map.Aeson Strict                          | 3015247      | 179048       | 20379      | 1         | 7340032     |
| All.Full Persons Array.Ordered Keys.Hermes Decode   | 137983906300 | 4445159582   | 223423967  | 79419548  | 94371840    |
| All.Full Persons Array.Ordered Keys.Aeson Lazy      | 463567135600 | 38943933222  | 1212510512 | 268001465 | 175112192   |
| All.Full Persons Array.Ordered Keys.Aeson Strict    | 364116425600 | 35327713108  | 1212278671 | 200579286 | 175112192   |
| All.Full Persons Array.Unordered Keys.Hermes Decode | 144397238000 | 6456176710   | 222902980  | 74461432  | 175112192   |
| All.Full Persons Array.Unordered Keys.Aeson Lazy    | 461632813200 | 44132112704  | 1212645124 | 268070262 | 175112192   |
| All.Full Persons Array.Unordered Keys.Aeson Strict  | 361916016400 | 32991162496  | 1212031894 | 200459665 | 175112192   |
| All.Partial Twitter.Hermes Decode                   | 529247143    | 47063962     | 580214     | 7417      | 175112192   |
| All.Partial Twitter.Aeson Lazy                      | 17368166575  | 709898588    | 53178967   | 7589753   | 175112192   |
| All.Partial Twitter.Aeson Strict                    | 14367482400  | 936459902    | 53294082   | 5943851   | 175112192   |
|                                                     |
<!-- AUTO-GENERATED-CONTENT:END (BENCHES) --> 

![](./hermesbench/bench.svg)

#### Threaded runtime

<!-- AUTO-GENERATED-CONTENT:START (BENCHES_THREADED) -->
| Name                                                | Mean (ps)    | 2*Stdev (ps) | Allocated  | Copied    | Peak Memory |
| --------------------------------------------------- | ------------ | ------------ | ---------- | --------- | ----------- |
| All.Small Map.Hermes Decode                         | 1900714      | 175942       | 5345       | 144       | 7340032     |
| All.Small Map.Aeson Lazy                            | 3119735      | 165670       | 20404      | 2         | 7340032     |
| All.Small Map.Aeson Strict                          | 3061460      | 184364       | 20380      | 2         | 7340032     |
| All.Full Persons Array.Ordered Keys.Hermes Decode   | 139121873500 | 4154303488   | 223406892  | 79406503  | 94371840    |
| All.Full Persons Array.Ordered Keys.Aeson Lazy      | 473572236000 | 40433167018  | 1212519382 | 268106592 | 174063616   |
| All.Full Persons Array.Ordered Keys.Aeson Strict    | 373430497400 | 33768509168  | 1212291757 | 200699515 | 174063616   |
| All.Full Persons Array.Unordered Keys.Hermes Decode | 144171486800 | 4161021084   | 222903366  | 74492787  | 174063616   |
| All.Full Persons Array.Unordered Keys.Aeson Lazy    | 463409272000 | 7738096592   | 1213065840 | 264464248 | 209715200   |
| All.Full Persons Array.Unordered Keys.Aeson Strict  | 364960089900 | 34922354232  | 1212036871 | 190919683 | 220200960   |
| All.Partial Twitter.Hermes Decode                   | 539464050    | 44702534     | 580220     | 7476      | 220200960   |
| All.Partial Twitter.Aeson Lazy                      | 17512258200  | 1074981182   | 52867027   | 7542374   | 220200960   |
| All.Partial Twitter.Aeson Strict                    | 14659514950  | 1091769804   | 53294973   | 5960135   | 220200960   |
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
