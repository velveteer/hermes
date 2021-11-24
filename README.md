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

When decoding fails for a known reason, you will get a `Left HermesException` indicating if the error came from `simdjson` or from an internal `hermes` call. The exception contains a `HError` record with some useful information, for example:
```haskell
*Main> decodeEither (withObject . atKey "hello" $ list text) "{ \"hello\": [\"world\", false] }" 
Left (SIMDException (HError {path = "/hello/1", errorMsg = "Error while getting value of type text. The JSON element does not have the requested type.", docLocation = "false] }", docDebug = "json_iterator [ depth : 3, structural : 'f', offset : 21', error : No error ]"}))
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
| All.1 Million 3-Arrays.Hermes [[Double]]            | 476562673000  | 7262236730   | 548172707  | 404036426 | 470810624   |
| All.1 Million 3-Arrays.Aeson [[Double]]             | 2438400494400 | 83734161878  | 9202386634 | 948901876 | 978321408   |
| All.Small Object to Map.Hermes Decode               | 1663756       | 110252       | 4212       | 139       | 978321408   |
| All.Small Object to Map.Aeson Lazy                  | 3537389       | 184328       | 20294      | 0         | 978321408   |
| All.Small Object to Map.Aeson Strict                | 3481400       | 177114       | 20270      | 0         | 978321408   |
| All.Full Persons Array.Ordered Keys.Hermes Decode   | 76567271100   | 5754850536   | 128037168  | 15748622  | 978321408   |
| All.Full Persons Array.Ordered Keys.Aeson Lazy      | 492081747400  | 19224580124  | 1180336100 | 218985376 | 978321408   |
| All.Full Persons Array.Ordered Keys.Aeson Strict    | 397270768800  | 15567391666  | 1182267248 | 176758028 | 978321408   |
| All.Full Persons Array.Unordered Keys.Hermes Decode | 107574088400  | 6576080488   | 150435621  | 45897258  | 978321408   |
| All.Full Persons Array.Unordered Keys.Aeson Lazy    | 489430419600  | 25408638096  | 1180433634 | 219077306 | 978321408   |
| All.Full Persons Array.Unordered Keys.Aeson Strict  | 397768917800  | 16325007274  | 1182085558 | 176763430 | 978321408   |
| All.Partial Twitter.Hermes Decode                   | 478208884     | 26050142     | 245762     | 275       | 978321408   |
| All.Partial Twitter.Aeson Lazy                      | 13479893550   | 894222142    | 49302390   | 849258    | 978321408   |
| All.Partial Twitter.Aeson Strict                    | 11923342700   | 468673028    | 52422411   | 991347    | 978321408   |
|                                                     |
<!-- AUTO-GENERATED-CONTENT:END (BENCHES) --> 

![](./hermesbench/bench.svg)

#### Threaded runtime

<!-- AUTO-GENERATED-CONTENT:START (BENCHES_THREADED) -->
| Name                                                | Mean (ps)     | 2*Stdev (ps) | Allocated  | Copied    | Peak Memory |
| --------------------------------------------------- | ------------- | ------------ | ---------- | --------- | ----------- |
| All.1 Million 3-Arrays.Hermes [[Double]]            | 495615126400  | 13916467348  | 548172846  | 404058015 | 470810624   |
| All.1 Million 3-Arrays.Aeson [[Double]]             | 2465229540000 | 101879044954 | 9202387121 | 948966824 | 978321408   |
| All.Small Object to Map.Hermes Decode               | 1799173       | 102126       | 4212       | 139       | 978321408   |
| All.Small Object to Map.Aeson Lazy                  | 3529662       | 254538       | 20294      | 0         | 978321408   |
| All.Small Object to Map.Aeson Strict                | 3483730       | 181048       | 20270      | 0         | 978321408   |
| All.Full Persons Array.Ordered Keys.Hermes Decode   | 78538283100   | 6173203202   | 128033563  | 15744244  | 978321408   |
| All.Full Persons Array.Ordered Keys.Aeson Lazy      | 495245322400  | 25333464174  | 1180341250 | 219023815 | 978321408   |
| All.Full Persons Array.Ordered Keys.Aeson Strict    | 400370595600  | 26470108508  | 1182266859 | 176783709 | 978321408   |
| All.Full Persons Array.Unordered Keys.Hermes Decode | 110363031900  | 7396849364   | 150434297  | 45905837  | 978321408   |
| All.Full Persons Array.Unordered Keys.Aeson Lazy    | 495551398200  | 17360198522  | 1180432917 | 219112193 | 978321408   |
| All.Full Persons Array.Unordered Keys.Aeson Strict  | 400388041200  | 18150442834  | 1182086348 | 176788754 | 978321408   |
| All.Partial Twitter.Hermes Decode                   | 482294442     | 37931602     | 245774     | 283       | 978321408   |
| All.Partial Twitter.Aeson Lazy                      | 13621086650   | 903949710    | 49302454   | 849432    | 978321408   |
| All.Partial Twitter.Aeson Strict                    | 11910862550   | 723845838    | 49148469   | 869856    | 978321408   |
|                                                     |
<!-- AUTO-GENERATED-CONTENT:END (BENCHES_THREADED) --> 

![](./hermesbench/bench_threaded.svg)

## Performance Tips

* Decode to `Text` instead of `String` wherever possible!
* Decode to `Int` or `Double` instead of `Scientific` if you can.
* Decode your object fields in order. Out of order field lookups will slightly degrade performance. If encoding with `aeson`, you can leverage `toEncoding` to enforce ordering.
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
