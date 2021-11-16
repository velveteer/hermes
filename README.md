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
| All.1 Million 3-Arrays.Hermes [[Double]]            | 527544953100  | 25889806384  | 567060781  | 555780966 | 548405248   |
| All.1 Million 3-Arrays.Aeson [[Double]]             | 1974288189800 | 230180518292 | 9240070266 | 919275248 | 817889280   |
| All.Small Object to Map.Hermes Decode               | 1442395       | 87646        | 4267       | 142       | 817889280   |
| All.Small Object to Map.Aeson Lazy                  | 3039015       | 264080       | 20429      | 1         | 817889280   |
| All.Small Object to Map.Aeson Strict                | 2984872       | 195272       | 20379      | 1         | 817889280   |
| All.Full Persons Array.Ordered Keys.Hermes Decode   | 108534298300  | 2071130684   | 149299759  | 80391524  | 817889280   |
| All.Full Persons Array.Ordered Keys.Aeson Lazy      | 458802868400  | 43024912018  | 1212519095 | 268053927 | 817889280   |
| All.Full Persons Array.Ordered Keys.Aeson Strict    | 360200644000  | 34212211242  | 1212278401 | 200623113 | 817889280   |
| All.Full Persons Array.Unordered Keys.Hermes Decode | 117247623800  | 2828144788   | 150493856  | 77490574  | 817889280   |
| All.Full Persons Array.Unordered Keys.Aeson Lazy    | 448589908200  | 8940134736   | 1213065712 | 264398784 | 817889280   |
| All.Full Persons Array.Unordered Keys.Aeson Strict  | 358849118600  | 31346210716  | 1212031724 | 200506847 | 817889280   |
| All.Partial Twitter.Hermes Decode                   | 457497967     | 23579286     | 384206     | 4772      | 817889280   |
| All.Partial Twitter.Aeson Lazy                      | 17005647650   | 1112043338   | 52866100   | 7545951   | 817889280   |
| All.Partial Twitter.Aeson Strict                    | 14157931700   | 443648854    | 53497741   | 5922051   | 817889280   |
|                                                     |
<!-- AUTO-GENERATED-CONTENT:END (BENCHES) --> 

![](./hermesbench/bench.svg)

#### Threaded runtime

<!-- AUTO-GENERATED-CONTENT:START (BENCHES_THREADED) -->
| Name                                                | Mean (ps)     | 2*Stdev (ps) | Allocated  | Copied    | Peak Memory |
| --------------------------------------------------- | ------------- | ------------ | ---------- | --------- | ----------- |
| All.1 Million 3-Arrays.Hermes [[Double]]            | 555357333900  | 28776487856  | 567062173  | 555849004 | 548405248   |
| All.1 Million 3-Arrays.Aeson [[Double]]             | 1957512648400 | 131570909722 | 9240070858 | 919906911 | 817889280   |
| All.Small Object to Map.Hermes Decode               | 1512607       | 32124        | 4326       | 144       | 817889280   |
| All.Small Object to Map.Aeson Lazy                  | 3039437       | 177426       | 20404      | 2         | 817889280   |
| All.Small Object to Map.Aeson Strict                | 3030979       | 197548       | 20380      | 2         | 817889280   |
| All.Full Persons Array.Ordered Keys.Hermes Decode   | 111603983100  | 1623576758   | 149296747  | 80406628  | 817889280   |
| All.Full Persons Array.Ordered Keys.Aeson Lazy      | 456329749600  | 8845630900   | 1212937927 | 264372762 | 817889280   |
| All.Full Persons Array.Ordered Keys.Aeson Strict    | 365205700000  | 23712982132  | 1212295637 | 200743515 | 817889280   |
| All.Full Persons Array.Unordered Keys.Hermes Decode | 121036923800  | 3948733262   | 150494052  | 77517151  | 817889280   |
| All.Full Persons Array.Unordered Keys.Aeson Lazy    | 457424519200  | 12298131932  | 1213065840 | 264506291 | 817889280   |
| All.Full Persons Array.Unordered Keys.Aeson Strict  | 365064049000  | 26238661162  | 1212032196 | 200615649 | 817889280   |
| All.Partial Twitter.Hermes Decode                   | 464838462     | 21144978     | 384231     | 4863      | 817889280   |
| All.Partial Twitter.Aeson Lazy                      | 17261342900   | 1282477558   | 52866228   | 7572831   | 817889280   |
| All.Partial Twitter.Aeson Strict                    | 14322117325   | 550258986    | 53497869   | 5938597   | 817889280   |
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
