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

For an incremental JSON parser in Haskell, see [json-stream](https://hackage.haskell.org/package/json-stream).

## Usage

This library does _not_ offer a Haskell API over the entire simdjson On Demand API. It currently binds only to what is needed for defining and running a `Decoder`. You can see the tests and benchmarks for example usage. `simdjson::ondemand` exceptions will be caught and re-thrown with enough information to troubleshoot. In the worst case you may run into a segmentation fault that is not caught, which you are encouraged to report as a bug.

### Decoders

```haskell
import qualified Data.ByteString as BS
import qualified Data.Hermes as H

personDecoder :: H.Decoder Person
personDecoder = H.withObject $ \obj ->
  Person
    <$> H.atKey "_id" H.text obj
    <*> H.atKey "index" H.int obj
    <*> H.atKey "guid" H.text obj
    <*> H.atKey "isActive" H.bool obj
    <*> H.atKey "balance" H.text obj
    <*> H.atKey "picture" (H.nullable H.text) obj
    <*> H.atKey "latitude" H.scientific obj

-- Decode a strict ByteString.
decodePersons :: BS.ByteString -> Either H.HermesException [Person]
decodePersons = H.decodeEither $ H.list personDecoder
```
### Aeson Integration

While it is not recommended to use hermes if you need the full DOM, we still provide a performant interface to decode aeson `Value`s. See an example of this in the `hermes-aeson` subpackage. Ideally, you could use hermes to selectively decode aeson `Value`s on demand, for example:

```haskell
> H.decodeEither (H.atPointer "/statuses/99/user/screen_name" H.hValueToAeson) twitter
Right (String "2no38mae")
```

### Exceptions

When decoding fails for a known reason, you will get a `Left HermesException` indicating if the error came from `simdjson` or from an internal `hermes` call.

```haskell
> decodeEither (withObject . atKey "hello" $ list text) "{ \"hello\": [\"world\", false] }"
Left (SIMDException (DocumentError {path = "/hello/1", errorMsg = "Error while getting value of type text. The JSON element does not have the requested type."))
```

## Benchmarks
We benchmark the following operations using both `hermes-json` and `aeson` strict ByteString decoders:
* Decode an array of 1 million 3-element arrays of doubles
* Full decoding of a large-ish (12 MB) JSON array of Person objects
* Partial decoding of Twitter status objects to highlight the on-demand benefits
* Decoding entire documents into `Data.Aeson.Value`

### Specs

* GHC 9.4.4
* aeson-2.1.2.1 (using `Data.Aeson.Decoding`) with text-2.0.2
* Apple M1 Pro

![](https://raw.githubusercontent.com/velveteer/hermes/master/hermes-bench/bench.svg)

<!-- AUTO-GENERATED-CONTENT:START (BENCHES) -->
| Name                                    | Mean (ps)     | 2*Stdev (ps) | Allocated  | Copied     | Peak Memory |
| --------------------------------------- | ------------- | ------------ | ---------- | ---------- | ----------- |
| All.Decode.Arrays.Hermes                | 267104550000  | 18274712758  | 503599934  | 439150544  | 541065216   |
| All.Decode.Arrays.Aeson                 | 2205838200000 | 108871466542 | 7094759154 | 2392723275 | 1166016512  |
| All.Decode.Persons.Hermes               | 47153700000   | 3880584170   | 144901928  | 57032737   | 1166016512  |
| All.Decode.Persons.Aeson                | 134265700000  | 7195219536   | 357269946  | 188529734  | 1166016512  |
| All.Decode.Partial Twitter.Hermes       | 246448046     | 20934312     | 348540     | 3088       | 1166016512  |
| All.Decode.Partial Twitter.JsonStream   | 2105484765    | 73210262     | 15261108   | 273820     | 1166016512  |
| All.Decode.Partial Twitter.Aeson        | 4297434375    | 139205712    | 12547656   | 4625157    | 1166016512  |
| All.Decode.Persons (Aeson Value).Hermes | 108099550000  | 9966602188   | 303649194  | 138051155  | 1166016512  |
| All.Decode.Persons (Aeson Value).Aeson  | 119240200000  | 9148201308   | 286148916  | 177027844  | 1166016512  |
| All.Decode.Twitter (Aeson Value).Hermes | 4261312500    | 149205128    | 12555922   | 4151184    | 1166016512  |
| All.Decode.Twitter (Aeson Value).Aeson  | 4832229687    | 242990712    | 12539421   | 5527422    | 1166016512  |
|                                         |
<!-- AUTO-GENERATED-CONTENT:END (BENCHES) -->

## Performance Tips

* Use `text` >= 2.0 to benefit from its UTF-8 implementation.
* Decode to `Text` instead of `String` wherever possible!
* Decode to `Int` or `Double` instead of `Scientific` if you can.
* Decode your object fields in order. If encoding with `aeson`, you can leverage `toEncoding` to enforce ordering.

If you need to decode in tight loops or long-running processes (like a server), consider using the `withHermesEnv/mkHermesEnv` and `parseByteString` functions instead of `decodeEither`. This ensures the simdjson instances are not re-created on each decode. Please see the [simdjson performance docs](https://github.com/simdjson/simdjson/blob/master/doc/performance.md#performance-notes) for more info. But please ensure that you use one `HermesEnv` per thread, as simdjson is [single-threaded by default](https://github.com/simdjson/simdjson/blob/master/doc/basics.md#thread-safety).

## Limitations

Because the On Demand API uses a forward-only iterator (except for object fields), you must be mindful to not access values out of order. This library tries to prevent this as much as possible, i.e. making `Decoder Value` impossible.

Because the On Demand API does not validate the entire document upon creating the iterator (besides UTF-8 validation and basic well-formed checks), it is possible to parse an invalid JSON document but not realize it until later. If you need the entire document to be validated up front then a DOM parser is a better fit for you.

> The On Demand approach is less safe than DOM: we only validate the components of the JSON document that are used and it is possible to begin ingesting an invalid document only to find out later that the document is invalid. Are you fine ingesting a large JSON document that starts with well formed JSON but ends with invalid JSON content?

This library currently cannot decode scalar documents, e.g. a single string, number, boolean, or null as a JSON document.

## Portability

Per the `simdjson` documentation:

> A recent compiler (LLVM clang6 or better, GNU GCC 7.4 or better, Xcode 11 or better) on a 64-bit (PPC, ARM or x64 Intel/AMD) POSIX systems such as macOS, freeBSD or Linux. We require that the compiler supports the C++11 standard or better.

However, this library relies on `std::string_view` without a shim, so C++17 or better is highly recommended.
