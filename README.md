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

Hermes requires the entire document in memory. For an incremental JSON parser that supports streaming, see [json-stream](https://hackage.haskell.org/package/json-stream).

## Usage

This library does _not_ offer a Haskell API over the entire simdjson On Demand API. It currently binds only to what is needed for defining and running a `Decoder`. You can see the tests and benchmarks for example usage. `simdjson::ondemand` exceptions will be caught and re-thrown with enough information to troubleshoot. In the worst case you may run into a segmentation fault that is not caught, which you are encouraged to report as a bug.

### Decoders

```haskell
import qualified Data.ByteString as BS
import qualified Data.Hermes as H

personDecoder :: H.Decoder Person
personDecoder = H.object $
  Person
    <$> H.atKey "_id" H.text
    <*> H.atKey "index" H.int
    <*> H.atKey "guid" H.text
    <*> H.atKey "isActive" H.bool
    <*> H.atKey "balance" H.text
    <*> H.atKey "picture" (H.nullable H.text)
    <*> H.atKey "latitude" H.scientific

-- Decode a strict ByteString.
decodePersons :: BS.ByteString -> Either H.HermesException [Person]
decodePersons = H.decodeEither $ H.list personDecoder
```
### Aeson Integration

While it is not recommended to use hermes if you need the full DOM, we still provide a performant interface to decode aeson `Value`s. See an example of this in the `hermes-aeson` subpackage. You could use hermes to selectively decode aeson `Value`s on demand, for example:

```haskell
> decodeEither (atPointer "/statuses/99/user/screen_name" hValueToAeson) twitter
Right (String "2no38mae")
```

### Exceptions

When decoding fails for a known reason, you will get a `Left HermesException` indicating if the error came from `simdjson` or from an internal `hermes` call.

```haskell
> decodeEither (object . atKey "hello" $ list text) "{ \"hello\": [\"world\", false] }"
Left (SIMDException (DocumentError {path = "/hello/1", errorMsg = "Error while getting value of type text. INCORRECT_TYPE: The JSON element does not have the requested type."}))
```

## Benchmarks
We benchmark the following operations using both `hermes-json` and `aeson` strict ByteString decoders:
* Decode a small array of 3-element arrays of doubles
* Full decoding of a large-ish (12 MB) JSON array of Person objects
* Partial decoding of Twitter status objects to highlight the on-demand benefits
* Decoding entire documents into `Data.Aeson.Value`

### Specs

* GHC 9.4.6 w/ -O1
* aeson-2.2 with text > 2.0
* Apple M1 Pro

![](hermes-bench/bench.svg)

<!-- AUTO-GENERATED-CONTENT:START (BENCHES) -->
| Name                                    | Mean (ps)    | 2*Stdev (ps) | Allocated | Copied    | Peak Memory |
| --------------------------------------- | ------------ | ------------ | --------- | --------- | ----------- |
| All.Decode.Arrays.Hermes                | 1163478906   | 110052976    | 4021168   | 42946     | 94371840    |
| All.Decode.Arrays.Aeson                 | 17484631250  | 1662041376   | 70812389  | 2086285   | 94371840    |
| All.Decode.Persons.Hermes               | 49395500000  | 3518452962   | 122952365 | 36536392  | 177209344   |
| All.Decode.Persons.Aeson                | 129151300000 | 5125738624   | 349498135 | 130919445 | 253755392   |
| All.Decode.Partial Twitter.Hermes       | 283987963    | 22457742     | 288938    | 252       | 253755392   |
| All.Decode.Partial Twitter.JsonStream   | 2406579687   | 217513512    | 15092664  | 12836     | 253755392   |
| All.Decode.Partial Twitter.Aeson        | 2777335937   | 210155450    | 12321844  | 142687    | 253755392   |
| All.Decode.Persons (Aeson Value).Hermes | 110568500000 | 9598321792   | 259405472 | 98712919  | 253755392   |
| All.Decode.Persons (Aeson Value).Aeson  | 111529556250 | 4305492988   | 278903097 | 107836686 | 253755392   |
| All.Decode.Twitter (Aeson Value).Hermes | 2819365625   | 255135356    | 10691334  | 221575    | 253755392   |
| All.Decode.Twitter (Aeson Value).Aeson  | 2878399218   | 174939126    | 12220660  | 208889    | 253755392   |
|                                         |
<!-- AUTO-GENERATED-CONTENT:END (BENCHES) -->

## Performance Tips

* Decode to `Text` instead of `String` wherever possible!
* Decode to `Int` or `Double` instead of `Scientific` if you can.
* Decode your object fields in order. If encoding with `aeson`, you can leverage `toEncoding` to enforce ordering.

If you need to decode in tight loops or long-running processes (like a server), consider using the `withHermesEnv/mkHermesEnv` and `parseByteString` functions instead of `decodeEither`. This ensures the simdjson instances are not re-created on each decode. See the [simdjson performance docs](https://github.com/simdjson/simdjson/blob/master/doc/performance.md#performance-notes) for more info. Please ensure that you use one `HermesEnv` per thread, as simdjson is [single-threaded by default](https://github.com/simdjson/simdjson/blob/master/doc/basics.md#thread-safety).

## Limitations

Because the On Demand API in simdjson uses a forward-only iterator (except for object fields), it is possible to introduce [unsafe iteration](https://github.com/simdjson/simdjson/blob/master/doc/ondemand_design.md#iteration-safety). Hermes tries to prevent this as much as possible with the type system.

The On Demand API does not validate the entire document upon creating the iterator (besides UTF-8 validation and basic well-formed checks). It is possible to parse an invalid JSON document but not realize it until later. If you need the entire document to be validated up front then a DOM parser is a better fit for you.

> The On Demand approach is less safe than DOM: we only validate the components of the JSON document that are used and it is possible to begin ingesting an invalid document only to find out later that the document is invalid. Are you fine ingesting a large JSON document that starts with well formed JSON but ends with invalid JSON content?

Other limitations inherited from simdjson:

* Cannot decode scalar documents, e.g. a single string, number, boolean, or null as a JSON document.
* 4GB is the maximum document size that simdjson supports.

## Portability

Per the `simdjson` documentation:

> A recent compiler (LLVM clang6 or better, GNU GCC 7.4 or better, Xcode 11 or better) on a 64-bit (PPC, ARM or x64 Intel/AMD) POSIX systems such as macOS, freeBSD or Linux. We require that the compiler supports the C++11 standard or better.

However, this library relies on `std::string_view` without a shim, so C++17 or later is required.

The `native_comp` cabal flag enables passing `-march=native` to the C++ compiler.

> Passing -march=native to the compiler may make On Demand faster by allowing it to use optimizations specific to your machine. You cannot do this, however, if you are compiling code that might be run on less advanced machines. That is, be mindful that when compiling with the -march=native flag, the resulting binary will run on the current system but may not run on other systems (e.g., on an old processor).

> If you are compiling on an ARM or POWER system, you do not need to be concerned with CPU selection during compilation. The -march=native flag is useful for best performance on x64 (e.g., Intel) systems but it is generally unsupported on some platforms such as ARM (aarch64) or POWER.
