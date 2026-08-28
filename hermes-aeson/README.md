# hermes-aeson

Decode JSON bytes into [`Data.Aeson.Value`](https://hackage.haskell.org/package/aeson/docs/Data-Aeson.html#t:Value)
using `hermes-json`'s simdjson bindings instead of aeson's pure-Haskell
parser.

Useful when you want an aeson `Value` (e.g. for a downstream `FromJSON`
instance, or as a generic JSON representation) but don't want to pay
for aeson's decoder. On the `persons9000` fixture, decoding to a
`Value` is roughly 1.6x faster than `Data.Aeson.Decoding.decodeStrict`
with about 15% lower peak memory.

## Usage

For container documents (objects and arrays at the top level), use
`aesonValue` as a `Decoder`:

```haskell
import qualified Data.Hermes as H
import qualified Data.Hermes.Aeson as HA

H.decodeEither HA.aesonValue bs :: Either H.HermesException A.Value
```

For documents that may be top-level scalars (numbers, strings,
booleans, null), use `parseAesonValue`. simdjson's
`document.get_value()` rejects scalar documents with
`SCALAR_DOCUMENT_AS_VALUE`, so the value-level `Decoder` cannot
handle them.

```haskell
HA.parseAesonValue env bs :: Either H.HermesException A.Value
```

## Combining with aeson's `FromJSON`

To keep aeson's per-field errors when decoding into a target type, run
`aesonValue` first and then `Data.Aeson.parseEither`:

```haskell
case H.decodeEither HA.aesonValue bs of
  Left  e -> ... -- simdjson-level error (malformed JSON)
  Right v -> case A.parseEither A.parseJSON v of
    Left  e -> ... -- aeson-level error (Value doesn't fit FromJSON)
    Right a -> ...
```

## Notes

- Errors from `aesonValue` and `parseAesonValue` carry a source snippet
  near simdjson's `current_location`, not a JSON-pointer path. The
  bulk walker does not track per-key paths.
- Numbers are parsed exactly. `Int64` values go through a fast int
  path. Everything else (unsigned, floating-point, big integer) is
  parsed via `parseScientificText` over the raw token, preserving
  precision.
- Object key order in the resulting `KeyMap` depends on aeson's build
  configuration (the `ordered-keymap` flag). This matches aeson's own
  behavior.
