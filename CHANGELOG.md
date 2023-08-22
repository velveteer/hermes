# Revision history for hermes-json

## 0.6.0.0 -- 2023-08-22

### Breaking Changes:
* Drop support for GHC 8.10
* Drop support for text < 2.0
* Remove `withArray` and `withObject`

### Changed
* Update simdjson to 3.2.2
* Support GHC 9.6
* Replace attoparsec-iso8601 with text-iso8601
* Replace `Scientific` parser with Text version
* Remove `attoparsec` dependency
* Remove unnecessary allocation for array and object iterators
* Remove unnecessary allocation for objects and field lookups
* Remove unnecessary strictness in iterator loops
* Update benchmarks

### Fixed
* Fix bug where internal path was not being reset on each parse

### Added
* Add `object` which replaces obsolete `withObject`
* Add `uint` for decoding unsigned 64-bit integers (Word)
* Expose `listOfInt` and `listOfDouble` for users who don't rely on rewrite rules
* Add array and object reset behavior for better `Alternative` instance

## 0.5.0.0 -- 2023-03-08

* Convert `DecoderM` callbacks to `Decoder`. This is a breaking change.
* Add useful instances for the `Decoder` monad
* Remove some redundant functions
* Adjust `vector` bounds

## 0.4.0.0 -- 2023-03-07

* Update simdjson to 3.1.3
* Add `containers`, `primitive` and `vector` dependencies
* Add example of decoding `Data.Aeson.Value` in `hermes-aeson`
* Remove debug string and document location in errors
* Fix JSON pointer formatting
* Fix incorrect path in errors from `atPointer`
* Refactor `Decoder` to prevent passing opaque `Value`s
* Add general performance improvements
* Update benchmarks

## 0.3.0.0 -- 2023-03-01

* Remove MonadIO and MonadUnliftIO instances for `Decoder`
* Remove unliftio dependency
* Support system-cxx-std-lib for GHC >= 9.4
* Fix GitHub CI badge

## 0.2.0.1 -- 2022-10-10

* Add support for GHC 9.4.1
* Bump upper bound on `time`

## 0.2.0.0 -- 2022-02-22

* Add support for `text` 2.0

## 0.1.0.1 -- 2022-01-18

* Make `-march=native` opt-in

## 0.1.0.0 -- 2021-12-24

* First version. Released on an unsuspecting world.
