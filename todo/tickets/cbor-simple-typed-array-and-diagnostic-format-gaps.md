# `CBOR::Simple`'s own upstream suite fails broadly outside the narrow slice Cro::HTTP/Log::Timeline exercise

## Symptom

Bundling `CBOR::Simple` (a `Log::Timeline` → `Cro::HTTP` dependency, see
`docs/batteries/cro-deps.md`) and running its own upstream test suite via
`scripts/battery-testsuite.sh` shows most files failing, e.g.:

```
$ mutsu -I modules/CBOR-Simple/lib -I modules/TinyFloats/lib t/06-typed-arrays.rakutest
array cannot be parameterized
  in sub cbor-decode at .../CBOR/Simple.rakumod line 594
```

`00-use.rakutest` (1/1) is the only fully-passing file; `01-basic.rakutest`,
`02-malformed.rakutest`, `03-diagnostic.rakutest`, `04-tags.rakutest`,
`05-malformed-tags.rakutest`, `06-typed-arrays.rakutest` all fail heavily
(see `tmp/battery-update.log` from the 2026-08-13 Cro-bundling session for
the exact counts).

## Why this does not block the Cro battery

`Cro::HTTP` (35/35) and `Log::Timeline`'s own `has-output.rakutest` pass
cleanly — the CBOR surface Cro actually exercises (via `Log::Timeline`'s
CBOR-sequence output backend) is narrow, per
`docs/batteries/cro-http.md`'s "Cro reachability" notes: "the buf read/write
family only runs when log outputs are activated". `06-typed-arrays.rakutest`
needs `array[int32]`-style parameterized array types (`"array cannot be
parameterized"`), which is a general mutsu gap, not CBOR::Simple-specific.

## Root cause (not yet diagnosed per-file)

At least two distinct gaps bundled together in the failures:

1. Parameterized/typed `array` types (`array[uint8]` etc.) — general
   language feature gap, affects `06-typed-arrays.rakutest` at minimum.
2. Unclear for `01-basic`/`02-malformed`/`03-diagnostic`/`04-tags`/
   `05-malformed-tags` — needs the standard investigation procedure
   (run under `raku` first, `--dump-ast` the failing assertion, compare).

## Next steps

1. Run each failing file under `raku` to establish the expected baseline.
2. `--dump-ast` the `array[...]`-parameterized declarations in
   `06-typed-arrays.rakutest` to see what mutsu's parser/typechecker does
   with them.
3. Triage `01`–`05` individually — likely several unrelated general bugs
   bundled into one ticket; split into separate tickets once diagnosed.

## Reproduce

```sh
git clone https://github.com/japhb/CBOR-Simple.git /tmp/cbor-simple
cd /tmp/cbor-simple && git checkout 0.1.4
timeout 20 mutsu -I /path/to/mutsu/modules/CBOR-Simple/lib -I /path/to/mutsu/modules/TinyFloats/lib t/06-typed-arrays.rakutest
```
