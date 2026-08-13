# `Log::Timeline`'s CBOR/JSON-lines/socket output backends produce wrong data

## Symptom

`Log::Timeline`'s own upstream test suite (bundled as a `Cro::HTTP`
dependency, see `docs/batteries/cro-deps.md`) fails on the output-format
files:

```
$ mutsu -I modules/Log-Timeline/lib -I modules/CBOR-Simple/lib -I modules/TinyFloats/lib t/logging.rakutest
ok 1 - Logging an event is a no-op if no output
ok 2 - Can log an event with no data
ok 3 - Can log an event with data
not ok 4 - Got expected output
# expected: '2'
#      got: '0'
not ok 5 - First event logged correctly
# expected: {:data(${}), :event(Bool::True), :parent-id(0), :type(My::Test::EventA)}
#      got: Any
```

`output-cbor-sequence.rakutest`, `output-json-lines.rakutest`,
`output-socket.rakutest` fail similarly. `has-output.rakutest` (basic
on/off checks) passes.

## Why this does not block the Cro battery

`Cro::HTTP`'s own upstream suite (35/35) passes; whatever narrow slice of
`Log::Timeline` it actually exercises (registering start/end events,
per `docs/batteries/cro-http.md`'s "Cro reachability" notes) works. This is
about `Log::Timeline`'s own output serialization, a broader surface Cro
does not appear to exercise in its unit tests.

## Root cause (not yet diagnosed)

Not investigated per the standard procedure yet. `expected '2', got '0'`
for an event count suggests a queue/drain or eager-vs-lazy read-back
mismatch; `expected {...}, got Any` for an event record suggests either a
lost value or a type/coercion mismatch in whatever consumes the logged
event (possibly related to `CBOR::Simple`'s own gaps — see
`todo/tickets/cbor-simple-typed-array-and-diagnostic-format-gaps.md` — or
independent).

## Next steps

1. Run `raku` first for the exact expected output shape.
2. `MUTSU_TRACE` or a focused reduced repro of `t/logging.rakutest`'s setup
   (a `Log::Timeline::Output` subclass collecting logged events) to isolate
   whether the event is lost before or after reaching the output backend.
3. Check whether this is downstream of the `CBOR::Simple` typed-array gap
   before treating it as fully independent.

## Reproduce

```sh
git clone https://github.com/raku-community-modules/Log-Timeline.git /tmp/log-timeline
cd /tmp/log-timeline
timeout 20 mutsu -I /path/to/mutsu/modules/Log-Timeline/lib -I /path/to/mutsu/modules/CBOR-Simple/lib -I /path/to/mutsu/modules/TinyFloats/lib t/logging.rakutest
```
