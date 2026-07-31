# False "Method 'serialize' must be implemented" loading Cro::HTTP::BodySerializers

Loading the real `Cro::HTTP::BodySerializers` (Cro::HTTP 0.8.9.1) under mutsu
dies at module load with:

```
Method 'serialize' must be implemented by Cro::HTTP::BodySerializer::WWWFormUrlEncoded
because it is required by roles: Cro::BodySerializer, Cro::HTTP::BodySerializer.
  in block <unit> at .../lib/Cro/HTTP/BodySerializers.pm6 line 88
```

The class *does* implement it, as `proto method serialize(... --> Supply) {*}`
plus two `multi method serialize` candidates (line 88 is the proto). The role
requirement check does not count this proto/multi pair as an implementation in
this module's context.

This blocks `use Cro::HTTP::Request` / `Cro::HTTP::Response` (both pull in
BodySerializers), i.e. most of Cro::HTTP, even though `Cro::Core` modules load
fine.

## Reduced repros all pass individually

Same pattern as the `HTTP::Tiny` interaction bug recorded in
`docs/batteries/http-client.md` — needs dedicated isolation:

- one-file `role R { method f() { ... } }` + `class C does R { proto/multi f }` → OK
- two-module chain (`role A` in module 1; `role B does A` + consuming class
  with proto/multi in module 2) → OK
- with `--> Str`-style return constraints on role stub, proto, and multis → OK

Likely relevant differences in the real module: the base role
(`Cro::BodySerializer`) comes from a separate dist (`Cro::Core`), the file
defines several sibling classes implementing plain `method serialize` before
the proto/multi one, and the stub signature carries typed params from yet
another module (`Cro::HTTP::Message`).

Repro (with the Cro sources fetched from fez):

```
target/debug/mutsu -I <cro-core>/lib -I <cro-http>/lib \
  -e 'use Cro::HTTP::BodySerializers; say "ok"'
```
