# BOOTInt / BOOTNum / BOOTStr boxes are KnowHOW types outside Mu

`nqp::box_i($i, nqp::bootint())` and its `_n` / `_s` siblings used to make an
instance of a registry class `BOOTInt` with parents `Any` and `Mu`. So the box
passed `nqp::istype($b, Mu)` and inherited every `Int` method through `Any`.
Its HOW was `ClassHOW` and its REPR was `P6opaque`.

In Rakudo these are `KnowHOW` types with the `P6int` / `P6num` / `P6str`
REPRs, outside the `Mu` hierarchy, the same as the raw BOOT collection types
(#12693). They now share that mechanism (`runtime::boot_collection`):

- no class is registered for them;
- a method call on a box or on its type object dies with
  `X::Method::NotFound` naming the BOOT type;
- `.HOW` is `KnowHOW` and `.REPR` is the scalar REPR;
- `nqp::create` makes a box holding zero.

Matching Rakudo, a BOOT value has no type check at all: `nqp::istype` of a
box or of a raw collection is 0 even against its own `BOOT*` type. Collections
used to answer 1 there. A smartmatch against `Mu` still accepts one, because
`Mu.ACCEPTS` takes anything.
