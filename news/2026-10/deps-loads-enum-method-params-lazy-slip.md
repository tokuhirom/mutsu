# Deps: `for ... -> ::T`, enum-value method params, and `for |lazy-seq`

Working the `Deps` zef distribution (it was `blocked_load`) fixed four general gaps:

- A type capture is accepted as the pointy parameter of a `for` loop
  (`for Type.^roles -> ::Type { ... }`, `-> ::T $x`, `-> $a, ::T`) and binds `T` to the type
  of the received value.
- A bare enum value as a type-only *method* parameter (`multi method m(Store)`) now selects on
  that value in the fast method binder, like a sub already did, and wins over a package
  short-name alias of the same spelling.
- `use Pkg::Item::Sto` (a `unit class`) no longer makes the bare name `Sto` resolve to
  that class in the importer, which hid an enum value `Sto` imported from another module.
- `for |(lazy gather ...)` iterates the lazy sequence's values instead of one `Seq` item.

`Deps` now loads and `t/01-basic.rakutest` runs further; the remaining stop is
[#11064](https://github.com/tokuhirom/mutsu/issues/11064) (`$_ = [] without %!h{k}[0]`).
