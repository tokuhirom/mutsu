# `nqp::list` and `nqp::hash` build raw VM collections

`nqp::list`, `nqp::list_i/n/s` and `nqp::hash` now build Rakudo's raw `BOOTArray`,
`BOOTIntArray`/`BOOTNumArray`/`BOOTStrArray` and `BOOTHash` objects (REPR `VMArray` /
`VMHash`) instead of an ordinary `List` / `Hash` (ADR-12666, #12668). `nqp::hllize` turns
one into the `List` / `Hash` over the **same storage**: a different object (`nqp::eqaddr`
is 0) that still sees every `nqp::bindpos` / `push` / `bindkey` made through the raw
handle.

The raw values carry the distinction in the 8-byte word's kind tag and otherwise view as
the high-level collection, so native builtins (`say`, subscripts, `+`, `?`) keep working on
them exactly as Rakudo's argument binder makes them work. Where Rakudo tells the two apart,
mutsu now does too: `nqp::islist` / `nqp::ishash` are exact REPR tests (a Raku `Array`,
`List` or `Slip` is 0; a native array or a `Buf` is 1), the five `nqp::boot*` collection
type ops exist, a method call on a raw value dies with `X::Method::NotFound`, and binding a
raw value to a Raku parameter, or reading it out through a Raku subscript, hllizes it.
Modules such as AttrX::Mooish, GTK::Simple and Rakudo::Options branch on `nqp::islist` to
tell a raw task list from a callable, which the old approximation answered wrongly for a
Raku `Array`.
