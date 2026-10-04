# The HLL-specific `nqp::` ops, `force_gc`, `freshcoderef` and `markcodestatic`

The ten HLL-specific `nqp::` ops are implemented (#11504):

- `getcurhllsym` / `bindcurhllsym` read and bind symbols of the current HLL.
  The current HLL is `Raku`, so they use the same table as
  `gethllsym("Raku", ...)` / `bindhllsym("Raku", ...)`.
- `hllboxtype_i` / `_n` / `_s` answer `Int`, `Num` and `Str`, the types a
  native is boxed into.
- `hlllist` / `hllhash` answer the type of what mutsu's own `nqp::list()` /
  `nqp::hash()` build. This is `List` / `Hash`, where Rakudo has separate VM
  types (`BOOTArray` / `BOOTHash`); whether mutsu should grow those types is
  the open decision in #11553.
- `sethllconfig`, `usecompileehllconfig` and `usecompilerhllconfig` answer
  null and change nothing, because mutsu has a single HLL configuration with
  fixed boxing. Under Rakudo, `usecompilerhllconfig` leaves the program on the
  compiler's configuration, and its test harness then dies at
  `done-testing`.

`nqp::force_gc` runs a cycle collection right away, followed by any `DESTROY`
calls that collection makes due. It is the same routine as
`$*VM.request-garbage-collection`, which was moved out of the method body so
both callers share it.

Two code-object ops from the serialization-context family came along:

- `freshcoderef` returns a new code object with the same body, captures and
  name. It is a different object from the original (not `eqaddr`), as in
  Rakudo.
- `markcodestatic` answers null. Its only job is to tell precompilation that
  a code object is not a closure, and mutsu does not serialize compiled code.

The `nqp::` coverage table now counts 495 of 577 ops; the HLL-specific family
is complete at 13 of 13.
