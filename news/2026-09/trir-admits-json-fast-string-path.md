# JSON::Fast's string path runs in TRIR: the SPDX decode goes from 1.38 s to 0.21 s

ADR-0112 Step 2. About 80% of a JSON::Fast decode was its slow string path.
Every string that is not all word characters (every URL and every name in the
SPDX license list) goes through `parse-string-slow` → `nqp::strtocodes` →
`unjsonify-string`. TRIR declined both routines, so the path ran untyped at
~4.9 µs per character and ~123 µs of fixed cost per string. Rakudo spends
~0.1 µs and ~6-11 µs.

Each of these constructs made a routine decline, and each is now admitted:

- **`$end + 1` on `my $end := nqp::index(...)`.** A boxed slot `:=`-bound to
  an `nqp::` result, and never written again, is now `nqp`-sourced on read. It
  narrows in native arithmetic exactly as `nqp::index(...) + 1` already did. A
  write to such a slot declines, because it would invalidate reads that are
  already compiled.
- **A sigilless parameter (`Uni:D \codes`).** It is read as a bareword term,
  so it is kept under its own key: a bareword `pos` next to a `$pos` parameter
  is still a call to the routine `pos`. A sigilless parameter binds the
  caller's *container* when it is handed a variable, and a slot holding a
  value cannot stand in for that. So it is admitted only where its value is
  all that is used, as a direct `nqp::` operand, and every door declines a
  variable argument to it. `sub relay(\c) { leaf(c) }` stays untyped, and its
  callee's write still reaches the caller.
- **A nominal parameter type.** `Uni:D` is checked at bind time with the
  general binder's own type test. A failed check declines the call, and the
  untyped path then raises the error the program should see. A statically
  linked `CallTr`, which binds by plain copying, refuses such a callee. The
  call goes through `CallGen` instead, whose run-time link binds through the
  check.
- **`my uint32 $ordinal`.** Sized native integers (`int8`-`int32`,
  `uint8`-`uint32`, `byte`) are int-bank slots, and every store wraps them to
  their width. `uint`/`uint64` range past `i64` and still decline. So do
  increments of sized slots and passing them to a callee that might write
  them.
- **The inner `my sub fetch-codepoint`.** It is an ADR-0113 frame lexical, so
  it has no identity a program can observe. Each call to it is now inlined,
  compiled in the scope its declaration sees. The inliner refuses any shape
  where inlining is not plainly equivalent: parameters, a return type, a
  `return`, recursion, or a read of the sub's own `$_`/`$/`/`$!`.
- **`nqp::substr(...).Numeric`, `$r.Bool`, `nqp::chr($o).raku`,
  `$low.base(16)`.** A method call that only reads its receiver's value now
  goes out through the ordinary method dispatch as a `MethodGen` callout.
  Methods that could rebind their receiver (`push` autovivifying `$a`) stay
  excluded.
- **`"... {nqp::chr($o).raku} ..."`.** A bare block in value position is its
  statements, compiled in a nested scope.
- **`--> True` / `--> False`.** The body runs for its effects, and the routine
  answers the constant.

Admitting more routines exposed an older gap. A call from an untyped body to
a TRIR routine compiles to a statically linked `CallTrir`. When the TRIR bind
declines (for instance on a failed type check), the call falls back to
by-name dispatch. An ADR-0113 frame-lexical inner sub is never registered
under its name, so that fallback died with "Unknown function" instead of the
type-check error. The fallback now goes through the frame-lexical table
first. `scan_chunk` counts a `CallTrir` site as a bare call, so the caller's
chunk carries that table.

Differential testing found one real divergence, and it was already present
before this change: storing a boxed `nqp::` result into a native `int`
variable used `nqp`'s lenient `iarg` coercion, which reads a type object as 0.
The untyped path uses the assignment's check, which dies. So
`from-json('["\u12G4"]')` reported "invalid hexadecimal char" under TRIR and
"Cannot unbox a type object (Nil) to int." without it. Rakudo agrees with the
second. That store now has its own op (`NarrowStoreI`), which runs the untyped
path's `validate_native_int_assignment`.

Measured on a release build on a 4-core container, `from-json` of the
727-record document in `benchmarks/bench-json-fast-spdx.raku`:

| | before | after |
|---|---:|---:|
| decode | ~1.38 s | **~0.21 s** |
| ADR-0112 Step 2 gate | ≤ 0.334 s | met |

The decoded document is byte-identical to rakudo's and to mutsu's with TRIR
off. Rakudo took 0.043-0.048 s on the same box in the same session, so the
gap is now ~4.6x, down from ~30x. Steps 3 (typed container ops) and 4 (native lowering with
inlining) are next.

Pins: `t/vm/codegen/adr0112-trir-string-path.t` (with
`t/fixtures/trir-string-path.raku`). It checks that TRIR on, TRIR off and a
transcript verified against rakudo all agree for each construct, with every
call made twice. It also checks that every one of those routines is actually
accepted, so the agreement cannot be vacuous.
