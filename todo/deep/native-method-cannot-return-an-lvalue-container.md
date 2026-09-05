# A method cannot hand back its invocant's container, so `.snitch = ...` fails

**Designed: [ADR-0067](../../docs/adr/0067-a-routine-hands-back-the-container-it-was-given.md)**
(2026-09-05). Slice 1 of that ADR has landed; this file now tracks Slices 2-3,
which own this repro. The ticket
`todo/tickets/lvalue-chain-through-at-key-at-pos-object-root.md` is the same
design's Slices 4-5 — read the ADR, not the two files, for the mechanism.

Split off from `news/2026-08/snitch-method-unimplemented.md`, where the five
other documented `snitch` examples were fixed and this one was not.

## Repro

```raku
use v6.e.PREVIEW;
my $a = 42; $a.snitch = 5; say $a;   # raku: 42 then 5; mutsu: X::Assignment::RO
```

The parenthesised spelling in the original ticket
(`(my $a = 42).snitch = 666`) is not required — plain `$a.snitch = 5` behaves
the same in raku.

## Root cause (corrected 2026-09-05 — the original diagnosis was wrong twice)

The original text said (1) `my $a = 42; $a.VAR = 5` should print `5`, and
(2) the defect is that *a native method* has nowhere to put the answer. Both are
wrong, and ADR-0067 records the measurements:

1. **`.VAR` is not an acceptance case.** raku also refuses `$a.VAR = 5`
   (`Cannot assign to a readonly variable or a value`) — `.VAR` returns a
   readonly `Scalar` object. (The 2026-09-01 note below caught this; the body
   above it was never corrected, and an agent was later briefed with the wrong
   expectation because of that.) Only `.snitch` is the acceptance case.
2. **It is not a native-method problem.** A user-written
   `augment class Any { method mysn(\SELF:) is raw { SELF } }; $a.mysn = 5`
   fails identically. The contract raku enforces is that the invocant parameter
   is raw (`\SELF:`) *and* the routine is `is raw`/`is rw`; dropping either
   makes raku refuse too.

The real cause is that **the invocant never arrives as a container**. A
*positional* raw parameter does — `sub f(\x) { x = 7 }; my $a = 42; f($a)`
leaves `$a` at `7`, and so does `f(@a[0])` over an array element — but the
invocant does not: `method mut(\S:) { S = 7 }` called as `$a.mut` leaves `$a` at
`42` (raku: `7`).

**Do not use `.VAR` to test this.** `S.VAR.WHAT` reports `(Scalar)` inside the
method whether or not a container arrived, because ADR-0064 synthesises the
descriptor from the contained value. Only mutating through the invocant and
checking the caller distinguishes the two; a first pass at this investigation
was misled by exactly that false positive.

A second, adjacent gap in the same family: the method lvalue gate
(`methods_mut_method_lvalue.rs:1363`) tests `method_def.is_rw` alone, while the
sub path uses the shared `routine_is_rw_capable` oracle (`is rw || is raw ||
return-rw`). `MethodDef` has no `is_raw` field at all, so
`method m(\x) is raw { x }` and `method m(\x) { return-rw x }` are both refused
with "method 'm' is not rw" even though the sub spellings now work.

## What already works (so the remaining scope is small)

- Production and consumption are both built (ADR-0059): `assign_lvalue_container`
  writes through a returned `Proxy`/`ContainerRef`/`HashEntryRef`.
- Since ADR-0067 Slice 1, a **sigilless name tail** returns its container, so
  `sub f(\x) is raw { x }; f($a) = 5` and `class C { method m(\x) is rw { x } }`
  both work. Pinned by `t/sigilless-raw-param-container-return.t`.
- The lvalue call site already tags the invocant with `WrapVarRef` (carrying its
  source name); the missing op is the `CaptureVarCell` the `return-rw` tail
  emits right after it.

## Affected files

- `src/runtime/methods_mut_method_lvalue.rs` — the "cannot assign through .{} on
  non-instance" site (:1023) and the `is_rw`-only gate (:1363)
- `src/ast.rs:1348` (`MethodDecl`) and `src/runtime/decl_types.rs:94`
  (`MethodDef`) — neither carries `is_raw`
- `src/runtime/methods_io_dispatch.rs` — `dispatch_snitch`
- `src/builtins/methods_0arg/mod.rs:304` — takes `target: &Value`, so a native
  method can already *receive* a container

## Not the fix

Adding `snitch` to the compile-time erasure that already handles `.item` is
**incorrect**, not merely a band-aid: `$a.item = 5` compiles to a plain store to
`$a` with the call erased, which is sound only because `.item` is pure.
`.snitch` notes its invocant (raku prints `42` before the assignment lands), and
erasure would drop that. See ADR-0067's "What `.item` is, and why it is not the
design".
