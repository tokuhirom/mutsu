# `NativeHelpers::Blob`: the MoarVM-guts blocker and Gap A are both gone; two small parity gaps remain

*(The filename is historical — seven documents link to it, so it is kept. Two
findings this file recorded are now resolved: the original MoarVM-guts
blocker, and Gap A below (2026-08-19). What remains — Gap B and Gap C — is
small enough to be `todo/tickets/`-shaped, but the file stays under `deep/`
at this path rather than move and break those inbound links.)*

## Where this stands (re-measured 2026-08-19, debug build of `main` @ `6cc4a2973`)

This started as "`NativeHelpers::Blob` needs `nativesizeof`, and behind it
MoarVM's object guts" — a module that reads the raw memory of a Raku object and
casts it to hand-written `CStruct` mirrors of MoarVM's REPR bodies. That finding
produced [ADR-0015](../../docs/adr/0015-native-backed-container-storage-and-repr-bodies.md)
(Accepted), whose phases P0, P1, P2, P3a and P3b have all landed. The module is
now a **bundled battery** (`modules/NativeHelpers-Blob/`) with four of its five
upstream test files on the gate (`batteries-whitelist.txt`; the fifth,
`02-cstruct.t`, needs a C compiler at test time and fails even on raku itself
on this machine, so it can never be an all-green gate entry — see "Not in
scope" below).

Measured today, both interpreters on the same input:

| contract | raku | mutsu |
| --- | --- | --- |
| `nativesizeof(Pointer)` | `8` | `8` ✅ |
| `Buf.new(1,2,3,4).REPR` | `VMArray` | `VMArray` ✅ |
| `Buf.^array_type` | `uint8` | `uint8` ✅ |
| `CArray[uint8].new.REPR` | `CArray` | `CArray` ✅ |
| `my array[uint8] $a .= new(…).REPR` | `VMArray` | `VMArray` ✅ |
| `Pointer.new(0xdeadbeaf)` | accepted | accepted ✅ |
| `BODY_OF(array:D)` → `.elems` / `.realstart` | works | works ✅ |
| a C write through `realstart` is visible in Raku | yes | yes ✅ |
| `pointer-to($native_array)` (bare `array` param) | works | **works ✅ (fixed 2026-08-19)** |
| `Rec.new.REPR` (CStruct built in Raku) | `CStruct` | `P6opaque` — deliberate, see below |
| `CArray[Str].REPR` | `CArray` | `P6opaque` — ADR-0015 P3c, no consumer |

`runtime/nativecall_pin.rs` is deleted, `array-shapes.t` is whitelisted, and the
per-call `CArray` copy for numeric element types is bypassed — all as ADR-0015
predicted. **Do not re-derive any of the above; it is done.**

### Relationship to `nativecall-cannot-be-vendored.md`

None, and this is worth stating plainly because the two files look adjacent.
[`todo/deep/nativecall-cannot-be-vendored.md`](nativecall-cannot-be-vendored.md)
is about whether mutsu can stop *providing* `NativeCall` natively and run
rakudo's own `NativeCall.rakumod` instead (measured: no — `use QAST:from<NQP>`,
MoarVM dispatch programs, 61 missing `nqp::` ops). `NativeHelpers::Blob` is an
ordinary **rung-2** ecosystem module: it is vendored verbatim and already runs on
top of mutsu's native NativeCall provider. Nothing below needs NativeCall to be
vendorable, and nothing below is blocked on that file's reopen conditions. The
BATTERIES.md rung-3 ban is not in play here — no native reimplementation of
`NativeHelpers::Blob` is proposed (ADR-0015 §3 option D rejected exactly that).

## Gap A — RESOLVED 2026-08-19

ADR-0015 §6 stated P3b's acceptance as "`pointer-to(array:D)` works and
`array-shapes.t` T36-38 pass at native speed". The second half landed earlier;
the first half was blocked purely on dispatch, not storage: a *bare* `array`
type constraint matched no value at all, so
`NativeHelpers::Blob`'s `multi sub pointer-to(array:D \arr, :$typed)`
candidate could never be selected (mutsu reported the argument as `Array:D`,
never as `array:D`).

Root cause: `src/runtime/types/type_matching.rs` special-cased native
containers only after `parse_generic_constraint` split a `base[inner]` form,
so `array[uint8]` matched but bare `array` fell through to a plain
`value_type_name` comparison (`"Array"` for a `Value::Array`, never `"array"`).

Fix: the bare `array` arm now reads the same `container_type_metadata()`
`declared_type` the parameterized arm already consults, matching when it is
`array` or starts with `array[`. This is additive only — bare `array`
previously matched nothing, so this could only turn a `False` into a correct
`True`; a plain (non-native) `Array` still correctly answers `False` (raku
parity — `array` and `Array` are distinct types, see Gap C below).

Pinned by `t/bare-array-type-match.t` (mirrors the existing
`t/carray-base-type-match.t` for the bare-`CArray` case), which also exercises
the real `NativeHelpers::Blob` `pointer-to($native_array)` call end-to-end.
This also fixes `sizeof(array:D)` and `blob-from-pointer`'s `array` overloads,
which sit behind the same constraint.

## Remaining open items (small, independent — not deep)

### Gap B — a `:D` / `:U` smiley on a lowercase native type in *term* position

```raku
say (array:D).^name;   # raku: array:D    mutsu: X::Undeclared::Symbols: array:D
say (int:D).^name;     # raku: int:D      mutsu: X::Undeclared::Symbols: int:D
my array[uint8] $a .= new(1,2);
say $a ~~ array:D;     # raku: True       mutsu: (cannot even parse the RHS)
```

The same smiley parses correctly in **signature** position (`sub f(array:D \x)`
compiles and, since Gap A, type-checks correctly too). So this is a term/type-name
resolution gap confined to lowercase native type names — it affects `int:D`,
`num:D`, `str:D` equally and has nothing to do with native storage. It is not on
the `pointer-to` path (the module spells its constraint in a signature). Worth a
`todo/tickets/` slice of its own if picked up.

### Gap C — a native `array[T]` still answers `~~ Array` (measured, deferred)

```raku
my array[uint8] $a .= new(1,2);
say $a ~~ Array;   # raku: False   mutsu: True
```

mutsu over-reports here: in Raku `array` is a distinct type whose MRO is
`array, Cool, Any, Mu` — it is *not* an `Array`. mutsu's own
`builtin_type_catalog.rs` already encodes that MRO, and `receiver_class.rs`
asserts `["array[int32]", "array", "Cool", "Any", "Mu"]`, so the type model is
right; it is the value-side `value_type_name` collapse to `"Array"` that leaks.

**This is deliberately not folded into Gap A's fix.** Gap A only adds matches;
Gap C *removes* one, and a native `array[T]` is represented as a `Value::Array`,
so every site that reaches a native array through an `Array` constraint —
method dispatch, `Positional`/`Iterable` handling, coercion, the `ArrayData`
accessor layer — would have to be surveyed first. The payoff is smartmatch
parity on a construct with no bundled-battery consumer, and the risk is exactly
the "correct only under an incomplete analysis, therefore flaky" shape
CLAUDE.md's risk definition tells us to avoid taking casually.

**Reopen when** a bundled dist or a roast file actually depends on
`array !~~ Array`, and then do it as a measured sweep of the `Array`-constraint
sites, not as a one-line narrowing.

### Not in scope, and why

- **ADR-0015 P3c** (reference-element `CArray[Str]` / `CArray[Pointer]` stored
  natively) stays open and optional. The ADR already records that no bundled
  dist needs C to *write* a `CArray[Str]`; it is parity polish, and
  `docs/nativecall-repr-bodies.md` documents the `P6opaque` answer as
  intentional.
- **A Raku-constructed CStruct** (`Rec.new.REPR` → `P6opaque`, raku `CStruct`) is
  documented as deliberate in `docs/nativecall-repr-bodies.md` §"What does NOT
  get a body": it has no C storage yet, and under ADR-0015 §2.1's ordering rule
  an honest `.REPR` is a promise that a body exists behind it.
- **`02-cstruct.t`** remains un-whitelistable: raku itself fails its tests 13 and
  15 on this machine, so it can never be an all-green gate entry, and it needs a
  C compiler at test time.

## Re-measuring

```sh
MUTSU_BIN=target/release/mutsu BATTERIES_LOCK=<lock-with-just-this-battery> \
  BATTERIES_WHITELIST=/dev/null scripts/battery-testsuite.sh
```

## Note on the diagnostic

The first line mutsu prints for these files is `Use of uninitialized value of
type Any in string context`, which is a **warning in both implementations** and
not the failure. The real error is several lines down. Do not root-cause from the
first line.
