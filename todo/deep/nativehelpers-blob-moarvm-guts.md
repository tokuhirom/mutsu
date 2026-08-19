# `NativeHelpers::Blob`: the MoarVM-guts blocker is gone; what is left is the bare `array` type constraint

*(The filename is historical — seven documents link to it, so it is kept. The
finding it originally recorded is resolved; this file now tracks the remainder.)*

## Where this stands (re-measured 2026-08-19, debug build of `main` @ `bc664f13a`)

This started as "`NativeHelpers::Blob` needs `nativesizeof`, and behind it
MoarVM's object guts" — a module that reads the raw memory of a Raku object and
casts it to hand-written `CStruct` mirrors of MoarVM's REPR bodies. That finding
produced [ADR-0015](../../docs/adr/0015-native-backed-container-storage-and-repr-bodies.md)
(Accepted), whose phases P0, P1, P2, P3a and P3b have all landed. The module is
now a **bundled battery** (`modules/NativeHelpers-Blob/`) with four of its five
upstream test files on the gate (`batteries-whitelist.txt`).

The original blocker is genuinely gone. Measured today, both interpreters on the
same input:

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
| **`pointer-to($native_array)`** | works | **dies: no candidate matches** ❌ |
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

## The one remaining blocker, root-caused

ADR-0015 §6 states P3b's acceptance as "`pointer-to(array:D)` works and
`array-shapes.t` T36-38 pass at native speed". The second half landed; the first
half did not, and the reason is **not** storage — it is dispatch.

```raku
use NativeHelpers::Blob;
my array[uint8] $a .= new(1,2,3,4);
say pointer-to($a);
# mutsu: Cannot resolve caller pointer-to(Array:D); none of these signatures matches:
#     (Blob:D \blob, $typed)
#     (array:D \arr, $typed)
#     (CArray:D \arr, $typed)
```

Note what mutsu calls the argument: **`Array:D`**. Everything the module's body
would then do already works — proven by calling straight past the dispatcher:

```raku
use NativeCall;
use MoarVM::Guts::REPRs;
my array[uint8] $a .= new(1,2,3,4);
my $bb = BODY_OF($a);                 # MoarVM::Guts::REPRs::MVMArrayBody
say $bb.elems;                        # 4
my $p = nativecast(CArray[uint8], Pointer.new($bb.realstart));
$p[2] = 99;
say $a[2];                            # 99 — a C write lands in the Raku array
```

So P3b's storage, `.REPR`, `.WHERE` and body synthesis are all correct. The
single missing link is that a native-typed array does not satisfy the **bare**
`array` type constraint.

### Why: the bare constraint has no lookup, only the parameterized one does

`src/runtime/types/type_matching.rs` special-cases native containers only after
`Self::parse_generic_constraint(constraint)` splits a `base[inner]` form. The
`"array"` and `"CArray"` arms there read
`container_type_metadata(value).declared_type` and match when it starts with
`array[` / `CArray[`. A *bare* `array` constraint never reaches that code — it
falls through to the generic tail in
`src/runtime/types/type_matching_static.rs`, which compares the constraint
against `value_type_name`, and for a `Value::Array` that string is `"Array"`.

The result is that bare `array` matches **nothing at all**:

```raku
my array[uint8] $a .= new(1,2);
sub p(array[uint8] \x) { "param" }
sub b(array \x)        { "bare" }
say p($a);      # mutsu: param   (parameterized constraint works)
say b($a);      # mutsu: DIED    (raku: bare)
say b([1,2]);   # mutsu: DIED    (raku: DIED — correct, an Array is not an array)
```

The metadata the fix needs is already present and already populated for both
declaration forms (`my int @b` reports `declared_type` `array[int]`, which is why
the parameterized constraint matches it).

---

## Proposed design

Three separable gaps. **A** is the blocker and is small; **B** is an independent
parser slice; **C** is the one with a real trade-off and is deliberately
deferred.

### Gap A — bare `array` matches a native-typed array (the blocker)

Give the bare constraint the same metadata lookup the parameterized form already
has. In `type_matching.rs`, alongside the existing `if constraint == "CArray"`
block and before `parse_generic_constraint`, add: when `constraint == "array"`
and the value is a `ValueView::Array(..)` whose `container_type_metadata()`
yields a `declared_type` of `array` or `array[…]`, match. Otherwise do **not**
match — a plain `Array` must keep answering `False`, which is both raku parity
and mutsu's behaviour today.

- **Blast radius: minimal.** Bare `array` is currently a dead constraint that
  matches no value whatsoever, so this can only turn `False` into `True`, never
  the reverse. Nothing can regress by losing a match.
- **Acceptance:** `pointer-to($native_array)` returns a non-zero `Pointer`;
  `pointer-to($a, :typed)` returns `Pointer[uint8]`; a C write through it is
  visible in Raku (the probe above, as a `t/` pin); ADR-0015 §6's P3b acceptance
  sentence becomes true.
- **Also fixes** `sizeof(array:D)` and `blob-from-pointer`'s `array` overloads,
  which sit behind the same constraint.

### Gap B — a `:D` / `:U` smiley on a lowercase native type in *term* position

Independent of everything above, and not needed for Gap A:

```raku
say (array:D).^name;   # raku: array:D    mutsu: X::Undeclared::Symbols: array:D
say (int:D).^name;     # raku: int:D      mutsu: X::Undeclared::Symbols: int:D
my array[uint8] $a .= new(1,2);
say $a ~~ array:D;     # raku: True       mutsu: (cannot even parse the RHS)
```

The same smiley parses correctly in **signature** position (`sub f(array:D \x)`
compiles; it only type-checks wrong, which is Gap A). So this is a term/type-name
resolution gap confined to lowercase native type names — it affects `int:D`,
`num:D`, `str:D` equally and has nothing to do with native storage. Worth its own
`todo/tickets/` slice; it is not on the `pointer-to` path because the module
spells its constraint in a signature.

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

**This is deliberately not part of Gap A.** Gap A only adds matches; Gap C
*removes* one, and a native `array[T]` is represented as a `Value::Array`, so
every site that reaches a native array through an `Array` constraint — method
dispatch, `Positional`/`Iterable` handling, coercion, the `ArrayData` accessor
layer — would have to be surveyed first. The payoff is smartmatch parity on a
construct with no bundled-battery consumer, and the risk is exactly the
"correct only under an incomplete analysis, therefore flaky" shape CLAUDE.md's
risk definition tells us to avoid taking casually.

**Reopen Gap C when** a bundled dist or a roast file actually depends on
`array !~~ Array`, and then do it as a measured sweep of the `Array`-constraint
sites, not as a one-line narrowing. Until then the divergence is recorded here,
which is the point of writing it down.

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

### Suggested order

Gap A alone closes ADR-0015 P3b and is the only item with a concrete consumer.
Gap B is a small independent slice. Gap C waits for a consumer. If Gap A lands
and its pin holds, this file has nothing deep left in it and should move to
`news/2026-08/` with ADR-0015's status updated to "P0–P3b landed, P3c open".

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
