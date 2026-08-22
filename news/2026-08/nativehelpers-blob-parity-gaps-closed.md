# `NativeHelpers::Blob`: the last two native-array parity gaps are closed

This closes out `todo/deep/nativehelpers-blob-moarvm-guts.md`, the finding that
produced [ADR-0015](../../docs/adr/0015-native-backed-container-storage-and-repr-bodies.md).
That file started life as "`NativeHelpers::Blob` needs `nativesizeof`, and behind
it MoarVM's object guts" — a module that reads the raw memory of a Raku object
and casts it to hand-written `CStruct` mirrors of MoarVM's REPR bodies. ADR-0015
phases P0, P1, P2, P3a and P3b landed over July and August; the module has been
a **bundled battery** (`modules/NativeHelpers-Blob/`) with four of its five
upstream test files on the gate (`batteries-whitelist.txt`) since then.

Three findings the file recorded were resolved separately: the MoarVM-guts
blocker itself, and Gap A (a *bare* `array` type constraint matched no value at
all, so `pointer-to(array:D \arr)` could never select its candidate — fixed
2026-08-19, `news/2026-08/bare-array-type-constraint-pointer-to.md`). What
remained was Gap B and Gap C, both re-verified as still reproducing on
`main` @ `32cd73b27` before this work started. Both are now fixed.

## The contract, re-measured

| contract | raku | mutsu |
| --- | --- | --- |
| `nativesizeof(Pointer)` | `8` | `8` |
| `Buf.new(1,2,3,4).REPR` | `VMArray` | `VMArray` |
| `Buf.^array_type` | `uint8` | `uint8` |
| `CArray[uint8].new.REPR` | `CArray` | `CArray` |
| `my array[uint8] $a .= new(…).REPR` | `VMArray` | `VMArray` |
| `Pointer.new(0xdeadbeaf)` | accepted | accepted |
| `BODY_OF(array:D)` → `.elems` / `.realstart` | works | works |
| a C write through `realstart` is visible in Raku | yes | yes |
| `pointer-to($native_array)` (bare `array` param) | works | works (Gap A) |
| `(array:D).^name` | `array:D` | `array:D` (Gap B) |
| `$native_array ~~ Array` | `False` | `False` (Gap C) |
| `Rec.new.REPR` (CStruct built in Raku) | `CStruct` | `P6opaque` — deliberate |
| `CArray[Str].REPR` | `CArray` | `P6opaque` — ADR-0015 P3c, no consumer |

## Gap B — a `:D` / `:U` smiley on a lowercase native type in *term* position

```raku
say (array:D).^name;   # was: X::Undeclared::Symbols: array:D
say (int:D).^name;     # was: X::Undeclared::Symbols: int:D
my array[uint8] $a .= new(1,2);
say $a ~~ array:D;     # the RHS did not even parse
```

The same smiley had always parsed in **signature** position (`sub f(array:D \x)`
compiles, and since Gap A type-checks correctly too), so this was purely a
term/type-name resolution gap.

Root cause: `src/parser/primary/ident/identifier_call.rs` accepted a trailing
`:D` / `:U` / `:_` only when the name started with an ASCII uppercase letter.
That gate is not arbitrary — an ordinary lowercase identifier followed by `:D…`
is a colonpair adverb (`foo:Debug`), so the smiley arm cannot simply be widened
to "any lowercase name".

Fix: admit the smiley for a lowercase name when it is a **native type name**
(the new `runtime::native_types::is_native_type_name` — the native array element
types plus `array` itself), *and* the smiley is not the prefix of a longer
adverb name. `str(1, :Deep)` therefore still parses `:Deep` as an adverb rather
than as `str:D` followed by `eep`. `int:D`, `num64:U`, `uint32:_`, `byte:D`,
`atomicint:D` and `array:D` now all report the same `.^name` / `.WHAT` / `.gist`
as Rakudo. Pinned by `t/native-type-smiley.t`.

(One documented divergence: Rakudo answers `True` for the *literal* spelling
`$native_array ~~ array:U`, but `False` for both `(array:U).ACCEPTS($a)` and the
same check through a variable — i.e. its literal form folds the definiteness
check away for native types. mutsu answers the self-consistent `False`
everywhere. This is noted in the test.)

## Gap C — a native `array[T]` answered `~~ Array`

```raku
my array[uint8] $a .= new(1,2);
say $a ~~ Array;   # raku: False   mutsu was: True
say $a.isa(Array); # raku: False   mutsu was: True
say $a.isa(array); # raku: True    mutsu was: False
```

In Raku `array` is a distinct type whose MRO is `array, Cool, Any, Mu` — it does
`Positional`, `Iterable` and `Cool`, but it is *not* an `Array`. mutsu's
`builtin_type_catalog.rs` already encoded that MRO and `receiver_class.rs`
already asserted it, so the type model was right; what leaked was the
**value** side, where a native array is represented as a `Value::Array` and so
collapsed to the name `"Array"`.

The todo file deferred this on the grounds that narrowing a match is riskier
than adding one, and that every site reaching a native array through an `Array`
constraint would have to be surveyed. The survey turned out to be small,
because the declared type travels *embedded in the array's own backing data*
(`ArrayData::declared_type`) rather than in an interpreter-side table — so every
site can recover it without an interpreter loan. Three places needed it:

- `runtime/types/type_matching.rs` — an `Array` / `Array[T]` constraint now
  answers `False` for a value whose embedded declared type is `array` /
  `array[…]`. This is the exact mirror of the bare-`array` arm Gap A added, and
  it only narrows the `Array` name: `Positional`, `Iterable`, `Cool` and `Any`
  keep matching through the generic tail exactly as raku reports them.
- `value/types_isa.rs` — `isa_check` reports `"array"` (not `"Array"`) for such
  a value, mirroring the `Map` arm right below it, which reads the same embedded
  `declared_type`. `.isa(array)`, `.isa(Cool)`, `.isa(Any)` and `.isa(Mu)` are
  now all `True` and `.isa(Array)` `False`, matching raku.
- `runtime/utils/type_misc.rs` — a new `value_type_display_name` used by
  parameter-binding type errors. Without it the newly-correct rejection of a
  native array by an `Array $x` parameter read "expected Array, got Array",
  because `value_type_name` answers a `&'static str` from the `Value` tag alone.
  It now reads "expected Array, got array[uint8]".

Pinned by the seven new assertions in `t/bare-array-type-match.t`.

## Still not in scope, and why

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
- **Adjacent gaps found while measuring, deliberately left alone** (none has a
  bundled-battery consumer, and none is what this ticket was about): `5 ~~ int`
  answers `True` where raku answers `False` (a boxed `Int` is not a native
  `int`); `$a[0] = 7` on a native array dies with "expected array[uint8] but got
  Int" (the element check compares against the container's declared type, not
  its element type); `(Int:_).^name` keeps the `:_` where raku normalizes it
  away; and a smiley type object has no `.ACCEPTS` method.

## Relationship to `nativecall-cannot-be-vendored.md`

None, and it is worth restating because the two look adjacent.
[`todo/deep/nativecall-cannot-be-vendored.md`](../../todo/deep/nativecall-cannot-be-vendored.md)
is about whether mutsu can stop *providing* `NativeCall` natively and run
rakudo's own `NativeCall.rakumod` instead (measured: no — `use QAST:from<NQP>`,
MoarVM dispatch programs, 61 missing `nqp::` ops). `NativeHelpers::Blob` is an
ordinary **rung-2** ecosystem module: vendored verbatim, running on top of
mutsu's native NativeCall provider. Nothing here needed NativeCall to be
vendorable, and the BATTERIES.md rung-3 ban was never in play — no native
reimplementation of `NativeHelpers::Blob` was proposed (ADR-0015 §3 option D
rejected exactly that).

## Re-measuring the battery

```sh
MUTSU_BIN=target/release/mutsu BATTERIES_LOCK=<lock-with-just-this-battery> \
  BATTERIES_WHITELIST=/dev/null scripts/battery-testsuite.sh
```

The first line mutsu prints for these files is `Use of uninitialized value of
type Any in string context`, which is a **warning in both implementations** and
not the failure. The real error is several lines down. Do not root-cause from
the first line.
