# Dists that load but fail their own test suite (sweep `--run-tests`, 2026-07-25)

Found by running `scripts/dist-compat-sweep.py --run-tests` (n=60, seed 20260719)
on the same sample the load-only sweep uses. That axis runs each loading dist's
own suite with **raku as the baseline** — only files raku itself passes cleanly
are graded — and it is a far sharper frontier than "does it load":

```
=== test-suite axis: 28 load_ok dists ran tests ===
  test_pass          7  27% of graded
  test_fail          5  19% of graded
  test_die          14  54% of graded
```

Raw data: `tmp/sweep-tests.tsv` / `tmp/sweep-tests.log` (gitignored; regenerate
with the command above).

One of the 14 `test_die` rows is already fixed: `Prime::Factor` needed multi
dispatch to honour a named parameter's aliases
(`news/2026-07/multi-dispatch-honours-named-param-aliases.md`), and now matches
raku exactly across all four of its files. The rest are separate root causes.
Three are triaged below; the remainder are un-triaged.

## Triaged

### `String::Splice` — an imported `multi` whose `proto` is not exported is unreachable by name

```raku
use String::Splice;
say splice('', 0, 'Raku');   # raku: Raku      mutsu: Unknown function: splice
say &splice.defined;         # both: True
```

The dist declares `proto sub splice (Str(Any) $, |c) is pure {*};` **without**
`is export`, and its four candidates as `multi splice (...) is export`. So the
name resolves (`&splice` is defined) but the call finds no callable. Note the
method form is a red herring: `''.splice(0, 'Raku')` fails in **raku** too
("Routine does not have any candidates. Is only the proto defined?"), so this was
equally broken before the listop-shadow gate landed
(`news/2026-07/listop-rewrite-respects-user-routine-shadow.md`) — that gate only
changed which error appears.

Root cause confirmed 2026-08-06: `splice` is not a real multi-sub/proto in
mutsu at all — it's a special-cased "listop" compiled straight to native
array/string mutation opcodes (`src/parser/primary/ident/listop.rs` and
friends). The only accommodation for a user override,
`Compiler::user_listop_shadows`, only scans the **current file**'s literal
`SubDecl`/`ProtoDecl` statements and does an all-or-nothing handoff (never a
merge) — so it neither sees an *imported* multi candidate nor preserves the
core array-splice behavior when it does trigger locally. Full analysis and
why this needs a design pass rather than a quick patch:
[todo/deep/listops-are-not-real-multi-subs.md](../deep/listops-are-not-real-multi-subs.md).

### ~~`String::Splice` — spurious octal worry for a bare word inside `<...>`~~ — FIXED

Fixed: `news/2026-08/angle-word-leading-zero-no-octal-warning.md`.

### ~~`Text::Sorensen` — `.value` on Any~~ — not reproducible on current main

Fixed as a side effect of unrelated work; see
`news/2026-08/text-sorensen-value-on-any-not-reproducible.md`.

### ~~`Locale::Dates` — `Unknown function: Dates`~~ — FIXED

Was `Locale::Dates($locale)`, i.e. invoking a user class as a coercion. mutsu had
that path for built-in types, roles and enums but not for user classes. Fixed in
`news/2026-07/class-type-object-coercion-call.md`; both of its files now match
raku (24 subtests).

**Left over from that fix:** the same call form on a **subset** is still
unsupported — `subset Sm of Int where * < 10; Sm(5)` returns 5 in raku,
`Unknown function: Sm` in mutsu. Different mechanism (coerce to the base type,
then check the constraint).

### ~~`P5seek` — test suite failure~~ — not reproducible on current main

Fixed as a side effect of unrelated work; see
`news/2026-08/p5seek-not-reproducible.md`.

### ~~`Date::YearDay` — qualified constructor call + Date arithmetic~~ — FIXED

`self.Date::new(...)` (a qualified constructor call to a builtin ancestor)
fell back to unqualified dispatch and re-entered the caller's own `new`, and
Date `+`/`-` arithmetic only recognized the literal class name `"Date"`, so a
subclass instance was never treated as date-like. Both fixed generally; see
`news/2026-08/date-subclass-qualified-new-and-arithmetic.md`.

### ~~`PSpec` — two independent bugs~~ — FIXED

`lib/PSpec.rakumod` exports two custom word-form infix operators,
`infix:<times>` and `infix:<xxx>`, that each take a closure operand:
`20 times { $value++ }` and `{ $value--; } xxx 25`. Two independent bugs,
both fixed:

- A closure passed as an argument to a custom infix operator (the `times`
  case) did not write its mutation of an outer lexical back to the caller —
  fixed generally in `src/vm/vm_flipflop_ops.rs`
  (`news/2026-08/user-infix-closure-arg-writeback.md`).
- A leading `{ ... }` before ANY infix operator (custom or not) was never
  recognized as that operator's LHS operand — the `xxx` case, where the
  block was committed to a bare-block STATEMENT in mutsu's parse instead of
  a term. Fixed with a lookahead in `simple::block_stmt`
  (`news/2026-08/bare-block-infix-operand.md`).

### `Array::Rounded` — triaged, 4 general bugs fixed, 2 remain (needs a design pass)

Four separate, general bugs in how an `is Array` subclass interacts with construction, `nextwith`,
and fractional subscripts were found and fixed:
`news/2026-08/array-subclass-nextwith-and-num-subscript.md`. The dist's own test suite still fails
16/35 — its actual rounding mechanism is exported `multi sub postcircumfix:<[ ]>` candidates (an
operator overload on the subscript syntax, not the `AT-POS` method it also declares), which mutsu
never dispatches for `@obj[...]`; a `my @a is Rounded = ...` cross-module constant-alias `is` trait
gap also remains. Both documented, not fixed:
[todo/deep/user-postcircumfix-index-not-dispatched-for-instances.md](../deep/user-postcircumfix-index-not-dispatched-for-instances.md).

### `RSV` — triaged, root-caused, needs a compiler design pass

`lib/RSV.rakumod` declares `constant \EOV = blob8.new(255);` (and `\EOR`,
`\NULL`) directly inside `module RSV { ... }` and reads them from `sub
to-rsv`/`sub from-rsv` in the same block. mutsu resolves the bareword
reference to the **string** `"EOR"` instead of the constant's value — a
sigilless (`\NAME`) constant declared in a non-unit `module`/`package` block
is invisible to a nested `sub` referencing it, though the identical shape
inside a `class` body works. Full root-cause trace (compiler `BareWord`
resolution / `inherit_enclosing_scopes` / `constant_vars_in_scope`) and why
it needs a design pass rather than a quick patch:
[todo/tickets/sigilless-constant-invisible-in-nested-sub-inside-module.md](sigilless-constant-invisible-in-nested-sub-inside-module.md).

### `Math::Interval` — triaged, one general bug fixed, one remains (needs a design pass)

A sigilless `\name := $var` bind (used by the 2D-interval `TWEAK`, minimally:
`my $x1 = 5; my \x1 := $x1; x1 = 10; say $x1;`) was wrongly treated as
readonly, because mutsu decided mutability purely from whether a type
constraint was written on the sigilless declaration, never looking at what
the RHS actually was — the sigilled `$b := expr` path already had the right
rule (writable alias when the RHS is a plain variable, readonly otherwise)
but the sigilless path didn't share it. Fixed generally in
`src/parser/stmt/decl/my_decl_helpers.rs` (a shared `build_sigilless_bind_stmt`
helper now applies the same RHS-shape rule to all three sigilless bind forms:
`:=`, `::=`, plain `=`); pinned by `t/sigilless-bind-writable-alias.t`. See
`news/2026-08/sigilless-bind-writable-alias.md`.

The dist's own `TWEAK` actually binds four names from one **list
destructuring** (`my (\x1, \x2, \y1, \y2) := my ($x1, $x2, $y1, $y2);`),
which still fails: mutsu's destructuring-bind desugaring reads each element
back out of a temp array by index, losing per-element container identity —
the same class of gap as
[todo/deep/element-itemization-lost-in-scalar-binding.md](../deep/element-itemization-lost-in-scalar-binding.md),
tracked there.

## Triaged (round 2, 2026-08-14)

### ~~`Random::Choice`~~ — FIXED

`nqp::add_n`/`sub_n`/`mul_n`/`div_n`/`neg_n`/`abs_n` (native `num` arithmetic)
were entirely unimplemented ("Unsupported nqp:: op") — only the `_i` (native
int) family and `_n` comparisons existed in `src/runtime/nqp_ops.rs`. Added
generally; pinned by `t/nqp-native-num-arith.t`. Full suite now 6/6.

### ~~`App::SudokuHelper`~~ — not reproducible on current main

Both `t/basic.t` (9/9) and `t/combo-multi.t` (3/3) pass cleanly already —
fixed as a side effect of unrelated work between the original sweep and now.

### ~~`Mathematica::Serializer::Encoder`~~ — FIXED

`Nil ~~ UInt` wrongly returned `True` (`type_matching.rs`'s `UInt` branch had
a stray `ValueView::Nil => true` arm), so `given $obj { when UInt {...} }`
misclassified a `Nil` element and the encoder's `Pair["condo", Nil]` lost its
`NULL` output. The arm was originally added to let `$u = Nil` reset a
`UInt`-typed variable to its default (`d6fe2e3b7`, #851) — but that path is
already handled generically by a `!value.is_nil()` guard *before*
`type_matches_value` is even called (`vm_misc_typecheck.rs`), so the arm was
dead for its original purpose and only caused this smart-match regression.
Removed; `roast/S32-num/int.t` (its original motivating test, 165/165) still
passes. Pinned by `t/nil-uint-smartmatch.t`. Full suite now 3/3.

### `Crypt::RC4` — two bugs, one fixed, one deep

1. **FIXED**: `Blob() :$key!`-style coercion of an Array (`submethod
   TWEAK(Blob() :$key!)` called with a `uint8` array argument) raised
   "Impossible coercion from 'Array' into 'Blob'" — the coercion fallback in
   `try_coerce_value_with_method` only tries a target's `.new(positional)`
   when the target is a *user*-registered class (`registry().classes`);
   `Blob`/`Buf` are native types with no such registry entry, so the fallback
   never reached them even though `Blob.new(@array)` works fine when called
   directly. Fixed generally by also trying the native buf constructor for
   any `is_native_buf_constructible` target.
2. **Deep, not fixed**: after the coercion fix, the suite still dies with
   "Cannot modify an immutable Range" inside `setup()`, called from `TWEAK`
   via `@!state := setup($key)`. Root-caused to a VM-level "mark context"
   flag (`self.bind_context`, set by `MarkBindContext`) leaking across a
   *live function call* boundary — `setup()`'s own `my uint8 @state =
   0..255;` wrongly inherits the caller's pending bind-context and skips
   Range-to-array materialization. Full analysis, minimal repro, and why it
   needs auditing every compiled-function call boundary (not a local patch):
   [todo/deep/mark-context-flags-leak-across-live-call-boundary.md](../deep/mark-context-flags-leak-across-live-call-boundary.md).

   A **related but distinct** compile-time version of the same flag-leak
   class (the compiler's `bind_vardecl`, not the VM's `bind_context`) was
   found and fixed in the same investigation: `my @x := do { ...; my
   uint8 @y = 0..N; ...; @y }` leaked bind-context into the *nested* `@y`
   declaration. Fixed in `src/compiler/stmt.rs` (snapshot-and-clear
   `bind_vardecl` on entry to `Stmt::VarDecl`); pinned by
   `t/bind-do-block-nested-vardecl-leak.t`.

### `Hash::Restricted` — deep, not fixed

`trait_mod:<does>` is not a callable sub, and there is no real `Variable` MOP
object with a `.var` accessor to apply it to — `lib/Hash/Restricted.rakumod`
dynamically mixes a role into a *declared variable's* type at `my %h is
restricted = ...` time via `trait_mod:<is>(Variable:D \v, ...) {
trait_mod:<does>(v, SomeRole); v.var.WHAT.^set_name(...) }`. Genuine MOP work
per BATTERIES.md rung-2 (not a native stopgap). Full analysis:
[todo/deep/trait-mod-does-not-callable-and-no-variable-mop.md](../deep/trait-mod-does-not-callable-and-no-variable-mop.md).

### `P5tie` — deep, not fixed; two independent bugs

`scalar.rakutest`/`hash.rakutest` die with `No such method 'BIND-KEY' for
invocant of type 'Stash'` — P5tie's Perl-5-`tie()` emulation needs a real
container-binding protocol mutsu doesn't implement at all.
`array.rakutest` fails separately, at *parse* time, with
`X::Syntax::NoSelf` — not yet bisected, likely unrelated to the `tie`
gap. Full analysis:
[todo/deep/p5tie-container-protocol-and-array-parse-bug.md](../deep/p5tie-container-protocol-and-array-parse-bug.md).

### `Native::Overflow` — deep, not fixed

Plans 30 tests, runs 0: every assertion lives inside a `CATCH` that never
fires because the expected exception never gets thrown. Root cause: the dist
lexically shadows native type names (`int8`, `uint16`, ...) with `subset ...
where <range>` via its `EXPORT` sub — a real Raku feature that works fine in
mutsu for a **direct** assignment (`$a = 1000;` IS correctly type-checked).
The dist's actual test writes through a **sigilless bind alias** instead
(`my \x := $a; ...; x = $value;`, produced by a `for LIST -> \x, $value {
}` loop over a flattened variable/value list) — and assigning through such
an alias skips the target's type constraint entirely, general and
reproducible with no native-type involvement at all
(`my SmallInt $a = 5; my \x := $a; x = 1000;` — raku dies, mutsu silently
succeeds). Root cause and why a full fix needs the type constraint to live on
the container rather than a compile-time name-keyed map:
[todo/deep/sigilless-alias-assignment-skips-type-constraint.md](../deep/sigilless-alias-assignment-skips-type-constraint.md).

## Status

All dists from the original sweep's `test_die` bucket are now triaged (see
"Triaged" sections above): `Random::Choice` and `Mathematica::Serializer::Encoder`
fully fixed; `App::SudokuHelper` not reproducible; `Crypt::RC4` partially
fixed (one bug fixed, one filed as deep); `Hash::Restricted`, `P5tie`, and
`Native::Overflow` each filed as deep findings needing a design pass. None
remain un-triaged.

## How to work this list

Per `docs`/PLAN §B4 and the standing rule: **take the raku baseline first**
(`raku -I <dist>/lib <test>`), reduce to a minimal repro, verify the repro is
valid Raku by running it under raku, fix generally, pin in `t/`, one PR per root
cause. Do not trust the bucket label — in the load-only axis 4 of 6 "real mutsu
failures" turned out to be missing dependencies or harness artefacts.
