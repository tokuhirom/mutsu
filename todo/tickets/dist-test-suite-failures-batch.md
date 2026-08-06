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

### `PSpec` — triaged; one bug fixed, one gap needs a parser design pass

`lib/PSpec.rakumod` exports two custom word-form infix operators,
`infix:<times>` and `infix:<xxx>`, that each take a closure operand:
`20 times { $value++ }` and `{ $value--; } xxx 25`. Two independent bugs:

- A closure passed as an argument to a custom infix operator (the `times`
  case) did not write its mutation of an outer lexical back to the caller —
  fixed generally in `src/vm/vm_flipflop_ops.rs`
  (`news/2026-08/user-infix-closure-arg-writeback.md`).
- A leading `{ ... }` before ANY infix operator (custom or not) is never
  recognized as that operator's LHS operand — the `xxx` case, where the
  block is a bare block STATEMENT in mutsu's parse, not a term. This is a
  genuine grammar-ambiguity lookahead feature needing a design pass:
  [todo/deep/bare-block-as-infix-operand-not-recognized.md](../deep/bare-block-as-infix-operand-not-recognized.md).

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

## Un-triaged `test_die` / `test_fail`

`Math::Interval`, `Native::Overflow`, `App::SudokuHelper`, `P5tie`,
`Mathematica::Serializer::Encoder`, `Hash::Restricted`, `Crypt::RC4`,
`Random::Choice` (die).

## How to work this list

Per `docs`/PLAN §B4 and the standing rule: **take the raku baseline first**
(`raku -I <dist>/lib <test>`), reduce to a minimal repro, verify the repro is
valid Raku by running it under raku, fix generally, pin in `t/`, one PR per root
cause. Do not trust the bucket label — in the load-only axis 4 of 6 "real mutsu
failures" turned out to be missing dependencies or harness artefacts.
