# BEGIN-time evaluation of adverb values in extended identifiers

Split out of `todo/tickets/adverbial-pair-variable-name-syntax-incomplete.md` after
the key-less colon-pair half of that ticket was fixed (see
`news/2026-08/keyless-colon-pair-variable-names.md`). This file records the
remaining, genuinely deeper half.

## What still does not work

Raku's extended identifiers support *compile-time interpolation* of the adverb
value (`raku-doc/doc/Language/syntax.rakudoc:384`). Two spellings depend on it:

```raku
constant $c = 42;
my $a:foo<42> = "answer";
say $a:foo«$c»;      # raku: answer      mutsu: Nil
```

```raku
my $foo:bar<2> = 5;
say $foo:bar(1+1);   # raku: 5           mutsu: Nil
```

Both parse without error on mutsu; they simply resolve to a *different* name
than the declaration did, so the lookup misses and yields `Nil`.

The two are one feature, not two bugs: `«...»` interpolates (like `qqw`), while
`<...>` does not (`syntax.rakudoc` is explicit that angle brackets "mimic single
quote interpolation characteristics" and "cannot be used for the interpolation
of constant names"), and `(...)`/`[...]` hold arbitrary expressions. All of them
have to be *evaluated* and then stringified before the value becomes part of the
variable's name.

## Semantics to match (measured against real raku, 2026-08-23)

Evaluation is strictly **BEGIN-time**, not runtime:

- `constant $c = 42; ... $a:foo«$c»` works.
- `my $c = 42; ... $a:foo«$c»` does **not**: raku warns "Use of uninitialized
  value $c" and the name becomes `$a:foo<>`, then fails to compile with
  "Variable '$a:foo<>' is not declared. Did you mean '$a:foo<42>'?".
- An undeclared name is a compile error: `$a:foo«$nope»` → "Variable '$nope' is
  not declared".
- Arbitrary constant expressions fold: `$a:foo("a" ~ "b")` names `$a:foo<ab>`.

So the value must come from the compile-time constant environment, which is
exactly what mutsu's `constant` inlining (ADR-0006 §2.2) already models.

## Why this is deep, not a ticket

The obvious shape -- have the parser leave the unevaluated adverb value in the
name and let the compiler rewrite it using `constant_values` / `const_operand`
(`src/compiler/const_fold.rs:156,200`) -- founders on there being **no
normalization choke point for variable names**. A survey on 2026-08-23 found:

- The name is a bare `String` on ~8 AST variants: `Expr::Var` / `CaptureVar` /
  `ArrayVar` / `HashVar` / `CodeVar` (`src/ast.rs:417-421`),
  `Expr::AssignExpr.name` (`:601`), `Stmt::VarDecl.name` (`:801`),
  `Stmt::Assign.name` (`:833`), and the `Stmt::Mark*` siblings (`:823-832`).
- Consumption is scattered, not funneled: ~104 `local_map.get`/`contains_key`
  lookups across 20 files in `src/compiler/`, each keying directly off the raw
  AST string, plus ~180 `Expr::Var(` and ~30 `Stmt::VarDecl` pattern matches in
  the compiler and ~133 more in `src/runtime/`, `src/vm/`, `src/whatever_curry/`
  and `src/rakuast/`.
- Two AST-level collectors never see the compiler at all:
  `collect_all_my_decl_names` (`src/ast.rs:1288`, called from
  `src/opcode.rs:4643`) and `collect_routine_body_local_names`
  (`src/ast.rs:1369`, called from `src/runtime/calls.rs:90`).

Slot *minting* is well funneled (`alloc_fresh_local`, `src/compiler/mod.rs:1800`,
is the sole `local_map.insert`) and package qualification funnels through
`qualify_variable_name` (`mod.rs:1742`), but neither is on the path of every
*read* of a name.

The parser cannot do the job either: it is a pure `&str -> AST` pass whose
results are **memoized** (`src/parser/memo.rs`), so giving it a mutable
"constants seen so far" table would make memo hits depend on state the key does
not capture -- a correctness hazard well out of proportion to the feature.

## Sketch of the real fix

1. Parser: stop canonicalizing adverb values that need evaluation, and instead
   preserve their source spelling in the name so a later pass can recognize
   them (`«...»` whose content holds a sigil; `(...)`/`[...]` whose items are
   not plain quoted words). The purely literal spellings must keep
   canonicalizing at parse time exactly as they do now -- that path is what
   `roast/S02-names-vars/varnames.t` (29 tests) pins.
2. A recursive **AST pre-pass** that rewrites every name-bearing variant listed
   above, run before `compile_stmt`/`compile_expr`, ordered so a `constant`
   declaration is folded before any later name that mentions it.
3. Relax the sigil-collision guard in `constant_value`
   (`src/compiler/const_fold.rs:164`): it currently refuses to resolve a
   `constant $c` because the sigil-stripped key `"c"` collides with `local_map`,
   and `const_operand` only resolves `Expr::BareWord`. The documented spelling
   uses a `$`-sigil constant, so both need to handle it.
4. Report an undeclared/non-constant interpolation as a compile-time error
   rather than silently producing a name that will never match.

Step 2 plus step 3 are the parts that want an ADR: a general AST name-rewriting
pre-pass is new machinery, and loosening the constant-inlining guard touches
ADR-0006 §2.2's shadowing rules.

## Priority

Low, and worth weighing before starting. There is **no roast coverage** of this:
`roast/S02-names-vars/varnames.t` is the only roast file exercising adverbial
variable names, it tests only literal values across `<>`/`«»`/`[]`/`()`, and it
already passes. The gap is documentation-driven only (found by the doc-diff
harness against `Language/syntax.rakudoc:384`). Weigh that against introducing a
whole-AST name-rewriting pass on the hot compile path.

## Minimal repro

```raku
constant $c = 42;
my $a:foo<42> = "answer";
say $a:foo«$c»;      # expected: answer, got: Nil

my $foo:bar<2> = 5;
say $foo:bar(1+1);   # expected: 5, got: Nil
```
