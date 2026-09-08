# BEGIN-time evaluation of adverb values in extended identifiers

Raku's extended identifiers (`Language/syntax.rakudoc`, "Extended identifiers")
*evaluate* the adverb value before it becomes part of a variable's name. Three
of the four bracket spellings depend on that:

```raku
constant $c = 42;
my $a:foo<42> = "answer";
say $a:foo«$c»;      # answer -- «...» interpolates, like qqw
my $foo:bar<2> = 5;
say $foo:bar(1+1);   # 5      -- (...) / [...] hold an expression list
```

`<...>` deliberately does neither — the documentation is explicit that angle
brackets "mimic single quote interpolation characteristics" and "cannot be used
for the interpolation of constant names".

mutsu parsed all of these without error but canonicalized them literally, so
`$a:foo«$c»` named `$a:foo<$c>` and `$foo:bar(1+1)` named `$foo:bar<1+1>`. Both
missed the declaration and evaluated to `Nil`. `roast/S02-names-vars/names.t`'s
"can use compile-time variables in names" subtest is the spec test for it.

## Why the parser could not do it

The value has to come from the compile-time `constant` environment, which the
parser does not have: it is a pure `&str -> AST` pass and its results are
memoized (`src/parser/memo.rs`), so giving it a mutable "constants seen so far"
table would make memo hits depend on state the key does not capture.

The obvious alternative — a recursive AST pre-pass that rewrites every
name-bearing variant before compilation — founders on there being no
normalization choke point for variable names: the name is a bare `String` on
about a dozen `Expr`/`Stmt` variants and is consumed at roughly a hundred
`local_map` lookups across `src/compiler/`.

## What was done instead

The parser keeps canonicalizing every spelling it can decide alone, and leaves
the rest wrapped in a U+0001 sentinel that preserves the source spelling in the
name (`src/adverb_name.rs` owns the convention both sides speak). The compiler
finishes them at the top of `compile_expr` / `compile_stmt`
(`src/compiler/adverb_interp.rs`), where the `constant` environment built by the
statements compiled so far is already in scope. That gets the BEGIN-time
ordering right for free — a `constant` is recorded before any later statement
that mentions it — and needs no recursive rewriter, only the handful of variants
that carry a name: `Expr::Var` / `CaptureVar` / `ArrayVar` / `HashVar` /
`CodeVar` / `AssignExpr`, and `Stmt::VarDecl` / `Assign` / `Mark*`. Everything
else recurses through ordinary compilation, so a nested use is reached the same
way any other expression is.

Only the spellings that need it take that route:

- `«...»` is marked when it mentions a sigil; without one it is plain `qw` and
  canonicalizes at parse time as before.
- `(...)` / `[...]` are marked unless every comma item is a plain quoted word —
  which is exactly the shape `roast/S02-names-vars/varnames.t` (29 tests)
  exercises, so the common case never leaves the parser.

Two supporting changes:

- `Compiler::compile_time_constant` reads the same `constant` environment as
  `constant_value` but without the `const_fold_enabled` gate, and
  `note_constant_decl` records unconditionally. Inlining a constant read is an
  optimization and is rightly switched off with folding (`MUTSU_CONST_FOLD=0`,
  or a unit that declares its own operators); deciding *which variable a program
  is talking about* is not.
- `const_operand_begin_time` additionally resolves a sigilled `constant $c`
  read (`Expr::Var`), which ordinary folding leaves alone because it shares its
  `local_map` key with a `my $x` and would need shadowing rules the folder does
  not have (ADR-0006 §2.2). The BEGIN-time mode is confined to adverb values,
  whose result is a name rather than a value substituted into running code, so
  the folding rules are untouched.

Canonicalization also now normalizes whitespace, matching rakudo: `$a:foo< a  b >`,
`$a:foo«a b»`, `$a:foo['a','b']` and `$a:foo('a','b')` are one name.

## Failure modes

BEGIN time means only a `constant` is visible. raku rejects a runtime lexical
there ("Use of uninitialized value $x", then "Variable '$a:foo<>' is not
declared"), and mutsu now reports the offending variable directly instead of
silently producing a name that can never match. An `EVAL`'s static
undeclared-variable check skips a name still awaiting evaluation, since the
unevaluated spelling is not the one that will be looked up.

A `(...)` / `[...]` value that does not evaluate to a compile-time constant
falls back to the parser's literal reading rather than erroring: that spelling
is also how a plain word list is written (`$today:foo('a','b')`), and mutsu's
constant folder does not model every expression raku's BEGIN-time evaluation
would.

Pinned by `t/adverb-value-begin-time-interpolation.t` (26 tests).
