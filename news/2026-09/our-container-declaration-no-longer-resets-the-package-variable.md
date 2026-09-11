# A bare `our @a;` / `our %h;` no longer resets the package variable

`our %Store; BEGIN %Store = (a => 1); say %Store<a>;` answered `(Any)`, where
rakudo answers `1`. The write was not lost anywhere exotic: a `BEGIN` phaser
body runs *before* the declaration statement that precedes it in the source, and
the declaration then overwrote the populated hash with an empty one — silently,
with no error, which is worse than failing. The array half behaved identically
(`our @L; BEGIN @L = (1,2)` gave `[]`), while an `our` *scalar* and a `my`
container both survived.

## Root cause

The parser synthesizes a default RHS for every uninitialized declaration, and
its shape differs by sigil: `Literal(Nil)` for `$`/`&`, an empty
`Literal(Array)` for `@`, and an empty `Expr::Hash` for `%`. The compiler's
`our` path recognised only the first of the three:

```rust
let is_our_redecl_nil =
    *is_our && matches!(expr, Expr::Literal(lit) if lit.is_nil());
```

A match loaded the existing package variable (`OpCode::GetOurVar`) instead of
the synthesized default, which is why `our $x = 3; ... our $x` preserved the 3.
A `%`/`@` declaration failed that test, took the ordinary
compile-RHS-and-store path, and reset the container on every execution of the
declaration.

Two things follow from "on every execution", and the `BEGIN` repro is only the
more visible one. The reset also fired on each call of a sub containing a bare
`our` container, so

```raku
sub bump() { our %Count; %Count<n>++ }
bump; bump;
our %Count; say %Count<n>;    # was 1, rakudo says 2
```

started from an empty hash every time.

## Fix

The `my` path already had a correct predicate for "is this the parser's
synthesized default", including the `__has_initializer` trait check that keeps
an explicit `our %h = ()` distinguishable from a bare `our %h;`. That predicate
is now `Compiler::is_synthesized_decl_default`, shared by both paths, and the
`our` test is spelled in terms of it.

`GetOurVar` had to grow one matching case: it answered `Value::NIL` for a name
with nothing stored, which is right for a scalar but would have made a
first-ever `our %h;` declare `Nil` rather than an empty `Hash`. It now defaults
by sigil — an empty `Array` for `@`, an empty `Hash` for `%`, `Nil` otherwise.

The expression-position declaration path (`my $x = (our %h)`) carried the same
gap in its own container branch and got the same treatment, so both paths now
load rather than reset.

## Where it was reached

Found by P5 triage of the first full-corpus ecosystem sweep (#7785) while
locating parse failures in the `blocked_load` bucket, through **`PDF::Content`**
— `lib/PDF/Content/Ops.rakumod:248` opens `BEGIN %Store = (`, a large
compile-time operator table. `our %H; BEGIN %H = (...)` is the standard way to
build a lookup table once at compile time, so the construct is not obscure.

Pinned by `t/modules/our-container-decl-preserves-value.t` (17 assertions,
verified identical against the rakudo oracle): the `BEGIN` repros for `%`, `@`
and `$`, the fresh-declaration types and emptiness, explicit initializers
(including an explicit `= ()` clear) still running, bare redeclaration
preserving contents, the sub-body accumulation case, expression position, and a
`module`-scoped declaration reaching its qualified name.

Closes #7953.
