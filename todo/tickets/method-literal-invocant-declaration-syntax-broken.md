# `method` literal invocant-declaration syntax variants (named / type-only) are broken

Discovered via the doc-diff harness on `raku-doc/doc/Type/Metamodel/MethodContainer.rakudoc`
(around line 15) and `raku-doc/doc/Type/Method.rakudoc` (around line 18). Two related but
distinct bugs in how a `method (...)` literal declares its invocant.

## Bug 1 — a named invocant (`method ($x:) { ... }`) doesn't bind `$x` from the invocant

```raku
my $m = method ($invocant: $param) {
    say "$invocant: '$param'";
}
"greeting".$m("hello");
```

- `raku`: `greeting: 'hello'`
- `mutsu` (`target/debug/mutsu`): `Too few positionals passed; expected 3 arguments but got 2`

A shorter repro without a trailing positional, showing the invocant name itself is simply not
declared as a variable in the method body:

```raku
my $m = method ($x:) { say $x };
5.&$m;
```

- `raku`: `5`
- `mutsu`: `Too few positionals passed; expected 2 arguments but got 1`

The same shape crashes differently when used via `.^add_method`:

```raku
Int.^add_method('double', method ($x:) { 2 * $x });
say 21.double;
```

- `raku`: `42`
- `mutsu`: `Variable '$x' is not declared` (a runtime error, not a compile error — the method
  body compiles, but `$x` was never bound/allocated as a local)

## Bug 2 — a type-only, unnamed invocant (`method (List:D:) { ... }`) is a hard parse error

```raku
<a b c>.&(my method (List:D:) { say self.raku; self }).say;
```

- `raku`: `("a", "b", "c")` then `(a b c)` (the block runs with `self` bound to the invocant,
  type-checked against `List:D`)
- `mutsu`: `===SORRY!=== ... Confused. expected statement: expected expression statement or ')'`
  — the parser doesn't accept a bare type constraint (no parameter name) followed directly by
  `:` inside a method-literal signature.

## Root cause hypothesis

Both bugs point at the same area: the method-literal signature parser/compiler's handling of the
invocant declaration (the part before the first `:` in a `method (...)` parameter list) only
supports the default, unwritten `self:` case. It does not:
1. Allocate/bind a *named* invocant parameter (`$x:`) as a normal parameter local the body can
   read (Bug 1) — the invocant is consumed as the call's receiver but never registered under its
   given name, so the parameter-count bookkeeping for the *rest* of the signature is also thrown
   off by one (hence "expected N but got N-1").
2. Even *parse* a type-only invocant declaration with no name at all (`List:D:`) (Bug 2) — likely
   because the invocant-parsing code expects a leading `$name` token and doesn't fall back to
   accepting a bare type-constraint term before the `:`.

## Affected files (starting point)

- `src/parser/` — signature parsing for `method (...) { }` literals / `method` declarations,
  specifically how the pre-`:` invocant token is recognized (name vs. type-only)
- `src/compiler/` — signature-to-bytecode lowering for the invocant parameter, to bind a named
  invocant into the method body's locals like any other parameter
