# A parenthesized anonymous `role` declaration fails to parse as an expression term

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`,
`Type/Metamodel/ParametricRoleHOW.rakudoc:29`).

## Repro

```raku
(role Zape[::T] {}).HOW.say;
(role Zape2 {}).HOW.say;
```

- `raku`: both succeed, printing `Perl6::Metamodel::ParametricRoleHOW.new`.
- `mutsu` (`target/debug/mutsu`): both are a **hard parse error**:
  ```
  ===SORRY!=== Error while compiling ...
  Confused. expected statement: expected use statement or import statement or no statement or need statement or unit statement or ...
  ```

Verified directly, and narrowed to a plain (non-parameterized) `role` declaration too:

```
$ target/debug/mutsu -e '(role Zape2 {}).HOW.say;'
===SORRY!=== ... Confused. expected statement ...
$ target/debug/mutsu -e '(class Foo {}).HOW.say;'
Perl6::Metamodel::ClassHOW.new
```

So `(class NAME {...})` already parses fine as a parenthesized expression term, but the
equivalent `(role NAME {...})` does not — this is `role`-declaration-specific, not a
general "package declaration in expression position" gap.

## Root cause hypothesis

The parser's handling of `role` declarations (in whatever module recognizes
`class`/`role`/`grammar`/`enum` as expression-position terms — likely near wherever
`class {...}` as a term is special-cased) doesn't extend to `role`. Since `class {...}`
already works as a term, the fix is probably adding `role` (and checking `grammar`)
alongside it in the same dispatch point.

## Affected files (starting point)

- `src/parser/` — wherever an anonymous `class {...}` expression-term is recognized;
  grep for how that dispatches and check why `role` isn't included.
