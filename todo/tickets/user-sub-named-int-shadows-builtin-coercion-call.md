# A user `sub Int(...)` wrongly shadows the built-in `Int(...)` type-coercion call syntax

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Sub.rakudoc:19`).

## Root cause

In real Raku, declaring a sub with the same name as a core type coercer (e.g.
`sub Int(Str $s) {...}`) does NOT shadow the bareword type-coercion call `Int(x)` — that
call form keeps resolving to the built-in `Int` coercer. Only an explicit `&Int(x)` call
(taking the routine by its `&`-sigiled name) reaches the user-defined sub. mutsu resolves
*both* call forms to the user sub, losing access to the built-in coercion entirely once a
same-named sub is declared.

## Minimal repro

```raku
sub Int(Str $s){'what?'};
say [Int, Int('42'), &Int('42')];
```

- `raku`: `[(Int) 42 what?]` — `Int` (bare term) is still the type object, `Int('42')`
  still calls the built-in coercer (`42`), and only `&Int('42')` reaches the user sub
  (`what?`).
- `mutsu` (`target/debug/mutsu`): `[(Int) what? what?]` — `Int('42')` also resolves to the
  user sub.

## Affected files (starting point)

- Wherever a bareword call `Name(...)` is resolved against user-declared subs vs. the
  built-in type-coercer table — likely in the call-dispatch/resolution path (`runtime/
  calls.rs`, `runtime/dispatch.rs`, or the compiler's call-site resolution in
  `compiler/expr.rs`/`compiler/helpers_call_args.rs`). The fix needs the bareword-call
  form for a name that collides with a core type name to prefer the built-in coercer
  unless there is no user sub at all under that name reachable via `&Name`, matching
  Raku's rule that user subs never occlude built-in type-coercion call syntax for a
  colliding name — only the explicit `&`-sigiled call reaches the user definition.
