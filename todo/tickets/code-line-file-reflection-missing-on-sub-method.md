# `.line`/`.file` reflection methods missing on `Sub`/`Method`

Found by the doc-diff harness batch-4 re-run (`docs/doc-diff-backlog.md`,
`Type/Code.rakudoc:166,175,195`).

## Root cause

`Code` (and its subtypes `Sub`, `Method`) support `.file` and `.line` reflection
methods that report the source location where the routine was declared. mutsu never
registered these methods for `Sub`/`Method` invocants at all, so any call throws
`X::Method::NotFound`.

## Minimal repro

```raku
class Food { has $.ingredients; method eat {} }
say Food.^lookup('eat').line;
say &infix:<+>.file;
say &infix:<+>.line;
```

- `raku`: prints a line number for `.line` (the exact value differs by
  Rakudo version — the doc's own stated `# OUTPUT` of `4`/`1`/`208` is stale/doc-drift
  against current raku's `2`/`2`/`209`, but raku always succeeds and returns an `Int`),
  and `.file` on a core-setting sub prints `SETTING::src/core.c/Numeric.rakumod`.
- `mutsu` (`target/debug/mutsu`): both throw
  `No such method 'line'/'file' for invocant of type 'Method'/'Sub'`.

```
$ target/debug/mutsu -e 'class Food { has $.ingredients; method eat {} }; say Food.^lookup("eat").line;'
No such method 'line' for invocant of type 'Method'
Did you mean 'clone'?
  in block <unit> at -e line 1
```

Note: the *value* raku reports for user-defined subs is drift-prone (it depends on
exactly where in the source the routine's body starts, which shifts between Rakudo
versions), so a fix does not need to match the doc's literal numbers — it needs to
return a real line number (and for `.file`, the declaring file path or a
`SETTING::...` marker for core-setting routines) instead of erroring.

## Affected files (starting point)

- Wherever `Code`/`Sub`/`Method` 0-arg reflection methods are dispatched — likely
  `src/builtins/methods_0arg/` (grep for existing `Code` reflection methods like
  `.name`, `.arity`, `.candidates`) plus wherever a `Sub`/`Method` value's declaration
  site (source file + line) is tracked in the AST/compiler, since that's the data
  `.file`/`.line` need to surface.
