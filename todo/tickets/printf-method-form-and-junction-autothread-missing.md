# `.printf` method form is unimplemented on Str, and format directives don't autothread over a Junction

Found by the doc-diff harness (`docs/doc-diff-backlog.md`,
`Type/independent-routines.rakudoc:687` and `:692`).

## Bug 1: `.printf` method form missing

The doc (`independent-routines.rakudoc:677-688`) documents `printf` as callable both
as a sub (`printf($format, *@args)`) and as a method
(`$format.printf(*@args)`) — the same convention as `sprintf`, which mutsu already
supports as a method.

```raku
"%s is %s".printf("thor", "mighty");    # method form
printf( "%s is %s", "thor", "mighty");  # sub form
```

- `raku`: both print `thor is mighty`.
- `mutsu` (`target/debug/mutsu`): the sub form works; the method form dies with
  `No such method 'printf' for invocant of type 'Str'` (mutsu even suggests `Did you
  mean 'print'?`, confirming `printf` isn't registered as a Str method at all).

## Bug 2: format directives don't autothread over a Junction

```raku
printf( "%.2f ", 1/3 | 1/4 | 3/4 );
```

- `raku`: `0.33 0.25 0.75 ` — the doc notes "On Junctions, it will also autothread,
  without a guaranteed order."
- `mutsu`: dies with `Directive %f not applicable for type Junction` — `printf`/
  `sprintf`'s format-directive matching doesn't special-case a Junction argument by
  autothreading over its members the way most numeric operators already do.

## Affected files (starting point)

- Wherever `sprintf` is registered as a Str method (grep for `"sprintf"` in
  `src/builtins/methods_narg.rs` or similar) — add `printf` as a method following the
  same pattern (format the string via the existing sprintf logic, then write it via
  `print`).
- The format-directive type-matching logic (grep for `"not applicable for type"`) —
  needs a Junction-autothreading branch, likely reusing the same autothreading
  machinery other builtins already use for Junction arguments.
