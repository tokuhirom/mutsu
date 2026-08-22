# `"non-numeric-string".Rat` should return a `Failure`, mutsu returns `Rat` `0`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Cool.rakudoc:1416`).

## Repro

```raku
say "foo".Rat.^name;   # raku: Failure   mutsu: Rat
say "foo".Rat;         # raku: dies when sunk (Failure); mutsu: prints 0
```

Contrast with `.Num`, which already fails correctly on both:

```raku
say "foo".Num;
# raku:  Cannot convert string to number: base-10 number must begin with valid digits or '.' ...
# mutsu: Cannot convert string to number: base-10 number must begin with valid digits or '.' ...  -- matches
```

## Root cause hypothesis

`Str.Rat` coercion on a non-numeric string should produce a `Failure` (a lazily-thrown
exception carrier), matching `Str.Num`'s behavior of raising a conversion error. mutsu's `.Rat`
coercion path for `Str` likely falls back silently to `0/1` (or similar) on a parse failure
instead of routing through the same failure/error path `.Num` already uses correctly — i.e. the
`.Rat` string-parsing code has its own separate, more lenient numeric-parse fallback that
swallows the error instead of reusing (or delegating to) `.Num`'s strict parser.

## Affected files (starting point)

- `Str`/`Cool` `.Rat` coercion method — likely in `builtins/methods_0arg/` or
  `runtime/builtins_*` numeric-coercion code; compare against the `.Num` implementation's
  error path to find where `.Rat` diverges into a silent zero-fallback.
