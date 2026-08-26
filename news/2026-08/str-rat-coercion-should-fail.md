# `"foo".Rat` returns a `Failure` instead of a silent `0`

`Str.Rat` was the one numeric coercion on `Str` that swallowed a parse failure:
`"foo".Rat.^name` was `Rat` (value `0`) where Rakudo gives `Failure`, and
`say "foo".Rat` printed `0` where Rakudo dies with `X::Str::Numeric`. Found by
the doc-diff harness (`Type/Cool.rakudoc:1416`).

## Root cause

A 57-value probe of `"foo"`, `"12abc"`, `""`, `"   "`, `"3.14"` and `" 42 "` across
`.Rat`/`.Int`/`.Num`/`.Complex`/`.FatRat` showed that **only `.Rat` diverged** —
every sibling already produced the lazy `X::Str::Numeric` `Failure`, and every
one already treated the empty and whitespace-only strings as a plain numeric
zero (that is Rakudo's behaviour too, not a bug).

The guard was literally missing from one arm. `native_method_0arg`'s `"FatRat"`
arm in `src/builtins/methods_0arg/dispatch_core_math.rs` already read

```rust
ValueView::Str(s)
    if crate::runtime::str_numeric::parse_raku_str_to_numeric(s.trim()).is_none() =>
    { ... str_numeric_failure(&s) ... }
```

before falling through to `str_to_rat`, while the `"Rat"` arm went straight to
`str_to_rat`, whose own `.parse()` calls end in `.unwrap_or_default()` — a silent
`0`. Adding the same guard to `"Rat"` makes the two arms symmetric, so `.Rat`
now shares the exact exception, message, `pos` and source-indicator that
`.Int`/`.Num` produce (`Cannot convert string to number: base-10 number must
begin with valid digits or '.' in '<HERE>foo'`), including the
`trailing characters after number` variant for `"12abc"`.

## Result

`"foo".Rat` and `"12abc".Rat` are `Failure`s that are falsy, undefined, and throw
when used unhandled; `"".Rat`, `"   ".Rat`, `"3.14".Rat`, `" 42 ".Rat` and
`"1/4".Rat` are unchanged. Pinned by `t/str-coercion-and-dispatch.t`.
