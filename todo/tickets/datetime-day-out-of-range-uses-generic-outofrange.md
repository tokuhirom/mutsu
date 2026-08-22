# `DateTime` range-check errors throw generic `X::OutOfRange` instead of `X::Temporal::OutOfRange`

Found by the doc-diff harness batch-4 re-run (`docs/doc-diff-backlog.md`,
`Type/DateTime.rakudoc:137`).

## Root cause

mutsu already registers `X::Temporal::OutOfRange` as a proper exception subclass of
`X::OutOfRange` (`src/runtime/runtime_init.rs:2414`,
`register_x("X::Temporal::OutOfRange", "X::OutOfRange", &["X::Temporal"]);`), but the
actual `DateTime`/`Date` range-check helpers in
`src/builtins/methods_0arg/temporal.rs` (the `day`/`second` out-of-range builders
around lines 70-113) construct the error with
`RuntimeError::typed("X::OutOfRange", attrs)` — the generic parent type — instead of
`"X::Temporal::OutOfRange"`. So the specific type is registered and introspectable but
never actually used at the real throw site.

## Minimal repro

```raku
say DateTime.new("2012-02-29T12:34:56Z").clone(year => 2015);
CATCH { default { put .^name, ': ', .Str } };
```

- `raku`: `X::Temporal::OutOfRange: Day out of range. Is: 29, should be in 1..28`
- `mutsu` (`target/debug/mutsu`): `X::OutOfRange: Day out of range. Is: 29, should be
  in 1..28` (same message text, wrong/too-generic exception type — a
  `try { ... }; CATCH { when X::Temporal::OutOfRange { ... } }` written against the
  documented type would not catch it in mutsu).

## Affected files (starting point)

- `src/builtins/methods_0arg/temporal.rs` — the `day`/`second` out-of-range error
  builders (~lines 70-113) that call `RuntimeError::typed("X::OutOfRange", attrs)`;
  switch them to `"X::Temporal::OutOfRange"` (the type is already registered and
  `does` `X::Temporal`, so no registration changes should be needed).
