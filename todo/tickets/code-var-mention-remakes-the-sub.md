# `&f` re-materializes a fresh `Sub` on every mention, so its identity is unstable

Extracted from PLAN.md §8.1 (2026-08-02); found 2026-07-23 by the generic-`WHERE` slice and
re-verified on `main` 2026-08-02.

## Repro

```raku
sub f() { 1 }
say &f.WHICH;   # mutsu: Sub|27
say &f.WHICH;   # mutsu: Sub|29     raku: the same value both times
```

## Root cause

`sub_value_from_function_def` builds a **new** `SubData` (fresh id + fresh env snapshot) on every
`resolve_code_var`, so each mention of `&f` is a different object. `&f === &f` masks the bug because
identity there is decided by a fingerprint comparison rather than the id, and `&f.WHERE` inherits the
same instability.

## Why it is not a small slice

A stable per-`FunctionDef` identity interacts with the wrap-chain machinery: `wrap_chains` is keyed
by `data.id`, so making the id stable changes which wraps a given code object sees. The fix has to
decide where the canonical `Sub` value for a `FunctionDef` lives (and when its env snapshot is
refreshed) before the id can be reused.

## Affected files

`src/runtime/` code-variable resolution (`resolve_code_var`, `sub_value_from_function_def`), and the
wrap-chain registry keyed by `SubData::id`.
