# `call_compiled_closure` binds `$_` for a bare block even when the body wants `@_` instead

## Summary

`call_compiled_closure_with_topic` (`src/vm/vm_closure_dispatch.rs`, the
`uses_positional` check around line 559) unconditionally binds the implicit
topic `$_` to the first positional argument of a bare block whenever
`data.params` contains no non-`_`/non-`:` entry — regardless of whether the
block's body actually reads `@_` instead of `$_`. The tree-walk closure-call
branch in `call_sub_value` (`src/runtime/resolution_call_sub.rs`, around line
559-560) gets this right: it additionally calls
`crate::method_signature_shared::auto_signature_uses(&data.body)`, an AST scan
that detects a bare `@_` read in the body, and skips the `$_` bind in that
case.

Both code paths compute `data.params` identically for a plain bare block
(`{ ... }` with no explicit signature) — it is empty either way, since
`collect_placeholders_shallow` does not treat `@_` as a placeholder param — so
`call_compiled_closure`'s narrower params-only check cannot tell the two
shapes apart and always binds `$_`.

## Repro

Confirmed live against the current binary, comparing two call paths for the
identical body shape:

```
# .() direct call routes to call_compiled_closure via vm_call_on_value
$ raku -e '{ say "topic=$_ args=@_[]" }.(1,2,3)'
topic= args=1 2 3
$ target/debug/mutsu -e '{ say "topic=$_ args=@_[]" }.(1,2,3)'
topic=1 args=1 2 3          # WRONG — $_ should stay empty since the body uses @_

# Promise.then routes through call_sub_value's tree-walk branch (merge_all=true)
$ target/debug/mutsu -e 'await Promise.new.keep(1).then({ say "topic=$_ args=@_[]" })'
topic= args=Promise(Kept)   # correct — matches raku
```

## Affected files

- `src/vm/vm_closure_dispatch.rs` — `call_compiled_closure_with_topic`,
  `uses_positional` computation around line 559.
- `src/method_signature_shared.rs` — `auto_signature_uses` (the correct
  reference implementation, already used by the tree-walk branch).

## Fix direction

Call `auto_signature_uses(&data.body)` from `call_compiled_closure_with_topic`
the same way the tree-walk branch does, and skip the implicit `$_` bind when
the body reads `@_` instead. `data.body` is already available on the `SubData`
passed in, so this needs no new plumbing — just reusing the existing helper.

## How this was found

Investigating `todo/deep/eval-block-value-recompiles-every-call.md`'s "larger
fix" (routing `call_sub_value`'s general closure branch through
`call_compiled_closure`). An audit agent comparing the two closure-invocation
paths for feature parity found this as a live, already-present divergence
between `.()` (which already uses `call_compiled_closure` via
`vm_call_on_value`) and other call sites still on the tree-walk branch —
independent of whatever the audit concludes about the larger fork.
