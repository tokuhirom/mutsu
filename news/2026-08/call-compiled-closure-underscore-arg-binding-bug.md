# `call_compiled_closure` no longer binds `$_` for a bare block whose body reads `@_` instead

`{ say "topic=$_ args=@_[]" }.(1,2,3)` used to print `topic=1 args=1 2 3` in
mutsu (`raku` prints `topic= args=1 2 3`): `call_compiled_closure_with_topic`
(`src/vm/vm_closure_dispatch.rs`) unconditionally bound the implicit topic
`$_` to the first positional argument of a bare block whenever the block
declared no explicit signature/placeholder params — regardless of whether
the block's body actually read `@_` instead of `$_`.

The tree-walk closure-call branch in `call_sub_value`
(`src/runtime/resolution_call_sub.rs`) already got this right: it calls
`method_signature_shared::auto_signature_uses(&data.body)`, an AST scan that
detects a bare `@_` read in the body, and skips the `$_` bind in that case.
Both code paths compute `data.params` identically for a plain bare block (it
is empty either way, since `collect_placeholders_shallow` does not treat
`@_` as a placeholder param), so `call_compiled_closure`'s narrower
params-only check could not tell the two shapes apart.

## Fix

`call_compiled_closure_with_topic` now runs the same `auto_signature_uses`
scan (guarded on `data.params.is_empty()`, since a block with any explicit/
placeholder param already takes a different branch) and skips the implicit
`$_` bind when the body reads `@_`. Verified against `raku`'s actual rule:
reading `@_` anywhere in the body suppresses the `$_` auto-bind entirely
(even when `$_` is also read in the same body) and lifts the 0-or-1 arity
restriction that a `$_`-only bare block has.

## Tests

`t/bare-block-args-underscore-not-topic.t` (new) — covers: a bare block
reading only `@_` (no `$_` bind, any arity); the no-args case still
inheriting the caller's topic; a bare block reading only `$_` (still binds,
arity restricted to 0-or-1); a block reading both `$_` and `@_` (still no
`$_` bind, matching raku).

Found while auditing `todo/deep/eval-block-value-recompiles-every-call.md`'s
larger fork-`call_sub_value`-into-`call_compiled_closure` proposal for
feature parity between the two closure-invocation paths — independent of
that larger effort.

PR [#TBD](https://github.com/tokuhirom/mutsu/pull/TBD).
