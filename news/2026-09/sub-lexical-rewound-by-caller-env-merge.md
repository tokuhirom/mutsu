# A Sub-valued lexical was rewound to the caller's routine by the post-call env merge

`Template::Jinja2`'s `{% call %}` blocks did not merely produce a wrong answer
under mutsu — they took the process down with
`fatal runtime error: stack overflow, aborting`, which is why four of that
dist's test files could not even be counted ([#7729](https://github.com/tokuhirom/mutsu/issues/7729)).
The renderer resolved the `caller` closure correctly, then invoked the *macro*
closure instead; each wrong dispatch pushed another scope, and the
`render-body → !eval → !eval-call → render-body → …` cycle ran until the stack
ran out.

## Root cause

`cheaply_unchanged` (`src/vm/vm_method_dispatch.rs`) is the O(1), conservative
"did the callee change this value?" test. It proves equality by Arc/Gc pointer
identity for the heap container types (`Str`, `Array`, `Hash`, `ContainerRef`)
and by value for the immutable scalars; anything it cannot cheaply prove counts
as changed. It had **no arm for `Sub`**, so two env entries holding literally
the same routine object always compared as changed.

That matters because a nested method call flattens the caller's scoped overlay,
copying every parent lexical into the callee's env. On return,
`merge_method_env` merges the callee overlay back and reports the entries it
considers changed in `changed_caller_locals`; the Slice F write-through then
replaces each of those names in the caller's compiled **local slot** with the
env value. For a Sub-valued lexical, that write-through fired on every
non-pure nested call even though nothing had changed.

Normally the env copy and the slot agree, so the redundant write-through is
invisible. It stops being invisible in a routine that is **re-entered through a
Sub stored in a data structure**:

1. the outer invocation binds `$callable` to the macro closure, and its
   `$callable ~~ Callable` smartmatch flushes the frame's locals into env
   (`sync_regex_interpolation_env_from_locals`), so env now carries that Sub;
2. `$callable(|@args)` runs the macro, which re-enters the same routine;
3. the inner invocation binds its own `$callable` to the `caller` closure — in
   its local slot; env still holds the *outer* invocation's Sub, inherited
   through the flattened overlay;
4. the inner invocation makes an ordinary nested method call, the merge reports
   the inherited Sub as "changed", and the write-through rewinds the inner
   slot to the outer routine;
5. calling it re-enters the macro. Repeat until the stack overflows.

## Fix

Give `cheaply_unchanged` the `Sub` arm it was missing, with the same
pointer-identity proof the other Gc-backed values already use:

```rust
(ValueView::Sub(a), ValueView::Sub(b)) => crate::gc::Gc::ptr_eq(&a, &b),
```

Distinct routine objects stay "changed" (conservative), so this only removes
write-throughs that were provably no-ops.

## Result

The reduced `{% macro %}` + `{% call %}` repro now prints `[[data]]`, matching
Rakudo. All four `Template::Jinja2` files that aborted now run to completion and
report ordinary TAP failures instead — including `15-reference-remaining` and
`18-reference-final`, which the issue had flagged as possibly a *different*
overflow. One fix covered all four.

Pinned by `t/lexical-sub-not-clobbered-by-nested-call.t` (a plain-Raku reduction
of the renderer's shape: a re-entrant private method whose Sub lexical must
survive an intervening method call) and by two unit tests on
`cheaply_unchanged` itself.
