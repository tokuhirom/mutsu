# A computed-name monitor method call inside a loop no longer leaks the topic

```raku
use OO::Monitors;
monitor M { method d($p) { 1 } }
my $m = M.new;

for <d> -> $n { $_ = 'C'; $m."$n"('x'); say $_.raku }  # raku: "C"  mutsu (before): (Any)
```

`news/2026-08/monitor-method-no-longer-leaks-topic-and-self.md` fixed the
general case: the wrap-chain dispatch in `vm/vm_call_method_compiled.rs` was
writing the wrapper's persisted closure-env overrides — including `$_` and
`self` — back into the caller's env, and excluding `_`/`/`/`!`/`self` there
fixed both a static call and a top-level computed call. But a **computed**
method name (`."$n"(…)`, which compiles to `CallMethodDynamicMut`) on a
**monitor** invocant **inside a loop body** still lost the topic: unlike that
shared write-back site, `exec_call_method_dynamic_mut_op`
(`vm/vm_call_method_mut_ops.rs`) already saved and restored the caller's
`self` around its own dispatch, but never did the same for `$_` — so its own
save/restore gap was a second, independent hole in the same family.

Fixed by saving and restoring `$_` around the dispatch the same way `self`
already was. No new mechanism was needed — just filling in the missing half
of an existing pattern.

Pin: `t/monitor-dynamic-dispatch-topic-leak-in-loop.t` (computed name in a
loop, static name in a loop as a no-regression check, and computed name at
top level as another no-regression check — all three match `raku`).
