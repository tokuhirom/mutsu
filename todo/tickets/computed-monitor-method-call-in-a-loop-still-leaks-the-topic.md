# A computed-name monitor method call inside a loop still loses the topic

```raku
use OO::Monitors;
monitor M { method d($p) { 1 } }
my $m = M.new;

for <d> -> $n { $_ = 'C'; $m."$n"('x'); say "dyn in for:    ", $_.raku }   # (Any)  -- wrong
for <d> -> $n { $_ = 'E'; $m.d('x');    say "static in for: ", $_.raku }   # "E"    -- right
$_ = 'D'; $m."d"('x');                  say "dyn top level: ", $_.raku;    # "D"    -- right
```

`raku` answers `"C"` for the first line.

Only the combination matters: a **computed** method name (`."$n"(…)`, which
compiles to `CallMethodDynamicMut`) on a **monitor** invocant, **inside a loop
body**. Any one of the three on its own is fine.

## Context

`news/2026-08/monitor-method-no-longer-leaks-topic-and-self.md` fixed the general
case: the wrap-chain dispatch in `vm/vm_call_method_compiled.rs` was writing the
wrapper's persisted closure-env overrides — including `$_` and `self` — back into
the caller's env. Excluding `_`/`/`/`!`/`self` there fixed the static and the
top-level computed call, but not this shape, so the dynamic-dispatch path must
reach the caller's topic by some other route (`exec_call_method_dynamic_mut_op`
in `vm/vm_call_method_mut_ops.rs` is the entry point to look at).

Not currently known to block anything — Cro calls its monitor methods by literal
name — but it is the same family and cheap to keep on file.
