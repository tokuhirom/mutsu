# `$v := 42; $v = 5` dies again in the same frame

```raku
my $v = 1; $v := 42; $v = 5;   # raku: Cannot assign to an immutable value
```

mutsu assigned `5`. The `:=` store in `exec_set_local_op_inner`
(`src/vm/vm_var_assign_set_local.rs`) decides `bind_marks_immutable` for a
bind to a value that has no container, and marks the name readonly. A
*rebind* then runs its own bookkeeping further down. That bookkeeping clears
the name's previous readonly state so a rebind to a container becomes
writable, and it cleared the mark this same rebind had just set. The
rebind's own decision now survives. A rebind to a container (`$y := $z`) is
still unmarked and writes through.

This is the same-frame half of #9277. The cross-frame half (a rebind made
inside a sub does not durably change an outer name's readonly state, because
`readonly_vars` is name-keyed and each frame's unmarks are undone when it
exits) needs writability to become a property of the binding. That is
ADR-0097's binding descriptor, and it stays open on the issue.

Pinned by `t/vm/binding/bind-rebind-to-immutable-value-same-frame.t`.
