# An `is rw` argument forwarded through `|c` keeps its container

A variable bound to an `is rw` parameter and passed on through a `|c` capture
to another routine's `is rw` parameter lost that routine's write (#13921):

```raku
sub add($x is rw) { $x += 2 }
sub fwd(|c) { add(|c) }
sub top($o is rw) { fwd($o) }
my $a = 0; top($a); say $a;   # 2 (was 0)
```

A routine called through the light entry keeps an `is rw` parameter's shared
cell in its frame slot only. Since ADR-12529, a callee no longer sees its
caller's names. So the capture binder found no cell for `$o`. It made a fresh
one and relied on a by-name write-back, which never reached the slot.

The binder now takes the calling frame's own slot for the argument. That slot
is the one the call site's `WrapVarRef` resolved, or the caller's only local of
that name for a method call, whose arguments carry no slot. If the slot holds a
cell, the binder keeps that cell, so the forwarded slip hands the final callee
the caller's real container.

This affected Intl::CLDR's binary loader. It threads one `uint64 $offset is
rw` through `method new(|c) { self.bless!add-items: |c }`, and with the bug
every sibling section re-read the same bytes.
