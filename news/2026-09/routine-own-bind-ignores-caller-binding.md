# A routine's own `my $r := ...` no longer leaks into a caller's bound `$r`

```raku
my $t = "ab"; my $r := substr-rw($t, 0, 1);
sub f { my $s = "hello"; my $r := substr-rw($s, 0, 1); $r = "J"; $s }
say f();   # raku: Jello   mutsu (before): hello, and $t became "Jb"
```

A routine's own `:=` declaration was treated as a write to whatever an outer
binding of the same name had left behind, through three separate by-name
channels in `exec_set_local_op_inner` (`src/vm/vm_var_assign_set_local.rs`):

- **The Proxy STORE step ran for a bind.** "If the slot holds a Proxy, call its
  STORE instead of overwriting" was not gated on the store being an
  assignment, so a `:=` declaration whose slot still held the caller's Proxy
  STOREd the new Proxy into it (the caller's `$t` got the FETCHed `"h"`) and
  the routine's own `$r` never got its binding. It now runs only for a plain
  assignment — never for a `:=` / rebind / scalar bind / declaration.
- **The declaration's stale-binding guard ignored Proxies.** A fresh `my`
  replaced a same-named outer `ContainerRef` in env with a `Nil` marker, but
  not a bound Proxy, so a later `$r = v` could lazy-sync the caller's Proxy
  back out of env. The guard now covers both.
- **A `:=` declaration kept the caller's forward alias.** A mainline
  `my $r := $x` records `alias::r = x`; the declaration guard dropped that
  forward alias only for `=` declarations. A value bind (`my $r :=
  substr-rw(...)`) records no alias of its own, so the alias walk after the
  store wrote the routine's Proxy into the caller's `$x`. The guard now drops it
  for every declaration; a `:=` from a variable re-records its own alias later
  in the same store.

Pinned by `t/vm/binding/routine-own-bind-ignores-caller-binding.t` (#9244).
