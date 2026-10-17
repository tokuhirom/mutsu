# A routine's rebind of its caller's `$*x` reaches the declaring frame directly

```raku
sub rebind() { $*t := $*root }
sub r() { my $*root = 'root'; my $*t = 'orig'; rebind(); $*t }
say r();   # root
```

A routine that rebinds a dynamic variable its caller declared, with `:=`
or `nqp::bindlexdyn`, now records the rebind and carries it to the frame
that declares the variable, the way a symbolic store to an ordinary
lexical already travels. It used to arrive only because a call's return
copied the callee's variables back over the caller's, which ADR-12529 is
retiring; with this slice nothing in the test suites depends on that copy
any more (§3, slice 40).
