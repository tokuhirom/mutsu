# `substr-rw` refuses a binding with no container

`.substr-rw(...) = v` and `substr-rw($x, ...) = v` on a name bound straight
to a value (`my $k := "lit"`, `my \k = "lit"`, `constant $c`) or on a
non-`is rw` parameter used to succeed silently, either rewriting the
binding or writing into a cell nobody else could see. As in rakudo, they now
die with `'substr-rw' requires a writeable container` (#10893).

Three places had been handing such a binding a fresh writable cell. The
raw-invocant boxing for `.substr-rw` now leaves a readonly name bare, so the
method's own write refuses it. A routine whose body runs `$p.m(...) = v` on a
read-only parameter is left to the bytecode VM rather than TRIR, because only
the VM's binder records the parameter's readonly mark. And a List literal
built from a readonly scalar binding now holds that binding's value instead
of boxing it. That last change also fixes `my $b := "lit"; my $l = ($b, 1);
$l[0] = 5`, which used to rewrite `$b` and now dies with `Cannot modify an
immutable List ((lit 1))`.
