use v6;
use Test;

# ADR-0019 C6d-4: a code object built from a registry routine carries the
# routine's compiled body (SubData::compiled_routine, C6c), and the
# interpreter carrier call_sub_value — the path a .wrap chain routes the
# original sub's direct run through — now executes that bytecode instead of
# the AST copy the declaration left in the code object. These cases pin the
# wrap-chain legs that reach that carrier. Expected values were taken from
# raku first.

plan 10;

# callsame runs the original through the carrier's direct-run leg.
sub f($x) { $x * 2 }
my $h = &f.wrap(sub ($x) { "w(" ~ callsame() ~ ")" });
is f(21), "w(42)", "callsame reaches the original's compiled body";

# state cell stays one cell when the original runs through the chain.
sub counter() { state $n = 0; ++$n }
&counter.wrap(sub () { "c:" ~ callsame() });
is counter(), "c:1", "state initializes once through the chain";
is counter(), "c:2", "and the same cell persists across chained calls";

# nextcallee hands out the original as a code object; calling it runs
# the compiled body directly (__mutsu_wrap_direct leg).
sub g($x) { "g:$x" }
&g.wrap(sub ($x) { my &orig = nextcallee; "n(" ~ orig($x) ~ ")" });
is g(7), "n(g:7)", "nextcallee code object runs the compiled body";

# rw parameter writeback still chains through the original.
sub bump($x is rw) { $x = $x + 1; $x }
&bump.wrap(sub ($x is rw) { "b:" ~ callsame() });
my $v = 10;
is bump($v), "b:11", "rw candidate computes through the chain";
is $v, 11, "rw writeback reaches the caller through the chain";

# explicit return from the original unwraps at the routine boundary.
sub r($x) { return "ret:$x"; "unreached" }
&r.wrap(sub ($x) { "R(" ~ callsame() ~ ")" });
is r(5), "R(ret:5)", "explicit return unwraps inside the chain";

# unwrap restores the plain compiled path.
$h.restore;
is f(3), 6, "restore returns dispatch to the unwrapped sub";

# rw writeback through a plain code object (no wrap): the compiled-closure
# exit must flush the rw param's slot value into env before the writeback
# reads it, or the caller sees the stale bind-time value. This was broken
# on the C6c value-dispatch path before this flush existed.
sub incr($x is rw) { $x = $x + 1; $x }
my &via = &incr;
my $w = 20;
is via($w), 21, "code-object rw call computes";
is $w, 21, "and its writeback reaches the caller";

done-testing;
