use v6;
use Test;

# ADR-0019 C6d-3: the interpreter proto-`{*}` fallback (call_proto_dispatch)
# runs the selected candidate through the shared compiled entry
# call_routine_def instead of a per-call run_block compile. The VM defers to
# that fallback exactly when the candidate's body trips the OTF gate (a class
# declaration or a `start` call in the body), so these cases pin the rewired
# path; the plain cases pin the VM path around it. Expected values were taken
# from raku first.

plan 13;

# state-declaring candidate under a non-trivial proto: state must persist
# across calls (one cell, not one per re-compile).
proto sub counter(|) { {*} }
multi sub counter(Int $x) { state $n = 0; $n++; "int:$x:$n" }
multi sub counter(Str $s) { "str:$s" }
is counter(1), "int:1:1", "state candidate first call";
is counter(2), "int:2:2", "state cell persists across proto-dispatched calls";
is counter("a"), "str:a", "sibling candidate unaffected";

# class-declaring candidate: the OTF gate defers this to the interpreter
# proto path, which now runs the plan-compiled body.
proto sub cls(|) { {*} }
multi sub cls(Int $x) { class Inner { method m($v) { "cls:$v" } }; Inner.m($x) }
multi sub cls(Str $s) { "str:$s" }
is cls(4), "cls:4", "class-declaring candidate runs through the fallback";
is cls("z"), "str:z", "compilable sibling still dispatches";

# start-in-body candidate (the other OTF-gate trigger).
proto sub st(|) { {*} }
multi sub st(Int $x) { my $p = start { $x * 2 }; await $p }
is st(21), 42, "start-declaring candidate runs through the fallback";

# explicit return from a deferred candidate must unwrap at the candidate
# boundary, not escape the proto body.
proto sub retc(|) { {*} }
multi sub retc(Int $x) { class R3 { }; return "ret:$x"; }
is retc(7), "ret:7", "explicit return from a deferred candidate";

# rw param through a class-declaring candidate: the call's own value is
# correct. The writeback to the caller's container through a non-trivial
# proto body is a known pre-existing gap
# (todo/tickets/rw-writeback-through-nontrivial-proto-body-is-lost.md).
proto sub bump($x is rw) { {*} }
multi sub bump($x is rw) { class B2 { }; $x = $x + 1; $x }
my $v = 10;
is bump($v), 11, "rw candidate computes through the fallback";
# is $v, 11, "rw writeback chains through the proto"; # enable with the ticket

# callsame walks proto-dispatched candidates in order.
proto sub walk(|) {*}
multi sub walk(Int $x) { "int(" ~ callsame() ~ ")" }
multi sub walk(Any $x) { "any:$x" }
is walk(5), "int(any:5)", "callsame inside a proto-dispatched candidate";

# a proto body that post-processes the {*} result.
proto sub post($x) { "[" ~ {*} ~ "]" }
multi sub post(Int $x) { "i$x" }
is post(3), "[i3]", "proto body post-processes the dispatch result";

# a proto BODY that itself trips the OTF gate runs through the interpreter
# carrier, whose `{*}` reaches call_proto_dispatch directly.
proto sub pb(|) { class PB { method tag() { "pb" } }; PB.tag ~ ":" ~ {*} }
multi sub pb(Int $x) { "i$x" }
multi sub pb(Str $s) { "s$s" }
is pb(5), "pb:i5", "interpreter-carried proto body dispatches {*}";
is pb("q"), "pb:sq", "and again for the second candidate";

# no-candidate error still reports X::Multi::NoMatch.
proto sub nomatch(Int $x) { {*} }
multi sub nomatch(Int $x where * > 100) { "big" }
throws-like { nomatch(1) }, Exception,
    message => /"Cannot resolve caller"/,
    "no matching candidate raises the proto no-match error";

done-testing;
