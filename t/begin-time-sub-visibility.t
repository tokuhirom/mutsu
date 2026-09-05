use Test;

# ADR-0041 section 9: a sub's *callability* is hoisted for the whole enclosing scope,
# but a name reference evaluated at BEGIN time (a `constant` initializer or a
# `BEGIN`/`CHECK` body) must see only the declarations the program has
# textually reached, exactly as rakudo's compile-time pad install does.
#
# Every expectation below was verified against real rakudo before being pinned.

plan 8;

# --- BEGIN-time references only see what precedes them ------------------------

sub outer-only() { "outer" }

{
    my constant &alias = &outer-only;
    is alias(), "outer",
       'a `constant &alias = &name` inside a block captures the OUTER routine, not the block-local one declared after it';
    sub outer-only() { "inner" }
}

{
    my $begin-call = BEGIN outer-only();
    is $begin-call, "outer",
       'a BEGIN-time call inside a block reaches the OUTER routine, not the block-local one declared after it';
    sub outer-only() { "shadow" }
}

sub declared-first() { 42 }
is (BEGIN declared-first()), 42,
   'a BEGIN-time call to a routine declared BEFORE it resolves normally';

eval-dies-ok q[constant Z = only-later(); sub only-later() { 1 }; Z],
   'a BEGIN-time call to a routine declared only LATER is a compile-time failure';

eval-lives-ok q[sub already-there() { 1 }; constant Z = already-there(); Z],
   'the same call after the declaration compiles';

# --- regression controls: ordinary RUNTIME references still see everything ----
# These are the two rows ADR-0041 section 6.3 rejected "register at the textual
# position" over: rakudo installs the pad entry at compile time, so a plain
# runtime reference is order-blind and must keep seeing the whole scope.

sub runtime-ref() { "outer" }

{
    my $captured = &runtime-ref;
    is $captured(), "inner",
       'a plain runtime `&name` reference sees the block-local routine declared after it';
    sub runtime-ref() { "inner" }
}

{
    my $forward = across-constant();
    constant ACROSS = 1;
    is $forward, 7,
       'a plain runtime forward call still resolves across a `constant` declaration';
    sub across-constant() { 7 }
}

sub calls-later() { declared-later() }
sub declared-later() { 9 }
is calls-later(), 9,
   'forward-reference calling between mainline subs is unaffected';
