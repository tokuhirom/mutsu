use Test;

# Slice F (env<->locals coherence): a qualified / `our` sub reached by name
# (`module M { our sub foo() { $called = True } }; M::foo`) is dispatched as a
# BareWord through `call_compiled_function_named`, not the 0-arg fast path. When
# its body mutates a captured outer lexical the new value reaches `env` by name,
# but the caller's local slot was kept coherent only by the reverse
# `sync_locals_from_env` pull. This pins that the BareWord call-site drains the
# captured-outer write through to the caller's local slot. Run with
# `MUTSU_NO_REVERSE_SYNC=1` to confirm coherence without the reverse pull.

plan 9;

# Plain assignment to a captured outer scalar via a qualified sub.
{
    my $called = False;
    module M1 { our sub foo() { $called = True } }
    M1::foo;
    ok $called, 'qualified sub captured-outer assignment is visible';
}

# Post-increment across two qualified-sub calls.
{
    my $count = 10;
    module M2 { our sub up() { $count++ } }
    M2::up;
    M2::up;
    is $count, 12, 'qualified sub captured-outer post-increment accumulates';
}

# Compound assignment.
{
    my $acc = 100;
    module M3 { our sub add() { $acc += 7 } }
    M3::add;
    is $acc, 107, 'qualified sub captured-outer compound += is visible';
}

# Two captured outer scalars written in one qualified-sub call.
{
    my $a = 0;
    my $b = 0;
    module M4 { our sub both() { $a = 5; $b = 9 } }
    M4::both;
    is "$a/$b", "5/9", 'qualified sub writes two captured-outer scalars';
}

# String value.
{
    my $msg = "x";
    module M5 { our sub app() { $msg = $msg ~ "!" } }
    M5::app;
    M5::app;
    is $msg, "x!!", 'qualified sub captured-outer string mutate is visible';
}

# Captured outer array, pushed via a qualified sub.
{
    my @log;
    module M6 { our sub rec() { @log.push(1) } }
    M6::rec;
    M6::rec;
    is @log.elems, 2, 'qualified sub captured-outer array push is visible';
}

# Postfix-if guarding a qualified-sub call (the original roast-style case).
{
    my $ran = False;
    module M7 { our sub go() { $ran = True } }
    M7::go if True;
    ok $ran, 'qualified sub call in postfix if writes captured-outer var';
}

# Read-modify-write reading back the value a prior call left.
{
    my $v = 21;
    module M8 { our sub dbl() { $v = $v * 2 } }
    M8::dbl;
    is $v, 42, 'qualified sub captured-outer read-modify-write is visible';
}

# Caller slot stays coherent for a later expression.
{
    my $base = 0;
    module M9 { our sub set() { $base = 40 } }
    M9::set;
    is $base + 2, 42, 'caller slot coherent in a later expression';
}
