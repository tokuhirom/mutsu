use Test;

# A block with an explicit CATCH/CONTROL phaser must still evaluate to its
# final expression on the success path (sub, closure, and `do` contexts).
# Regression: the implicit-try wrapper used to discard the body value when a
# CATCH block was present, so such subs/closures/do-blocks returned Nil.

plan 11;

# --- subroutine implicit return ---
sub s-tuple() { CATCH { default {} }; my $x = 5; ($x, 1) }
is-deeply s-tuple(), (5, 1), 'sub with CATCH returns implicit tuple';

sub s-scalar() { CATCH { default {} }; my $x = 5; $x }
is s-scalar(), 5, 'sub with CATCH returns implicit scalar';

sub s-literal() { CATCH { default {} }; 42 }
is s-literal(), 42, 'sub with CATCH returns trailing literal';

sub s-control() { CONTROL { default {} }; my $x = 7; $x }
is s-control(), 7, 'sub with CONTROL returns implicit value';

# --- explicit return still works ---
sub s-explicit() { CATCH { default {} }; return (3, 4) }
is-deeply s-explicit(), (3, 4), 'sub with CATCH honours explicit return';

# --- closures ---
my $c-tuple = -> { CATCH { default {} }; my $x = 5; ($x, 1) };
is-deeply $c-tuple(), (5, 1), 'closure with CATCH returns implicit tuple';

my $c-literal = { CATCH { default {} }; 42 };
is $c-literal(), 42, 'block closure with CATCH returns trailing literal';

# --- do blocks ---
is-deeply (do { CATCH { default {} }; my $x = 5; ($x, 1) }), (5, 1),
    'do block with CATCH returns implicit tuple';
is (do { my $x = 5; CATCH { default {} }; 42 }), 42,
    'do block with CATCH returns trailing literal';

# --- the catch path still yields Nil (exception swallowed) ---
sub s-throws() { CATCH { default {} }; die "boom"; 99 }
is s-throws(), Nil, 'sub whose body throws and is caught returns Nil';

# --- value survives a thread-mutation writeback in the body ---
sub s-thread() {
    my $x;
    my @p;
    CATCH { default { return -1 } }
    @p.push: start { $x = 5 }
    await @p;
    ($x, 1)
}
is-deeply s-thread(), (5, 1), 'sub with CATCH returns value mutated in a thread';

# vim: expandtab shiftwidth=4
