use v6;
use Test;

# A fresh `my` declaration must not inherit a stale type constraint left by an
# earlier same-named lexical. `SetVarDynamic` clears the constraint on every
# declaration; the clear has a fast path that must not skip a real clear.

plan 8;

# A later untyped `my $x` must be free of the earlier `Int` constraint.
my Int $x = 1;
{
    my $x;
    $x = "free";
    is $x, "free", 'fresh untyped my does not inherit the outer Int constraint';
}
is $x, 1, 'the outer typed lexical is untouched';

# The constraint is still enforced on the typed lexical itself.
{
    my $failed = False;
    { CATCH { default { $failed = True } }; $x = "bad" }
    ok $failed, 'the Int constraint is still enforced after the clear fast path';
}

# Across routines: a typed lexical in one sub must not constrain a same-named
# untyped lexical in another.
sub typed() { my Int $v = 1; $v }
sub untyped() { my $v; $v = "str"; $v }
is typed(), 1, 'typed lexical works';
is untyped(), "str", 'same-named untyped lexical in another sub is unconstrained';

# Hash key-type constraints clear the same way.
my %h{Int} = (1 => "a");
is %h{1}, "a", 'typed hash key constraint works';
{
    my %h;
    %h{"s"} = "t";
    is %h{"s"}, "t", 'fresh untyped hash does not inherit the Int key constraint';
}

# A `my $d` must not inherit the dynamic flag of an outer `my $*d`.
my $*d = 1;
{
    my $d = 99;
    is $d, 99, 'plain my shadowing a same-named dynamic stays plain';
}
