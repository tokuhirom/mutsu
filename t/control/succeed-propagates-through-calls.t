use Test;

# A `succeed` raised inside a routine or block that has no `when`/`default`
# of its own unwinds dynamically to the caller's `given`/`when`, the way
# `last`/`next` reach a caller's loop. Only a body that lexically contains a
# `when`/`default` absorbs it (#10554).

plan 15;

{
    my &d = sub (|c) { succeed };
    my @s;
    given 5 {
        when 5 { @s.push('when'); d(); @s.push('nr') }
        default { @s.push('default') }
    }
    is-deeply @s, ['when'], 'bare succeed in an anonymous sub leaves the caller\'s when';
}

sub g { succeed 42 }
is (given 1 { when 1 { g(); 7 } }), 42, 'succeed VALUE in a named sub is the given\'s value';

sub inner { succeed 'deep' }
sub outer { inner() }
is (given 1 { when 1 { outer(); 7 } }), 'deep', 'succeed unwinds through nested calls';

multi mm(Int $) { succeed 'multi' }
is (given 1 { when 1 { mm(1); 7 } }), 'multi', 'succeed from a multi candidate';

class C { method s { succeed 9 } }
is (given 1 { when 1 { C.s; 7 } }), 9, 'succeed from a method';

my $b = { succeed 4 };
is (given 1 { when 1 { $b(); 7 } }), 4, 'succeed from a bare block called as a closure';
is (given 1 { when 1 { -> { succeed 6 }(); 7 } }), 6, 'succeed from a pointy block';

sub in-sub { given 1 { when 1 { inner(); 8 } } }
is in-sub(), 'deep', 'the given inside a routine catches a succeed from its callee';

my $n = 0;
for ^200 {
    $n += (given $_ { when 0 { 1 }; default { sub { succeed 2 }(); 3 } });
}
is $n, 399, 'propagation holds on a hot (JIT-eligible) path';

# A body that contains its own `when`/`default` still absorbs it.
sub f($_) { when 1 { 'one' }; 'other' }
is f(1), 'one', 'a routine\'s own when still ends the routine';
is f(2), 'other', 'and falls through when it does not match';
sub ft($_ --> Str) { when 1 { 'one' }; 'other' }
is ft(1), 'one', 'same with a return type';
my &a = sub ($_) { when 1 { 'a-one' }; 'a-other' };
is a(1), 'a-one', 'an anonymous sub with its own when';
is-deeply (1, 2).map({ when 1 { 'x' }; 'y' }).List, <x y>, 'a map block with its own when';

# With no when clause anywhere to leave, it is an X::ControlFlow.
is run($*EXECUTABLE, '-e', 'sub m { succeed 3 }; m(); say "after"', :out, :err).err.slurp(:close).lines[0],
    'succeed without when clause', 'succeed with no when clause to leave is reported';
