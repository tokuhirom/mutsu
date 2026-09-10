use v6;
use Test;

plan 10;

# A reference cycle through an instance renders in Rakudo's backreference
# binding form `(my \Bug_<id> = Bug.new(myself => [Bug_<id>]))` instead of
# the terminating-but-wrong `Bug.new(myself => [Bug()])` (and instead of a
# stack overflow for a direct instance->instance cycle).

class Bug { has @.myself }
my $a = Bug.new;
$a.myself[0] = $a;

like $a.raku, /^ '(my \Bug_' (\d+) ' = Bug.new(myself => [Bug_' $0 ']))' $/,
    'self-cycle through an array attribute uses the backref binding';
is $a.gist, $a.raku, 'default gist matches the .raku form';
like [$a].raku, /^ '[(my \Bug_' (\d+) ' = Bug.new(myself => [Bug_' $0 ']))]' $/,
    'cyclic instance nested in an array';
my %h = b => $a;
like %h.raku, /^ '{:b((my \Bug_' (\d+) ' = Bug.new(myself => [Bug_' $0 '])))}' $/,
    'cyclic instance nested in a hash';

# A mutual cycle through plain `$` attributes previously overflowed the
# stack; only the outer re-entered instance gets the binding.
class P { has $.next is rw }
my $x = P.new;
my $y = P.new;
$x.next = $y;
$y.next = $x;
like $x.raku, /^ '(my \P_' (\d+) ' = P.new(next => P.new(next => P_' $0 ')))' $/,
    'mutual cycle binds only the outer instance';
like $y.raku, /^ '(my \P_' (\d+) ' = P.new(next => P.new(next => P_' $0 ')))' $/,
    'the same cycle rendered from the other side';

# Distinct renders of the same object agree (ids are stable).
is $a.raku, $a.raku, 'repeated renders are identical';

# Non-cyclic structures are untouched by the guard.
class Foo { has $.x }
is Foo.new(x => 1).raku, 'Foo.new(x => 1)', 'plain instance unchanged';
is [Foo.new(x => 1)].raku, '[Foo.new(x => 1)]', 'nested plain instance unchanged';
class B2 { has $.o }
is B2.new(o => Foo.new(x => 3)).raku, 'B2.new(o => Foo.new(x => 3))',
    'instance-valued attribute unchanged';
