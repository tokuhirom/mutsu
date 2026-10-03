use Test;

# A `.map` block with its own signature (`-> \x --> Str { ... }`) runs as a
# compiled closure call per element (#9494). Its semantics are unchanged.

plan 12;

class F { has $.t; method Str { $!t } }
my @f = F.new(t => 'a'), F.new(t => 'b');

is-deeply @f.map(-> \x --> Str { x.Str }).List, ('a', 'b'), 'sigilless param, return type';
is-deeply (1, 2, 3).map(-> \x { x * 2 }).List, (2, 4, 6), 'over a List';
is-deeply [1, 2, 3].map(-> Int $x { $x + 1 }).List, (2, 3, 4), 'a typed param over an Array';

throws-like { [1, 2].map(-> \x --> Str { x }).eager }, X::TypeCheck::Return,
    'the return type is still checked';
throws-like { ['a'].map(-> Int $x { $x }).eager }, X::TypeCheck::Binding,
    'the parameter type is still checked';

my $sum = 0;
[1, 2, 3].map(-> $x { $sum += $x }).eager;
is $sum, 6, 'a write to an outer lexical';

$_ = 'outer';
is-deeply [1, 2].map(-> $x { $_ ~ $x }).List, ('outer1', 'outer2'),
    '$_ in a block with a signature is the outer topic';

is-deeply [1, 2, 3, 4].map(-> $x { next if $x == 2; $x }).List, (1, 3, 4), 'next';
is-deeply [1, 2, 3, 4].map(-> $x { last if $x == 3; $x }).List, (1, 2), 'last';

sub first-big(@a) { @a.map(-> $x { return $x if $x > 1 }).eager; 'none' }
is first-big([1, 5, 7]), 5, 'return leaves the enclosing routine';

is-deeply [1, 2, 3, 4].map(-> $a, $b { $a + $b }).List, (3, 7), 'two parameters';

my @a = 1, 2, 3;
@a.map(-> $v is rw { $v *= 10 }).eager;
is-deeply @a, [10, 20, 30], 'an rw parameter writes the element';
