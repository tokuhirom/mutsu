use Test;

# The `.candidates` of a setting routine can be called: each runs the
# built-in routine on the arguments its signature bound, past any `multi` a
# program adds to the same name (#18009).

plan 9;

is &infix:<+>.candidates[5](2, 3), 5, 'a native int candidate of infix:<+>';
is &infix:<+>.candidates[2](4, 5), 9, 'a sigilless candidate';
is &infix:<+>.candidates[0](), 0, 'the nullary candidate';
is &infix:<+>.candidates[1](7), 7, 'the unary candidate';
is &abs.candidates[1](-3), 3, 'a candidate of abs';
is &join.candidates[0](',', 1, 2, 3), '1,2,3', 'a slurpy candidate of join';
throws-like { &abs.candidates.first(*.signature.raku.contains('Num:D'))(-3) },
    X::TypeCheck::Binding, 'a candidate binds its own signature';

multi infix:<+>(Str $a, Int $b) { 'user' }
my $int-int = &infix:<+>.candidates.first(*.signature.raku.contains('Int:D $a, Int:D $b'));
is $int-int(2, 3), 5, 'a setting candidate of a joined family runs the built-in';
my $any-any = &infix:<+>.candidates.first(*.signature.raku eq ':(\\a, \\b)');
ok $any-any('a', 3) ~~ Failure, '... and does not re-enter the program multi';
