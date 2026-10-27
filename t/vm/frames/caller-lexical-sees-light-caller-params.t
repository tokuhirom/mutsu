use Test;

# A reader of the immediate caller's frame must see the slot-only parameters
# of a light caller even though no routine mirrors its parameters into the
# env for it (#14337).

plan 5;

sub rd-n  { CALLER::LEXICAL::<$n> }
sub rd-m  { CALLER::LEXICAL::<$m> }
sub light-one($n)      { rd-n() }
sub light-two($n, $m)  { rd-m() }
sub light-typed(Int $n) { rd-n() }

is light-one(5), 5, 'CALLER::LEXICAL sees a light caller\'s parameter';
is light-two(1, 2), 2, 'the second parameter too';
is light-typed(7), 7, 'a typed light caller';

my $sum = 0;
$sum += light-one($_) for 1..4;
is $sum, 10, 'repeated calls see each call\'s own value';

sub rd-code { &CALLER::LEXICAL::("infix:<foo>") }
sub with-code($n, &infix:<foo>) { rd-code() }
ok with-code(1, sub ($a, $b) { 3 }).defined, 'a &-routine parameter is visible';
