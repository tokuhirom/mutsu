use Test;

plan 14;

# A built-in chaining operator takes no adverb: rakudo refuses one at compile
# time with X::Syntax::Adverb, naming the operator (#18452).

for (
    '1 < 2 :foo',       '&infix:«<»',
    '1 == 2 :foo',      '&infix:<==>',
    '1 eq 2 :foo',      '&infix:<eq>',
    '1 ~~ 2 :foo',      '&infix:<~~>',
    '1 !eqv 2 :foo',    '&infix:<eqv>',
    '1 !=== 2 :foo',    '&infix:<===>',
    '1 != 2 :foo',      '&infix:<!=>',
    '1 >= 2 :foo',      '&infix:«>=»',
    '1 (elem) 2 :foo',  '&infix:<(elem)>',
    '1 < 2 < 3 :foo',   '&infix:«<»',
    'my $x = 1 < 2 :!foo', '&infix:«<»',
) -> $code, $name {
    my $err = (try { EVAL $code; Nil }) // $!;
    is $err.?message, "You can't adverb $name", "$code";
}

isa-ok ((try { EVAL '1 < 2 :foo'; Nil }) // $!), X::Syntax::Adverb, 'the exception type';

sub infix:<qq>($a, $b, :$x) { "$a $b {$x // 0}" }
is (1 qq 2 :x), '1 2 True', 'a user infix takes adverbs';
is (1 < 2 ?? 'y' !! 'n'), 'y', 'a comparison without an adverb';
