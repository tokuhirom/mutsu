use Test;
plan 9;

# A builtin operator called through a code value (`&[cmp]`, `&infix:<+>`) goes by
# NAME through the call ladder. A routine carrying that name must still win: the
# ladder skips its user-routine resolutions only when no routine has the name.
my $c = &[cmp];
is $c(1, 2), Less, 'a builtin operator through a code value';
is $c("b", "a"), More, 'on strings';
is &infix:<+>(2, 3), 5, '&infix:<+> by name';

class Foo { has $.v }
multi sub infix:<cmp>(Foo $a, Foo $b) { $a.v <=> $b.v }
is $c(Foo.new(:v(1)), Foo.new(:v(2))), Less, 'a user candidate for the same operator is reached';
is $c(1, 2), Less, 'and the builtin candidates still answer';

# A lexical user operator and a candidate-less proto are not in the function table.
{
    my sub infix:<plus>($a, $b) { $a + $b + 100 }
    is &infix:<plus>(1, 2), 103, 'a lexical user operator through its code value';
}
proto sub shout(|) {*}
my &s = &shout;
dies-ok { s(1) }, 'a proto with no candidates still reports the failed dispatch';

is (1, 2, 3).reduce(&[+]), 6, 'reduce with an operator code value';
is (1, 3, 2).sort(&[<=>]).join(","), '1,2,3', 'sort with an operator code value';
