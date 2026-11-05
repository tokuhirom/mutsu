use Test;

# The signature of a `for` loop's pointy block (`for @a -> $x, $y { }`) and of
# a pointy block with a sigilless parameter (`-> \x { }`) carry origins, as in
# rakudo (#15780): the `Signature` spans the parameters, each `Parameter` its
# text (with the backslash), a sigilless `Term` and its `Name` the name alone.

plan 4;

use experimental :rakuast;

sub origins(Str $src) {
    my @l;
    sub w($n) { @l.push($n.^name.split('::').tail ~ "@" ~ .from ~ "-" ~ .to) with $n.origin; $n.visit-children(&w) }
    w($src.AST);
    @l.grep(/^[Signature | Parameter | Var | Term]/).join(' ');
}

is origins('for 1,2 -> $x, $y { }'),
    'Signature@11-17 Parameter@11-13 Var@11-13 Parameter@15-17 Var@15-17', 'for with two scalar parameters';
is origins('for 1,2 -> \x, \y { }'),
    'Signature@11-17 Parameter@11-13 Term@12-13 Parameter@15-17 Term@16-17', 'for with sigilless parameters';
is origins('my $c = -> \v { };'), 'Signature@11-13 Parameter@11-13 Term@12-13', 'a pointy block with a sigilless parameter';
is origins('for 1,2 -> $x { }'), 'Signature@11-13 Parameter@11-13 Var@11-13', 'for with one parameter';
