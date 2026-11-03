use Test;

# A one-element list written with its trailing comma (`("a",)`, `5,`) is an
# `ApplyListInfix` whose range and whose comma `Infix` carry origins (#15780),
# as rakudo gives them; a lone parenthesised term is not a list.

plan 5;

use experimental :rakuast;

sub origins(Str $src) {
    my @l;
    sub w($n) {
        @l.push($n.^name.split('::').tail ~ "@" ~ .from ~ "-" ~ .to) with $n.origin;
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.grep(/^[SemiList | ApplyListInfix | Infix]/).join(' ');
}

is origins('my $x = ("a",);'), 'SemiList@9-13 ApplyListInfix@9-13 Infix@12-13', 'a parenthesised string and comma';
is origins('my $z = (3 , );'), 'SemiList@9-12 ApplyListInfix@9-12 Infix@11-12', 'whitespace before the comma';
is origins('my @d = 5,;'), 'ApplyListInfix@8-10 Infix@9-10', 'without brackets';
is origins('my @a = [1,];'), 'SemiList@9-11 ApplyListInfix@9-11 Infix@10-11', 'in an array composer';
is origins('my $c = (1);'), 'SemiList@9-10', 'a lone term is not a list';
