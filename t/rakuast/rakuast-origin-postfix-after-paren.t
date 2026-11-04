use Test;

# A postfix operator or subscript after a parenthesised operand (`($i)++`,
# `(@a)[0]`): the parentheses are not a node of their own in rakudo's tree, so
# the `Postfix` / subscript starts after their `)` (#15780).

plan 5;

use experimental :rakuast;

sub origins(Str $src) {
    my @l;
    sub w($n) { @l.push($n.^name.split('::').tail ~ "@" ~ .from ~ "-" ~ .to) with $n.origin; $n.visit-children(&w) }
    w($src.AST);
    @l.grep(/^[Postfix | ArrayIndex | SemiList | ApplyPostfix]/).join(' ');
}

is origins('my $i = 1; ($i)++;'), 'ApplyPostfix@11-17 Postfix@15-17', 'a parenthesised operand of ++';
is origins('my $i = 1; ($i)--;'), 'ApplyPostfix@11-17 Postfix@15-17', 'and of --';
is origins('my $i = 1; (($i))++;'), 'ApplyPostfix@11-19 SemiList@13-15 Postfix@17-19', 'doubly parenthesised';
is origins('my @a = 1,2; (@a)[0]++;'),
    'ApplyPostfix@13-22 ApplyPostfix@13-20 ArrayIndex@17-20 SemiList@18-19 Postfix@20-22', 'a subscript after a parenthesised operand';
is origins('my $i = 1; $i++;'), 'ApplyPostfix@11-15 Postfix@13-15', 'an unparenthesised operand is unchanged';
