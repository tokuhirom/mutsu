use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: the `Initializer` of a
# `constant` declaration runs from its `=` / `:=` to the end of its value.

plan 4;

sub o($n) { $n.origin ?? $n.origin.from ~ '-' ~ $n.origin.to !! '-' }
sub shape($src) {
    my $c = $src.AST.statements[0].expression;
    o($c) ~ ' ' ~ o($c.initializer) ~ ' ' ~ o($c.initializer.expression)
}

is shape(Q[constant VER = '4.5.6';]), '0-22 13-22 15-22', 'an assigned constant';
is shape(Q[my constant %h := hash a => 1;]), '0-29 15-29 18-29', 'a bound constant';
is shape(Q[our constant @a = 1, 2;]), '0-22 16-22 18-22', 'a list constant';
is shape(Q[constant $x = 1 + 2;]), '0-19 12-19 14-19', 'a sigiled constant';
