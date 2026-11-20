use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a reduction's operator and
# arguments, the subscript and postfix of a curried `*` (`*{}`, `*[0]`,
# `*++`), an angle-quoted number (the text inside its brackets), and a
# `Pragma`, which visits none of its parts.

plan 7;

sub o($n) { $n.origin ?? $n.origin.from ~ '-' ~ $n.origin.to !! '-' }

my $r = Q[say [\+] 1, 2;].AST.statements[0].expression.args.args[0];
is o($r.infix) ~ ' ' ~ o($r.args), '6-7 9-13', 'a triangular reduction';
$r = Q[say [+](1, 2);].AST.statements[0].expression.args.args[0];
is o($r.infix) ~ ' ' ~ o($r.args), '5-6 8-12', 'a reduction with parenthesized arguments';

my $w = Q[my $c = *{};].AST.statements[0].expression.initializer.expression;
is o($w.postfix) ~ ' ' ~ o($w.postfix.index), '9-11 10-10', 'the zen hash subscript of *';
$w = Q[my $c = *[0];].AST.statements[0].expression.initializer.expression;
is o($w.postfix) ~ ' ' ~ o($w.postfix.index), '9-12 10-11', 'the array subscript of *';
$w = Q[say (1,2).map(*++);].AST.statements[0].expression.args.args[0].postfix.args.args[0];
is o($w.postfix), '15-17', 'the postfix of *';

is Q[say <1/2>, <1+2i>;].AST.statements[0].expression.args.args.map(&o).join(' '), '5-8 12-16',
    'angle-quoted numbers';

my @c;
Q[use lib "x";].AST.statements[0].visit-children({ @c.push: $_ });
is @c.elems, 0, 'a Pragma visits no children';
