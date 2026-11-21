use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a curry the parser keeps
# no position for (`* + *`, `1 min *`) as an initializer, a call argument or
# a comma-list operand is placed from a fragment parse of its text, and a
# primed `*` at either end of a list-associative application is found in
# the source.

plan 10;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) unless $class ~~ /Statement/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[my $f = * + *]),
    'VarDeclaration::Simple@0-13 Initializer::Assign@6-13 ApplyInfix@8-13 Infix@10-11 ArgList@8-13',
    'an assigned curry of two primed operands';
is placed(Q[my $g := * ~ *]),
    'VarDeclaration::Simple@0-14 Initializer::Bind@6-14 ApplyInfix@9-14 Infix@11-12 ArgList@9-14',
    'a bound curry';
is placed(Q[constant H = * * *]),
    'VarDeclaration::Constant@0-18 Initializer::Assign@11-18 ApplyInfix@13-18 Infix@15-16 ArgList@13-18',
    'a constant curry';
is placed(Q[say(* + *)]),
    'Call::Name@0-10 Name@0-3 ArgList@4-9 ApplyInfix@4-9 Infix@6-7 ArgList@4-9',
    'a curry as the parenthesized argument';
is placed(Q[say * - *]),
    'Call::Name::WithoutParentheses@0-9 Name@0-3 ArgList@4-9 ApplyInfix@4-9 Infix@6-7 ArgList@4-9',
    'a curry as the argument after a blank';
is placed(Q[say * max *]),
    'Call::Name::WithoutParentheses@0-11 Name@0-3 ArgList@4-11 ApplyListInfix@4-11 Infix@6-9',
    'a list-associative curry of two primed operands';
is placed(Q[say(1 min *)]),
    'Call::Name@0-12 Name@0-3 ArgList@4-11 ApplyListInfix@4-11 Infix@6-9 IntLiteral@4-5',
    'a list-associative curry with a primed last operand';
is placed(Q[my @a = * + *, 2]),
    'VarDeclaration::Simple@0-16 Initializer::Assign@6-16 ApplyListInfix@8-16 Infix@13-14 ApplyInfix@8-13 Infix@10-11 ArgList@8-13 IntLiteral@15-16',
    'a curry first in an initializer list';
is placed(Q[say 1, * + *]),
    'Call::Name::WithoutParentheses@0-12 Name@0-3 ArgList@4-12 IntLiteral@4-5 ApplyInfix@7-12 Infix@9-10 ArgList@7-12',
    'a curry last in an argument list';
is placed(Q[say(1, * ~ *, 3)]),
    'Call::Name@0-16 Name@0-3 ArgList@4-15 IntLiteral@4-5 ApplyInfix@7-12 Infix@9-10 ArgList@7-12 IntLiteral@14-15',
    'a curry between parenthesized arguments';
