use Test;
use MONKEY-SEE-NO-EVAL;

# `Term::Reduce` holds the reduced list's elements as its arguments and wraps a
# metaoperator in its infix; `Term::Rand` renders without parentheses. Measured on
# Rakudo 2026.11 (#18252).

plan 15;

sub reduce-of(Str $code) { $code.AST.statements[0].expression }

my $plain = reduce-of('[+] 1, 2');
is $plain.^name, 'RakuAST::Term::Reduce', 'a reduction';
is $plain.args.args.elems, 2, 'the list elements are the arguments';
is $plain.infix.^name, 'RakuAST::Infix', 'a plain infix';

my $one = reduce-of('[+] (1, 2)');
is $one.args.args.elems, 1, 'a parenthesized list is one argument';

my $zip = reduce-of('[Z+] 1, 2');
is $zip.infix.^name, 'RakuAST::MetaInfix::Zip', 'Z+ is a Zip metaoperator';
is $zip.infix.infix.operator, '+', 'over +';
is reduce-of('[X+] 1, 2').infix.^name, 'RakuAST::MetaInfix::Cross', 'X+ is a Cross';
is reduce-of('[R-] 1, 2').infix.^name, 'RakuAST::MetaInfix::Reverse', 'R- is a Reverse';
is reduce-of('[Z] 1, 2').infix.^name, 'RakuAST::Infix', 'a bare Z stays an infix';
is-deeply reduce-of('[\+] 1, 2').triangle, True, 'the triangle flag';

is EVAL('[+] 1, 2, 3'.AST), 6, 'EVAL of a plain reduction';
is-deeply EVAL('[R-] 1, 10'.AST), 9, 'EVAL of a reverse reduction';
is-deeply EVAL('[\+] 1, 2, 3'.AST).List, (1, 3, 6), 'EVAL of a triangle reduction';

is 'my $x = rand'.AST.statements[0].expression.initializer.expression.raku, 'RakuAST::Term::Rand.new',
    'Term::Rand renders without parentheses';
is 'my $x = rand + 1'.AST.statements[0].expression.initializer.expression.left.raku, 'RakuAST::Term::Rand.new',
    'also as an operand';
