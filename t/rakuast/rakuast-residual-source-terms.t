use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 12;

my $words = Q[my enum Empty < >].AST.statements[0].expression.term;
isa-ok $words, RakuAST::QuotedString, 'an empty angle enum keeps its quoted term';
is $words.segments[0].value, ' ', 'the whitespace inside the angle term is retained';
my $parens = Q[my enum Empty2 ()].AST.statements[0].expression.term;
isa-ok $parens, RakuAST::Circumfix::Parentheses,
    'an empty parenthesized enum keeps its parentheses';
is $parens.semilist.statements.elems, 0, 'the parenthesized enum has no statements';

my $complex = Q[<-0-0i>].AST.statements[0].expression;
isa-ok $complex, RakuAST::ComplexLiteral,
    'signed complex zero remains a whole Complex literal';
like $complex.raku, /'<-0-0i>'/, 'both zero signs survive rendering';
ok EVAL(Q[<-0-0i> === <-0-0i>].AST),
    'the round-tripped signed zeros preserve identity';

my $temp = Q[my $d; temp undefine($d) = "baz"].AST.statements[1].expression;
isa-ok $temp, RakuAST::ApplyInfix, 'temp undefine with assignment is an infix';
isa-ok $temp.left, RakuAST::ApplyPrefix, 'the assignment is around the temp prefix';
is $temp.left.prefix.operator, 'temp', 'the prefix is temp';
isa-ok $temp.left.operand, RakuAST::Call::Name,
    'the prefix operand is the undefine call';
is $temp.left.operand.name.canonicalize, 'undefine',
    'the operand retains the undefine routine name';
