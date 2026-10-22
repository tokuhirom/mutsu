use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 6;

# `my $ = 5 andthen 2`: the anonymous declaration leads the loose tail.
my $tail = Q[my $ = 5 andthen 2].AST.statements.tail.expression;
isa-ok $tail, RakuAST::ApplyListInfix, 'an anonymous declaration with a loose tail is one application';
isa-ok $tail.operands[0], RakuAST::VarDeclaration::Anonymous, 'the anonymous declaration is the first operand';
is EVAL(Q[my $ = 5 andthen 7].AST), 7, 'the converted application evaluates';
is EVAL(Q[my $ = "a" andthen -> $x { $x ~ "b" }].AST), 'ab', 'andthen passes the declared value on';

# `my Int \x .= new`: a typed sigilless declaration keeps its type.
my $term = Q[my Int \x14609 .= new].AST.statements.tail.expression;
isa-ok $term.type, RakuAST::Type::Simple, 'the typed term declaration keeps its type';
is EVAL(Q[my Int \x14609b .= new; x14609b].AST), 0, 'the invocant is the declared type, not Mu';
