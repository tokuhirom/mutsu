use Test;
use experimental :rakuast;

plan 7;

# `$::(EXPR)::name` keeps the name's parts (#18292; measured on rakudo
# 2026.11): `Var::Package(Name(EmptyEdge, Part::Expression, Part::Simple))`.
my $u = "OUR";
is Q[$::($u)::x110].AST.DEPARSE.trim, '$::($u)::x110', 'one static segment';
is Q[$::($u)::a::b].AST.DEPARSE.trim, '$::($u)::a::b', 'two static segments';
my $name = Q[$::($u)::x110].AST.statements[0].expression.name;
is $name.parts.map(*.^name).join(','),
    'RakuAST::Name::Part::EmptyEdge,RakuAST::Name::Part::Expression,RakuAST::Name::Part::Simple',
    'the parts';
is Q[$::($u ~ "::x110")].AST.DEPARSE.trim, '$::($u ~ "::x110")', 'a written concatenation stays as written';
is Q[$::($u)].AST.DEPARSE.trim, '$::($u)', 'no tail';

our $y7 = 9;
is $::($u)::y7, 9, 'the lookup still works';
is EVAL(Q[our $z8 = 11; my $p = "OUR"; $::($p)::z8].AST), 11, 'EVAL of the tail form';
