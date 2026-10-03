use Test;

# `proto` routines, `{*}` and coercion constraints in RakuAST, measured on
# rakudo 2026.09.

plan 13;

sub decl($src) { $src.AST.statements.head.expression }

my $p = decl(Q[proto sub f(|) {*}]);
isa-ok $p, RakuAST::Sub, 'a proto sub is a Sub';
is $p.multiness, 'proto', 'with multiness proto';
isa-ok $p.body, RakuAST::OnlyStar, 'a bare {*} body is an OnlyStar';
isa-ok RakuAST::OnlyStar.new, RakuAST::Blockoid, 'which is a Blockoid';

my $wrapped = decl(Q[proto g($x) { say 1; {*} }]);
isa-ok $wrapped.body, RakuAST::Blockoid, 'a body around {*} is a Blockoid';
isa-ok $wrapped.body.statement-list.statements[1].expression, RakuAST::OnlyStar,
    'holding the {*} as an OnlyStar expression';

my $m = decl(Q[class C { proto method m(|) {*} }]).body.body.statement-list.statements[0].expression;
isa-ok $m, RakuAST::Method, 'a proto method is a Method';
is $m.multiness, 'proto', 'with multiness proto';

my $c = decl(Q[sub k(Int(Cool) $x) { }]).signature.parameters[0].type;
isa-ok $c, RakuAST::Type::Coercion, 'Int(Cool) is a Type::Coercion';
is $c.constraint.name.gist, 'RakuAST::Name.from-identifier("Cool")', 'with its constraint';

is EVAL(Q[proto sub f2(|) {*}; multi sub f2(Int $x) { "int $x" }; multi sub f2(Str $x) { "str $x" }; f2(1) ~ ' ' ~ f2('a')].AST),
    'int 1 str a', 'a proto survives the round trip';
is EVAL(Q[proto g2($x) { "<" ~ {*} ~ ">" }; multi g2(Int $x) { $x * 2 }; g2(21)].AST),
    '<42>', 'a {*} inside a proto body survives it';
is EVAL(Q[sub k2(Int(Cool) $x) { $x + 1 }; k2("41")].AST), 42, 'a coercion constraint survives it';
