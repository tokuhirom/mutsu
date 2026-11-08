use Test;
use experimental :rakuast;

plan 10;

# An anonymous optional parameter `$?` is a bare `$` target with the optional
# marker; the parser's internal name must not show (#14845).
for 'sub f($? where { 1 }) { }' => "sub f (\$? where \{\n    1\n}) \{ }",
    'sub f($?) { }'             => 'sub f ($?) { }',
    'sub f(Int $?) { }'         => 'sub f (Int $?) { }',
    'sub f(@?) { }'             => 'sub f (@?) { }',
    'sub f($x?) { }'            => 'sub f ($x?) { }' -> (:key($src), :value($want)) {
    is $src.AST.DEPARSE.trim, $want, "DEPARSE of '$src'";
}

my $param = 'sub f(Int $?) { }'.AST.statements[0].expression.signature.parameters[0];
isa-ok $param, RakuAST::Parameter, 'a Parameter';
is $param.target.raku, RakuAST::ParameterTarget::Var.new(name => '$').raku, 'its target is the bare sigil';
is $param.optional.raku, 'Bool::True', 'marked optional';
nok $param.target.name.contains('ANON'), 'no internal name';
is 'sub f($?, $y?) { }'.AST.DEPARSE.trim, 'sub f ($?, $y?) { }', 'two optionals';
