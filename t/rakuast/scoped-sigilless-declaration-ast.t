use Test;
use experimental :rakuast;

plan 12;

# A sigilless declaration with an `our` or `state` scope is a
# `VarDeclaration::Term` carrying `scope`, as rakudo builds it (#18426).

for Q[our \T := Int], Q[state \x = 1], Q[our Int \y = 3], Q[state Str \s := "a"] -> $code {
    is $code.AST.DEPARSE.trim, $code, "DEPARSE of $code";
}

my $our = Q[our Int \y = 3].AST.statements[0].expression;
is $our.^name, 'RakuAST::VarDeclaration::Term', 'the node class';
is $our.scope, 'our', 'its scope';
is $our.type.raku, 'RakuAST::Type::Simple.from-identifier("Int")', 'its type';
like $our.raku, /^ 'RakuAST::VarDeclaration::Term.new(' \s* 'scope' \s* '=> "our"'/,
    '.raku names the scope first';

is Q[state \s = 1].AST.statements[0].expression.scope, 'state', 'a state scope';
nok Q[my \m = 1].AST.statements[0].expression.raku.contains('scope'), 'my has no scope';

is Q[state \x = 5; x].AST.EVAL, 5, 'EVAL of a state declaration';
is RakuAST::VarDeclaration::Term.new(
    scope       => "our",
    name        => RakuAST::Name.from-identifier("Z"),
    initializer => RakuAST::Initializer::Bind.new(RakuAST::IntLiteral.new(7)),
).DEPARSE, 'our \Z := 7', 'a built node deparses with its scope';
