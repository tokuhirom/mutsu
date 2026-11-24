use Test;

plan 13;

for Q[qq:w/a b/], Q[qq:w{a b}], Q[q:w/a b/], Q[qq:w<a b>] -> $source {
    my $literal = $source.AST.statements[0].expression.segments[0];
    is $literal.dump, "StrLiteral  ⎡a b⎤\n", "$source: word literal has its body origin";
    nok $literal.origin.is-key, "$source: word literal origin is not a key";
}

my $statement = Q["x{1}y"].AST.statements[0].expression.segments[1].body.statement-list.statements[0];
ok $statement.origin.is-key, 'interpolated block statement has a key origin';
is $statement.dump-markers, '▪𝄞', 'interpolated block statement has both markers';
is Q["x{1}y"].AST.EVAL, 'x1y', 'the origin does not change interpolation';

my $statements = Q["x{say 1; say 2}y"].AST.statements[0].expression.segments[1].body.statement-list.statements;
ok $statements[0].origin.is-key, 'first statement in an interpolated block has a key origin';
ok $statements[1].origin.is-key, 'last statement in an interpolated block has a key origin';
