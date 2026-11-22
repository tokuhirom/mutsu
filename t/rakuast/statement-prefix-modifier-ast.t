use Test;
use experimental :rakuast;

# A statement modifier written after a prefixed statement belongs to the
# statement inside the prefix (`StatementPrefix::Quietly(Statement(...,
# condition-modifier => ...))`), as in rakudo (#18510).

my @prefixes = <quietly try do sink eager lazy hyper race gather start once>;
plan 2 * @prefixes + 2;

for @prefixes -> $p {
    my $outer = "$p say 1 if 1".AST.statements[0];
    nok $outer.condition-modifier.defined, "$p: the outer statement has no modifier";
    my $inner = $outer.expression.blorst;
    ok $inner ~~ RakuAST::Statement && $inner.condition-modifier.defined,
        "$p: the prefixed statement carries the modifier";
}

is Q[quietly say 1 if 1].AST.DEPARSE.trim, 'quietly say 1 if 1', 'DEPARSE round-trips';
is Q[sink 1 for 1, 2].AST.EVAL, Nil, 'EVAL of a prefix over a looped statement';
