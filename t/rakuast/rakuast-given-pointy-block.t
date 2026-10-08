use Test;

# `given EXPR -> PARAM { }` in RakuAST, measured on rakudo 2026.09: the body is
# a `PointyBlock` (not the implicit-topic `Block` of the parameterless form).
# EVAL of the tree binds the parameter to the topic as the parsed program does.

plan 15;

my $given = Q[given 5 -> $x { say $x }].AST.statements[0];
isa-ok $given, RakuAST::Statement::Given, 'a pointy `given` is a Statement::Given';
isa-ok $given.body, RakuAST::PointyBlock, 'its body is a PointyBlock';
my @params = $given.body.signature.parameters;
is @params.elems, 1, 'with one parameter';
is @params[0].target.name, '$x', 'named `$x`';

my $term = Q[given 5 -> \y { say y }].AST.statements[0];
isa-ok $term.body.signature.parameters[0].target, RakuAST::ParameterTarget::Term,
    'a sigilless parameter is a ParameterTarget::Term';

my $plain = Q[given 5 { say $_ }].AST.statements[0];
isa-ok $plain.body, RakuAST::Block, 'the parameterless form stays a Block';

sub run($src) { EVAL($src.AST) }
is run(Q[my $r; given 5 -> $x { $r = $x + 1 }; $r]), 6, 'the parameter is bound to the topic';
is run(Q[my $r; given 7 -> \y { $r = y * 2 }; $r]), 14, 'a sigilless parameter is bound';
is run(Q[my @a = 1, 2; given @a -> @p { @p.push(3) }; @a.join(",")]), '1,2,3',
    'an array parameter aliases the source';
is run(Q[my $a = 1; given $a -> $p is rw { $p = 9 }; $a]), 9, '`is rw` writes back';
is run(Q[my $a = 1; given $a -> $p is copy { $p = 9 }; $a]), 1, '`is copy` does not write back';
is run(Q[my $r; given 3 -> Int $n { $r = $n }; $r]), 3, 'a typed parameter';
is run(Q[my $r = "o"; $_ = "outer"; given 4 -> $v { $r = $_ }; $r]), 'outer',
    '`$_` stays the enclosing topic';
is run(Q[my $r; given (1, 2) -> ($a, $b) { $r = $a + $b }; $r]), 3,
    'a destructuring parameter';
is run(Q[my $r; given 1 -> $o { given 2 -> $i { $r = $o + $i } }; $r]), 3,
    'nested pointy givens';
