use Test;

# A `for` loop's pointy block in RakuAST, measured on rakudo 2026.09: a
# `Statement::For` whose body is a `PointyBlock` carrying every parameter.
# EVAL hands each parameter back with its type and traits, so the
# round-tripped loop binds exactly as the parsed one does.

plan 9;

my $for = Q[for (1, 2).kv -> $i, $k { }].AST.statements.head;
isa-ok $for, RakuAST::Statement::For, '`for … -> $i, $k { }` is a Statement::For';
isa-ok $for.body, RakuAST::PointyBlock, 'with a pointy body';
is $for.body.signature.parameters.elems, 2, 'holding both parameters';

is EVAL(Q[my @g; for (1, 2).kv -> $i, $k { @g.push($i + $k) }; @g.join(',')].AST),
    '1,3', 'two parameters survive the round trip';
is EVAL(Q[my @g; for 1 .. 6 -> $a, $b, $c { @g.push($a * $b * $c) }; @g.join(',')].AST),
    '6,120', 'and three';
throws-like { EVAL(Q[for 1, "a" -> Int $a { }].AST) }, X::TypeCheck::Binding,
    'a typed parameter still type-checks';
is EVAL(Q[my @g; for 1, 2 -> $a is copy { $a++; @g.push($a) }; @g.join(',')].AST),
    '2,3', 'an `is copy` parameter stays writable';
is EVAL(Q[my @x = 1, 2; for @x -> $a is rw { $a *= 10 }; @x.join(',')].AST),
    '10,20', 'an `is rw` parameter still writes through';
is EVAL(Q[my @g; for (1, 2), (3, 4) -> @a { @g.push(@a.sum) }; @g.join(',')].AST),
    '3,7', 'an array parameter keeps its sigil';
