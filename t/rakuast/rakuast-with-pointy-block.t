use Test;

# `with` / `without` blocks with an explicit signature in RakuAST, measured on
# rakudo 2026.09: the clause body is a `PointyBlock` (not the implicit-topic
# `Block` of the parameterless form), and `with` may carry an `else`. EVAL of
# the tree binds the parameter to the tested value as the parsed program does.

plan 14;

my $with = Q[my $x; with $x -> Int $v { 1 }].AST.statements[1];
isa-ok $with, RakuAST::Statement::With, 'a `with` block with a signature is a Statement::With';
isa-ok $with.then, RakuAST::PointyBlock, 'its clause is a PointyBlock';
my @params = $with.then.signature.parameters;
is @params.elems, 1, 'with one parameter';
is @params[0].target.name, '$v', 'named `$v`';
is @params[0].type.name.canonicalize, 'Int', 'typed Int';
ok $with.then.signature.defined, 'the signature is kept';

my $without = Q[without Int -> $z { 1 }].AST.statements[0];
isa-ok $without, RakuAST::Statement::Without, '`without -> $z` is a Statement::Without';
isa-ok $without.body, RakuAST::PointyBlock, 'with a PointyBlock body';

my $plain = Q[with 1 { 2 }].AST.statements[0];
isa-ok $plain.then, RakuAST::Block, 'the parameterless form stays a Block';

sub run($src) { EVAL($src.AST) }
is run(Q[my $x = 5; my $r; with $x -> Int $v { $r = $v + 1 }; $r]), 6, 'the parameter is bound';
is run(Q[my $r = 0; with Int -> $v { $r = 1 } else { $r = 2 }; $r]), 2, 'else runs for an undefined value';
is run(Q[my $r; without Int -> $z { $r = "u" }; $r]), 'u', 'without binds the undefined value';
is run(Q[my %h = a => 1; my @r; with %h<a> -> $q is copy { $q++; @r = $q, %h<a> }; @r.join(",")]),
    '2,1', 'a copy parameter does not write back';
is run(Q[my $r = (with 1 -> $a { $a + 41 }); $r]), 42, 'a `with` block is usable as an expression';
