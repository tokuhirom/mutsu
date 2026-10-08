use Test;

# `@$s` is the array contextualizer: it re-reads a reified Seq, where an
# explicit `.list` consumes it (#9930). The parser spells it as a sugared
# `.list` call, so the RakuAST round trip has to keep it a contextualizer
# (`Contextualizer::List`) instead of an `ApplyPostfix` `.list`, whose lowering
# is the consuming explicit call.

plan 4;

my $code = Q[my $s = (1, 2, 3).map({ $_ }); (@$s.elems, @$s.elems)];
is-deeply $code.AST.EVAL.List, (3, 3), '@$s twice over a Seq survives the AST round trip';

my $loops = Q[my $s = (1, 2, 3).map({ $_ }); my @a; for @$s -> $x { @a.push($x) }; for @$s -> $x { @a.push($x) }; @a];
is-deeply $loops.AST.EVAL.List, (1, 2, 3, 1, 2, 3), 'for @$s twice re-reads the Seq after the round trip';

my $explicit = Q[my $s = (1, 2, 3).map({ $_ }); $s.list; try { $s.List }; $!.^name];
is $explicit.AST.EVAL, 'X::Seq::Consumed', 'an explicit .list still consumes after the round trip';

my $node = Q[my $s; @$s].AST.statements[1];
isa-ok $node.expression, RakuAST::Contextualizer::List, '@$s is a Contextualizer::List node';
