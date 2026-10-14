use Test;
use experimental :rakuast;

# `* OP= VALUE` in RakuAST, measured on rakudo 2026.09: an
# `ApplyInfix(left => Term::Whatever, infix => MetaInfix::Assign(Infix(OP)))`
# that EVALs to a WhateverCode mutating its argument, as `{ $_ OP= VALUE }`.
# The read-back tree spells its `*` as a priming argument; the round trip
# must still curry rather than assign to a read-only `*`.

plan 7;

my $node = Q[my @m; @m.map(* *= 2)].AST.statements[1].expression.postfix.args.args[0];
isa-ok $node, RakuAST::ApplyInfix, '`* *= 2` is an ApplyInfix';
isa-ok $node.infix, RakuAST::MetaInfix::Assign, '... over MetaInfix::Assign';

my @m = 1, 2, 3;
is-deeply Q[@m.map(* *= 2).List].AST.EVAL, (2, 4, 6), 'the round trip maps through the WhateverCode';
is-deeply @m, [2, 4, 6], '... and writes back into the array';

my @s = <a b>;
is-deeply Q[@s.map(* ~= "!").List].AST.EVAL, <a! b!>, 'a string compound assignment';

my $built = RakuAST::ApplyInfix.new(
    left  => RakuAST::Term::Whatever.new,
    infix => RakuAST::MetaInfix::Assign.new(RakuAST::Infix.new('+')),
    right => RakuAST::IntLiteral.new(10));
my $code = $built.EVAL;
isa-ok $code, WhateverCode, 'a hand-built tree EVALs to a WhateverCode';
my $v = 1;
$code($v);
is $v, 11, '... that assigns to its argument';
