use Test;

# Terms the parser folds to a value -- a type object (`Any`), a Num (`1e0`,
# `Inf`, `NaN`) and `Empty` -- render as the nodes rakudo keeps for them
# (measured on 2026.09) and lower back (ADR-10723 Stage 1). This file passes
# under both mutsu and raku.

plan 15;

sub expr(Str $src) { $src.AST.statements[0].expression }

isa-ok expr(Q[Any]), RakuAST::Type::Simple, 'Any is a Type::Simple';
like expr(Q[Any]).gist, /'Name.from-identifier("Any")'/, 'naming Any';

isa-ok expr(Q[1e0]), RakuAST::NumLiteral, '1e0 is a NumLiteral';
is expr(Q[1e0]).gist, 'RakuAST::NumLiteral.new(1e0)', 'and renders as a Num literal';
is expr(Q[2.5e3]).gist, 'RakuAST::NumLiteral.new(2500e0)', 'with the Num spelling';
is expr(Q[Inf]).gist, 'RakuAST::NumLiteral.new(Inf)', 'Inf is a NumLiteral';
is expr(Q[NaN]).gist, 'RakuAST::NumLiteral.new(NaN)', 'so is NaN';
is expr(Q[1e0]).value, 1e0, 'NumLiteral.value';

isa-ok expr(Q[Empty]), RakuAST::Term::Name, 'Empty is a Term::Name';
like expr(Q[Empty]).gist, /'Name.from-identifier("Empty")'/, 'naming Empty';

# Write direction.
is EVAL(Q[Any].AST).^name, 'Any', 'Any round-trips to the type object';
ok EVAL(Q[Any].AST) === Any, 'the same type object';
is EVAL(Q[2.5e0 * 2].AST), 5e0, 'a Num literal round-trips';
is EVAL(Q[-Inf].AST), -Inf, 'Inf round-trips';
is EVAL(Q[(1, Empty, 2)].AST).elems, 2, 'Empty round-trips as the empty Slip';
