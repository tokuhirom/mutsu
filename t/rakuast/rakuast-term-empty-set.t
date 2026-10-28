use Test;

# From Physics::Unit: `has MixHash $.dmix is rw = ∅.MixHash;`. Rakudo reads `∅`
# as `RakuAST::Term::EmptySet`; the converter refused the parser's literal empty
# Set ("literal Set(...)"), so the module could not load.
plan 7;

my $ast = Q[∅].AST;
my $expr = $ast.statements.head.expression;
isa-ok $expr, RakuAST::Term::EmptySet, '∅ is a Term::EmptySet';

isa-ok RakuAST::Term::EmptySet.new, RakuAST::Term, 'and a Term';
is-deeply EVAL($ast), set(), 'which evaluates to the empty set';
is-deeply EVAL(RakuAST::Term::EmptySet.new), set(), 'also when built by hand';

my $postfix = Q[∅.MixHash].AST.statements.head.expression;
isa-ok $postfix, RakuAST::ApplyPostfix, 'a method call on it is a postfix application';
isa-ok $postfix.operand, RakuAST::Term::EmptySet, 'whose operand is the empty set term';
is-deeply EVAL(Q[∅.MixHash].AST), ∅.MixHash, 'and the round trip still runs';

# vim: expandtab shiftwidth=4
