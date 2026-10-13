use v6;
use Test;
use experimental :rakuast;

# A hyper prefix (`-« @a`, `++<< @a`) is an ApplyPrefix whose prefix is a
# MetaPrefix::Hyper over the plain Prefix. Measured on rakudo 2026.09.
plan 12;

# --- read direction ---------------------------------------------------------

my $neg = Q[-« (1, 2)].AST.statements[0].expression;
is $neg.^name, 'RakuAST::ApplyPrefix', 'a hyper prefix is an ApplyPrefix';
is $neg.prefix.^name, 'RakuAST::MetaPrefix::Hyper', 'its prefix is a MetaPrefix::Hyper';
is $neg.prefix.prefix.^name, 'RakuAST::Prefix', 'over the plain prefix';
is $neg.prefix.prefix.operator, '-', 'which keeps the operator';
is $neg.operand.^name, 'RakuAST::Circumfix::Parentheses', 'the operand is the term';

my $inc = Q[my @a; ++<< @a].AST.statements[1].expression;
is $inc.prefix.prefix.operator, '++', 'the ASCII spelling of ++« is the same node';

# --- write direction (EVAL) and semantics -----------------------------------

my $built = RakuAST::ApplyPrefix.new(
    prefix  => RakuAST::MetaPrefix::Hyper.new(RakuAST::Prefix.new('-')),
    operand => RakuAST::IntLiteral.new(3),
);
is-deeply $built.EVAL, (-3,), 'a hand-built hyper prefix evaluates';
is-deeply Q[-« (1, (2, 3))].AST.EVAL, (-1, (-2, -3)), 'a round-tripped one keeps the nesting';
is-deeply Q[?« (0, 5, "")].AST.EVAL, (False, True, False), 'and boolifies element-wise';

# Ordinary (not round-tripped) forms compute the same.
my @x = 1, 2;
my @r = ++« @x;
is-deeply @x, [2, 3], '++« increments each element in place';
is-deeply (~« (1, 2)), ('1', '2'), '~« stringifies element-wise';
is-deeply (-<< (1, (2, 3))), (-1, (-2, -3)), '-<< keeps the nesting';
