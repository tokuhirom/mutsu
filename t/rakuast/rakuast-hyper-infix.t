use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST hyper infix operators (ADR-0011), both directions.
#
# `@a >>+<< @b` is an `ApplyInfix` whose infix is a `MetaInfix::Hyper` wrapping
# the base `Infix`. Each `<<` side sets the corresponding dwim flag, and raku's
# gist omits a dwim field whose value is False. mutsu's internal `Expr::HyperOp`
# already keeps the operator text and both flags, so the mapping is 1:1.
#
# Verified against Rakudo; passes under BOTH mutsu and raku.

plan 15;

# --- read side: the non-dwim form -------------------------------------------
my $strict = Q[my @a; my @b; @a >>+<< @b].AST.gist;
ok $strict.contains('RakuAST::MetaInfix::Hyper.new('),
    'a hyper infix renders as a MetaInfix::Hyper';
ok $strict.contains('infix      => RakuAST::Infix.new("+")')
    || $strict.contains('infix => RakuAST::Infix.new("+")'),
    'the base operator is kept as an Infix node';
nok $strict.contains('dwim-left'), '>>+<< sets no dwim-left';
nok $strict.contains('dwim-right'), '>>+<< sets no dwim-right';

# --- read side: the dwim forms ----------------------------------------------
my $both = Q[my @a; @a <<+>> 1].AST.gist;
ok $both.contains('dwim-left') && $both.contains('=> True'), '<<+>> sets dwim-left';
ok $both.contains('dwim-right => True'), '<<+>> sets dwim-right';

my $right = Q[my @a; @a >>+>> 1].AST.gist;
nok $right.contains('dwim-left'), '>>+>> sets no dwim-left';
ok $right.contains('dwim-right => True'), '>>+>> sets dwim-right';

my $left = Q[my @a; @a <<+<< 1].AST.gist;
ok $left.contains('dwim-left => True'), '<<+<< sets dwim-left';
nok $left.contains('dwim-right'), '<<+<< sets no dwim-right';

# --- introspection ----------------------------------------------------------
my $meta = Q[my @a; my @b; @a >>+<< @b].AST.statements[2].expression.infix;
is $meta.^name, 'RakuAST::MetaInfix::Hyper', 'the infix child is a MetaInfix::Hyper';
is $meta.infix.gist, 'RakuAST::Infix.new("+")', 'MetaInfix::Hyper.infix is the base operator';
# An omitted dwim field still answers False through the accessor.
is $meta.dwim-left, False, 'an elided dwim-left reads as False';

# --- write side: EVAL round-trips the hyper operator ------------------------
is EVAL(Q[my @a = 1, 2, 3; my @b = 10, 20, 30; (@a >>+<< @b).join(",")].AST),
    '11,22,33', 'an element-wise hyper infix round-trips through EVAL';
is EVAL(Q[my @a = 1, 2, 3; (@a >>+>> 1).join(",")].AST), '2,3,4',
    'a dwim-right hyper infix round-trips through EVAL';
