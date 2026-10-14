use v6.e.PREVIEW;
use Test;
use experimental :rakuast;

# `@a[|| @i]` spreads the list `@i` over the subscript's dimensions (6.e).
# Measured on rakudo 2026.09: the `||` is an `ApplyPrefix` of
# `Prefix::Multislice` in the subscript's one-statement SemiList, and an
# assignment folds into the postcircumfix's `assignee`. The round trip spreads
# the list as the parsed program does.

plan 10;

my $index = Q[my @a; my @i; @a[|| @i]].AST.statements[2].expression.postfix.index;
my $outer = $index.statements[0].expression;
isa-ok $outer, RakuAST::ApplyPrefix, '`||` is an ApplyPrefix ...';
isa-ok $outer.prefix, RakuAST::Prefix::Multislice, '... of Prefix::Multislice';
isa-ok $outer.prefix, RakuAST::Prefix, '... a Prefix';
is $outer.operand.name, '@i', '... over the spread list';

my $assign = Q[my @a; my @i; @a[|| @i] = 7].AST.statements[2].expression;
isa-ok $assign.postfix.assignee, RakuAST::IntLiteral, 'an assignment is the subscript\'s assignee';

my @a;
my @i := (0, 1), 0;
is-deeply Q[(@a[|| @i] = 42, 666)].AST.EVAL, (42, 666), 'the round trip assigns through the spread';
is-deeply @a, [[42], [666]], '... into nested arrays';

my %h;
my @k := <a b>, <c>;
Q[%h{|| @k} = 1, 2].AST.EVAL;
is-deeply %h, { a => { c => 1 }, b => { c => 2 } }, 'a hash subscript spreads too';
is-deeply Q[%h{|| @k}:delete].AST.EVAL, (1, 2), 'with an adverb';
is-deeply %h, { a => {}, b => {} }, '... which deletes the spread keys';
