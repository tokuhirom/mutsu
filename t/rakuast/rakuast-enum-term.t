use v6;
use Test;
use experimental :rakuast;

# RakuAST::Type::Enum beyond a plain word list or pair list: a declared base
# type (`of`), a word-quoted body with colonpair values (each a QuoteWordsAtom
# segment), and a computed parenthesized body kept as written. Shapes measured
# on rakudo 2026.09.
plan 22;

# --- read direction ---------------------------------------------------------

my $typed = Q[our Str enum TSel « :Alpha<a-val> :Beta<b-val> »].AST.statements[0].expression;
is $typed.^name, 'RakuAST::Type::Enum', 'a typed enum renders as Type::Enum';
is $typed.of.^name, 'RakuAST::Type::Simple', 'its base type is the `of` field';
is $typed.of.name.canonicalize, 'Str', 'the base type names Str';
my @atoms = $typed.term.segments.grep(RakuAST::QuoteWordsAtom);
is @atoms.elems, 2, 'each colonpair word is a QuoteWordsAtom segment';
is @atoms[0].atom.^name, 'RakuAST::ColonPair::Value', 'over a ColonPair::Value';
is @atoms[0].atom.key, 'Alpha', 'that keeps its key';

my $mixed = Q[enum MixA «:one(1) two three»].AST.statements[0].expression;
is $mixed.term.processors.raku, '("quotewords", "val")', 'the body keeps quotewords';
is $mixed.term.segments[*-1].^name, 'RakuAST::StrLiteral', 'plain words stay literal text';
is $mixed.term.segments[*-1].value.words.join(','), 'two,three', 'with the plain words';

my $computed = Q[enum CSlip (:20CA, slip <CB CC>)].AST.statements[0].expression;
is $computed.term.^name, 'RakuAST::Circumfix::Parentheses',
    'a computed body is the parenthesized term';
ok $computed.gist.contains('slip'), 'which keeps the slip call as written';

ok !Q[enum Plain <a b>].AST.statements[0].expression.of.defined,
    'an enum without a base type has no `of`';

# --- write direction (EVAL) and semantics -------------------------------------

my $typed-map = Q[our Str enum ESel « :Alpha<a-val> :Beta<b-val> »].AST.EVAL;
is $typed-map<Alpha>, 'a-val', 'a typed word-quoted enum keeps its first value';
is $typed-map<Beta>, 'b-val', 'and its second value';

my $mixed-map = Q[enum EMix «:one(1) two three»].AST.EVAL;
is $mixed-map<one>, 1, 'an explicit colonpair value';
is $mixed-map<two>, 2, 'the next plain word counts on from it';
is $mixed-map<three>, 3, 'and the one after';

my $computed-map = Q[enum ESlip (:20SA, slip <SB SC>)].AST.EVAL;
is $computed-map<SA>, 20, 'a computed body evaluates its colonpair';
is $computed-map<SC>, 22, 'and its slipped words count on';

# Ordinary (not round-tripped) declarations compute the same values.
our Str enum Sel « :Alpha<a-val> »;
ok Sel::Alpha ~~ Str, 'a Str-enum value is a Str';
module Pkg { enum Fmt <Text Binary> }
is Pkg::Binary.value, 1, 'an enum value is reachable through its package';
enum Slipped (:30SLA, slip <SLB>);
is SLB.value, 31, 'a slipped value counts on from the colonpair';
