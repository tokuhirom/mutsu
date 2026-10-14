use v6;
use Test;
use experimental :rakuast;

# `@a[]:exists...` / `%h{}:exists...`: an empty subscript with adverbs is an
# ArrayIndex / HashIndex with an empty SemiList and the adverbs as colonpairs.
plan 10;

my $ast = 'my @a; @a[]:exists:delete'.AST.statements[1].expression;
my $idx = $ast.postfix;
isa-ok $idx, RakuAST::Postcircumfix::ArrayIndex, 'an array zen slice with adverbs';
is $idx.index.statements.elems, 0, 'the SemiList is empty';
is $idx.colonpairs.elems, 2, 'both adverbs are colonpairs';
isa-ok $idx.colonpairs[0], RakuAST::ColonPair::True, ':exists is a true colonpair';
is $idx.colonpairs[1].key, 'delete', 'in source order';

is-deeply EVAL('my @a = 1,2,3; (@a[]:exists:delete)'.AST), (True, True, True),
    'exists + delete over a zen slice round-trips';
is-deeply EVAL('my @a = 1,2,3; (@a[]:!exists)'.AST), (False, False, False),
    'a negated exists round-trips';
is-deeply EVAL('my @a = 1,2; (@a[]:delete:exists)'.AST), (True, True),
    'delete before exists round-trips';
is-deeply EVAL('my @a = 1,2; (@a[]:exists:kv)'.AST), (0, True, 1, True),
    'exists with a value adverb round-trips';
is-deeply EVAL('my %h = a => 1, b => 2; (%h{}:exists).sort'.AST), (True, True),
    'a hash zen slice round-trips';
