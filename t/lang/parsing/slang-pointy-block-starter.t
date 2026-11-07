use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang's `pointy-block-starter` token adds lexemes that open a pointy block
# (Slang::Lambda: `λ`). The built-in `->` keeps working.

plan 5;

use PointyStarterSlang;

is-deeply (1, 2, 3).map(λ $x { $x + 1 }), (2, 3, 4).Seq, 'the new starter opens a pointy block';
is-deeply (1, 2, 3).map(-> $x { $x * 2 }), (2, 4, 6).Seq, 'the built-in -> still does';
my &f = λ $a, $b { $a ~ $b };
is f('x', 'y'), 'xy', 'it takes a multi-parameter signature';
is (↔ $x { $x + 1 })(1), 2, 'every listed lexeme counts';
is (λ { 42 }).(), 42, 'and a block without parameters';
