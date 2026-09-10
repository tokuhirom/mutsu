use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST slice 24 (ADR-0011): array-composer literals `[1, 2, 3]`, both
# directions. raku models `[…]` as a `Circumfix::ArrayComposer` wrapping a
# single-statement `SemiList` (a comma list). The read side (`.AST`) emits that
# node and the write side (`EVAL`) lowers it to an array literal. Verified against
# Rakudo; passes under BOTH mutsu and raku.

plan 5;

# --- read side: the literal renders as Circumfix::ArrayComposer -------------
ok Q{[1, 2, 3]}.AST.gist.contains('RakuAST::Circumfix::ArrayComposer.new('),
    'an array literal renders as Circumfix::ArrayComposer';

# --- element count ----------------------------------------------------------
is EVAL(Q{[1, 2, 3].elems}.AST), 3, 'a three-element array literal';

# --- a list method on the literal -------------------------------------------
is EVAL(Q{[1, 2, 3].sum}.AST), 6, 'sum over an array literal';
is EVAL(Q{[3, 1, 2].sort.join(",")}.AST), '1,2,3', 'sort/join over an array literal';

# --- a single-element literal -----------------------------------------------
is EVAL(Q{[5].elems}.AST), 1, 'a single-element array literal';
