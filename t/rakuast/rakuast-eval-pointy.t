use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST slice 36 (ADR-0011): pointy blocks as code values (`-> $x { … }`).
# A single-parameter `RakuAST::PointyBlock` in expression position lowers to a
# closure (`Expr::Lambda`), so a named-parameter closure can be stored, passed,
# and called. Verified against Rakudo; passes under BOTH mutsu and raku.
#
# Note: multi-/zero-parameter pointy blocks and `sub ($x) { … }` code values are
# separate constructs and stay the coverage boundary.

plan 5;

# --- a pointy closure stored and called -------------------------------------
is EVAL(Q{my $f = -> $x { $x * 2 }; $f(9)}.AST), 18,
    'a pointy closure is stored and called';

# --- the argument binds the named parameter ---------------------------------
is EVAL(Q{my $f = -> $n { $n + 100 }; $f(5)}.AST), 105,
    'the argument binds the pointy parameter';

# --- a pointy passed as a callback ------------------------------------------
is EVAL(Q{sub app($g) { $g(5) }; app(-> $n { $n + 1 })}.AST), 6,
    'a pointy closure passed as a callback';

# --- a pointy as a map block ------------------------------------------------
is EVAL(Q{(1, 2, 3).map(-> $x { $x * $x }).sum}.AST), 14,
    'a pointy block as a map argument';

# --- a multi-statement pointy body ------------------------------------------
is EVAL(Q{my $f = -> $x { my $y = $x + 1; $y * $y }; $f(3)}.AST), 16,
    'a multi-statement pointy body';
