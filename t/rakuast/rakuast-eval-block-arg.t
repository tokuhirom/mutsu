use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST slice 34 (ADR-0011): blocks as values (`{ … }` arguments to `.map`,
# `.grep`, `.first`, …). A `RakuAST::Block` in expression position lowers to a
# bare-block closure (`Expr::AnonSub`), so higher-order list methods run. Verified
# against Rakudo; passes under BOTH mutsu and raku.
#
# Note: placeholder-parameter blocks (`{ $^a <=> $^b }`) and calling a code
# variable (`$f(…)`) are separate constructs and stay the coverage boundary.

plan 5;

# --- map with a topic block -------------------------------------------------
is EVAL(Q{(1, 2, 3).map({ $_ * 2 }).sum}.AST), 12, 'map with a { $_ } block';

# --- grep with a topic block ------------------------------------------------
is EVAL(Q{(1..6).grep({ $_ %% 2 }).join(",")}.AST), '2,4,6', 'grep with a { $_ } block';

# --- first with a predicate block -------------------------------------------
is EVAL(Q{(3, 7, 2, 9).first({ $_ > 5 })}.AST), 7, 'first with a predicate block';

# --- a multi-statement block ------------------------------------------------
is EVAL(Q{(1, 2, 3).map({ my $y = $_ * $_; $y + 1 }).join(",")}.AST), '2,5,10',
    'a multi-statement block';

# --- chained higher-order calls ---------------------------------------------
is EVAL(Q{(1..5).map({ $_ * $_ }).grep({ $_ > 5 }).join(",")}.AST), '9,16,25',
    'chained map/grep blocks';
