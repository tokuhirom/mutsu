use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST slice 37 (ADR-0011): multi-parameter closures (`sub ($a, $b) { … }` and
# `-> $a, $b { … }`, which both render as a multi-parameter `PointyBlock`) lower
# to `Expr::AnonSubParams`, extending slice 36's single-parameter case. Verified
# against Rakudo; passes under BOTH mutsu and raku.
#
# Note: zero-parameter blocks stay the coverage boundary.

plan 5;

# --- a two-parameter anonymous sub ------------------------------------------
is EVAL(Q{my $add = sub ($a, $b) { $a + $b }; $add(3, 4)}.AST), 7,
    'a two-parameter anonymous sub';

# --- a two-parameter pointy block -------------------------------------------
is EVAL(Q{my $mul = -> $a, $b { $a * $b }; $mul(3, 4)}.AST), 12,
    'a two-parameter pointy block';

# --- a default parameter in an anonymous sub --------------------------------
is EVAL(Q{my $f = sub ($x, $y = 10) { $x + $y }; $f(5)}.AST), 15,
    'a default parameter in an anonymous sub';

# --- a multi-parameter closure passed as a callback -------------------------
is EVAL(Q{sub apply2($g) { $g(6, 7) }; apply2(-> $a, $b { $a * $b })}.AST), 42,
    'a two-parameter closure passed as a callback';

# --- the single-parameter case (slice 36) still works -----------------------
is EVAL(Q{my $f = -> $x { $x * 2 }; $f(9)}.AST), 18,
    'a single-parameter pointy block still lowers';
