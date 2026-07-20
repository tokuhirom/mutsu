use Test;

# A user-declared infix operator must override native arithmetic even when the
# operands fall off the Int/Num fast paths (Rat, mixed types). This pins the
# `try_user_infix` fast-bail guard: the guard skips full operator resolution
# only when no `sub infix:<op>` is registered, so a registered one must still
# win on every operand kind.

plan 8;

{
    my sub infix:<*>(Int $a, Int $b) { "II($a,$b)" }
    is 3 * 4, "II(3,4)", "user infix:<*> overrides Int * Int";
}

{
    my sub infix:<*>($a, $b) { "MUL($a,$b)" }
    is (1/2) * 4, "MUL(0.5,4)", "user infix:<*> overrides Rat * Int";
    is (1/2) * (2/3), "MUL(0.5,0.666667)", "user infix:<*> overrides Rat * Rat";
}

{
    my sub infix:</>($a, $b) { "DIV($a,$b)" }
    is 10 / 2, "DIV(10,2)", "user infix:</> overrides Int / Int";
    is 1.5 / 2, "DIV(1.5,2)", "user infix:</> overrides Rat / Int";
}

# No user infix declared: plain arithmetic that leaves the Int fast path must
# still produce correct results (the guard bails to native arithmetic).
is 355/113 * 2, 710/113, "plain Rat * Int is unchanged without a user infix";
is 1/3 + 1/6, 1/2, "plain Rat + Rat is unchanged without a user infix";
is 7 / 2, 3.5, "plain Int / Int -> Rat is unchanged without a user infix";
