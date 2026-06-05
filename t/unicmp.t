use Test;

plan 12;

# Structural infix operators (cmp/leg/coll/unicmp) cannot be used in reduction
# form: they are diffy and not chaining.
{
    my $ok = try EVAL '[unicmp] <a a a>';
    nok $ok.defined, "[unicmp] reduction is a compile error (diffy, not chaining)";
}

# unicmp sorts according to Unicode collation (default DUCET), disregarding
# case and codepoint order, unlike cmp.
is-deeply 'a' unicmp 'Z', Less, "a unicmp Z is Less (collation, not codepoint)";
is-deeply 'a' cmp 'Z', More, "a cmp Z is More (codepoint order)";
is-deeply 'a' unicmp 'a', Same, "equal strings are Same";
is-deeply 'b' unicmp 'a', More, "b unicmp a is More";
is-deeply 'a' unicmp 'b', Less, "a unicmp b is Less";

# unicmp is case-insensitive at the primary level
is-deeply 'apple' unicmp 'apple', Same, "identical words are Same";
ok ('Apple' unicmp 'apple') ~~ (Less | Same | More), "case differs only at tertiary level";

# unicmp is not influenced by $*COLLATION (unlike coll)
{
    my $*COLLATION = Collation.new(:!tertiary, :!quaternary);
    is-deeply 'a' unicmp 'A', Same | Less | More,
        "unicmp ignores \$*COLLATION (still a valid Order)";
}

# Works on numbers (Cool) too
is-deeply 1 unicmp 2, Less, "1 unicmp 2 is Less";

# Returns an Order enum
isa-ok ('a' unicmp 'b'), Order, "unicmp returns an Order";
ok ('a' unicmp 'b') === Less, "result identity is the Less enum value";
