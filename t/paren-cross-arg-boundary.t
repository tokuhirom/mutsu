use Test;

plan 7;

# A parenthesized X/Z meta-op keeps its boundary in an argument list. The
# list-infix lift hoists a BARE meta-op's preceding comma items into its left
# operand (`join "", "+" X~ @l` crosses ("", "+") with @l — list-infix is
# looser than the argument comma), but parens close the operand off:
# `join "", ("+" X~ @l)` keeps "" as the separator. Cro::MediaType's subtype
# action built subtype names with exactly this shape and silently lost every
# suffix but the last.

my @lit = "a", "b";

is (join "", ("+" X~ @lit)), "+a+b",
    'parenthesized cross stays one argument';
is (join "", "+" X~ @lit), "",
    'unparenthesized cross still lifts the comma items into its left operand';

my @r = "", ("+" X~ @lit);
is @r.elems, 2, 'comma list keeps the parenthesized cross as one element';
is @r[0], "", '... and the preceding item intact';

is-deeply (1, ("a" X~ "b"), 2), (1, ("ab",).Seq, 2),
    'mid-list parenthesized cross unaffected by neighbours';
is-deeply (1 X~ 2), ("12",).Seq, 'standalone parenthesized cross';
is-deeply (1, 2 Z+ 10, 20), (11, 22).Seq, 'bare zip still lifts across the comma';
