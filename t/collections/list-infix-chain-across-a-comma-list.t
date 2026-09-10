use Test;

# Raku's list infixes (`Z`, `X`, `Zop`, `Xop`, `minmax`) are LOOSER than the
# comma, so `1, 2 Z <a b> Z <c d>` is a THREE-column zip whose first column is
# the comma list `1, 2`. mutsu had three spellings of that source taking three
# different paths, and two of them could not see a chain:
#
#   (1, 2 Z <a b> Z <c d>)      died "Confused. Two terms in a row"
#   [1, 2 Z <a b> Z <c d>]      answered [(1, "c"), (((2, "a"),).Seq, "d")]
#   my @r = 1, 2 Z <a b> Z <c d>          ditto
#
# The paren failure was not a parse problem at all: the (chain-aware) lift built
# `zip(..., with => infix:<>)` for a BARE `Z`, and `infix:<>` names nothing --
# which is why the `Z+` spelling of the very same shape always worked. The other
# two spellings ran a one-level lift that pushed the outer metaop's `left` back
# as an item, which cannot see a chain.
#
# Every expectation below was measured against raku v2026.07 first.

plan 24;

# --- the chain, in all three spellings ------------------------------------

is (1, 2 Z <a b c> Z <x y>).raku, '((1, "a", "x"), (2, "b", "y")).Seq',
   'a Z chain with a comma-list left operand, parenthesised';
is [1, 2 Z <a b> Z <c d>].raku, '[(1, "a", "c"), (2, "b", "d")]',
   '... in a bracket array';
{
    my @r = 1, 2 Z <a b> Z <c d>;
    is @r.raku, '[(1, "a", "c"), (2, "b", "d")]', '... and assigned to an array';
}

# The doc block this came from used `<+ ->`, which was a red herring.
is (1, 2 Z <a b c> Z <+ ->).raku, '((1, "a", "+"), (2, "b", "-")).Seq',
   'an operator-looking word list is just a list';

# Longer chains and wider columns.
is (1, 2 Z <a b> Z <c d> Z <e f>).raku, '((1, "a", "c", "e"), (2, "b", "d", "f")).Seq',
   'a three-Z chain';
is (1, 2, 3 Z <a b c> Z <x y z>).raku, '((1, "a", "x"), (2, "b", "y"), (3, "c", "z")).Seq',
   'a three-element first column';

# --- Zop chains, which return a Seq like every other zip ------------------

is (1, 2 Z+ <3 4> Z+ <5 6>).raku, '(9, 12).Seq', 'a Z+ chain';
is (1, 2 Z+ <3 4> Z+ <5 6>).WHAT.^name, 'Seq', '... is a Seq, not a List';
is (1, 2 Z~ <a b> Z~ <c d>).raku, '("1ac", "2bd").Seq', 'a Z~ chain';
{
    my @z = 1, 2 Z+ <3 4> Z+ <5 6>;
    is @z.raku, '[9, 12]', '... and assigned to an array';
}
is zip((1, 2), (3, 4), with => &infix:<+>).WHAT.^name, 'Seq',
   'zip with a :with adverb is a Seq';
is zip(with => &infix:<+>).raku, '().Seq', '... including the empty case';

# --- X chains take the same route ----------------------------------------

is (1, 2 X <a b> X <c d>).elems, 8, 'an X chain with a comma-list left operand';
{
    my @x = 1, 2 X <a b> X <c d>;
    is @x.elems, 8, '... and assigned to an array';
    is @x[0].raku, '$(1, "a", "c")', '... with the right first element (Array elements itemize)';
}

# --- minmax is at the same precedence level and was never lifted here -----

is (1, 2 minmax 3, 4).raku, '1..4', 'minmax over a comma list, parenthesised';
{
    my @m = 1, 2 minmax 3, 4;
    is @m.raku, '[1, 2, 3, 4]', '... and assigned to an array';
}

# --- what must NOT move ---------------------------------------------------

is (1, 2 Z <a b>).raku, '((1, "a"), (2, "b")).Seq', 'a single Z is unchanged';
is (1, 2 Z <a b>, 3).raku, '((1, ("a", "b")), (2, 3)).Seq',
   'a comma AFTER the right operand still binds looser';
is (1 Z <a b> Z <c d>).raku, '((1, "a", "c"),).Seq', 'a single left operand is unchanged';
is ((1, 2) Z <a b> Z <c d>).raku, '((1, "a", "c"), (2, "b", "d")).Seq',
   'an explicitly parenthesised left operand is unchanged';
is (1, 2 xx 2).raku, '(1, (2, 2).Seq)', 'xx is tighter than the comma and unaffected';
is (* Z+ *)(1, 2).raku, '(3,).Seq', 'a Whatever-curried Z+ still curries';
is-deeply (<a b> Z <c d>).List, (('a', 'c'), ('b', 'd')), 'a plain two-list zip';
