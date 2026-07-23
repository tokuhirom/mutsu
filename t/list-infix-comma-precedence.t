use v6;
use Test;

# Raku's list-infix operators (Z, X, the Z+/X* meta-ops, and minmax) are LOOSER
# than the comma that separates listop arguments (operators.rakudoc precedence
# table: "Comma operator" is tighter than "List infix"). So in `say a, b Zop c, d`
# the whole comma list on each side is one operand: `say((a, b) Zop (c, d))`.
# mutsu previously bound the meta-op tighter than comma, giving `say(a, (b Zop c), d)`.

plan 25;

# Capture what a `say`/`print` statement actually emits, so we test the
# unparenthesized listop statement path (not the parenthesized-list path).
class Cap {
    has @.out;
    method print(*@a) { @.out.append(@a) }
}
sub captured(&code) {
    my $c = Cap.new;
    {
        my $*OUT = $c;
        code();
    }
    $c.out.join;
}

is captured({ say 100, 200 Z+ 42, 23 }), "(142 223)\n",
    'say: (100,200) Z+ (42,23)';
is captured({ say 1, 2 Z 3, 4 }), "((1 3) (2 4))\n",
    'say: (1,2) Z (3,4)';
is captured({ say 1, 2, 3 X* 10, 100 }), "(10 100 20 200 30 300)\n",
    'say: (1,2,3) X* (10,100)';
is captured({ say "a", 1, 2 Z 3, 4 }), "((a 3) (1 4))\n",
    'say: a leading non-numeric arg joins the left operand';
is captured({ say 0, 1, 2 Z 3, 4 }), "((0 3) (1 4))\n",
    'say: all leading args join the left operand';
is captured({ say 1, 2 Z 3, 4, 5 }), "((1 3) (2 4))\n",
    'say: Z stops at the shorter side (5 dropped)';
is captured({ say 1, 2 X 3, 4 }), "((1 3) (1 4) (2 3) (2 4))\n",
    'say: full cross of both comma lists';
is captured({ say 1, 2 Z+ 3, 4 Z+ 5, 6 }), "(9 12)\n",
    'say: chained Z+ is list-associative across the whole level';
is captured({ say 1, 5 minmax 3, 0 }), "0..5\n",
    'say: minmax is list-infix — (1,5) minmax (3,0)';
is captured({ say 1, 2 min 3, 0 }), "120\n",
    'say: min is NOT list-infix (tighter than comma) — 1, (2 min 3), 0';

my $x = 5;
is captured({ say $x, 10 Z+ 1, 2 }), "(6 12)\n",
    'say: a scalar variable joins the left comma operand';

# Array assignment RHS already absorbed the whole comma level; keep it pinned.
my @a = 1, 2 Z 3, 4;
is-deeply @a, [(1, 3), (2, 4)],
    'array assignment RHS absorbs the whole comma level';

# The lift must NOT Whatever-curry a `*` that is a list extender inside an
# operand (`1 xx *` is a lazy infinite repeat, not a curry placeholder), or the
# whole Z becomes a WhateverCode (regression guard for repeat.t test 34).
my @b = flat <a b c> Z (1 xx *);
is @b.join('|'), 'a|1|b|1|c|1', 'Z with an `xx *` extender operand does not curry';

# A no-paren user-sub call gobbles a whole comma level as its argument list, and
# the list-infix operator is LOOSER than that comma, so it owns both operands:
# `f 1, 2 X 3, 4` is `f((1,2) X (3,4))`, one cross-product argument.
sub collect(*@a) { @a.join('|') }
is (collect 1, 2 X 3, 4), '1|3|1|4|2|3|2|4',
    'user-sub no-paren: (1,2) X (3,4) is one argument';
is (collect 100, 200 Z+ 42, 23), '142|223',
    'user-sub no-paren: (100,200) Z+ (42,23)';
is (collect 1, 2 Z 3, 4), '1|3|2|4',
    'user-sub no-paren: (1,2) Z (3,4)';
is (collect 1, 2, 3), '1|2|3',
    'user-sub no-paren: a plain comma list is unaffected';

# A no-paren BUILTIN listop (join/reverse/sort/min/sum/...) gobbles the whole
# comma level, and the list-infix operator is LOOSER than that comma, so it owns
# both operands: `reverse 1, 2 Z 3, 4` is `reverse((1,2) Z (3,4))`. The whole
# argument list (separator included) becomes the meta-op's left operand.
is-deeply (reverse 1, 2 Z 3, 4), ((2, 4), (1, 3)),
    'builtin reverse: ((1,2) Z (3,4)) is one argument, then reversed';
is-deeply (sort 3, 1 Z 2, 4), ((1, 4), (3, 2)),
    'builtin sort: sorts the cross ((3,1) Z (2,4))';
is (min 1, 2 Z 3, 4), (1, 3),
    'builtin min: over ((1,2) Z (3,4))';
is (sum 1, 2 Z 3, 4), 4,
    'builtin sum: over ((1,2) Z (3,4))';

# Junction constructors (all/any/one/none) are list-prefix, so they too absorb
# the whole comma level into a single cross/zip meta-op operand.
ok (? all 1, 2 X<= 2, 3, 4), 'junction all: all((1,2) X<= (2,3,4))';
ok (? one 1, 2 X== 2, 3, 4), 'junction one: one((1,2) X== (2,3,4))';

# `=:=`/`!=:=` compare CONTAINER identity, so a cross/zip operand list of scalar
# variables keeps each element's container (raku Lists retain element
# containers). Four distinct `my` scalars are four distinct containers.
{
    my ($a, $b, $c, $d);
    ok (? all $a, $b X!=:= $c, $d),
        'X!=:= : distinct my-scalars are distinct containers (all True)';
    $c := $b;
    ok (? one $a, $b X=:= $c, $d),
        'X=:= : exactly one pair shares a binding root after `$c := $b`';
}
