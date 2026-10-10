use Test;

# The implicit topic of a bare block aliases a bare List/Seq/Array value; it has
# no Scalar of its own, so using it as a call argument or list element must not
# render as an itemized `$(...)` (#12518).
plan 7;

is {Pair.new('E', $_)}.(("a", "b")).raku, ':E(("a", "b"))', 'Pair.new with a list topic';
is {Pair.new('E', $_)}.(["a", "b"]).raku, ':E(["a", "b"])', 'Pair.new with an array topic';
is {Pair.new('E', $_)}.((1, 2).Seq).raku, ':E((1, 2).Seq)', 'Pair.new with a Seq topic';
is {(1, $_)}.(("a", "b")).raku, '(1, ("a", "b"))', 'list literal with a list topic';
is (("a", "b"),).map({ Pair.new('E', $_) }).raku, '(:E(("a", "b")),).Seq', 'map over lists';

my $l = ("a", "b");
is {Pair.new('E', $_)}.($l).raku, ':E($("a", "b"))', 'a $-container argument stays itemized';
is {(1, $_)}.(5).raku, '(1, 5)', 'scalar topic unchanged';
