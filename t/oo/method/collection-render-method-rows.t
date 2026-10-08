use Test;

# ADR-11276: gist on List, Seq, Hash/Map and Pair is a method-table row sharing
# one renderer with the cascade (user-gist elements and cycles still route away).

plan 16;

is [1, 2, (3, 4)].gist, "[1 2 (3 4)]", "Array.gist";
is (1, 2, 3).gist, "(1 2 3)", "List.gist";
is (1, 2, 3).Seq.gist, "(1 2 3)", "Seq.gist";
is slip(1, 2).gist, "(1 2)", "Slip.gist";
is [1 .. 200].gist.ends-with(" ...]"), True, "Array.gist caps at 100 elements";
is %(a => 1, b => 2).gist, "\{a => 1, b => 2}", "Hash.gist";
is (a => 1).gist, "a => 1", "Pair.gist";
is ((a => 1) => 2).gist, "(a => 1) => 2", "Pair with a Pair key";
is {a => [1, 2]}.gist, "\{a => [1 2]}", "nested Hash.gist";

is Map.new((a => 1)).gist, "Map.new((a => 1))", "an immutable Map keeps its Map.new form";

class Fancy { method gist { "FF" } }
is [Fancy.new, 1].gist, "[FF 1]", "an element's own gist is used";
is (a => Fancy.new).gist, "a => FF", "Pair value's own gist is used";

my @c;
@c.push(@c);
like @c.gist, /^ '(\\Array_' \d+ ' = [Array_' \d+ '])' $/, "a cyclic Array renders the back-reference";

throws-like { (1/0,).gist }, X::Numeric::DivideByZero, "a zero-denominator element dies";

my $big = Map.new((1 .. 202).list);
ok $big.gist.ends-with(", ...))"), "a Map with more than 100 pairs ends in ...";
is $big.gist.comb("=>").elems, 100, "... after the first 100 sorted pairs";
