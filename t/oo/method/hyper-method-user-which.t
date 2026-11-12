use Test;

# `WHICH` and `WHY` are ordinary methods: a hyper call `>>.WHICH` over objects
# whose class declares one runs the user's method for every element, exactly
# as the scalar `$obj.WHICH` does (the hyper path dispatches it through the
# compiled user-method dispatch).

plan 8;

class P { has $.v; method WHICH { ValueObjAt.new("P|$!v") } }
class Q {
    has $.v;
    multi method WHICH(::?CLASS:D:) { ValueObjAt.new("Q|$!v") }
    multi method WHICH(::?CLASS:U:) { ValueObjAt.new("Q") }
}
class R { has $.v; method WHY { "because $!v" } }

my @a = P.new(:v(1)), P.new(:v(2)), 3, "x", Q.new(:v(5)), Q;
is @a>>.WHICH.map(*.Str).grep(*.starts-with("P" | "Q")).join(" "), "P|1 P|2 Q|5 Q",
    'user WHICH per element, mixed with built-ins and a type object';
is (P.new(:v(7)), Q.new(:v(8)))>>.WHICH>>.Str.join(" "), "P|7 Q|8",
    'single and multi user WHICH';
is (R.new(:v(1)), R.new(:v(2)))>>.WHY.join(","), "because 1,because 2", 'user WHY';
is (P.new(:v(9)), [P.new(:v(10))])>>.WHICH>>.Str.raku, '("P|9", $["P|10"])',
    'hyper descends into a nested Array';
is (P.new(:v(1)), Q.new(:v(2)))».WHICH.join("|"), "P|1|Q|2", 'french quotes';
is (P.new(:v(1)),)>>.?WHICH>>.Str.join, "P|1", '.? modifier';
is (1, 2)>>.WHICH>>.Str.join(" "), "Int|1 Int|2", 'built-in WHICH is untouched';

class Node {
    has @.args;
    method WHICH { ValueObjAt.new: "Node|" ~ @!args>>.WHICH.join("|") }
}
my $tree = Node.new(args => (Node.new(args => (P.new(:v(1)),)), P.new(:v(2))));
is $tree.WHICH.Str, "Node|Node|P|1|P|2", 'recursive user WHICH through the hyper';
