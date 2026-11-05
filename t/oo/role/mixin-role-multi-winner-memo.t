use Test;
plan 11;

# A role multi called on a mixin: the winner is chosen by the argument types
# and the receiver's definedness, and stays right across repeated calls.
role Pick {
    multi method pick(::?CLASS:D: Int:D $x) { "int:$x" }
    multi method pick(::?CLASS:D: Str:D $x) { "str:$x" }
    multi method pick(::?CLASS:D: Any:D $x) { "any:$x" }
    multi method pick(::?CLASS:U: Int:D $x) { "type-int:$x" }
}

class Base { has $.n = 1 }
my $obj = Base.new but Pick;

for 1 .. 3 -> $round {
    is $obj.pick(5), 'int:5', "Int candidate, round $round";
    is $obj.pick('a'), 'str:a', "Str candidate, round $round";
}
is $obj.pick(2.5), 'any:2.5', 'Any candidate for a Rat';

my $type = Base but Pick;
is $type.pick(7), 'type-int:7', 'a type-object receiver takes the :U candidate';
is $obj.pick(8), 'int:8', 'and the defined receiver still takes :D';

# A second object with the same role set shares the winner.
my $other = Base.new(n => 9) but Pick;
is $other.pick(4), 'int:4', 'another receiver of the same role';
is $other.pick('z'), 'str:z', 'and another argument type';
