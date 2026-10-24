use v6;
use Test;

plan 8;

# A `but` mixin keeps the `Bool` of the instance it wraps (its class's own, or
# one a composed role declares).
class Base { method Bool { False } }
role Extra { }
my $m = Base.new but Extra;
is $m.Bool, False, ".Bool";
is ?$m, False, "prefix ?";
is !$m, True, "prefix !";
is so($m), False, "so";
is ($m ?? "t" !! "f"), "f", "ternary condition";

role HasBool { method Bool { False } }
class Comp does HasBool { }
my $c = Comp.new but Extra;
is ?$c, False, "a composed role's Bool under the mixin";
is !$c, True, "and !";

role Own { method Bool { True } }
is ?(Base.new but Own), True, "the mixin's own Bool still wins";
