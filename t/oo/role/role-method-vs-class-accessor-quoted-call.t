use Test;

# From the Contact distribution: a role stub `method street { Str }` composed
# into a class that declares `has Str $.street`. The class accessor outranks
# the role method for every call form, including a quoted / run-time method
# name on a named receiver.

plan 6;

role R {
    method street { Str }
    method attrs { <street> }
    method components { self.attrs.map: { self."$_"() // '' } }
}
class G does R { has Str $.street; }

my $g = G.new(street => "123 Main");
my $n = "street";

is $g.street, "123 Main", "plain call reads the accessor";
is $g."street"(), "123 Main", "quoted literal name reads the accessor";
is $g."$n"(), "123 Main", "run-time name reads the accessor";
is-deeply $g.components.list, ("123 Main",), "role method dispatching by name sees the accessor";

class H does R { }
is-deeply H.new."street"(), Str, "no class accessor: the role method still answers";

role R2 { method s2 { "role" } }
class G2 does R2 { method s2 { "class" } has $.s2; }
is G2.new(s2 => "attr")."s2"(), "class", "an explicit class method still beats the accessor";
