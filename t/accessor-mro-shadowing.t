use v6;
use Test;

plan 9;

# The auto-generated accessor for `has $.x` is an ordinary method of its
# declaring class, so it participates in the MRO like any other method.
# Regression coverage for zef's Zef::Distribution, whose `has $.name`
# accessor was shadowed by the parent DependencySpecification's expensive
# `method name` (a full identity-grammar parse per call).

# 1. Child attr accessor shadows parent explicit method
class P1 { method name { "parent" } }
class C1 is P1 { has $.name }
is C1.new(name => "attr").name, "attr",
    "child attribute accessor shadows parent method";

# 2. Child explicit method beats parent attr accessor
class P2 { has $.name }
class C2 is P2 { method name { "method" } }
is C2.new(name => "attr").name, "method",
    "child method beats parent attribute accessor";

# 3. Same class: explicit method suppresses the accessor
class B3 { has $.x; method x { "method" } }
is B3.new(x => "attr").x, "method",
    "same-class explicit method suppresses accessor";

# 4. Role method vs class attr at same level: attr wins (class prioritization)
role R4 { method y { "role" } }
class C4 does R4 { has $.y }
is C4.new(y => "attr").y, "attr",
    "class accessor beats role-composed method at same level";

# 5. Role method composed into child vs parent attr: role method wins
class P5 { has $.z }
role R5 { method z { "role" } }
class C5 is P5 does R5 { }
is C5.new(z => "attr").z, "role",
    "role method composed into child beats parent accessor";

# 6. Accessor in the middle of a deeper hierarchy wins over grandparent method
class G6 { method w { "grand" } }
class M6 is G6 { has $.w }
class C6 is M6 { }
is C6.new(w => "attr").w, "attr",
    "inherited accessor shadows grandparent method";

# 7. Child rw attr shadows parent method: write path goes through the accessor
class P7 { method v { "parent" } }
class C7 is P7 { has $.v is rw }
my $c7 = C7.new(v => "init");
lives-ok { $c7.v = "written" }, "assignment through shadowing rw accessor lives";
is $c7.v, "written", "assignment through shadowing rw accessor sticks";

# 8. Parent method still reachable when child declares no attribute
class P8 { method u { "parent" } }
class C8 is P8 { }
is C8.new.u, "parent", "parent method still dispatches without a shadowing attribute";

done-testing;
