use Test;

# ADR-0019 E7 step 5: `.^lookup` (`Interpreter::classhow_lookup`,
# `src/runtime/methods_classhow_lookup.rs`) used to consult only the
# receiver's OWN class registration, never walking the MRO to an ancestor —
# so a method declared solely on a parent class was invisible to
# `.^lookup` on a subclass, even though real `raku` finds it. Confirmed
# against `raku`: `B.^lookup('foo').defined` is `True` for `class B is A {}`
# when `foo` is declared only on `A`.

plan 14;

# (a) The exact confirmed repro: a method declared on a parent class only.
class A1 { method foo { "A1::foo" } }
class B1 is A1 {}
ok B1.^lookup("foo").defined, 'inherited method is found via .^lookup on the subclass';
is B1.new.foo, "A1::foo", 'the inherited method actually runs too';

# (b) A method declared on the receiver's own class directly must still work
# (do not regress the pre-existing own-class behavior).
class C1 { method bar { "C1::bar" } }
ok C1.^lookup("bar").defined, '.^lookup still finds an own-class method';

# (c) A role-composed method: role methods are flattened into the composing
# class's own method table at composition time, so this path already worked
# before this fix and must keep working.
role R1 { method baz { "R1::baz" } }
class D1 does R1 {}
ok D1.^lookup("baz").defined, '.^lookup finds a role-composed method';

# (d) A method declared 2+ levels up the inheritance chain.
class E1 { method deep { "E1::deep" } }
class F1 is E1 {}
class G1 is F1 {}
ok G1.^lookup("deep").defined, '.^lookup walks 2+ levels up the MRO';
is G1.new.deep, "E1::deep", 'the 2-levels-up inherited method actually runs';

# (e) A method that does not exist anywhere in the chain must still answer Nil.
class H1 is E1 {}
nok H1.^lookup("nonexistent").defined,
    '.^lookup on a nonexistent method stays Nil even with a populated MRO';

# A more-derived override still wins over an ancestor's method of the same name.
class I1 { method who { "I1" } }
class J1 is I1 { method who { "J1" } }
is J1.^lookup("who").(J1.new), "J1", 'a more-derived override wins over the ancestor';

# An inherited multi method is also found (E8 will unify multi/proto
# candidate-sequence modeling; today's `.^lookup` answer for an own-class
# multi already returns just the first candidate, and the MRO walk fix
# generalizes that same per-level behavior to an inherited multi without
# combining candidates across levels -- confirmed matching `raku`'s own
# `B.^lookup('greet').defined` answer for this shape).
class K1 {
    multi method greet(Int $x) { "int $x" }
    multi method greet(Str $x) { "str $x" }
}
class L1 is K1 {}
ok L1.^lookup("greet").defined, '.^lookup finds an inherited multi method';

# A submethod on an ancestor is still visible to `.^lookup` on a subclass
# (raku confirms: `.^lookup` finds it even though submethods are not
# actually inherited by ordinary dispatch -- `.^lookup` is a raw MRO
# textual search, not a dispatch simulation).
class M1 { submethod boot { "M1 boot" } }
class N1 is M1 {}
ok N1.^lookup("boot").defined,
    '.^lookup finds an ancestor submethod (raku: lookup search is unfiltered by is_my)';

# `.^find_method` and `.can` are STRICTER than `.^lookup` about ancestor
# submethods -- raku confirms `N1.^find_method("boot").defined` and
# `N1.can("boot").elems` are both false/0, unlike `.^lookup` above, while the
# DECLARING class (M1) still finds it via either. This is a regression test
# for a bug this same fix introduced and then fixed: naively routing
# `classhow_find_method`'s fallback through the new MRO-walking
# `classhow_lookup` broke `.can`'s submethod-exclusion rule (`t/can-does.t`
# test 15) because `.can` on a Package receiver reuses `classhow_find_method`.
ok M1.^find_method("boot").defined,
    '.^find_method finds a submethod on its own declaring class';
nok N1.^find_method("boot").defined,
    '.^find_method does NOT find an ancestor submethod (unlike .^lookup)';
ok M1.can("boot"), '.can finds a submethod on its own declaring class';
nok N1.can("boot"), '.can does NOT find an ancestor submethod (unlike .^lookup)';

done-testing;
