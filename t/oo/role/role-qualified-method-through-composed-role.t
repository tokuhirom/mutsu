use Test;

# From the Map::Ordered distribution: Map::Agnostic does Hash::Agnostic and
# calls `self.Hash::Agnostic::STORE(...)`. A role reachable only through
# another role must satisfy the qualifier check.

plan 4;

role A { method STORE(*@v) { "A::STORE" } }
role B does A {
    method STORE(\it, :$INITIALIZE!) { self.A::STORE(it, :INITIALIZE) }
}

class D does B { }
is D.new.STORE(1, :INITIALIZE), "A::STORE", "class consuming a role that does a role";

role C does B { }
is C.new.STORE(1, :INITIALIZE), "A::STORE", "punned role instance";

my %m is B = 1, 2;
is %m.STORE(1, :INITIALIZE), "A::STORE", "role mixed into a Hash via `is`";

role Unrelated { }
class E does Unrelated { }
dies-ok { E.new.A::STORE(1) }, "an unrelated role is still rejected";

done-testing;
