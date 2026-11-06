use Test;

# Regression pin (found via RedFactory/Red): a named parameter that only renames
# its variable (`:a($u)`, `:if-not-exists($x) where ...`) is optional like any
# named parameter; a call without it must still reach the candidate. A real
# sub-signature that needs an element (`:literal($) ($payload, *@rest)`) stays
# inapplicable when the name is absent (#16612).

plan 9;

multi k1(:a($u)) { "k1" }
multi k2($m, :a($u)) { "k2" }
multi k3(\m, :a($u) where not .defined) { "k3" }
multi k4(\m, :if-not-exists($x) where not .defined) { "k4" }
multi k5($m, :a(@u)) { "k5" }

is k1(), 'k1', 'multi, only an aliased named parameter, called bare';
is k1(:a(3)), 'k1', 'and with the name';
is k2(5), 'k2', 'positional plus an aliased named, name absent';
is k3(5), 'k3', 'aliased named with a where clause, name absent';
is k4(5), 'k4', 'long name differing from the variable, where clause, absent';
is k5(5), 'k5', 'aliased array variable';

class H {
    multi method c1(\m, :if-not-exists($u) where not .defined) { "c1" }
    multi method c2(\m, :a($u)) { "c2" }
}
is H.new.c1(5), 'c1', 'multi method with an aliased named + where, name absent';
is H.new.c2(5), 'c2', 'multi method with an aliased named, name absent';

multi sub real(:literal($) ($payload, *@rest)) { "literal" }
multi sub real(:op(@) ($head, *@tail)) { "op" }
is real(:op([1, 2])), 'op', 'a real sub-signature on an absent name still is not applicable';
