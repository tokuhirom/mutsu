use Test;

# A closure literal's `params` / `param_defs` are shared (an `Arc` per
# `stmt_pool` slot) instead of being deep-cloned on every creation. Every
# closure built from the SAME literal therefore hands out the SAME signature
# object, so anything that appears to "edit" one closure's signature must
# produce a fresh signature rather than mutating the shared one.

plan 20;

# --- Two closures from one literal in a loop ---------------------------------
my @made;
for 1..3 {
    @made.push: -> $a, $b = 10, :$c = 20 { $a + $b + $c };
}
is @made.elems, 3, 'three closures created from one literal';
is @made[0](1), 31, 'first closure applies its defaults';
is @made[2](1), 31, 'third closure applies the same defaults';
is @made[1](1, 2, :c(3)), 6, 'explicit arguments still win';
is @made[0].arity, 1, 'arity is read off the shared signature';
is @made[0].count, 2, 'count is read off the shared signature';
is @made[0].signature.params.elems, 3, 'all three parameters are visible';

# --- .assuming must not disturb the literal it primed ------------------------
my $primed = @made[0].assuming(5);
is $primed(), 35, 'the primed closure binds its assumed argument';
is @made[0](1), 31, 'priming one closure leaves it callable unchanged';
is @made[1](1), 31, 'priming does not leak into a sibling from the same literal';
is @made[1].arity, 1, 'a sibling keeps its own arity after a sibling is primed';
is @made[1].signature.params.elems, 3,
    'a sibling keeps its full parameter list after a sibling is primed';

# --- Implicit placeholder signatures are also derived once per literal -------
my @ph;
for 1..3 {
    @ph.push: { $^x - $^y };
}
is @ph[0](10, 4), 6, 'a placeholder block from a loop binds both placeholders';
is @ph[2](10, 4), 6, 'the last placeholder block binds them the same way';
is @ph[1].arity, 2, 'the placeholder signature reports arity 2';

# --- A nested literal in a closure that is itself created repeatedly ---------
sub make-adder($n) { return -> $x { $x + $n } }
my @adders = (1, 2, 3).map({ make-adder($_) });
is @adders[0](10), 11, 'first adder closes over its own capture';
is @adders[2](10), 13, 'third adder closes over its own capture';
is @adders[1].signature.params.elems, 1, 'each adder reports one parameter';

# --- A signature with sub-signatures and slurpies ---------------------------
my @slurpy;
for 1..2 {
    @slurpy.push: -> $head, *@tail { "$head|" ~ @tail.join(',') };
}
is @slurpy[0](1, 2, 3), '1|2,3', 'slurpy parameter binds on the first copy';
is @slurpy[1](1, 2, 3), '1|2,3', 'slurpy parameter binds on the second copy';
