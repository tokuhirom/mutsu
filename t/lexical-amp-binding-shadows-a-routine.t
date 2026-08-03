use v6;
use Test;

# A `my &f` binding is an ordinary lexical and shadows any package/registry
# routine of the same name, so a BARE-NAME call must reach the binding -- just
# as `&f(...)` does. mutsu compiled the bare name to a by-name `CallFunc`, which
# resolves against the routine registry, so an outer `sub f` won.
#
# The second half: a `&`-sigil binding may live only in a frame's LOCAL SLOT and
# never in env -- that is how a `&`-sigil named parameter binds -- and
# `CallOnCodeVar` consulted only env, so `&cb()` inside `sub f(:&cb)` answered
# "Unknown function: cb" while `&cb.defined` worked.

plan 10;

my @log;

sub tester($a, $b) { @log.push: "outer $a $b"; "outer" }

is tester(1, 2), 'outer', 'the outer routine is reachable before the binding';

{
    my &tester = -> $x { @log.push: "lexical $x"; "lexical" };
    is tester(9), 'lexical', 'a my &f binding shadows an outer sub of the same name';
    is &tester(8), 'lexical', 'and the explicit &f form agrees';
}

is tester(3, 4), 'outer', 'the outer routine is reachable again after the block';
is-deeply @log, ['outer 1 2', 'lexical 9', 'lexical 8', 'outer 3 4'],
    'each call reached the routine Raku says it should';

# A binding installed inside a block that runs as a *callable* -- the shape
# roast/S04-statements/given.t uses (`my &test-given = produce-tester $cond`).
sub run(&b) { b() }
run {
    my sub helper($a, $b) { "helper $a $b" }
    is helper(1, 2), 'helper 1 2', 'a my sub in a callable block is callable';
}
run {
    my &helper = -> $x { "bound $x" };
    is helper(5), 'bound 5', 'and a later block may bind the same name to something else';
}

# `&`-sigil named parameters: the binding is slot-only.
sub takes-cb(:&cb) { &cb.defined ?? &cb() !! 'unpassed' }
is takes-cb(:cb({ 'called' })), 'called', 'calling a :&cb named parameter as &cb() works';
is takes-cb(), 'unpassed', 'and an unpassed one is still undefined';

# The bare-name form of the same parameter.
sub takes-cb-bare(:&cb) { &cb.defined ?? cb() !! 'unpassed' }
is takes-cb-bare(:cb({ 'bare' })), 'bare', 'and so does the bare-name form';
