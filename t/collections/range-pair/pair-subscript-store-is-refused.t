use Test;

# A `Pair` DOES `Associative`, so rakudo descends into it on a nested store and
# refuses at the VALUE the next subscript reaches. mutsu refused on the SLOT's
# type with `X::AdHoc` "Type Pair does not support associative indexing" -- the
# class rakudo raises for a genuinely non-Associative slot -- and then, on the
# deeper walk, overwrote the Pair with a fresh Hash and reported success.
#
# Both mattered to Crane, whose `CATCH { when X::Assignment::RO }` maps the
# refusal to `X::Crane::OpSet::RO`, and whose fixtures are colonpair chains
# (`:a(:pair(:is(:not(:a<hash>))))`).

plan 10;

{
    my %h = :x(:y(1));
    throws-like { %h<x><y> = 2 }, X::Assignment::RO,
        'a nested store into a Pair is refused',
        message => /'Cannot modify an immutable Int (1)'/;
    is-deeply %h, {:x(:y(1))}, 'and the refused store changed nothing';
}

{
    # A REFUSED store must not destroy the variable. mutsu invalidates the
    # target's local slot before walking and refreshes it from `env` when it
    # finishes; returning early left the caller's `%h` reading `Nil`.
    # An `Int` slot is not Associative at all, so this one is rakudo's
    # `Any.AT-KEY` X::AdHoc rather than an RO refusal.
    my %g = :x(1);
    throws-like { %g<x><y> = 2 }, X::AdHoc,
        'a nested store through a defined non-container is refused',
        message => /'does not support associative indexing'/;
    is-deeply %g, {:x(1)}, 'and the variable survives';
}

{
    my %i = :a(:pair(:is(:not(:a<hash>))));
    throws-like { %i<a><pair><is><not> = {:a<Hash>} }, X::Assignment::RO,
        'a four-level colonpair chain is refused';
    is-deeply %i, {:a(:pair(:is(:not(:a<hash>))))}, 'and the chain is intact';
}

{
    # Reading through a Pair still works, and so does storing where there IS a
    # container.
    my %h = :x(:y(1));
    is %h<x><y>, 1, 'reading through a Pair still works';
    %h<x> = 5;
    is-deeply %h, {:x(5)}, 'replacing the Pair itself still works';
}

{
    my $p = (a => 1);
    throws-like { $p<a> = 2 }, X::Assignment::RO,
        'a direct Pair element store is refused (unchanged)';
}

{
    # Autovivification through a genuinely UNDEFINED slot is untouched.
    my %v;
    %v<a><b> = 7;
    is-deeply %v, {:a({:b(7)})}, 'an undefined slot still autovivifies';
}
