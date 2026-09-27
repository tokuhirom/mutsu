use v6;
use Test;

plan 6;

# `(elem)`/`∈` tests membership by `.WHICH`, and a List's identity is the List
# itself, not the container slot it is read through. After `@b.sort` the
# array's slots are aliasing container cells; membership used to fall through
# to a structural `eqv`, so a distinct but equal List tested as a member.
# Found through Game::Entities' t/sorting.t (`@c.grep(* ∈ @b)`).

my @b = (1..3).map: { ($_,) };
my @c = (1, 3).map: { ($_,) };

nok @c[0] ∈ @b, 'an equal but distinct List is not a member';
my $sorted = @b.sort;
nok @c[0] ∈ @b, '... still not after the array was sorted';
is-deeply @c.grep(* ∈ @b), (), 'no element of @c is a member of @b';
ok @b[0] ∈ @b, 'the List itself is a member';

my $l = (1, 2);
ok $l ∈ ($l,), 'a List held in a scalar is a member of a list holding it';
nok $[1, 2] === $[1, 2], 'two separately built Arrays are not identical';
