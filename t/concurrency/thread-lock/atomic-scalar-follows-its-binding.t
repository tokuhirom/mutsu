use Test;

plan 8;

# An atomic scalar's value belongs to its BINDING, not to its bare name. A
# same-named lexical declared anywhere else in the program is a different
# variable and must not disturb the counter.

sub other-name() { my $j = -1; $j++; $j }
sub same-name-decl-only() { my $i; $i }
sub same-name-assign() { my $i = -1; $i }

my atomicint $i = 0;
is ⚛$i, 0, 'atomicint starts at its initializer';
is ++⚛$i, 1, 'pre-increment counts';

other-name();
is ⚛$i, 1, 'an unrelated differently-named lexical leaves the counter alone';

same-name-decl-only();
is ⚛$i, 1, 'a same-named bare declaration elsewhere leaves the counter alone';

same-name-assign();
is ⚛$i, 1, 'a same-named assigned declaration elsewhere leaves the counter alone';

is ++⚛$i, 2, 'the counter carries on from where it was';

# The same binding, bumped from a closure that a routine stored -- the shape a
# Cro `route { my atomicint $i; get -> { ++⚛$i } }` request counter has.
{
    my @handlers;
    sub register(&h) { @handlers.push: &h }

    sub make-counter() {
        my atomicint $c = 0;
        register { ++⚛$c };
    }
    make-counter();

    my @seen = @handlers[0](), @handlers[0](), await(start { @handlers[0]() });
    is @seen, [1, 2, 3], 'a stored closure accumulates into one atomic binding';
}

# ...and keeps accumulating while a same-named lexical is declared between the
# calls. This is the Cro `route { my atomicint $i; get -> { ++⚛$i } }` counter,
# whose route block's `$i` collided with a `my $i` in the router's own library.
{
    my @handlers;
    sub register2(&h) { @handlers.push: &h }
    sub scope(&blk) { blk() }
    sub collide() { my $c = -1; $c }

    scope {
        my atomicint $c = 0;
        register2 { ++⚛$c };
    }

    my @seen = @handlers[0]();
    collide();
    @seen.push: @handlers[0]();
    collide();
    @seen.push: await(start { @handlers[0]() });
    is @seen, [1, 2, 3], 'a same-named lexical in another routine does not reset it';
}
