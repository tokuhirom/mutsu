use Test;

# An `is rw` routine hands its caller a LOCATION. When the tail names something
# that is not one, rakudo refuses the caller's assignment with
# `Cannot modify an immutable <Type> (<value>)` -- and that refusal is what
# `Crane::Set`'s `CATCH { when X::Assignment::RO }` maps to `X::Crane::OpSet::RO`.
#
# Two shapes were missing it:
#
# * a TYPE-OBJECT rw method (`Crane::In.in(%h, @path) = $v`) fell back to the
#   legacy `$obj.name($value)` setter convention, which re-calls the method with
#   the ASSIGNED VALUE as its only argument. With a `*@steps` slurpy that is a
#   perfectly bindable call (`in(9)`), so the zero-step candidate handed `9`
#   straight back and the assignment reported success while writing nowhere.
# * `return-rw c[0]` on an immutable `List` promoted the element to a private
#   cell, so the write landed somewhere nothing else could see.

plan 16;

# --- a type-object rw method whose tail is a plain value -------------------

multi sub step(Associative:D \c, @s where { .elems > 1 }) is rw {
    return-rw step(c{@s[0]}, @s[1..*]);
}
multi sub step(Associative:D \c, @s where { .elems == 1 }) is rw {
    return-rw c{@s[0]};
}
multi sub step(\c, @s where { .elems == 0 }) is rw { return-rw c }

class Walk {
    method at(\c, *@s) is rw { return-rw step(c, @s) }
}

{
    my %h = :a({:b({:c(1)})});
    Walk.at(%h, <a b c>.Array) = 9;
    is-deeply %h, {:a({:b({:c(9)})})}, 'a writable path still writes through';
}

{
    # A `Pair` DOES `Associative`, so the descent steps into it and stops at a
    # plain value. The re-called-as-a-setter fallback made this succeed.
    my %h = :x(:y(1));
    throws-like { Walk.at(%h, <x y>.Array) = 9 }, X::Assignment::RO,
        'a rw method landing on a Pair value is refused',
        message => 'Cannot modify an immutable Int (1)';
    is-deeply %h, {:x(:y(1))}, 'and the store changed nothing';
}

{
    my %i = :a(:pair(:is(:not(:a<hash>))));
    throws-like { Walk.at(%i, <a pair is not>.Array) = {:a<Hash>} }, X::Assignment::RO,
        'a four-level colonpair chain reached through a rw method is refused',
        message => 'Cannot modify an immutable Pair (a => hash)';
    is-deeply %i, {:a(:pair(:is(:not(:a<hash>))))}, 'and the chain is intact';
}

{
    # The plain-sub spelling always refused; the two must agree.
    my %h = :x(:y(1));
    throws-like { step(%h, <x y>.Array) = 9 }, X::Assignment::RO,
        'the sub spelling refuses identically',
        message => 'Cannot modify an immutable Int (1)';
}

{
    # `.typename` is what Crane reads back out of the refusal.
    my %h = :x(:y(1));
    my $ex;
    { Walk.at(%h, <x y>.Array) = 9; CATCH { default { $ex = $_ } } }
    is $ex.typename, 'Int', 'X::Assignment::RO carries the refused value type';
}

# --- `return-rw` into an immutable List ------------------------------------

sub elem(\c) is rw { return-rw c[0] }
sub past-end(\c) is rw { return-rw c[5] }

{
    my @a = 1, 2, 3;
    elem(@a) = 9;
    is-deeply @a, [9, 2, 3], 'an Array element is still written through';
}

{
    my $l = (1, 2, 3);
    throws-like { elem($l) = 9 }, X::Assignment::RO,
        'a List element is refused',
        message => 'Cannot modify an immutable Int (1)';
    is-deeply $l, $(1, 2, 3), 'and the List is unchanged';
}

{
    my $l = (1, 2, 3);
    throws-like { past-end($l) = 9 }, X::Assignment::RO,
        'an out-of-range List index is refused, not grown',
        message => 'Cannot modify an immutable Nil value';
    is $l.elems, 3, 'and the List did not grow';
}

{
    # A `List` whose element IS a container stays writable -- that is what
    # `take-rw` builds, and the refusal must not be keyed on the list kind.
    my @spot = 10, 20, 30;
    my $l = eager gather { take-rw @spot[1] };
    elem($l) = 999;
    is @spot[1], 999, 'a List holding a live cell is still written through';
}

# --- the `:=` declaration bind keeps its own (already correct) answer ------

{
    my $l = (5, 6);
    my $x := $l[0];
    throws-like { $x = 10 }, X::AdHoc,
        'a declaration bind of a List element is still refused at the write',
        message => 'Cannot assign to an immutable value';
}

{
    my @a = 5, 6;
    my $x := @a[0];
    $x = 10;
    is @a[0], 10, 'a declaration bind of an Array element still writes through';
}

{
    # A chunked loop binding leans on the element promotion and must keep it.
    my @flat = 1, 2, 3, 4;
    my @seen;
    for @flat -> \a, \b { @seen.push: a ~ '-' ~ b }
    is-deeply @seen, ["1-2", "3-4"], 'a chunked sigilless loop bind is unaffected';
}
