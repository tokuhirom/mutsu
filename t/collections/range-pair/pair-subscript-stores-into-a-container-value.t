use Test;

# `Pair` DOES `Associative`, and its `AT-KEY` hands back `.value` ITSELF -- a
# Pair gives its value no `Scalar` of its own. So `$p<key> = ...` is not an
# element store at all: it is a store to the value the subscript reaches, which
# succeeds exactly when that value IS a container (`Array`/`Hash` take it, the
# way `@a = LIST` does) and is `X::Assignment::RO` for everything else.
#
# mutsu had only the refusing half (#8275), applied to every value alike, so
# every one of the spellings below died where rakudo stores -- and the three
# descending spellings silently wrote nowhere while reporting success.
#
# This is the shape `Crane` builds a nested in-place operation on:
# `Crane.add(%i, :path<a b c>, :value(...), :in-place)` over the colonpair
# fixture `:a({:b(:c([...]))})` reaches `Crane::At.at($root, @path){$step} =
# $value`, whose `$step` lands on the Pair `c => [...]`.

plan 23;

sub id($x) is rw { return-rw $x }

class Walk {
    method at($root, *@steps) is rw {
        my $sel := $root;
        for @steps -> $s { $sel := $sel{$s} }
        return-rw $sel;
    }
}

# --- the store reaches the container the Pair holds -------------------------

{
    my $p = (c => [1, 2]);
    $p<c> = [3, 4];
    is-deeply $p, (c => [3, 4]), 'a store through a Pair key reaches its Array';
}

{
    # `Array.STORE` takes a non-list rvalue as a one-element list.
    my $p = (c => [1, 2]);
    $p<c> = 5;
    is-deeply $p, (c => [5]), 'a scalar rvalue STOREs as a one-element Array';
}

{
    my $p = (c => {:a(1)});
    $p<c> = {:b(2)};
    is-deeply $p, (c => {:b(2)}), 'a Hash value takes the store too';
}

{
    # The container is shared, not copied: the store must be visible through
    # every other alias of it, which is the whole point of not replacing it.
    my @x = 1, 2;
    my $p = (c => @x);
    $p<c> = [7, 8];
    is-deeply @x, [7, 8], 'the store writes through to the source container';
}

# --- and the same store reached through every other spelling ----------------

{
    my %h = :x(:y([1, 2]));
    %h<x><y> = [3, 4];
    is-deeply %h, {:x(:y([3, 4]))}, 'a two-level chain through a Hash';
}

{
    my @a = ((c => [1, 2]),);
    @a[0]<c> = [3, 4];
    is-deeply @a, [(c => [3, 4]),], 'a two-level chain through an Array';
}

{
    my %h = :x({:y(:z([1, 2]))});
    %h<x><y><z> = [3, 4];
    is-deeply %h<x><y>, (z => [3, 4]), 'a three-level chain';
}

{
    my $p = (c => [1, 2]);
    id($p)<c> = [3, 4];
    is-deeply $p, (c => [3, 4]), 'a call result';
}

{
    my %i = :a({:b(:c([1, 2]))});
    my $root := %i;
    Walk.at($root, 'a', 'b')<c> = [3, 4];
    is-deeply %i<a><b>, (c => [3, 4]), 'a package-level path accessor';
}

# --- descending THROUGH the Pair into its container -------------------------

{
    my $p = (c => [1, 2]);
    $p<c>[0] = 9;
    is-deeply $p, (c => [9, 2]), 'an element store through a Pair key';
}

{
    my $p = (c => {:a(1)});
    $p<c><a> = 7;
    is-deeply $p, (c => {:a(7)}), 'an associative element store through a Pair key';
}

{
    my %h = :x(:y([1, 2]));
    %h<x><y>[0] = 9;
    is-deeply %h, {:x(:y([9, 2]))}, 'a descent two levels in';
}

# --- everything else is still immutable -------------------------------------

{
    my $p = (c => 1);
    throws-like { $p<c> = 9 }, X::Assignment::RO,
        'a plain value behind the key is still refused',
        message => 'Cannot modify an immutable Int (1)';
    is-deeply $p, (c => 1), 'and the Pair is unchanged';
}

{
    my $p = (c => [1, 2]);
    throws-like { $p<zz> = 9 }, X::Assignment::RO,
        'a key the Pair does not hold is still refused',
        message => 'Cannot modify an immutable Nil value';
    is-deeply $p, (c => [1, 2]), 'and the Array is unchanged';
}

{
    # An immutable `List` is not a container: rakudo refuses here too.
    my $p = (c => (1, 2));
    dies-ok { $p<c> = [3, 4] }, 'an immutable List behind the key is refused';
}

{
    my @a = ((c => 1),);
    throws-like { @a[0][0] = 9 }, X::Assignment::RO,
        'a POSITIONAL subscript on a Pair names the Pair itself',
        message => 'Cannot modify an immutable Pair (c => 1)';
}

{
    my %i = :a(:b(:c(True)));
    my $root = %i.deepmap({ .clone });
    throws-like { Walk.at($root, 'a', 'b'){'c'} = {:d(True)} }, X::Assignment::RO,
        'the accessor spelling still refuses a plain value',
        message => 'Cannot modify an immutable Bool (True)';
    is-deeply $root, ${:a(:b(:c(True)))}, 'and left the copy alone';
}

# --- neighbours that must not have moved ------------------------------------

{
    my $p = (c => [1, 2]);
    throws-like { $p<c> := [3, 4] }, X::Bind,
        'binding into a Pair is still rejected outright';
}

{
    my %h = :c(True);
    my $gone = %h<c>:delete;
    is $gone, True, 'a real Hash still deletes';
}

{
    my $p = (c => [1, 2]);
    throws-like { $p<c>:delete }, X::AdHoc,
        'a removal from a Pair is still refused',
        message => 'Can not remove values from a Pair';
}
