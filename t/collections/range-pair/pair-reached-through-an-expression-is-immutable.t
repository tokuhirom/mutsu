use Test;

# A `Pair` DOES `Associative`, so `<k>` descends into it -- but it is immutable,
# and rakudo refuses both a store and a removal. mutsu applied that rule only to
# a subscript chain written directly on a VARIABLE (`%h<x><y> = 2`); reached
# through an EXPRESSION it invented a container instead, reported success, and
# in the accessor case wrote a detached `Any` back over the caller's variable.
#
# Both spellings are what Crane's non-in-place operations are built on
# (`Crane::At.at($root, @path){$step} = $value`, `... {$step}:delete`), and
# `Crane.replace` / `Crane.remove` map the refusals to `X::Crane::Replace::RO` /
# `X::Crane::Remove::RO`.

plan 14;

sub id($x) is rw { return-rw $x }

class Walk {
    method at($root, *@steps) is rw {
        my $sel := $root;
        for @steps -> $s { $sel := $sel{$s} }
        return-rw $sel;
    }
}

# --- store through a sub-call result ---------------------------------------

{
    my $p = (c => True);
    throws-like { id($p){'c'} = 9 }, X::Assignment::RO,
        'a store through a call result that is a Pair is refused',
        message => 'Cannot modify an immutable Bool (True)';
    is-deeply $p, (c => True), 'and the Pair is unchanged';
}

{
    my $p = (c => True);
    throws-like { id($p){'zz'} = 9 }, X::Assignment::RO,
        'a key the one-entry Pair does not hold is refused as Nil',
        message => 'Cannot modify an immutable Nil value';
}

{
    # A real Hash behind the same call shape still stores.
    my $h = {c => True};
    id($h){'c'} = 9;
    is-deeply $h, ${:c(9)}, 'a Hash call result still stores';
}

# --- store through a package-level path accessor ---------------------------

{
    my %i = :a(:b(:c(True)));
    my $root = %i.deepmap({ .clone });
    throws-like { Walk.at($root, 'a', 'b'){'c'} = {:d(True)} }, X::Assignment::RO,
        'a store through a path accessor landing on a Pair is refused',
        message => 'Cannot modify an immutable Bool (True)';
    is-deeply $root, ${:a(:b(:c(True)))}, 'and the refused store left the copy alone';
}

{
    my %i = :a({:b({:c(True)})});
    my $root = %i.deepmap({ .clone });
    Walk.at($root, 'a', 'b'){'c'} = {:d(True)};
    is-deeply $root<a><b><c>, {:d(True)}, 'the Hash-leaf spelling still stores';
}

# --- removal ----------------------------------------------------------------

{
    my $p = (c => True);
    throws-like { $p<c>:delete }, X::AdHoc,
        'a removal from a Pair is refused',
        message => 'Can not remove values from a Pair';
    is-deeply $p, (c => True), 'and the Pair is unchanged';
}

{
    my @a = (a => 1), 2;
    throws-like { @a[0]<a>:delete }, X::AdHoc,
        'a removal from a Pair element is refused too',
        message => 'Can not remove values from a Pair';
}

{
    my %h = :c(True);
    my $gone = %h<c>:delete;
    is $gone, True, 'a real Hash still deletes';
    is-deeply %h, {}, 'and loses the entry';
}

# --- `deepmap` hands its block a value, not a container of a container ------

{
    # `my $x := %h<a>` promotes the element to a cell. `deepmap` boxed that cell
    # a second time, so the block's `$_` was a container around a container and
    # every method dispatched on it missed: this died with
    # "No such method 'clone' for invocant of type 'Pair'".
    my %h = :a(:b(1));
    my $x := %h<a>;
    is-deeply %h.deepmap({ .clone }), {:a(:b(1))},
        'deepmap decontainerizes a promoted leaf before calling the block';
}

{
    # A mutating block still writes back into the source structure.
    my @a = 1, 2, 3;
    @a.deepmap({ $_++ });
    is-deeply @a, [2, 3, 4], 'a mutating deepmap block still writes through';
}
