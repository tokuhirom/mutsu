use Test;

plan 17;

# `splice` is declared on Array only. A List or Range invocant therefore
# resolves NO candidate at all and raises X::Multi::NoMatch -- it never reaches
# an immutability check, unlike the six mutators rakudo does define on them.
# mutsu used to route all seven through the same X::Immutable arm, which is the
# wrong class AND the wrong message; Crane's CATCH maps only rakudo's spelling
# to X::Crane::Add::RO.

my $no-candidates = 'Routine does not have any candidates.  Is only the proto defined?';

# --- splice: no candidate --------------------------------------------------

{
    my $l = (1, 2, 3);
    throws-like { $l.splice(0, 1) }, X::Multi::NoMatch,
        message => "Cannot resolve caller splice(List:D, Int:D, Int:D); $no-candidates",
        'splice on a List resolves no candidate';
    is $l.elems, 3, 'the List is untouched';
}

{
    my @a := (1, 2, 3);
    throws-like { @a.splice(0, 1) }, X::Multi::NoMatch,
        message => "Cannot resolve caller splice(List:D, Int:D, Int:D); $no-candidates",
        'the same through an @-sigiled bind to a List';
}

{
    my $r = 1 .. 3;
    throws-like { $r.splice(0, 1) }, X::Multi::NoMatch,
        message => "Cannot resolve caller splice(Range:D, Int:D, Int:D); $no-candidates",
        'splice on a Range names Range in the signature';
}

# The rendered signature is the invocant plus every argument, so it tracks both
# the arity and the argument types.
{
    my $l = (1, 2, 3);
    throws-like { $l.splice() }, X::Multi::NoMatch,
        message => "Cannot resolve caller splice(List:D); $no-candidates",
        'no arguments renders the invocant alone';
    throws-like { $l.splice(0) }, X::Multi::NoMatch,
        message => "Cannot resolve caller splice(List:D, Int:D); $no-candidates",
        'one argument';
    throws-like { $l.splice(0, 1, 9) }, X::Multi::NoMatch,
        message => "Cannot resolve caller splice(List:D, Int:D, Int:D, Int:D); $no-candidates",
        'a replacement argument is listed too';
    throws-like { $l.splice("x") }, X::Multi::NoMatch,
        message => "Cannot resolve caller splice(List:D, Str:D); $no-candidates",
        'the argument type is the real one, not a coerced Int';
}

# --- the six that DO exist on a List still throw X::Immutable ---------------

{
    my $l = (1, 2, 3);
    for <push pop shift unshift append prepend> -> $m {
        throws-like { $l."$m"(9) }, X::Immutable,
            message => "Cannot call '$m' on an immutable 'List'",
            "$m on a List is still X::Immutable";
    }
}

{
    my $r = 1 .. 3;
    throws-like { $r.push(9) }, X::Immutable,
        message => "Cannot call 'push' on an immutable 'Range'",
        'push on a Range is still X::Immutable';
}

# --- splice on a real Array is untouched -----------------------------------

{
    my @a = 1, 2, 3;
    @a.splice(1, 1, 9);
    is-deeply @a, [1, 9, 3], 'splice on a mutable Array still works';
}

{
    my @a = 1, 2, 3;
    @a.splice(*-0, 0, 9);
    is-deeply @a, [1, 2, 3, 9], 'a Whatever-closure start still appends at the end';
}
