use Test;

# `splice` validates its offset and size BEFORE it writes: an offset outside
# `0..elems` and a negative size are `X::OutOfRange` in rakudo, not a splice
# clamped to the end. The lvalue path (`@a.splice(...)` on a named variable)
# always did this; the by-value path -- a literal, a function result, an
# element read, anything reached through `return-rw` -- did not, so
# `[1,2,3].splice(5, 0, 'x')` silently appended and `Crane`'s
# `CATCH { when X::OutOfRange }` never fired.
#
# Verified against rakudo v2026.07: every `throws-like` below is that
# interpreter's own answer, message included.

plan 13;

sub at(\c) is rw { my $root := c; return-rw $root }

# --- a literal invocant (no variable anywhere) ---------------------------

throws-like(
    { [1, 2, 3].splice(5, 0, 'x') },
    X::OutOfRange,
    :message(/'Offset argument to splice out of range. Is: 5, should be in 0..3'/),
    'a literal array refuses an offset past the end',
);

throws-like(
    { [1, 2, 3].splice(*-5, 0, 'x') },
    X::OutOfRange,
    :message(/'Offset argument to splice out of range. Is: -2, should be in 0..3'/),
    'a from-the-end offset is resolved before the bounds check',
);

throws-like(
    { [1, 2, 3].splice(1, -1) },
    X::OutOfRange,
    :message(/'Size argument to splice out of range. Is: -1, should be in 0..^2'/),
    'a literal array refuses a negative size',
);

# --- a by-value invocant reached through a call --------------------------

{
    my @a;
    throws-like(
        { at(@a).splice(1, 0, 'x') },
        X::OutOfRange,
        :message(/'Offset argument to splice out of range. Is: 1, should be in 0..0'/),
        'an empty array reached through return-rw refuses offset 1',
    );
    is-deeply(@a, [], 'and the refused splice wrote nothing');
}

{
    my %doc = :a([]);
    throws-like(
        { at(%doc<a>).splice(*-1, 0, 'x') },
        X::OutOfRange,
        :message(/'Offset argument to splice out of range. Is: -1, should be in 0..0'/),
        'an empty hash element reached through return-rw refuses *-1',
    );
    is-deeply(%doc, {:a([])}, 'and the refused splice wrote nothing');
}

# --- the lvalue path still agrees ---------------------------------------

{
    my @a;
    throws-like(
        { @a.splice(1, 0, 'x') },
        X::OutOfRange,
        :message(/'Offset argument to splice out of range. Is: 1, should be in 0..0'/),
        'the named-variable path refuses the same offset',
    );
}

# --- in-range splices are untouched --------------------------------------

{
    my @a = 1, 2, 3;
    my @removed = at(@a).splice(1, 1, 'x');
    is-deeply(@a, [1, 'x', 3], 'an in-range splice through return-rw still writes');
    is-deeply(@removed, [2], 'and returns what it removed');
}

{
    my @a = 1, 2, 3;
    at(@a).splice(*-0, 0, 'x');
    is-deeply(@a, [1, 2, 3, 'x'], 'offset == elems is in range (append)');
}

{
    my @a;
    at(@a).splice(0, 0, 'x');
    is-deeply(@a, ['x'], 'offset 0 on an empty array is in range');
}

# A non-Int, non-Whatever, non-Callable offset matches no candidate at all --
# that stays X::Multi::NoMatch and must not be reported as a range error.
throws-like(
    { [1, 2, 3].splice('x') },
    X::Multi::NoMatch,
    'a Str offset is still a no-candidate error, not a range error',
);

# done
