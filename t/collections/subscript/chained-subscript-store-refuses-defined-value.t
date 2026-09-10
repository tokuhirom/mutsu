use Test;

plan 23;

# A chained subscript store autovivifies only through an UNDEFINED slot.
# A slot already holding a defined value with no writable container behind it
# is refused, exactly as rakudo refuses it -- mutsu used to treat "not a
# container" as "vivify me" and silently clobbered the value (or, through a
# hash root, dropped the write entirely).

# --- positional outer subscript: X::Assignment::RO naming the value ---------

{
    my @a = 1, 2, 3;
    throws-like { @a[1][0] = 9 }, X::Assignment::RO,
        message => 'Cannot modify an immutable Int (2)',
        'positional store through a defined Int element is refused';
    is-deeply @a[1], 2, 'the refused store left the element alone';
}

{
    my @a = "x", 2;
    throws-like { @a[0][0] = 9 }, X::Assignment::RO,
        message => 'Cannot modify an immutable Str (x)',
        'a Str element is refused too';
}

{
    my @a = 1.5, 2;
    throws-like { @a[0][0] = 9 }, X::Assignment::RO,
        message => 'Cannot modify an immutable Rat (1.5)',
        'a Rat element is refused, and the message names the gist';
}

{
    my @a = (a => 1), 3;
    throws-like { @a[0][0] = 9 }, X::Assignment::RO,
        message => 'Cannot modify an immutable Pair (a => 1)',
        'a Pair element is refused';
}

# --- the same rule one level deeper ----------------------------------------

{
    my @a = 1, 2;
    throws-like { @a[0][0][0] = 9 }, X::Assignment::RO,
        message => 'Cannot modify an immutable Int (1)',
        'the 3+ level walk refuses a defined value at the first step';
    is @a[0], 1, 'and leaves it alone';
}

# --- through a hash root ----------------------------------------------------

{
    my %h = a => 1;
    throws-like { %h<a>[0] = 9 }, X::Assignment::RO,
        message => 'Cannot modify an immutable Int (1)',
        'a hash entry holding a defined value is refused, not silently dropped';
    is %h<a>, 1, 'the entry is unchanged';
}

# --- associative outer subscript: X::AdHoc, the AT-KEY wording -------------

{
    my @a = 1, 2, 3;
    throws-like { @a[1]<k> = 9 }, X::AdHoc,
        message => 'Type Int does not support associative indexing.',
        'associative store through a defined Int element is refused';
    is @a[1], 2, 'the element is unchanged';
}

# --- what still autovivifies -----------------------------------------------

{
    my @a;
    @a[1][0] = 9;
    is-deeply @a[1], $[9], 'an absent slot still autovivifies';
    nok @a[0].defined, 'the gap it grew over stays undefined';
}

{
    my @a = Any, 2;
    @a[0][0] = 9;
    is @a[0][0], 9, 'a type-object slot still autovivifies';
    is @a[1], 2, 'the sibling is untouched';
}

{
    my @a = Nil, 2;
    @a[0][0] = 9;
    is @a[0][0], 9, 'a Nil slot still autovivifies';
}

{
    my @a = [1, 2], 3;
    @a[0][0] = 9;
    is @a[0][0], 9, 'a mutable Array element is still written through';
    is @a[0][1], 2, 'and keeps its other elements';
}

{
    my %h;
    %h<a><b> = 1;
    is %h<a><b>, 1, 'a hash chain still autovivifies through an absent key';
}

{
    my %h = a => { b => 1 };
    %h<a><b> = 2;
    is %h<a><b>, 2, 'an existing nested hash is still written through';
}

{
    my @a = 1, 2;
    my @inner = 7, 8;
    @a[0] := @inner;
    @a[0][1] = 9;
    is @inner[1], 9, 'a :=-bound element cell is still descended, not refused';
}

{
    my @a;
    @a[0] = [];
    @a[0][0][0] = 5;
    is @a[0][0][0], 5, 'the deep walk still vivifies through an empty Array';
}

# A `List` slot is NOT refused on its kind. Rakudo's refusal for one is decided
# by the element the next subscript reaches, and a List whose elements ARE
# containers is written through -- which is exactly what `take-rw` builds
# (`t/take-rw-shared-cell.t`). Refusing the kind regressed that file.
{
    my @spot = 10, 20, 30;
    my @n;
    @n[0] = eager gather { take-rw @spot[1] };
    @n[0][0] = 999;
    is @spot[1], 999, 'a List holding a live container is still written through';
}
