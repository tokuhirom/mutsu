use v6;
use Test;

# #10317: a hole in an `is default(...)` array -- a slot `:delete`d from the
# middle, or a gap an out-of-range store grew -- reads as the container's
# default in the whole-array views, exactly as `@a[$i]` already did. The slot
# keeps holding the `Any` hole marker (so `:exists`, `.List` and the trailing
# hole trim still tell a hole from an explicit element); the substitution
# happens on the way out (`ArrayData::items_with_default`).

plan 31;

# --- a :delete'd slot ---
{
    my @m is default(7) = 1, 2, 3;
    @m[0]:delete;
    is @m[0], 7, 'a direct element read gives the default (unchanged)';
    is @m.raku, '[7, 2, 3]', '.raku renders the hole as the default';
    is @m.gist, '[7 2 3]', '.gist renders the hole as the default';
    is @m.Str, '7 2 3', '.Str renders the hole as the default';
    is @m.join(','), '7,2,3', '.join renders the hole as the default';
    is @m.list.raku, '[7, 2, 3]', '.list reads the hole as the default';
    is (~@m), '7 2 3', 'prefix ~ stringifies the hole as the default';
    is @m.elems, 3, '.elems still counts the deleted slot';
    ok @m[0]:!exists, ':exists still reports the deleted slot as missing';
    ok @m[1]:exists, ':exists is still True for an explicit element';
}

# --- what must NOT change: the hole is still a hole underneath ---
{
    my @m is default(7) = 1, 2, 3;
    @m[1]:delete;
    is @m.List.raku, '(1, Nil, 3)', '.List keeps the hole as Nil (Rakudo)';
    is @m.Slip.raku, 'slip(1, 7, 3)', '.Slip substitutes the default';
    ok @m[1]:!exists, 'the deleted slot is still absent';
}

# --- writing the default value explicitly is data, not a hole ---
{
    my @m is default(7) = 1, 2, 3;
    @m[1]:delete;
    @m[1] = 7;
    ok @m[1]:exists, 'an explicitly assigned default value exists';
    is @m.raku, '[1, 7, 3]', 'the explicit element renders as itself';
    @m.push(7);
    ok @m[3]:exists, 'a pushed default value exists';
    is @m.raku, '[1, 7, 3, 7]', 'a pushed default value renders as itself';
}

# --- a gap grown by an out-of-range store ---
{
    my @g is default(7) = 1;
    @g[3] = 4;
    is @g.raku, '[1, 7, 7, 4]', '.raku renders a grown gap as the default';
    is @g.gist, '[1 7 7 4]', '.gist renders a grown gap as the default';
    is @g.Str, '1 7 7 4', '.Str renders a grown gap as the default';
    is @g.join(','), '1,7,7,4', '.join renders a grown gap as the default';
    is @g.List.raku, '(1, Nil, Nil, 4)', '.List keeps a grown gap as Nil';
}

# --- a typed array renders through its own `Array[T].new(...)` path ---
{
    my Int @t is default(7) = 1, 2, 3;
    @t[0]:delete;
    is @t.raku, 'Array[Int].new(7, 2, 3)', 'typed array: .raku renders the hole as the default';
    is @t.gist, '[7 2 3]', 'typed array: .gist renders the hole as the default';
}

# --- state array, the other spelling in the ticket ---
{
    sub f() {
        state @q is default(7) = 1, 2;
        @q[0]:delete;
        @q.raku;
    }
    is f(), '[7, 2]', 'state @q is default(7): the hole renders as the default';
}

# --- an array without an `is default` value is untouched ---
{
    my @p = 1, 2, 3;
    @p[0]:delete;
    is @p.raku, '[Any, 2, 3]', 'no is default: the hole renders as Any';
    is @p.gist, '[(Any) 2 3]', 'no is default: .gist shows the type object';
}

# --- a trailing delete still shrinks the array ---
{
    my @e is default(7) = 1, 2, 3;
    @e[2]:delete;
    is @e.elems, 2, 'a trailing delete shrinks the array';
    is @e.raku, '[1, 2]', '... and the trimmed slot is not rendered';
}

# --- nested: the inner array's own default applies inside an outer one ---
{
    my @inner is default(7) = 1, 2, 3;
    @inner[0]:delete;
    my @outer = @inner, 9;
    is @outer[1], 9, 'an array holding a holey array still indexes';
    is @inner.raku, '[7, 2, 3]', 'the inner array keeps rendering its default';
}
