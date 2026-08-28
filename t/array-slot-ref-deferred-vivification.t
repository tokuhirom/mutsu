use Test;

# `my $r := @a[5]` must NOT grow `@a`. Rakudo defers the vivification to the
# first write through the binding, and only then fills the gap. mutsu used to
# grow the array at bind time (`array_slot_ref` pushed holes unconditionally),
# so `@a.elems` reported 6 where rakudo reports 2.

plan 23;

# --- bind without write: no growth, no pollution -------------------------
{
    my @a = 1, 2;
    my $r := @a[5];
    is @a.elems, 2, 'binding an out-of-range element does not grow the array';
    is @a.raku, '[1, 2]', 'the array is untouched after the bind';
    is $r.raku, 'Any', 'reading the unwritten binding yields Any';
    is @a.elems, 2, 'reading through the binding still does not grow the array';
    nok @a[5]:exists, ':exists is not polluted by the deferred bind';
    is @a.end, 1, '.end is not polluted either';
}

# --- bind then write: the gap is filled on the write ---------------------
{
    my @a = 1, 2;
    my $r := @a[5];
    $r = 9;
    is @a.raku, '[1, 2, Any, Any, Any, 9]', 'the write fills the gap and stores';
    is $r, 9, 'the bound variable reads back its own write';
    $r = 10;
    is @a.raku, '[1, 2, Any, Any, Any, 10]', 'a second write goes through too';
    is @a[5], 10, 'the array element sees the second write';
    @a[5] = 11;
    is $r, 11, 'the binding is a live alias after the first write';
}

# --- the hole value honours the element type / `is default(...)` ---------
{
    my Int @i = 1, 2;
    my $r := @i[5];
    is $r.raku, 'Int', 'an unwritten bind on a typed array reads the type object';
    $r = 9;
    is @i.raku, 'Array[Int].new(1, 2, Int, Int, Int, 9)',
        'the gap is filled with the declared element type';
}
{
    my @d is default(42) = 1, 2;
    my $s := @d[5];
    is $s.raku, '42', 'an unwritten bind reads the `is default` value';
    $s = 7;
    is @d.raku, '[1, 2, 42, 42, 42, 7]', 'the gap is filled with the default value';
}

# --- deep / nested paths -------------------------------------------------
{
    my @m = [1, 2], [3, 4];
    my $u := @m[0][7];
    is @m.raku, '[[1, 2], [3, 4]]', 'a nested out-of-range bind grows nothing';
    $u = 'z';
    is @m.raku, '[[1, 2, Any, Any, Any, Any, Any, "z"], [3, 4]]',
        'the nested write fills the inner gap only';
}
{
    my @n = 1, 2;
    my $t := @n[5];
    $t = [10, 20];
    is @n.raku, '[1, 2, Any, Any, Any, [10, 20]]',
        'a container value written through the deferred bind lands at the index';
}

# --- an independent write does NOT retro-connect the bind ----------------
# Same rule the hash side has had since t/phantom-entry-bind.t: rakudo only
# connects a deferred bind through a write made THROUGH the bound variable.
{
    my @a = 1, 2;
    my $t := @a[5];
    @a[5] = 7;
    is $t.raku, 'Any', 'an independent element write does not retro-bind';
    is @a.raku, '[1, 2, Any, Any, Any, 7]', 'the independent write still lands';
}

# --- %h-side non-regression ---------------------------------------------
{
    my %h = a => 1;
    my $v := %h<zz>;
    is %h.raku, '{:a(1)}', 'a missing-key bind still creates nothing';
    nok %h<zz>:exists, 'the hash :exists is still not polluted';
    $v = 5;
    is %h.raku, '{:a(1), :zz(5)}', 'the hash write still vivifies the key';
}
