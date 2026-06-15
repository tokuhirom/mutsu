# Pins the aliased in-place container mutations that go through the single
# `crate::value::arc_contents_mut` choke point (Track C: concentrating the
# scattered `Arc::as_ptr as *mut` casts). These exercise the cyclic-builder
# sites (fixup_circular_*_refs), UndefineAggregate, shared `.push`, and
# `:=`-alias element writes.
use Test;

plan 12;

# --- circular array self-reference (fixup_circular_array_refs) ---
{
    my @a = 1, 2;
    @a = 42, @a;
    is @a[0], 42, 'circular array: head element';
    is @a[1][0], 42, 'circular array: one level of cycle';
    is @a[1][1][0], 42, 'circular array: deeper cycle resolves';
}

# --- circular hash self-reference (fixup_circular_hash_refs) ---
{
    my %h = a => 1;
    %h = b => 2, c => %h;
    is %h<b>, 2, 'circular hash: plain entry';
    is %h<c><b>, 2, 'circular hash: self-reference resolves';
}

# --- UndefineAggregate clears the shared container in place ---
{
    my @b = 1, 2, 3;
    undefine @b;
    is @b.elems, 0, 'undefine empties an array';
    my %m = x => 1, y => 2;
    undefine %m;
    is %m.elems, 0, 'undefine empties a hash';
}

# --- shared `.push` (hash push on an aliased Hash) ---
{
    my %p = a => 1;
    %p.push: (a => 2);
    is %p<a>.sort.join(','), '1,2', 'hash push combines values';
}

# --- `:=` alias: push/index through an alias is visible at the source ---
{
    my @c = 1, 2;
    my @d := @c;
    @d.push(3);
    is @c.join(','), '1,2,3', 'push through := alias reaches source array';
    @d[0] = 99;
    is @c[0], 99, 'index assign through := alias reaches source array';
}

{
    my %src = a => 1;
    my %al := %src;
    %al<b> = 2;
    is %src<b>, 2, 'key assign through := alias reaches source hash';
}

# --- deepmap leaf writeback into a shared array (deepmap_iterate_inner) ---
{
    my @e = [1, 2], [3, 4];
    @e.deepmap: * + 10;
    is @e[0][0], 1, 'deepmap does not mutate a non-rw source leaf';
}
