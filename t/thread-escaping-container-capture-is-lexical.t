use Test;

plan 5;

# ADR-0055 residue, closed by ADR-0068: a container lexical captured by a
# thread-escaping closure used to be excluded from the declaration-site
# container cell (`needs_cell_unvouched_containers`), which left it on the
# name-keyed `__mutsu_atomic_arr::` / `__mutsu_atomic_hash::` lane. That lane
# is keyed by NAME, so a same-named `my @a` in whatever frame happened to be
# calling the closure hijacked the capture. The exclusion existed because an
# element store through the cell was unsynchronized; ADR-0068's guarded funnels
# made the celled path synchronized, so the exclusion is retired and these
# captures resolve lexically like every other closure capture.

{
    my @a = 1, 2;
    @a.push(3);
    my $f = -> { start { @a.elems } };
    sub collide-a() { my @a = 9; await $f.() }
    is collide-a(), 3, 'thread-escaping @-capture keeps its own lexical';
}

{
    my %h = a => 1, b => 2;
    my $g = -> { start { %h.elems } };
    sub collide-h() { my %h = z => 9; await $g.() }
    is collide-h(), 2, 'thread-escaping %-capture keeps its own lexical';
}

{
    my @b = 10, 20;
    my $p = start { @b.push(30); @b.elems };
    is (await $p), 3, 'a worker push through the captured container is visible to it';
    is @b.elems, 3, 'and to the declaring frame afterwards';
}

{
    my @c = 1;
    sub outer() { my @c = 5, 6, 7; my $q = start { @c.elems }; await $q }
    is outer(), 3, 'an inner frame shadowing the name does not leak into the worker';
}
