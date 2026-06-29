use v6;
use Test;

# A file-scoped `@a`/`%h` mutated from a sub becomes a shared `ContainerRef`
# cell (cross-frame dual-store coherence). Multi-dimensional subscript ops
# (`@a[i;j;k] = v`, `@a[i;j;k]:delete`) must read/mutate *through* that cell so
# the live container — visible to every frame — observes the change.

plan 11;

# --- array: assign through a cross-frame cell ---
{
    my @array;
    sub setup() { @array = [[[42,666,[314]],],]; }

    setup();
    @array[0;0;0] = 999;
    is-deeply @array, [[[999,666,[314]],],], 'multidim assign through cross-frame cell';

    setup();
    @array[0;0;3] = 999;
    is-deeply @array, [[[42,666,[314],999],],], 'multidim assign auto-vivifies (append)';

    setup();
    @array[0;1;0] = 999;
    is-deeply @array, [[[42,666,[314]],[999]],], 'multidim assign auto-vivifies (intermediate)';

    setup();
    @array[*-1;*-1;*-1] = 999;
    is-deeply @array, [[[42,666,999],],], 'WhateverCode indices assign through cell';
}

# --- array: delete through a cross-frame cell ---
{
    my @array;
    sub setup2() { @array = [[[42,666,[314]],],]; }

    setup2();
    is @array[0;0;0]:delete, 42, 'multidim :delete returns the element (cross-frame cell)';
    is-deeply @array, [[[Any,666,[314]],],], 'multidim :delete mutates the live array';

    setup2();
    my $delete = True;
    is @array[0;0;1]:$delete, 666, 'dynamic :$delete returns the element (cross-frame cell)';
    is-deeply @array, [[[42,Any,[314]],],], 'dynamic :$delete mutates the live array';
}

# --- hash: delete through a cross-frame cell ---
{
    my %hash;
    sub setup3() { %hash = a => { b => { c => 42, d => 666 } }; }

    setup3();
    is %hash{"a";"b";"c"}:delete, 42, 'hash multidim :delete returns value (cross-frame cell)';
    is-deeply %hash, {a => {b => {d => 666}}}, 'hash multidim :delete mutates the live hash';
}

# --- read through a cross-frame cell ---
{
    my @array;
    sub setup4() { @array = [[[42,666,[314]],],]; }
    setup4();
    is @array[0;0;0], 42, 'multidim read through cross-frame cell';
}
