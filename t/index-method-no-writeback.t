use v6;
use Test;

plan 30;

# Mutating methods on subscripted elements mutate the element's shared node
# in place — no post-call writeback (container identity §3.2).
# All expected values verified against raku 2024+.

# --- push family on existing elements ---
{
    my @a = [1,2],;
    @a[0].push(3);
    is @a.raku, '[[1, 2, 3],]', 'push on array element';
}
{
    my @a = [1,2],;
    @a[0].append(3,4);
    is @a.raku, '[[1, 2, 3, 4],]', 'append on array element';
}
{
    my @a = [1,2],;
    @a[0].unshift(0);
    is @a.raku, '[[0, 1, 2],]', 'unshift on array element';
}
{
    my @a = [1,2],;
    @a[0].prepend(0);
    is @a.raku, '[[0, 1, 2],]', 'prepend on array element';
}
{
    my %h = k => [1,2];
    %h<k>.push(3);
    is %h.raku, '{:k($[1, 2, 3])}', 'push on hash element';
}

# --- autovivification (push family only) ---
{
    my @a;
    @a[2].push(3);
    is @a.raku, '[Any, Any, [3]]', 'push autovivifies a missing array element';
}
{
    my %h;
    %h<k>.push(1);
    is %h.raku, '{:k($[1])}', 'push autovivifies a missing hash element';
}
{
    my %h;
    %h<k>.unshift(1);
    is %h.raku, '{:k($[1])}', 'unshift autovivifies a missing hash element';
}
{
    my @a;
    @a[1].push();
    is @a.raku, '[Any, []]', 'zero-arg push still autovivifies';
}

# --- element aliasing: by-value holders observe the mutation ---
{
    my @a = [1,2],;
    my $c = @a[0];
    @a[0].push(9);
    is $c[2], 9, 'captured element sees push';
}
{
    my @a = [1,2,3],;
    my $c = @a[0];
    my $x = @a[0].pop;
    is $x, 3, 'pop returns the removed element';
    is @a.raku, '[[1, 2],]', 'pop mutates the element';
    is $c.elems, 2, 'captured element sees pop';
}
{
    my @a = [1,2,3],;
    my $y = @a[0].shift;
    is $y, 1, 'shift returns the removed element';
    is @a.raku, '[[2, 3],]', 'shift mutates the element';
}
{
    my @a = [1,2,3],;
    my @r = @a[0].splice(1,1);
    is @r.raku, '[2]', 'splice returns the removed elements';
    is @a.raku, '[[1, 3],]', 'splice mutates the element';
}
{
    my %m = k => [1,2,3];
    my $c = %m<k>;
    my $x = %m<k>.pop;
    is %m.raku, '{:k($[1, 2])}', 'pop mutates the hash element';
    is $c.elems, 2, 'captured hash element sees pop';
}
{
    my @w = [1,2,3,4],;
    my @r = @w[0].splice(*-2, 1);
    is @r.raku, '[3]', 'splice with WhateverCode offset returns removed';
    is @w.raku, '[[1, 2, 4],]', 'splice with WhateverCode offset mutates';
}

# --- pop/shift/splice do NOT autovivify (raku dies; container unchanged) ---
{
    my @a;
    try { @a[2].pop };
    is @a.raku, '[]', 'pop on a missing element does not grow the array';
}
{
    my %h;
    try { %h<k>.splice(0,1) };
    is %h.raku, '{}', 'splice on a missing hash key does not create it';
}

# --- push result is the element's array (aliases it) ---
{
    my %h = k => [1,2];
    my $r = %h<k>.push(3);
    $r.push(4);
    is %h.raku, '{:k($[1, 2, 3, 4])}', 'push result aliases the element';
}

# --- non-variable invocants: the shared node is still mutated ---
{
    my @a = [1,2,3],;
    sub f { @a[0] }
    my $x = f().pop;
    is $x, 3, 'pop on a function result returns the element';
    is @a.raku, '[[1, 2],]', 'pop on a function result mutates the source';
}
{
    my @a = [1,2,3],;
    sub g { @a[0] }
    my $x = g().shift;
    is $x, 1, 'shift on a function result returns the element';
    is @a.raku, '[[2, 3],]', 'shift on a function result mutates the source';
}

# --- defaults: raku does not store the pushed default into the container ---
{
    my %h is default([9]);
    %h<k>.push(1);
    is %h.raku, '{}', 'push on a defaulted missing hash key does not store';
}
{
    my @a is default([8]);
    @a[0].push(1);
    is @a.raku, '[]', 'push on a defaulted missing array element does not store';
}

done-testing;
