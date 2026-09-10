use Test;

plan 6;

# The `is rw` element-arg writeback temps (`__mutsu_index_rw_arg_N`) are
# compile-time-fixed global names, re-used on every execution of the call
# site. Storing the element snapshot with a write-through SetGlobal wrote
# each later loop iteration's value THROUGH the previous iteration's
# promoted element cell, corrupting the source hash at the first key
# (JSON::Unmarshal 040 "DateTime as a definite": %json degraded to an Int).

sub f($x) { $x }

{
    my %j = a => 1, b => 2, c => 3;
    my %args;
    for <a b c> -> $k {
        %args{$k} := f(%j{$k});
    }
    is-deeply %j, {a => 1, b => 2, c => 3}, 'source hash intact after bind loop';
    is-deeply %args, {a => 1, b => 2, c => 3}, 'target hash bound correctly';
}

# Same shape with an array source.
{
    my @src = 10, 20, 30;
    my @out;
    for 0, 1, 2 -> $i {
        @out[$i] := f(@src[$i]);
    }
    is-deeply @src, [10, 20, 30], 'source array intact after bind loop';
    is-deeply @out, [10, 20, 30], 'target array bound correctly';
}

# A genuine `is rw` element arg must still write back.
{
    sub bump($x is rw) { $x++ }
    my %h = n => 5;
    bump(%h<n>);
    is %h<n>, 6, 'is rw element writeback still works';

    my @a = 1, 2;
    for 0, 1 -> $i { bump(@a[$i]) }
    is-deeply @a, [2, 3], 'is rw element writeback in a loop';
}
