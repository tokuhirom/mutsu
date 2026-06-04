use Test;

# Auto-increment / auto-decrement hyper operators mutate their operand in
# place, so the original container variable (Array or Hash) must reflect the
# change -- for both prefix (`--<<%h`, `++<<@a`) and postfix (`%h>>++`,
# `@a>>--`) forms. Non-mutating prefixes like negation (`-<<`) must NOT mutate.
# Mirrors roast/S03-metaops/hyper.t "hash - correct result from --<< / >>++".

plan 20;

# --- prefix --<< / ++<< on a Hash: mutates in place ---
{
    my %a = a => 1, b => 2, c => 3;
    my %r = --<<%a;
    is %r<a>, 0, 'prefix --<< returns decremented values';
    is %r<c>, 2, 'prefix --<< returns decremented values (c)';
    is %a<a>, 0, 'prefix --<< mutates the hash in place';
    is %a<b>, 1, 'prefix --<< mutates the hash in place (b)';
    is %a<c>, 2, 'prefix --<< mutates the hash in place (c)';

    my %s = ++<<%a;
    is %a<a>, 1, 'prefix ++<< mutates the hash back up';
    is %s<c>, 3, 'prefix ++<< returns incremented values';
}

# --- postfix >>++ / >>-- on a Hash: mutates in place, returns old values ---
{
    my %a = a => 1, b => 2, c => 3;
    my %r = %a>>++;
    is %r<a>, 1, 'postfix >>++ returns the old values';
    is %a<a>, 2, 'postfix >>++ mutates the hash in place';
    is %a<c>, 4, 'postfix >>++ mutates the hash in place (c)';

    my %s = %a>>--;
    is %s<a>, 2, 'postfix >>-- returns the old values';
    is %a<a>, 1, 'postfix >>-- mutates the hash back down';
}

# --- prefix --<< / ++<< on an Array: mutates in place ---
{
    my @x = 1, 2, 3;
    --<<@x;
    is-deeply @x, [0, 1, 2], 'prefix --<< mutates the array in place';
    ++<<@x;
    is-deeply @x, [1, 2, 3], 'prefix ++<< mutates the array back up';
}

# --- postfix >>++ / >>-- on an Array: mutates in place ---
{
    my @y = 1, 2, 3;
    @y>>++;
    is-deeply @y, [2, 3, 4], 'postfix >>++ mutates the array in place';
    @y>>--;
    is-deeply @y, [1, 2, 3], 'postfix >>-- mutates the array back down';
}

# --- negation prefix -<< does NOT mutate ---
{
    my %n = a => 1, b => 2;
    my %m = -<<%n;
    is %n<a>, 1, 'negation -<< does not mutate the hash';
    is %m<a>, -1, 'negation -<< returns negated values';

    my @a = 1, 2, 3;
    my @b = -<<@a;
    is-deeply @a, [1, 2, 3], 'negation -<< does not mutate the array';
    is-deeply @b, [-1, -2, -3], 'negation -<< returns negated values';
}
