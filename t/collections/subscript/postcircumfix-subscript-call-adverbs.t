use v6;
use Test;

plan 12;

# `postcircumfix:<[ ]>` / `postcircumfix:<{ }>` called by name are ordinary
# CORE routines, so adverbs must be honored the same way `@a[...]:adverb`
# syntax honors them, instead of being miscounted as a positional argument
# (see #9682: `postcircumfix:<[ ]>(@a, 0, :nonesuch)` used to assign the
# adverb Pair itself into `@a[0]`).

# --- an adverb the CORE candidate set doesn't accept dies, and does not
#     corrupt the container ---
{
    my @a = ^3;
    dies-ok { postcircumfix:<[ ]>(@a, 0, :nonesuch) },
        'unrecognized adverb on the call form dies';
    is @a.join(' '), '0 1 2', 'the container is untouched by the failed call';
}

# --- :exists / :!exists ---
{
    my @a = ^3;
    is postcircumfix:<[ ]>(@a, 1, :exists), True, ':exists on an in-range index';
    is postcircumfix:<[ ]>(@a, 99, :exists), False, ':exists on an out-of-range index';
    is postcircumfix:<[ ]>(@a, 1, :exists(False)), False, ':exists(False) negates';
}

# --- :delete ---
{
    my @a = ^3;
    is postcircumfix:<[ ]>(@a, 2, :delete), 2, ':delete on an array returns the removed value';
    is @a.elems, 2, ':delete actually removes the (trailing) element';

    my %h = a => 1, b => 2;
    is postcircumfix:<{ }>(%h, "a", :delete), 1, ':delete on a hash returns the removed value';
    nok %h<a>:exists, ':delete actually removes the key';
}

# --- :k / :v / :kv / :p ---
{
    my @a = <x y z>;
    is postcircumfix:<[ ]>(@a, 1, :k), 1, ':k answers the index';
    is postcircumfix:<[ ]>(@a, 1, :v), 'y', ':v answers the value';
    is postcircumfix:<[ ]>(@a, 1, :kv).raku, (1, 'y').raku, ':kv answers (index, value)';
}
