use Test;

# Sigilless for-loop parameters (`-> \v`, `-> \k, \v`) are raw bindings that
# alias the source element directly. In Raku they are writable, and
# modifications propagate back to the source container.

plan 11;

# Single sigilless param over an array writes back.
{
    my @a = 1, 2, 3;
    for @a -> \v { v = v * 10 };
    is-deeply @a, [10, 20, 30], 'single sigilless param writes back to array';
}

# Single sigilless param over `.values` writes back.
{
    my @a = 1, 2, 3;
    for @a.values -> \v { v = 99 };
    is-deeply @a, [99, 99, 99], 'single sigilless param over .values writes back';
}

# Multi sigilless params over an array (pairwise) write back.
{
    my @a = 1, 2, 3, 4;
    for @a -> \k, \v { v = v * 10 };
    is-deeply @a, [1, 20, 3, 40], 'multi sigilless params write back the second slot';
}

# `%h.kv -> \k, \v` writes the value back to the hash.
{
    my %h = a => 1, b => 2;
    for %h.kv -> \k, \v { v = 99 };
    is %h<a>, 99, 'kv sigilless writeback updates value a';
    is %h<b>, 99, 'kv sigilless writeback updates value b';
}

# `%h.kv -> $k, $v is rw` (explicit sigil) keeps working.
{
    my %h = x => 1, y => 2;
    for %h.kv -> $k, $v is rw { $v = $v * 10 };
    is %h<x>, 10, 'kv explicit is rw writeback updates value x';
    is %h<y>, 20, 'kv explicit is rw writeback updates value y';
}

# Read-only use of sigilless params is unaffected.
{
    my @seen;
    for 1, 2, 3 -> \v { @seen.push(v) };
    is-deeply @seen, [1, 2, 3], 'read-only sigilless param over a literal list';
}

# Sigilless param over a grep result (read-only).
{
    my @a = 1, 2, 3, 4;
    my @big;
    for @a.grep(* > 2) -> \v { @big.push(v) };
    is-deeply @big, [3, 4], 'sigilless param over a grep result';
}

# Nested element aliasing via a sigilless param.
{
    my @m = (1, 2), (3, 4);
    my @joined;
    for @m -> \row { @joined.push(row.join(",")) };
    is-deeply @joined, ["1,2", "3,4"], 'sigilless param aliases nested array element';
}

# A sigilless param that is not modified leaves the source untouched.
{
    my @a = 7, 8, 9;
    for @a -> \v { my $x = v + 1 };
    is-deeply @a, [7, 8, 9], 'unmodified sigilless param does not mutate the source';
}
