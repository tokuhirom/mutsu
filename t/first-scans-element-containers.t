use Test;

# `.first` over a mutable array scans its element CONTAINERS, so the matcher's
# topic aliases the element rather than a copy of its value — the same contract
# `.map`, `.grep` and `@a.values.first(...)` already honour.
#
# `todo/deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md` section B
# producer 4.

plan 12;

{
    my @a = 1, 2, 3;
    @a.first({ $_ = 5 });
    is-deeply @a, [5, 2, 3], '.first binds the topic to the element container';
}

{
    my @a = 1, 2, 3;
    @a.first({ $_++; False });
    is-deeply @a, [2, 3, 4], '...for every element the scan reaches';
}

{
    my @a = 1, 2, 3;
    is @a.first({ $_ = 5 }), 5, '.first still answers the matched VALUE';
}

# ... and nothing else about `.first` changes.

{
    my @a = 1, 2, 3;
    is @a.first(* > 1), 2, '.first with a Whatever matcher';
    is @a.first(* > 1, :k), 1, '...with :k';
    is-deeply @a.first(* > 1, :kv).List, (1, 2), '...with :kv';
    is @a.first(* > 1, :p).gist, '1 => 2', '...with :p';
    is @a.first, 1, '...with no matcher at all';
    ok !@a.first(* > 9).defined, '...with no match';
    is @a.first(* > 1).WHAT.^name, 'Int', 'the answer is the element value';
    is-deeply @a, [1, 2, 3], 'a non-mutating matcher leaves the array alone';
}

{
    my @a = <a b c>;
    is @a.first(/b/), 'b', '.first with a regex matcher';
}
