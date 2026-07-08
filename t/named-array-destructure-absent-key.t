use v6;
use Test;

# A named-array destructuring bind (`my (:@a, :@b) := %hash`) whose key is ABSENT
# must bind the array to EMPTY, mirroring Rakudo parameter-binding semantics — not
# `[Any]`. A bare `%h<absent>` is `Any`, and `Any.list` is `(Any,)`, so the naive
# `%h<key>.list` lowering leaked a stray `(Any,)`. zef's `install` classifies its
# arguments this way (`my (:@paths, :@uris, :@identities) := @wants.classify(...)`)
# and a stray `(Any,)` later blew up a `%(...)` hash initializer with
# X::Hash::Store::OddNumber.

plan 8;

# classify with only some buckets populated: the empty ones bind to []
{
    my (:@paths, :@uris, :@identities) := <a ./b c>.classify: -> $w {
        $w.starts-with('.') ?? <paths> !! <identities>
    };
    is-deeply @paths.List,      ('./b',),   'populated bucket binds its elements';
    is-deeply @identities.List, ('a', 'c'), 'second populated bucket binds its elements';
    is-deeply @uris.List,       (),         'unmatched bucket binds to an empty array';
    is @uris.elems, 0, 'unmatched bucket has zero elements (not [Any])';
}

# itemized buckets are still de-itemized (a classify bucket is an itemized array)
{
    my (:@even, :@odd) := (1..5).classify: { $_ %% 2 ?? 'even' !! 'odd' };
    is-deeply @even.List, (2, 4),    'even bucket de-itemized';
    is-deeply @odd.List,  (1, 3, 5), 'odd bucket de-itemized';
}

# a plain hash destructure with a mix of present scalar and absent array
{
    my (:$a, :@b) := %(a => 'y');
    is $a, 'y', 'present scalar key binds';
    is @b.elems, 0, 'absent array key binds to empty array';
}
