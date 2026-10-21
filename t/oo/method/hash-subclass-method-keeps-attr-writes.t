use Test;

# A method of an `is Hash` class that writes a `%!attr` after calling a builtin
# Associative method on `self` keeps the write. The delegation rebuilt the
# instance from the attribute snapshot taken before the call, so a later
# `%!entries<y> = 2` was lost (PDF's `Tie::Hash.tie-init`, which assigns
# `%!entries` after `self{key} //= ...`). Expected values are the `raku`
# (v2026.09) output.

plan 6;

class H is Hash {
    has Int %.entries;
    method after-elems() { %!entries<x> = 1; self.elems; %!entries<y> = 2; }
    method after-keys()  { %!entries<x> = 1; self.keys;  %!entries<y> = 2; }
    method after-store() { %!entries<x> = 1; self<k> = 5; %!entries<y> = 2; }
    method after-delete() { %!entries<x> = 1; self<k>:delete; %!entries<y> = 2; }
    method after-default() { %!entries<x> = 1; self<k> //= 5; %!entries<y> = 2; }
}

for <after-elems after-keys after-store after-delete after-default> -> $m {
    my $h = H.new;
    $h."$m"();
    is $h.entries.keys.sort.join(','), 'x,y', "$m keeps the later attribute write";
}

my $h = H.new;
$h.after-store;
is $h<k>, 5, 'and the hash storage write is kept too';
