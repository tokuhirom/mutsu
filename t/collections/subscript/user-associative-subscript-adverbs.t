use Test;

# `:k`/`:v`/`:kv`/`:p` on a user Associative ask the requested keys of
# EXISTS-KEY and read them through AT-KEY. They used to snapshot the whole
# object through its `keys` method first, so an object without one (Trie)
# answered `()`, and one whose `keys` was positional got Int keys.

plan 9;

class C does Associative {
    method EXISTS-KEY($k) { $k eq 'a' | 'c' }
    method AT-KEY($k) { "v$k" }
}
my $c = C.new;

is-deeply ($c{'a'}:kv), ('a', 'va'), ':kv';
is-deeply ($c{'a'}:p), (a => 'va'), ':p';
is-deeply ($c{'a'}:k), 'a', ':k';
is-deeply ($c{'a'}:v), 'va', ':v';
is-deeply ($c{'b'}:k), (), 'a missing key is skipped';
is-deeply ($c{<a b c>}:v), ('va', 'vc'), 'a slice keeps only the existing keys';

class G does Associative {
    method EXISTS-KEY($) { True }
    method AT-KEY($) { gather { take 1; take 2 } }
}
is-deeply (G.new{'x'}:kv)[1].List, (1, 2), 'a gather value is read in full';

class K does Associative {
    has %!h = x => 1, y => 2;
    method keys { %!h.keys }
    method EXISTS-KEY($k) { %!h{$k}:exists }
    method AT-KEY($k) { %!h{$k} }
}
is-deeply (K.new{*}:k).sort.List, <x y>, 'a Whatever slice still lists its own keys';
is-deeply (K.new{'y'}:v), 2, 'and a key reads through AT-KEY';
