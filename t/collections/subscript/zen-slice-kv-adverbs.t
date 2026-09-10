use Test;

# A zen slice with a `:k`/`:v`/`:kv`/`:p` adverb maps to the keys/values/kv/pairs
# method: `%h<>:k` is `%h.keys`. Previously only `:v` was handled; `:k` (and
# `:kv`/`:p`) left the adverb stranded as a separate `:k` term.

plan 9;

my %h1 = a => 1;
is-deeply %h1<>:k.List, ("a",), '%h<>:k on a one-key hash';
is-deeply %h1<>:v.List, (1,),   '%h<>:v on a one-key hash';

my %h2 = a => 1, b => 2;
is-deeply %h2<>:k.sort.List, ("a", "b"), '%h<>:k on a two-key hash';
is-deeply %h2<>:v.sort.List, (1, 2),     '%h<>:v on a two-key hash';
is-deeply %h2<>:kv.sort.List, (1, 2, "a", "b"), '%h<>:kv yields keys and values';
is-deeply %h2<>:p.sort.List, (:a(1), :b(2)), '%h<>:p yields pairs';

# the bare zen slice still returns the hash
is-deeply %h1<>, %h1, '%h<> (no adverb) returns the hash';

# array zen slice adverbs
my @a = 1, 2, 3;
is-deeply @a[]:k.List, (0, 1, 2), '@a[]:k yields the indices';
is-deeply @a[]:v.List, (1, 2, 3), '@a[]:v yields the values';
