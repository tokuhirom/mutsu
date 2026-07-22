use Test;

# The `:v`/`:k`/`:kv`/`:p` slice adverbs must dispatch through a tied
# (user Associative) hash's subscript protocol, not silently return Nil.
# `:exists`/`:delete` were already wired; the value/key/pair adverbs were not.

plan 14;

class MyHash does Associative {
    has %.store;
    method AT-KEY($k)     is raw { %!store.AT-KEY($k)     }
    method EXISTS-KEY($k)        { %!store.EXISTS-KEY($k) }
    method DELETE-KEY($k)        { %!store.DELETE-KEY($k) }
    method keys()               { %!store.keys           }
}
my $mk = { MyHash.new(store => {a => 1, b => 2, c => 3}) };

my %h := $mk();

# Single key.
is (%h<a>:v), 1,          ':v on a single key returns the value';
is (%h<a>:k), 'a',        ':k on a single key returns the key';
is-deeply (%h<a>:kv), ('a', 1),   ':kv on a single key';
is-deeply (%h<a>:p), (a => 1),    ':p on a single key';

# Missing key.
is-deeply (%h<z>:v), (),  ':v on a missing key is empty';
is-deeply (%h<z>:exists), False, ':exists on a missing key';

# List slice.
is-deeply (%h<a b>:v), (1, 2),          ':v slice returns the values';
is-deeply (%h<a b>:k), ('a', 'b'),      ':k slice returns the keys';
is-deeply (%h<a b>:kv), ('a', 1, 'b', 2), ':kv slice is flat';

# Whatever / zen slices cover every key.
is-deeply (%h{*}:v).sort, (1, 2, 3),    ':v whatever-slice covers all values';
is-deeply (%h{}:v).sort,  (1, 2, 3),    ':v zen-slice covers all values';
is-deeply (%h{*}:k).sort, <a b c>,      ':k whatever-slice covers all keys';

# `:v:delete` reads the value AND removes the key via DELETE-KEY.
my %d := $mk();
is (%d<b>:v:delete), 2,   ':v:delete returns the deleted value';
nok (%d<b>:exists),       ':v:delete actually removed the key';
