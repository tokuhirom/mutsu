use Test;

# From Data::MessagePack (t/307-stream-unpack-hash): a Hash/Map in *value*
# position is just a value; only a Hash in key position flattens, and the odd
# element check must follow that same left-to-right consumption.

plan 7;

my @p;
@p.push('c');
@p.push({ x => 1 });
is-deeply @p.Hash, { c => { x => 1 } }, '.Hash: Hash element is a value';
is-deeply ('c', { x => 1 }).Hash, { c => { x => 1 } }, 'List.Hash: same';

my %h = 'c', { x => 1 };
is-deeply %h, { c => { x => 1 } }, 'list assignment: same';

my @q = 'b', [1], 'c', { aa => 3 }, 'd', [];
is-deeply @q.Hash, { b => [1], c => { aa => 3 }, d => [] }, 'mixed Array/Hash values';

is-deeply ({ a => 1 }, 'k', 2).Hash, { a => 1, k => 2 }, 'Hash in key position flattens';
dies-ok { ('a', 1, 'b').Hash }, 'dangling key dies';
dies-ok { ('a', { x => 1 }, 'b').Hash }, 'dangling key after a Hash value dies';
