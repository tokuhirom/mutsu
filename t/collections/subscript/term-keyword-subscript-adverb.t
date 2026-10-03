use Test;

# `self{$k}:exists` subscripts `self` and applies the adverb. It used to parse
# as `{$k}.self(exists)`, the invocant-colon form of `name { block }: args`,
# so the result was a Block (always true). OrderedHash's
# `method keys { @!keys.grep: { self{$_}:exists } }` then reported every slot
# as present (Trie).

plan 5;

class C does Associative {
    method EXISTS-KEY($k) { $k eq 'a' }
    method AT-KEY($k) { "v$k" }
    method DELETE-KEY($k) { "deleted-$k" }
    method one($k) { self{$k}:exists }
    method present { <a b>.grep: { self{$_}:exists } }
    method del($k) { self{$k}:delete }
}

my $c = C.new;
nok $c.one('b'), 'self{$k}:exists is False for a missing key';
ok $c.one('a'), 'and True for a present one';
is-deeply $c.present, ('a',), 'inside a grep block too';
is $c.del('a'), 'deleted-a', 'self{$k}:delete';

# A listop taking a block argument still parses as before.
is-deeply (map { $_ + 1 }, 1, 2).List, (2, 3), 'map { ... }, @list';
