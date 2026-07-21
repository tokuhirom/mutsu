use Test;

plan 4;

# `for %h` / `.map` over a tied container (`my %h is Foo`, Foo `does Iterable`
# with its own `iterator` method) must iterate via that method, not treat the
# instance as a single opaque value.

role TinyAssoc does Associative does Iterable {
    has %!store;
    method AT-KEY($k)         is raw { %!store.AT-KEY($k) }
    method ASSIGN-KEY($k, \v) is raw { %!store.ASSIGN-KEY($k, v) }
    method keys()                    { %!store.keys }
    method STORE(*@pairs) { for @pairs -> $p { self.ASSIGN-KEY($p.key, $p.value) }; self }
    method pairs()    { self.keys.map({ Pair.new($_, self.AT-KEY($_)) }) }
    method iterator() { self.pairs.iterator }
}
class TiedHash does TinyAssoc { }

my %h is TiedHash = (a => 1, b => 2, c => 3);

# for-loop yields the class's Pairs
my @seen;
for %h -> $p {
    isa-ok $p, Pair, 'for %h yields a Pair' if @seen == 0;
    @seen.push: "{$p.key}={$p.value}";
}
is @seen.sort.join(','), 'a=1,b=2,c=3', 'for %h iterates all pairs via .iterator';

# the tied instance is Iterable
ok %h ~~ Iterable, 'a tied hash does Iterable';

# a plain hash still iterates the ordinary way (no regression)
my %plain = (x => 1, y => 2);
my @pk;
for %plain -> $p { @pk.push: $p.key }
is @pk.sort.join(','), 'x,y', 'a plain hash still iterates normally';
