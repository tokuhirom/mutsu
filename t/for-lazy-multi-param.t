use Test;

plan 5;

# A `for` over a lazy gather Seq must consume one chunk of `arity` elements per
# iteration, exactly as it does over an eager list.
sub relay(@items) { gather for @items -> $x { take $x } }

my @pairs;
for relay(<a b c d>) -> $a, $b {
    @pairs.push("$a$b");
}
is-deeply @pairs, ['ab', 'cd'], 'arity 2 over a gather Seq chunks by two';

my @triples;
for relay(<a b c d e f>) -> $a, $b, $c {
    @triples.push("$a$b$c");
}
is-deeply @triples, ['abc', 'def'], 'arity 3 over a gather Seq chunks by three';

# The multi-param bindings must be writable, not read-only topic aliases.
my %seen;
for relay(<a b c d e f>) -> $a, $b, $c {
    %seen{$a ~ $b}{$c}++;
}
is-deeply %seen, {ab => {c => 1}, de => {f => 1}}, 'multi-param bindings are assignable';

# Arity 1 still binds a single element, not a one-element chunk.
my @singles;
for relay(<a b>) -> $x {
    @singles.push($x);
}
is-deeply @singles, ['a', 'b'], 'arity 1 over a gather Seq is unchanged';

# Laziness is preserved: an infinite gather must not be materialized.
sub counter() { gather { my $i = 0; loop { take $i++ } } }
my @taken;
for counter() -> $a, $b {
    @taken.push($a + $b);
    last if @taken == 3;
}
is-deeply @taken, [1, 5, 9], 'a chunked for over an infinite gather stays lazy';
