use Test;

# #14921: `for` over a `does Sequence does Iterable` instance with its own
# `iterator` iterates the elements, also once a role was mixed in with `but`
# (the mixed value stays `self` inside `iterator`); a `$` container holding it
# is one item, and a Sequence role instance keeps the default `.raku`.
plan 8;

class It does Iterator {
    has Int $.n = 0;
    method pull-one { return IterationEnd if $!n >= 2; ++$!n }
}
class Seqy does Sequence does Iterable { method iterator { It.new } }
class Self-aware does Sequence does Iterable {
    method iterator { It.new }
    method tag { 'seen' }
}
role Extra { method extra { 'extra' } }

my @bare;
for Seqy.new -> $x { @bare.push: $x }
is-deeply @bare, [1, 2], 'a bare instance iterates its elements';

my @mixed;
for (Seqy.new but Extra) -> $x { @mixed.push: $x }
is-deeply @mixed, [1, 2], 'a but-mixed instance iterates its elements too';

my @kinds;
for (Self-aware.new but Extra) -> $x { @kinds.push: $x.^name }
is-deeply @kinds, ['Int', 'Int'], 'each element is the pulled value';

my $s = Seqy.new;
my @held;
for $s -> $x { @held.push: $x.^name }
is-deeply @held, ['Seqy'], 'a scalar container holding it is one item';

my $m = Seqy.new but Extra;
my @heldm;
for $m -> $x { @heldm.push: $x.^name }
is @heldm.elems, 1, 'so is one holding the mixed value';

is Seqy.new.raku, 'Seqy.new', 'a Sequence role instance keeps the default .raku';
is Seqy.new.gist, '(1 2)', '.gist reads the iterator';
is Seqy.new.list.raku, '(1, 2)', '.list drains it';
