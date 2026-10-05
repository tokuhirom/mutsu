use Test;

# An `is BagHash`/`is SetHash` attribute is a container, so assigning to it,
# directly (`%!v = ...`) or through an rw method that hands it back
# (`$obj!values = ...`), coerces through the container's type and stores into
# it (CRDT's `G-Counter.copy`: `$obj!values = |%!values`). Both used to fail:
# the direct form built a plain Hash and died on an odd element count, and the
# rw-method form died with "Cannot modify an immutable BagHash".

plan 11;

class Counter {
    has %!values is BagHash;
    has %!seen is SetHash;
    method !values is rw { %!values }
    method !seen is rw { %!seen }
    method add($k) { %!values{$k}++; self }
    method total { %!values.values.sum }
    method values-raku { %!values.^name ~ ' ' ~ %!values.sort(*.key).map({ .key ~ '=' ~ .value }).join(',') }
    method set-direct(*@items) { %!values = @items; self }
    method set-pairs(@pairs) { %!values = @pairs; self }
    method copy {
        my $o = Counter.new;
        $o!values = |%!values;
        $o
    }
    method see(*@k) { self!seen = @k; %!seen }
}

my $c = Counter.new.add('a').add('a').add('b');
my $copy = $c.copy;
is $copy.values-raku, $c.values-raku, 'an rw method stores a slipped BagHash';
$copy.add('c');
isnt $copy.values-raku, $c.values-raku, 'the copy is its own container';
is Counter.new.copy.total, 0, 'copying an empty BagHash through a private rw accessor keeps it empty';
is (|BagHash.new).BagHash.elems, 0, 'coercing an empty Slip produces an empty BagHash';

is Counter.new.set-direct(<x x y>).values-raku, 'BagHash x=2,y=1',
    'a direct attribute assignment coerces a list';
is Counter.new.set-pairs([a => 3]).values-raku, 'BagHash a=3',
    'and a list of pairs';

my $s = Counter.new.see(<p q>);
isa-ok $s, SetHash, 'a SetHash attribute stays a SetHash';
is $s.keys.sort, <p q>, 'with the assigned keys';

my %h := BagHash.new;
%h = <m m n>;
isa-ok %h, BagHash, 'a %-variable bound to a BagHash stores into it';
is %h<m>, 2, 'with the coerced weights';

my %immutable := set <a>;
throws-like { %immutable = <b> }, Exception, 'an immutable Set is still refused';
