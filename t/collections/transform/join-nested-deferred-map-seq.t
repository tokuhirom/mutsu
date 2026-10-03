use Test;

# An outer `.map` whose block returns another (still deferred) `.map` Seq:
# joining the result must run the inner callbacks, not stringify their empty
# seed. From URI::Query::FromHash's
# `join '&', $hash.pairs.map: -> (:$key, :$value) { $value.List.map: {...} }`.

plan 6;

is (1, 2).map({ (3, 4).map({ 7 }) }).join('&'), '7 7&7 7', '.join method';
is join('&', (1, 2).map({ (1,).map({ $_ }) })), '1&1', 'join function flattens the inner Seqs';

my $q = join '&', (1, 2).map: -> $x { (1,).map: { "k=$x" } };
is $q, 'k=1&k=2', 'colon-call form';

is join(',', 1, (2, 3).map({ $_ * 2 }), [4, 5]), '1,4,6,4,5', 'mixed flat arguments';

my %h = a => 1, b => (2, 3);
is join('&', %h.pairs.sort(*.key).map: -> (:$key, :$value) {
    $value.List.map: { "$key=$_" }
}), 'a=1&b=2&b=3', 'the URI::Query::FromHash shape';

is (1, 2).map({ (3, 4).map({ 7 }) }).map(*.Str).join('&'), '7 7&7 7', 'explicit .Str agrees';
