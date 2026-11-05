use Test;

# A built-in iterator over a source that knows its size is a PredictiveIterator,
# as in rakudo; a lazy stream (map / grep / gather), an unending range and a
# hash's iterator are plain iterators.

plan 19;

sub kind($i) { $i ~~ PredictiveIterator ?? 'predictive' !! 'plain' }

is kind(().iterator), 'predictive', 'the empty list';
is kind((1, 2).iterator), 'predictive', 'a list';
is kind([1, 2].iterator), 'predictive', 'an array';
is kind((1..3).iterator), 'predictive', 'a finite range';
is kind("abc".comb.iterator), 'predictive', 'a comb';
is kind((1, 2).reverse.iterator), 'predictive', 'a reversed list';
is kind((1, 2).Seq.iterator), 'predictive', 'a Seq';
is kind((1..Inf).iterator), 'plain', 'an unending range';
is kind((1, 2).map(* + 1).iterator), 'plain', 'a map stream';
is kind((1, 2).grep(* > 1).iterator), 'plain', 'a grep stream';
is kind((gather { take 1 }).iterator), 'plain', 'a gather';
is kind(%(a => 1).iterator), 'plain', 'a hash';

sub predictive(PredictiveIterator:D $i --> PredictiveIterator:D) { $i }
is predictive((1, 2).iterator).count-only, 2, 'a PredictiveIterator parameter and return type accept a list iterator';
is predictive(().iterator).count-only, 0, 'also the empty one';
is (try predictive((1, 2).map(* + 1).iterator)) // 'died', 'died', 'a map iterator is refused';
ok ().iterator ~~ Iterator, 'a predictive iterator is an Iterator';
is ().iterator.count-only, 0, 'count-only is unchanged';
is ().iterator.bool-only, False, 'bool-only is unchanged';

class Countdown does PredictiveIterator {
    method pull-one { IterationEnd }
    method count-only { 0 }
}
is kind(Countdown.new), 'predictive', 'a user class that does the role';
