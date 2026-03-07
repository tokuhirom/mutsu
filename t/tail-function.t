use Test;

plan 6;

my @array = <a b c d>;
is-deeply tail(2, @array), <c d>, 'tail(N, @array) returns last N elements';

my $list = <a b c d>;
is-deeply tail(2, $list), ($list,), 'tail(N, $list) keeps scalar container';
is-deeply tail(2, $list<>), <c d>, 'tail(N, $list<>) operates on flattened values';

my $pulled = 0;
sub make-predictive-seq($i = 0) {
    Seq.new: class :: does PredictiveIterator {
        has $!i;
        has $!pulled;
        method !SET-SELF(\pulled, $!i) { $!pulled := pulled; self }
        method new(\pulled, \i) { self.bless!SET-SELF: pulled, i }
        method pull-one {
            $!pulled++;
            ++$!i <= 10 ?? $!i !! IterationEnd
        }
        method skip-one { ++$!i <= 10 ?? True !! False }
        method count-only { 0 max 10 - $!i }
    }.new: $pulled, $i
}

is-deeply make-predictive-seq.tail, 10, 'tail uses PredictiveIterator count-only path';
is $pulled, 1, 'PredictiveIterator tail calls pull-one once';
is-deeply (4, 5, 6, 7).tail(-2**100), (), 'tail accepts large negative Int counts';
