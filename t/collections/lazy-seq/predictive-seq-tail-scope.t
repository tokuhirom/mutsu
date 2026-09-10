use Test;

# Regression: a PredictiveIterator-backed Seq created by `Seq.new(iterator)`
# must keep its iterator association across scope boundaries, so `.tail` /
# `.Numeric` can use the count-only path even when the Seq was built inside a
# sub or block and consumed later in an outer scope.
#
# Previously the association lived in a `__mutsu_predictive_seq_iter::{id}` env
# key that was discarded on sub/block return, so `.tail` returned Nil.

plan 4;

my $cls = class :: does PredictiveIterator {
    has $!i;
    method pull-one  { ++$!i <= 10 ?? $!i !! IterationEnd }
    method skip-one  { ++$!i <= 10 ?? True !! False }
    method count-only { 0 max 10 - $!i }
};

# Built and consumed at top level.
is Seq.new($cls.new).tail, 10, 'tail count-only path at top level';

# Built inside a bare block, consumed outside.
my $s;
{ $s = Seq.new($cls.new) }
is $s.tail, 10, 'association survives a bare-block return';

# Built inside a sub, consumed outside.
sub make-seq { Seq.new($cls.new) }
is make-seq.tail, 10, 'association survives a sub return';

# .Numeric also uses count-only across scope.
my $s2;
{ $s2 = Seq.new($cls.new) }
is $s2.Numeric, 10, '.Numeric count-only survives scope';
