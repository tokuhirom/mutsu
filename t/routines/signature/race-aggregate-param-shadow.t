use v6;
use Test;
plan 1;

# A worker's shared-name store must not shadow a fresh aggregate parameter
# when both use the same uppercase name. IP::Random declares @IP and invokes
# its helper from a batched RaceSeq callback.
sub count-IP(@IP) { @IP.elems }
my int @IP = ^16;
my @batches = @IP.batch(4).list;

is (^4).race(batch => 1).map({ count-IP(@batches[$_]) }).list.sum,
    16, 'RaceSeq callback keeps an aggregate parameter local';
