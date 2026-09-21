use v6;
use Test;

# Data::Summarizers reconstructs DateTimes from numeric Pair values produced by
# a map over an Array. The element expression is a container reference at the
# native constructor boundary and must be decontainerized before dispatch.
plan 1;

my @epochs = 1641002940, 1.5;
my @pairs = :epoch(@epochs[0]), :fraction(@epochs[1]);
is @pairs.map({ DateTime.new($_.value).Str }).join('|'),
    '2022-01-01T02:09:00Z|1970-01-01T00:00:01.500000Z',
    'DateTime.new accepts values read from Pair elements in a map';
