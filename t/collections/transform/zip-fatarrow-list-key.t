use Test;

plan 3;

# CellularAutomata (ecosystem) builds a lookup table by zipping a list of
# List-valued keys against a list of values with `Z=>`. The Pair's key must
# stay the actual List, not get stringified into a joined string.
my @keys = ((1, 1, 1), (1, 1, 0), (0, 0, 0));
my @values = (0, 1, 1);
my @pairs = @keys Z=> @values;

is-deeply @pairs,
    [(1, 1, 1) => 0, (1, 1, 0) => 1, (0, 0, 0) => 1],
    'Z=> keeps a List-valued key as a List, not a stringified join';

is @pairs[0].key.^name, 'List', 'the Pair key stays a List';

is-deeply (cross(0..1, 0..1) Z=> (10, 20, 30, 40)).Array,
    [(0, 0) => 10, (0, 1) => 20, (1, 0) => 30, (1, 1) => 40],
    'Z=> over a cross() result keeps each tuple key intact';
