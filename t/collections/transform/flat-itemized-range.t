use Test;
# A Range stored as one element of a real Array remains one item when the
# array is flattened. This is used by Text::MathematicalCase's Unicode maps.
plan 2;
my @raw = (("A" .. "C", "a" .. "c", "0" .. "2"),
           ("D" .. "F", "d" .. "f", "3" .. "5"));
my @ranges;
for @raw -> @info {
    @ranges.push(@info[0]);
}
is @ranges.flat.elems, 2, 'flattening preserves itemized range elements';
is @ranges.flat[0].^name, 'Range', 'the preserved element remains a Range';
