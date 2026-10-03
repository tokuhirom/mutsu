use Test;

plan 4;

# A single non-integer real subscript addresses the element its Int names,
# whatever the target (Rakudo's `postcircumfix:<[ ]>` calls `AT-POS(pos.Int)`).
constant @grayscale = chr(0x25A1)..chr(0x25A9);
is @grayscale[6.0], '▧', 'a Rat index into a Range';
is (1..5)[1.5], 2, 'a non-integral Rat truncates';
is ('a'..'e')[2e0], 'c', 'a Num index';
my $convert = { ($_ + 1) * @grayscale.elems / 3 };
is @grayscale[$convert(1)], '▧', 'the computed index of Array::Shaped::Console';
