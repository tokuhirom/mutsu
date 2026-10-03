use Test;

# An identifier may contain `-` only between two word parts, so in a string
# `%PDF-{$v}` is the literal `%PDF-` followed by a block, not a hash named
# `PDF-`. From PDF::Grammar's `"%PDF-{$pdf-header-version}"`.

plan 5;

my $v = 1.5;
is "%PDF-{$v}", '%PDF-1.5', '%name- before a block is literal text';

my %PDF = a => 1;
is "%PDF-{$v}", '%PDF-1.5', '... even when a hash of that name exists';
is "%PDF{'a'}", '1', 'a hash subscript still interpolates';

my %kebab-name = k => 'v';
is "%kebab-name<k>", 'v', 'a hyphenated hash name still interpolates';

my $x = 3;
is "$x-1", '3-1', '$name followed by -digit stops at the hyphen';
