my constant HUFF = 3;
my constant @TABLE = 1, 2, 3;
constant OPEN = 4;
my $count = 0;
sub huff is export { HUFF + OPEN + @TABLE.elems }
