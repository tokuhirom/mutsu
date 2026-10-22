unit module ImportedCodeVar;
our sub unpack-raku($x, $y) { "imported" }
our &unpack is export(:pack) = &unpack-raku;
