use Test;

# A paragraph-form Pod table whose header separator is written with `=`
# (`====|====`) is still one table: a line is a new Pod directive only when
# `=` is followed by an identifier (#9329, Pod::To::Markdown's t/table.rakutest).

plan 5;

=for table
H 1 | H 2
====|====
A | B

=begin pod
=for table
X | Y
=+=
1 | 2
=end pod

is $=pod.elems, 2, 'the table and the pod block are two top-level blocks';
my $t = $=pod[0];
isa-ok $t, Pod::Block::Table, 'the `=` separator did not end the table';
is-deeply $t.headers.List, ('H 1', 'H 2'), 'the rows above the separator are the header';
is-deeply $t.contents.map(*.List).List, (('A', 'B'),), 'the row below it is data';
is-deeply $=pod[1].contents[0].headers.List, ('X', 'Y'), 'a mixed `=+=` separator works too';
