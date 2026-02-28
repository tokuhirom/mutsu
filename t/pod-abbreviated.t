use Test;

plan 9;

=foo

=table_not
    Constants 1
    Variables 10

=head3
Heading level 3

is $=pod.elems, 3, 'captures abbreviated pod entries';
isa-ok $=pod[0], Pod::Block::Named, '=foo creates a named pod block';
is $=pod[0].name, 'foo', 'named pod block keeps directive name';
is $=pod[0].contents.elems, 0, 'empty abbreviated block has no contents';
isa-ok $=pod[1], Pod::Block::Named, '=table_not is treated as named pod block';
is $=pod[1].contents[0].contents, 'Constants 1 Variables 10', 'continuation text is paragraph-normalized';
isa-ok $=pod[2], Pod::Heading, '=head3 creates Pod::Heading';
is $=pod[2].level, '3', 'heading level is captured from directive';
is $=pod[2].contents[0].contents, 'Heading level 3', 'heading body is captured';
