use Test;

plan 3;

=begin kwid

= DESCRIPTION
bla bla

foo
=end kwid

isa-ok $=pod[0], Pod::Block::Named, 'kwid block parses as named pod block';
is $=pod[0].contents[0].contents, '= DESCRIPTION bla bla',
    'flush-left = lines stay as literal paragraph text inside named pod block';
is $=pod[0].contents[1].contents, 'foo',
    'subsequent paragraphs remain intact';
