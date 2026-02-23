use Test;
plan 7;

=begin pod
=for comment
foo
bar

plain pod text
=end pod

=comment
single line comment text

=begin comment
alpha
=begin nested invalid pod
=end comment

is $=pod.elems, 3, "captures three Pod entries";
is $=pod[0].contents.elems, 1, "begin pod stores one comment child";
isa-ok $=pod[0].contents[0], Pod::Block::Comment, "comment child type";
is $=pod[0].contents[0].contents[0], "foo\nbar\n", "for comment captures paragraph text";
isa-ok $=pod[1], Pod::Block::Comment, "single =comment produces Pod::Block::Comment";
is $=pod[1].contents[0], "single line comment text\n", "=comment content is captured";
is $=pod[2].contents[0], "alpha\n=begin nested invalid pod\n", "=begin comment keeps raw text";
