use Test;

plan 8;

=begin pod
Intro line.

=item Apple
=item2 Banana
=begin item
Cherry line one.

Cherry line two.
=end item
=end pod

my $pod = $=pod[0];
is $pod.contents[0].contents, "Intro line.", "captures intro paragraph in begin pod";
isa-ok $pod.contents[1], Pod::Item, "captures =item as Pod::Item";
is $pod.contents[1].level, 1, "=item defaults to level 1";
is $pod.contents[1].contents[0].contents, "Apple", "captures inline item text";
is $pod.contents[2].level, 2, "=item2 sets level 2";
is $pod.contents[2].contents[0].contents, "Banana", "captures numbered inline item text";
is $pod.contents[3].contents.elems, 2, "block item captures multiple paragraphs";
is $pod.contents[3].contents[1].contents[0], "Cherry line two.", "captures later block paragraph text";
