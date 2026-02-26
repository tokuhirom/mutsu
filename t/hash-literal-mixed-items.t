use Test;

plan 5;

my $hash = { a => 1, "b", 2, c => 3 };

isa-ok $hash, Hash, "mixed hash literal items produce a Hash";
is $hash<a>, 1, "pair entry is preserved";
is $hash<b>, 2, "quoted key/value item list entry works";
is $hash<c>, 3, "later pair entry is preserved";
is $hash.elems, 3, "all entries are present";
