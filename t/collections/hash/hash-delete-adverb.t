use Test;

plan 8;

my %h = (a => 1, b => 2);
is ~(%h<a>:delete), "1", "stringified delete works on angle index";
ok !(%h<a>:exists), "deleted key no longer exists";

my %h2 = (a => 7);
my $deleted = ~(%h2<a>:delete);
is $deleted, "7", "parenthesized delete expression parses and returns deleted value";
ok !(%h2<a>:exists), "parenthesized delete removes key";

my %h3 = (a => 10);
is ~(%h3<a>:!delete), "10", ":!delete returns value without deleting";
ok %h3<a>:exists, ":!delete keeps key";

my %h4 = (a => 1, b => 2);
is +(%h4{*}:delete(0)), 2, ":delete(0) returns values without deleting";
is +%h4, 2, ":delete(0) keeps keys intact";
