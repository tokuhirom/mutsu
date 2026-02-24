use Test;

plan 4;

my %h = (a => 1, b => 2);
is ~(%h<a>:delete), "1", "stringified delete works on angle index";
ok !(%h<a>:exists), "deleted key no longer exists";

my %h2 = (a => 7);
my $deleted = ~(%h2<a>:delete);
is $deleted, "7", "parenthesized delete expression parses and returns deleted value";
ok !(%h2<a>:exists), "parenthesized delete removes key";
