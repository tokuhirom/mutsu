use Test;

# `List.ACCEPTS` compares the topic's `.list`, and a Hash's `.list` is its
# pairs, so `%h ~~ ()` is True for an empty hash. A Hash topic used to be
# treated as non-iterable, so it never matched a list (Path::Map's
# `ok $match.variables ~~ ()`).

plan 6;

my %empty;
ok %empty ~~ (), 'an empty Hash matches the empty list';
ok {} ~~ (), 'so does an empty hash literal';
nok { a => 1 } ~~ (), 'a non-empty one does not';
my %one = a => 1;
ok %one ~~ (a => 1,), 'a one-pair Hash matches its pair';
nok %one ~~ (b => 1,), 'and not another pair';
nok 5 ~~ (), 'a non-iterable topic still does not match';
