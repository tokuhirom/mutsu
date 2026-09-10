use v6;
use Test;

# `item` was missing from the parser's listop table, so the paren-less form
# dropped its argument: `item [1,2,3]` parsed as the bare word `item` with the
# array literal stranded, and `@vars.push: item [...]` pushed the string
# "item". `item($x)` (with parens) always worked, which is why it went
# unnoticed.
#
# rakudo's own Test.rakumod saves its module state that way
#     sub _push_vars { @vars.push: item [$num_of_tests_run, ...] }
# so every `subtest` restored garbage and the outer test counter reset to 1
# (todo/tickets/vendor-real-test-module.md).

plan 9;

is (item [1, 2, 3]).raku, '$[1, 2, 3]', 'item on an array literal itemizes it';
is (item 5), 5, 'item on a literal is the literal';
my $x = 3;
is (item $x), 3, 'item on a variable is its value';
is item(5), 5, 'the parenthesized form is unchanged';

my @v;
@v.push: item [1, 2, 3];
is @v.elems, 1, 'an itemized array pushes as ONE element';
is-deeply @(@v.pop), [1, 2, 3], 'and comes back out as the array';

# The save/restore shape from Test.rakumod: push a snapshot of several
# variables, clobber them, then restore by list assignment.
my @saved;
my ($a, $b, $c) = 1, 2, 'x';
@saved.push: item [$a, $b, $c];
($a, $b, $c) = 9, 99, 'y';
is "$a $b $c", '9 99 y', 'the variables really were clobbered';
($a, $b, $c) = @(@saved.pop);
is "$a $b $c", '1 2 x', 'and the snapshot restores every one of them';

# `item` stays available as an ordinary identifier elsewhere.
my %h = item => 1;
is %h<item>, 1, 'a bareword `item` pair key still parses as a pair';
