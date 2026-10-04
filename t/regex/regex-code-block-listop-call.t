use v6;
use Test;

plan 8;

# A regex code block is lexically inside the enclosing scope, so a sub declared
# there is a listop inside the block too: `dec ~$/` is `dec(~$/)`, not
# `dec() ~ $/` (#11616). Each shape below reaches a different matcher path
# (a pattern lowered from its parse-time tree, one re-parsed from its string
# at match time, a grammar token), and every one must parse the call the same.

sub dec($x) { "got:$x" }
sub neg($x) { "neg:$x" }

my $r;

"abc" ~~ / a { $r = dec ~$/ } /;
is $r, 'got:a', 'block in a literal pattern';

$r = Nil;
"abc" ~~ /^ \w+ { $r = dec ~$/ }/;
is $r, 'got:abc', 'block after a quantified character class';

$r = Nil;
"abc" ~~ /^ \w+ <?{ $r = dec ~$/; True }>/;
is $r, 'got:abc', 'code assertion';

$r = Nil;
"abc" ~~ / \w+ { $r = neg -1 }/;
is $r, 'neg:-1', 'a glued `-` prefix is an argument too';

$r = Nil;
"abc" ~~ / [ \w <?{ $r = dec ~$/; True }> ]+ /;
is $r, 'got:abc', 'assertion inside a quantified group';

grammar G {
    token TOP { \w+ { $r = dec ~$/ } }
}
$r = Nil;
ok G.parse('xyz'), 'the grammar parses';
is $r, 'got:xyz', 'block inside a grammar token';

{
    my sub inner($x) { "inner:$x" }
    $r = Nil;
    "abc" ~~ / \w+ { $r = inner ~$/ } /;
    is $r, 'inner:abc', 'a lexical `my sub` from an inner block';
}
