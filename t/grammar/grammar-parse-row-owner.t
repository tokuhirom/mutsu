use Test;

# `parse`, `subparse` and `parsefile` are declared on `Grammar` itself, so they are
# the last candidate of a user grammar's own override (ADR-11276 §9.46).

plan 8;

grammar G { token TOP { \d+ } }
ok G.^can('parse'), 'a grammar can parse';
ok G.^can('subparse'), 'a grammar can subparse';
ok Grammar.^can('parsefile'), 'Grammar itself can parsefile';

grammar WithParse {
    token TOP { \w+ }
    method parse(|c) { 'seen:' ~ callsame }
}
is WithParse.parse('abc'), 'seen:abc', 'callsame from a parse override reaches Grammar.parse';

grammar WithSub {
    token TOP { \d+ }
    method subparse(|c) { nextsame }
}
is ~WithSub.subparse('12ab'), '12', 'nextsame from a subparse override reaches Grammar.subparse';

grammar WithActions {
    token TOP { \d+ }
    method parse($text, |c) { nextwith($text, :actions(class { method TOP($/) { make 42 } }.new)) }
}
is WithActions.parse('7').made, 42, 'nextwith passes replacement arguments to Grammar.parse';

my $dir = $*TMPDIR.add("grammar-parse-row-$*PID");
$dir.mkdir;
my $file = $dir.add('in.txt');
$file.spurt('99');
grammar WithFile {
    token TOP { \d+ }
    method parsefile(|c) { nextsame }
}
is ~WithFile.parsefile($file.Str), '99', 'nextsame from a parsefile override reaches Grammar.parsefile';
$file.unlink;
$dir.rmdir;
ok !$dir.e, 'scratch directory removed';
