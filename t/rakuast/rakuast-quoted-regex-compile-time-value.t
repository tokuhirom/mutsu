use Test;

plan 5;

# `RakuAST::QuotedRegex.compile-time-value` is the `Regex` the node denotes.

my $q = Q|/^ a ** 2 $/|.AST.statements[0].expression;
my $r = $q.compile-time-value;
isa-ok $r, Regex, 'a Regex';
ok 'aa' ~~ $r, 'matches what the regex matches';
nok 'aaa' ~~ $r, 'and not what it does not';

my $alt = Q{/ foo | bar /}.AST.statements[0].expression.compile-time-value;
is ~('xbarx' ~~ $alt), 'bar', 'alternation';
my $cap = Q|/ (\d+) '-' (\d+) /|.AST.statements[0].expression.compile-time-value;
is ('12-34' ~~ $cap)[1], '34', 'captures';
