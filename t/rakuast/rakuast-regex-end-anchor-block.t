use Test;

plan 5;

ok so "x" ~~ / x ${ True } /, 'end anchor followed by a code block matches at the end';
nok so "xa" ~~ / x ${ True } /, 'end anchor rejects a suffix before the code block';

my $ast = Q[so "x" ~~ / x ${ True } /].AST;
my $dump = $ast.dump;
ok $dump.contains('Regex::Anchor::EndOfString'), 'the RakuAST regex retains the end anchor';
ok $dump.contains('Regex::Block'), 'the RakuAST regex retains the following code block';
ok $ast.EVAL, 'the round-tripped RakuAST expression matches';
