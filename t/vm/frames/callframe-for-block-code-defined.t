use Test;
plan 4;

my $f;
for ^1 { $f = callframe }
ok $f.code.defined, 'callframe inside a for block has a defined .code';
is $f.code.^name, 'Block', '.code is a Block';
nok $f.code ~~ Routine, 'the block frame code is not a Routine';
is $f.code.^name, 'Block', 'stable across calls';
