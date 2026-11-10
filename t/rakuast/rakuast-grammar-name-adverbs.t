use Test;
use experimental :rakuast;

plan 6;

# `grammar G:ver<..>:auth<..>` keeps the name's adverbs in `.AST` (as a class
# does) and in the grammar's meta data.
my $ast = Q[grammar G1:ver<1.0>:auth<me> { token TOP { a } }].AST;
my $gist = $ast.gist;
ok $gist.contains('colonpairs'), 'the name carries colonpairs';
ok $gist.contains('key   => "ver"'), 'the ver pair is there';
ok $gist.contains('key   => "auth"'), 'the auth pair is there';

is EVAL(Q[grammar G2:ver<2.0>:auth<you> { token TOP { a } }; G2.^ver.Str].AST), '2.0', 'EVAL of the tree sets .^ver';
is EVAL(Q[grammar G3:auth<you> { token TOP { a } }; G3.^auth].AST), 'you', 'and .^auth';
is EVAL(Q[grammar G4:ver<1.0> { token TOP { a } }; G4.parse("a").so].AST), True, 'the grammar still parses';
