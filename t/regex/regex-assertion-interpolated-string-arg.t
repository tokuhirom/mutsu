use v6.e.PREVIEW;
use experimental :rakuast;
use Test;

# An interpolating string argument of a regex assertion (`<.f("a{1}b")>`)
# lowers (#15823): the argument is spelled back with its `{...}` blocks.

plan 5;

my token declared { "a" <.panic("x{1}")> }
ok &declared.defined, 'a token whose assertion has an interpolated string argument is declared';

grammar G {
    token TOP { 'a' [ 'g' <.panic("n={1 + 1} t={'z'}")> | 'b' ] }
    method panic($msg) { die "PANIC: $msg" }
}
try G.parse('ag');
is $!.message, 'PANIC: n=2 t=z', 'the interpolated blocks are evaluated at match time';
ok G.parse('ab'), 'the other branch matches';

is EVAL(Q[my token t { 'a' <.panic("x{1}")> }; 5].AST), 5,
    'the same unit evaluates as a RakuAST tree';

my $node = Q[my token t { 'a' <.panic("x{1}")> }].AST;
ok $node ~~ RakuAST::StatementList, 'the unit reads as RakuAST';
