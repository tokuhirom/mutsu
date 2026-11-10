use Test;
use experimental :rakuast;

plan 4;

# `Any:U ::T` (a type capture with no variable) keeps its nominal type and
# smiley in .AST, and EVAL of the tree dispatches on the smiley.
my $ast = Q[sub f(Any:U ::T) { 1 }].AST;
my $gist = $ast.gist;
ok $gist.contains('RakuAST::Type::Definedness'), 'the smiley is a Definedness type';
ok $gist.contains('definite  => False'), ':U is definite => False';
ok $gist.contains('RakuAST::Type::Capture'), 'the capture is kept';

my $src = Q[multi sub g(Any:U ::Type) { "type" }
multi sub g(&c) { "callable" }
g(-> { 1 })];
is EVAL($src.AST), 'callable', 'a code argument does not pick the :U candidate';
