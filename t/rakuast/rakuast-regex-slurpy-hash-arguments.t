use v6;
use experimental :rakuast;
use Test;

plan 4;

my $ast = Q[grammar RakuASTSlurpyHash {
    token TOP { <.flatnamed(|{ c => 6, d => 7 })> }
    token flatnamed(:$c, :$d) { c }
}].AST;

ok $ast.gist.contains('RakuAST::ApplyPrefix.new'),
    'the regex argument keeps the flattened prefix application';
ok $ast.gist.contains('RakuAST::Block.new'),
    'the flattened hash composer keeps its block wrapper';

if $*VM.name eq 'mutsu' {
    my $grammar = EVAL($ast);
    ok $grammar.parse('c').defined,
        'the flattened named arguments round-trip through the regex subrule';
    ok !$grammar.parse('x').defined,
        'the reconstructed subrule still enforces its regex body';
} else {
    skip 'Rakudo currently evaluates this hand-built regex argument differently', 2;
}
