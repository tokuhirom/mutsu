use v6;
use experimental :rakuast;
use Test;

# `PRE` / `POST` phasers: rakudo calls the block of `PRE { COND }` from a
# statement (`ApplyPostfix(Block, Call::Term)`), wraps the block of `POST {
# COND }` as it is, and wraps the statement of a bare `PRE COND` (measured on
# rakudo 2026.09). Each EVALs back to a phaser that checks its condition.

plan 10;

sub text($node) { $node.raku.lines.map(*.trim).join(' ') }
sub phaser(Str $src) { $src.AST.statements[0].expression }

is phaser('PRE { 1 }').^name, 'RakuAST::StatementPrefix::Phaser::Pre', 'PRE is a Phaser::Pre';
is phaser('POST { 1 }').^name, 'RakuAST::StatementPrefix::Phaser::Post', 'POST is a Phaser::Post';
is text(phaser('PRE { 1 }')),
    'RakuAST::StatementPrefix::Phaser::Pre.new( RakuAST::Statement::Expression.new( expression => RakuAST::ApplyPostfix.new( operand => RakuAST::Block.new( body => RakuAST::Blockoid.new( RakuAST::StatementList.new( RakuAST::Statement::Expression.new( expression => RakuAST::IntLiteral.new(1) ) ) ) ), postfix => RakuAST::Call::Term.new ) ) )',
    'the block of PRE is called';
is phaser('POST { 1 }').blorst.^name, 'RakuAST::Block', 'the block of POST is the block';
is text(phaser('PRE 0')),
    'RakuAST::StatementPrefix::Phaser::Pre.new( RakuAST::Statement::Expression.new( expression => RakuAST::IntLiteral.new(0) ) )',
    'a bare PRE wraps the statement';

is Q|sub f($x) { PRE { $x > 0 }; $x * 2 }; f(2)|.AST.EVAL, 4, 'a satisfied PRE EVALs';
is Q|sub f($x) { POST { $_ > 3 }; return $x * 2 }; f(2)|.AST.EVAL, 4, 'a satisfied POST EVALs';

my $failed = Q|sub f($x) { PRE { $x > 0 }; $x }; f(-1)|;
throws-like { $failed.AST.EVAL }, X::Phaser::PrePost, 'a failing PRE dies';
try $failed.AST.EVAL;
is $!.condition.trim, '{ $x > 0 }', 'with the condition as written';
throws-like { Q|sub g { POST { False }; 1 }; g()|.AST.EVAL }, X::Phaser::PrePost,
    'a failing POST dies';
