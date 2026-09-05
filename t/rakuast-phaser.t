use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# Phasers in both RakuAST directions (ADR-0011).
#
# raku gives each phaser kind its own class,
# `RakuAST::StatementPrefix::Phaser::<Kind>`, wrapping the block positionally.
# mutsu's single `Stmt::Phaser { kind, .. }` maps onto them 1:1, so the whole
# family lands in one slice. Measured against rakudo 2026.07: the rendered gists
# are byte-for-byte identical.
#
# `PRE`/`POST` are the exception on the read side and stay a boundary: rakudo
# desugars them into a call *around* the block (the phaser's child is an
# `ApplyPostfix`, not a `Block`), and mutsu additionally keeps the condition's
# source text for the `X::Phaser::PrePost` message.
#
# `BEGIN` lowers like the rest since both EVAL carriers gained the mainline's
# own compile-time BEGIN pass — see t/eval-begin-phaser.t, which pins that
# behaviour for both the string and the RakuAST spelling.
#
# Passes under BOTH mutsu and raku.

plan 16;

sub phaser-class($src) {
    $src.AST.statements[0].expression.^name
}

# --- read side: one class per kind ------------------------------------------
is phaser-class(Q{BEGIN { 1 }}), 'RakuAST::StatementPrefix::Phaser::Begin', 'BEGIN';
is phaser-class(Q{CHECK { 1 }}), 'RakuAST::StatementPrefix::Phaser::Check', 'CHECK';
is phaser-class(Q{INIT { 1 }}),  'RakuAST::StatementPrefix::Phaser::Init',  'INIT';
is phaser-class(Q{END { 1 }}),   'RakuAST::StatementPrefix::Phaser::End',   'END';
is phaser-class(Q{ENTER { 1 }}), 'RakuAST::StatementPrefix::Phaser::Enter', 'ENTER';
is phaser-class(Q{LEAVE { 1 }}), 'RakuAST::StatementPrefix::Phaser::Leave', 'LEAVE';
is phaser-class(Q{KEEP { 1 }}),  'RakuAST::StatementPrefix::Phaser::Keep',  'KEEP';
is phaser-class(Q{UNDO { 1 }}),  'RakuAST::StatementPrefix::Phaser::Undo',  'UNDO';

# --- the block is a positional child ----------------------------------------
ok Q{INIT { 1 }}.AST.gist.contains('RakuAST::StatementPrefix::Phaser::Init.new(')
    && Q{INIT { 1 }}.AST.gist.contains('RakuAST::Block.new('),
    'a phaser wraps its block positionally';

# --- write side: the phaser still fires at its phase -------------------------
is EVAL(Q{my $x = 0; sub f { ENTER { $x = 2 }; 0 }; f(); $x}.AST), 2,
    'a lowered ENTER runs on entry';
is EVAL(Q{my $x = 0; sub f { LEAVE { $x = 3 }; 0 }; f(); $x}.AST), 3,
    'a lowered LEAVE runs on exit';
is EVAL(Q{my $n = 0; for 1..3 { FIRST { $n = $n + 10 }; $n = $n + 1 }; $n}.AST), 13,
    'a lowered FIRST runs once, before the first iteration';
is EVAL(Q{my $n = 0; for 1..3 { NEXT { $n = $n + 10 }; $n = $n + 1 }; $n}.AST), 33,
    'a lowered NEXT runs after each iteration';

# --- the compile-time phasers keep their pre-mainline ordering --------------
# `CHECK`/`INIT` run before the mainline, so the later `my $x = 0` wins.
is EVAL(Q{my $x = 0; INIT { $x = 1 }; $x}.AST), 0,
    'a lowered INIT runs before the mainline, not in statement position';
is EVAL(Q{my $x = 0; CHECK { $x = 1 }; $x}.AST), 0,
    'a lowered CHECK runs before the mainline too';
is EVAL(Q{my $x = 0; BEGIN { $x = 1 }; $x}.AST), 0,
    'a lowered BEGIN runs at compile time, not in statement position';
