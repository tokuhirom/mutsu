use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# Interpolated code blocks (`"a{ $x }b"`), both directions (ADR-0011).
#
# raku renders a `{ ... }` segment of an interpolated string as a plain
# `RakuAST::Block` alongside the `StrLiteral` runs. mutsu wraps the block in a
# `DoStmt` — that is how its parser makes a block an expression — which has no
# RakuAST counterpart, so `.AST` refused the whole string.
#
# The write direction needed the mirror of that: a `Block` *segment* is
# evaluated, not a closure value, so lowering it through the ordinary expression
# path built a closure and interpolated its stringification (`"a{ $x }b"` came
# out as `ab`).
#
# Measured against rakudo 2026.07: the rendered gists are byte-for-byte
# identical. Passes under BOTH mutsu and raku.

plan 10;

# --- read side ---------------------------------------------------------------
my $g = Q{my $x = 1; "a{ $x }b"}.AST.gist;
ok $g.contains('RakuAST::QuotedString.new('), 'an interpolated string is a QuotedString';
ok $g.contains('RakuAST::Block.new('), 'a code-block segment renders as a plain Block';
nok $g.contains('StatementPrefix::Do'), 'the DoStmt wrapper is not rendered';
ok $g.contains('RakuAST::StrLiteral.new("a")') && $g.contains('RakuAST::StrLiteral.new("b")'),
    'the literal runs around the block are kept';

# --- a plain variable interpolation is unchanged -----------------------------
my $v = Q{my $x = 1; "a$x b"}.AST.gist;
ok $v.contains('RakuAST::Var::Lexical.new('), 'a `$x` segment still renders as a Var';
nok $v.contains('RakuAST::Block.new('), 'a `$x` segment is not a Block';

# --- write side --------------------------------------------------------------
is EVAL(Q{my $x = 1; "a{ $x }b"}.AST), 'a1b', 'a code block interpolates its value';
is EVAL(Q{my $x = 2; "v{ $x + 1 }"}.AST), 'v3', 'the block is evaluated, not stringified';
is EVAL(Q{"{ 1 + 1 }"}.AST), '2', 'a string that is only a code block';
is EVAL(Q{my @a = 1, 2; "e{ @a.elems }"}.AST), 'e2', 'a method call inside a block';
