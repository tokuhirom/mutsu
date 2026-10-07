use v6;
use experimental :rakuast;
use Test;

# `nqp::const::NAME` and an `nqp::op` written without parentheses cross the
# RakuAST boundary: rakudo renders them as `Nqp::Const` and `Nqp` (measured on
# rakudo 2026.09), and each EVALs back to what the name means.

plan 8;

sub expr(Str $src) { $src.AST.statements[1].expression }

is expr(Q|use nqp; nqp::const::CCLASS_WORD|).^name, 'RakuAST::Nqp::Const',
    'nqp::const::NAME is an Nqp::Const';
is expr(Q|use nqp; nqp::const::CCLASS_WORD|).raku.lines.join(' ').trim,
    'RakuAST::Nqp::Const.new("CCLASS_WORD")', 'with the constant name as its only positional';
is expr(Q|use nqp; nqp::time|).^name, 'RakuAST::Nqp', 'nqp::op without parentheses is an Nqp';
is expr(Q|use nqp; nqp::time|).raku.lines.join(' ').trim, 'RakuAST::Nqp.new("time")',
    'with just the op name';
is expr(Q|use nqp; nqp::time()|).raku.lines.join(' ').trim, 'RakuAST::Nqp.new("time")',
    'the parenthesised spelling is the same node';

is Q|use nqp; nqp::const::CCLASS_WORD|.AST.EVAL, 8192, 'a constant EVALs';
is Q|use nqp; nqp::const::CCLASS_WORD + 1|.AST.EVAL, 8193, 'a constant in an expression';
ok Q|use nqp; (nqp::time) > 0|.AST.EVAL, 'an op without arguments EVALs';
