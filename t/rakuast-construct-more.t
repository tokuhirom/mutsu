use v6;
use experimental :rakuast;
use Test;

# RakuAST Phase 4 slice 4 (ADR-0011): more constructors — Var::Lexical,
# ApplyPrefix, ApplyPostfix, Postfix. Verified against Rakudo; this file passes
# under BOTH mutsu and raku.

plan 4;

# --- Var::Lexical (positional) ----------------------------------------------
is RakuAST::Var::Lexical.new("\$x").gist, 'RakuAST::Var::Lexical.new("\$x")',
    'Var::Lexical.new gist round-trips';

# --- ApplyPrefix (prefix, operand) ------------------------------------------
is RakuAST::ApplyPrefix.new(
    prefix  => RakuAST::Prefix.new("-"),
    operand => RakuAST::IntLiteral.new(5),
).gist, q:to/END/.chomp, 'ApplyPrefix.new(:prefix, :operand)';
    RakuAST::ApplyPrefix.new(
      prefix  => RakuAST::Prefix.new("-"),
      operand => RakuAST::IntLiteral.new(5)
    )
    END

# --- ApplyPostfix + Postfix (operator is a NAMED field) ---------------------
is RakuAST::ApplyPostfix.new(
    operand => RakuAST::IntLiteral.new(5),
    postfix => RakuAST::Postfix.new(operator => "++"),
).gist, q:to/END/.chomp, 'ApplyPostfix.new(:operand, :postfix) with Postfix(:operator)';
    RakuAST::ApplyPostfix.new(
      operand => RakuAST::IntLiteral.new(5),
      postfix => RakuAST::Postfix.new(
        operator => "++"
      )
    )
    END

# --- a constructed node is queryable ----------------------------------------
is RakuAST::ApplyPrefix.new(
    prefix  => RakuAST::Prefix.new("-"),
    operand => RakuAST::IntLiteral.new(5),
).operand.value, 5, 'accessor reads the operand of a constructed ApplyPrefix';
