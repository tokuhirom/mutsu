use v6;
use experimental :rakuast;
use Test;

# RakuAST Phase 4 slice 3 (ADR-0011): multi-field construction.
# Bare operator nodes (`Infix.new("+")`) and named-field constructors
# (`ApplyInfix.new(:left, :infix, :right)`, `Statement::Expression.new(:expression)`)
# build real, nestable, queryable nodes. Verified against Rakudo; this file
# passes under BOTH mutsu and raku.

plan 5;

# --- Infix (positional operator string) -------------------------------------
is RakuAST::Infix.new("+").gist, 'RakuAST::Infix.new("+")', 'Infix.new gist round-trips';

# --- Statement::Expression (one named field) --------------------------------
is RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(1)).gist,
    q:to/END/.chomp, 'Statement::Expression.new(:expression)';
    RakuAST::Statement::Expression.new(
      expression => RakuAST::IntLiteral.new(1)
    )
    END

# --- ApplyInfix (three named fields) ----------------------------------------
my $add = RakuAST::ApplyInfix.new(
    left  => RakuAST::IntLiteral.new(1),
    infix => RakuAST::Infix.new("+"),
    right => RakuAST::IntLiteral.new(2),
);
is $add.gist, q:to/END/.chomp, 'ApplyInfix.new(:left, :infix, :right)';
    RakuAST::ApplyInfix.new(
      left  => RakuAST::IntLiteral.new(1),
      infix => RakuAST::Infix.new("+"),
      right => RakuAST::IntLiteral.new(2)
    )
    END

# --- a constructed node is queryable ----------------------------------------
is $add.left.value, 1, 'accessor reads a field of a constructed node';
ok $add ~~ RakuAST::Expression, 'a constructed ApplyInfix isa Expression';
