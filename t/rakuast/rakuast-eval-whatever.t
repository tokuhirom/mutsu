use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST slice 27 (ADR-0011): the `*` whatever term, both directions.
# `Expr::Whatever` <-> `RakuAST::Term::Whatever` (a bare `.new` node). Verified
# against Rakudo; passes under BOTH mutsu and raku.
#
# Note: `* + 1` (WhateverCode) desugars to a closure in mutsu and stays the
# coverage boundary; only the bare `*` term is covered here.

plan 5;

# --- read side: `*` renders as Term::Whatever -------------------------------
is Q{*}.AST.gist.chomp, q:to/END/.chomp, '`*` renders as Term::Whatever';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Term::Whatever.new
      )
    )
    END

# --- `*` closes an infinite range -------------------------------------------
is EVAL(Q{(1..*).head(3).join(",")}.AST), '1,2,3', '`1..*` is an infinite range';
is EVAL(Q{(1..*).head(5).sum}.AST), 15, 'the head of an infinite range sums';
is EVAL(Q{(10..*).head(3).join("-")}.AST), '10-11-12', 'an infinite range from 10';

# --- a bare `*` is a Whatever value -----------------------------------------
is EVAL(Q{my $w = *; $w.WHAT.^name}.AST), 'Whatever', 'a bare `*` is a Whatever';
