use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST slice 28 (ADR-0011): fat-arrow pairs `a => 1`, both directions. A
# parenthesised/method-context pair is `Expr::PositionalPair` over a `FatArrow`
# binop internally, and raku models it as a `RakuAST::FatArrow(key, value)` node.
# The read side converts it and the write side lowers it back. Verified against
# Rakudo; passes under BOTH mutsu and raku.
#
# Note: mutsu renders a *bare* `a => 1` statement as an `ApplyInfix` (matching the
# call-argument form) rather than a `FatArrow` node, so this file checks the
# round-tripped *values*, which agree, rather than the gist.

plan 5;

# --- the key and value of a pair --------------------------------------------
is EVAL(Q{(a => 1).key}.AST), 'a', 'a pair exposes its key';
is EVAL(Q{(a => 5).value}.AST), 5, 'a pair exposes its value';

# --- a string-literal key ---------------------------------------------------
is EVAL(Q{("x" => 10).value}.AST), 10, 'a string-literal key';

# --- the value may be a full expression -------------------------------------
is EVAL(Q{(a => 3 + 4).value}.AST), 7, 'the pair value is an expression';

# --- .kv flattens to key and value ------------------------------------------
is EVAL(Q{(a => 1).kv.join(",")}.AST), 'a,1', '.kv yields the key and value';
