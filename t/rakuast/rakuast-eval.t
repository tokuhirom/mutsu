use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST Phase 5 slice 1 (ADR-0011): EVAL of a RakuAST node lowers it to the
# internal AST and runs it through the existing evaluator — no new engine.
# Slice 1 covers the literal cluster. Verified against Rakudo; this file passes
# under BOTH mutsu and raku.

plan 4;

# --- EVAL a constructed literal ---------------------------------------------
is EVAL(RakuAST::IntLiteral.new(42)), 42, 'EVAL(IntLiteral.new(42)) == 42';
is EVAL(RakuAST::StrLiteral.new("hi")), 'hi', 'EVAL(StrLiteral.new("hi")) == "hi"';
is EVAL(RakuAST::RatLiteral.new(3.5)), 3.5, 'EVAL(RatLiteral.new(3.5)) == 3.5';

# --- round-trip: source -> .AST -> EVAL -------------------------------------
is EVAL(Q[42].AST), 42, 'EVAL(Q[42].AST) round-trips to 42';
