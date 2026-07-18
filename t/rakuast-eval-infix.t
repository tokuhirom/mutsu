use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST Phase 5 slice 2 (ADR-0011): EVAL of infix/prefix operator nodes.
# `ApplyInfix` lowers to an internal Binary expression (the `Infix("+")` operator
# string maps back to a TokenKind). Verified against Rakudo; this file passes
# under BOTH mutsu and raku.

plan 5;

sub apply-add($a, $b) {
    RakuAST::ApplyInfix.new(
        left  => RakuAST::IntLiteral.new($a),
        infix => RakuAST::Infix.new("+"),
        right => RakuAST::IntLiteral.new($b),
    )
}

# --- EVAL a constructed ApplyInfix ------------------------------------------
is EVAL(apply-add(1, 2)), 3, 'EVAL(ApplyInfix 1 + 2) == 3';
is EVAL(RakuAST::ApplyInfix.new(
    left  => RakuAST::IntLiteral.new(4),
    infix => RakuAST::Infix.new("*"),
    right => RakuAST::IntLiteral.new(5),
)), 20, 'EVAL(ApplyInfix 4 * 5) == 20';

# --- round-trip a source expression through .AST ----------------------------
is EVAL(Q[3 * 4 + 1].AST), 13, 'EVAL(Q[3 * 4 + 1].AST) == 13';
is EVAL(Q[2 ** 8].AST), 256, 'EVAL(Q[2 ** 8].AST) == 256';
is EVAL(Q[10 - 3 - 2].AST), 5, 'EVAL(Q[10 - 3 - 2].AST) == 5';
