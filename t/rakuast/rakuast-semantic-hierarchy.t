use v6;
use experimental :rakuast;
use Test;

# RakuAST Phase 3 slice 4 (ADR-0011): the semantic Expression/Term hierarchy.
# Beyond the base RakuAST::Node and `::`-namespace ancestors, expression nodes
# also isa RakuAST::Expression, and the term subset (literals, variables, …)
# also isa RakuAST::Term. Verified against Rakudo; this file passes under BOTH
# mutsu and raku.

plan 8;

my $v;

# --- literals are Terms (and Expressions) -----------------------------------
ok Q[42].AST.statements[0].expression ~~ RakuAST::Expression, 'IntLiteral isa Expression';
ok Q[42].AST.statements[0].expression ~~ RakuAST::Term, 'IntLiteral isa Term';
ok Q["x"].AST.statements[0].expression ~~ RakuAST::Term, 'QuotedString isa Term';

# --- variables are Terms ----------------------------------------------------
ok Q[my $v; $v].AST.statements[1].expression ~~ RakuAST::Term, 'Var::Lexical isa Term';

# --- operator applications are Expressions but NOT Terms --------------------
ok Q[my $v; $v + 1].AST.statements[1].expression ~~ RakuAST::Expression, 'ApplyInfix isa Expression';
nok Q[my $v; $v + 1].AST.statements[1].expression ~~ RakuAST::Term, 'ApplyInfix is NOT a Term';
ok Q[my $v; $v ?? 1 !! 2].AST.statements[1].expression ~~ RakuAST::Expression, 'Ternary isa Expression';

# --- non-expression nodes are neither ---------------------------------------
nok Q[say 1].AST.statements[0].expression.name ~~ RakuAST::Expression,
    'a Name is NOT an Expression';
