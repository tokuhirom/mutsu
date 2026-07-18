use v6;
use experimental :rakuast;
use Test;

# RakuAST Phase 3 slice 2 (ADR-0011): the RakuAST type hierarchy under `~~`.
# Every node isa RakuAST::Node; a node isa any `::`-namespace ancestor of its
# class (Statement::If isa RakuAST::Statement). Verified against Rakudo; this
# file passes under BOTH mutsu and raku.

plan 6;

# --- every node isa RakuAST::Node -------------------------------------------
ok Q[42].AST ~~ RakuAST::Node, 'a StatementList isa RakuAST::Node';
ok Q[42].AST.statements[0].expression ~~ RakuAST::Node, 'a leaf node isa RakuAST::Node';

# --- exact class match ------------------------------------------------------
ok Q[42].AST.statements[0].expression ~~ RakuAST::IntLiteral, 'IntLiteral ~~ RakuAST::IntLiteral';
nok Q[42].AST.statements[0].expression ~~ RakuAST::StrLiteral, 'IntLiteral !~~ RakuAST::StrLiteral';

# --- `::`-namespace ancestor ------------------------------------------------
ok Q[if 1 { 2 }].AST.statements[0] ~~ RakuAST::Statement,
    'Statement::If isa RakuAST::Statement';

# --- the `::` boundary avoids a false StatementList ~~ Statement -------------
nok Q[42].AST ~~ RakuAST::Statement, 'StatementList is NOT a RakuAST::Statement';
