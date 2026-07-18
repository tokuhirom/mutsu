use v6;
use Test;

# RakuAST Phase 3 slice 1 (ADR-0011): node accessors.
# A RakuAST node exposes its fields as 0-arg accessor methods (`.condition`,
# `.expression`, `.infix`, `.sigil`, ...) and a `StatementList` exposes its
# children via `.statements` (a List). This lets you walk the tree
# programmatically, not just render it. Verified against Rakudo; this file
# passes under BOTH mutsu and raku.

plan 7;

# --- .statements returns a List of the top-level statements -----------------
is Q[42].AST.statements.^name, 'List', '.statements returns a List';
is Q[42].AST.statements.elems, 1, 'single statement -> one element';

# --- descend into a statement's expression ----------------------------------
is Q[42].AST.statements[0].^name, 'RakuAST::Statement::Expression',
    'first statement is a Statement::Expression';
is Q[42].AST.statements[0].expression.^name, 'RakuAST::IntLiteral',
    '.expression accessor returns the IntLiteral';

# --- named accessors on a Statement::If -------------------------------------
is Q[if 1 { 2 }].AST.statements[0].condition.^name, 'RakuAST::IntLiteral',
    '.condition accessor on Statement::If';
is Q[if 1 { 2 }].AST.statements[0].then.^name, 'RakuAST::Block',
    '.then accessor returns the then-Block';

# --- a leaf accessor returns the raw value ----------------------------------
is Q[my $x = 5].AST.statements[0].expression.sigil, '$',
    '.sigil accessor returns the sigil string';
