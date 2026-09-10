use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST Phase 5 slice 25 (ADR-0011): EVAL of positional subscripts `@a[EXPR]`.
# `ApplyPostfix` with a `Postcircumfix::ArrayIndex` postfix lowers to `Expr::Index`
# (the read side already emits it). Verified against Rakudo; passes under BOTH
# mutsu and raku.

plan 5;

# --- a variable subscript ---------------------------------------------------
is EVAL(Q{my @a = 10, 20, 30; @a[1]}.AST), 20, 'a positional subscript on a variable';

# --- the first and last elements --------------------------------------------
is EVAL(Q{my @a = 10, 20, 30; @a[0]}.AST), 10, 'the first element';
is EVAL(Q{my @a = 1, 2, 3, 4; @a[@a.elems - 1]}.AST), 4, 'a computed last-element index';

# --- subscripting an array literal ------------------------------------------
is EVAL(Q{[10, 20, 30][2]}.AST), 30, 'subscripting an array literal';

# --- subscript result drives arithmetic -------------------------------------
is EVAL(Q{my @a = 5, 6, 7; @a[0] + @a[2]}.AST), 12, 'two subscripts in an expression';
