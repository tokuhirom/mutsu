use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# Two more write-direction gaps closed (ADR-0011): both constructs have been
# readable for a while and simply had no lowering.
#
#   * `RakuAST::Term::Reduce`  — `[+] @a` and the triangle form `[\+] @a`.
#     mutsu's `Expr::Reduction` keeps the triangle marker in the operator string
#     itself (a leading backslash), which is how the converter reads it back out
#     into the `triangle` field, so the lowerer puts it back the same way.
#   * a zero-parameter `RakuAST::PointyBlock` — `-> { … }`. The single-parameter
#     form lowers to `Expr::Lambda` and the multi-parameter form to
#     `Expr::AnonSubParams`; the arity-0 form was left as a boundary even though
#     the parser builds exactly the same `AnonSubParams` node with an empty
#     parameter list.
#
# Passes under BOTH mutsu and raku.

plan 10;

# --- reductions -------------------------------------------------------------
is EVAL(Q{my @a = 1, 2, 3; [+] @a}.AST), 6, 'a `[+]` reduction lowers';
is EVAL(Q{my @a = 2, 3, 4; [*] @a}.AST), 24, 'a `[*]` reduction lowers';
is EVAL(Q{my @a = 1, 2; [~] @a}.AST), '12', 'a `[~]` reduction lowers';
# (`Q{...}` rather than `Q[...]`: mutsu currently mis-lexes a bracketing quote
# whose content starts with the same bracket — see
# todo/tickets/q-bracket-leading-nested-delimiter.md.)
is EVAL(Q{[+] 1, 2, 3}.AST), 6, 'a reduction over a literal list lowers';

# --- the triangle form ------------------------------------------------------
is EVAL(Q{my @a = 1, 2, 3; ([\+] @a).join(",")}.AST), '1,3,6',
    'a triangle reduction lowers and keeps its running results';

# --- zero-parameter pointy blocks -------------------------------------------
is EVAL(Q[my $f = -> { 42 }; $f()].AST), 42,
    'a zero-parameter pointy block lowers and can be called';
is EVAL(Q[my $f = -> { 42 }; $f.arity].AST), 0,
    'a lowered zero-parameter pointy block has arity 0';

# --- the neighbouring forms still lower -------------------------------------
is EVAL(Q[my $f = -> $x { $x * 2 }; $f(4)].AST), 8,
    'a single-parameter pointy block still lowers';
is EVAL(Q[my $f = -> $a, $b { $a + $b }; $f(1, 2)].AST), 3,
    'a two-parameter pointy block still lowers';
is EVAL(Q[my $f = { 42 }; $f()].AST), 42,
    'a bare block still lowers';
