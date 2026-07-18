use v6;
use Test;

# RakuAST Phase 2 slice 19 (ADR-0011): the ternary `?? !!` operator.
# `COND ?? THEN !! ELSE` -> Ternary(condition, then, else). Expected gists
# captured verbatim from Rakudo; this file passes under BOTH mutsu and raku.
#
# Note: raku constant-folds a literal-condition ternary (`1 ?? 2 !! 3` folds to
# IntLiteral(2)); mutsu does not (a documented divergence, the const-fold open
# question), so only non-constant conditions are pinned here.

plan 2;

# --- non-constant condition -------------------------------------------------
is Q[my $x; $x ?? 2 !! 3].AST.gist, q:to/END/.chomp, 'ternary -> Ternary(condition, then, else)';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
          sigil       => "\$",
          desigilname => RakuAST::Name.from-identifier("x")
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Ternary.new(
          condition => RakuAST::Var::Lexical.new("\$x"),
          then      => RakuAST::IntLiteral.new(2),
          else      => RakuAST::IntLiteral.new(3)
        )
      )
    )
    END

# --- branches can be arbitrary expressions ----------------------------------
my $t = Q[my $a; my $b; $a ?? $b !! $a].AST.gist;
ok $t.contains('RakuAST::Ternary.new(')
    && $t.contains('condition => RakuAST::Var::Lexical.new("\$a")')
    && $t.contains('then      => RakuAST::Var::Lexical.new("\$b")')
    && $t.contains('else      => RakuAST::Var::Lexical.new("\$a")'),
    'ternary branches carry their operand expressions';
