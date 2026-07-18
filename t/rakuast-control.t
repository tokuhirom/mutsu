use v6;
use Test;

# RakuAST Phase 2 slice 4 (ADR-0011): conditionals and loops
# (Statement::If, Statement::Loop::While, Statement::Loop).
# Expected gists captured verbatim from Rakudo; this file passes under BOTH
# mutsu and raku, so raku is the oracle.
#
# Note: mutsu desugars `unless X` to `if !X` and `until X` to `while !X`, so
# those render as Statement::If / Loop::While with a negated condition (a
# documented divergence from raku, ADR-0011) and are intentionally not pinned
# here — this file only asserts constructs mutsu and raku render identically.

plan 5;

# --- if without else --------------------------------------------------------
is Q[if 1 { 2 }].AST.gist, q:to/END/.chomp, 'if -> Statement::If(condition, then)';
    RakuAST::StatementList.new(
      RakuAST::Statement::If.new(
        condition => RakuAST::IntLiteral.new(1),
        then      => RakuAST::Block.new(
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::IntLiteral.new(2)
              )
            )
          )
        )
      )
    )
    END

# --- if with else -----------------------------------------------------------
is Q[if 1 { 2 } else { 3 }].AST.gist, q:to/END/.chomp, 'if/else -> Statement::If(condition, then, else)';
    RakuAST::StatementList.new(
      RakuAST::Statement::If.new(
        condition => RakuAST::IntLiteral.new(1),
        then      => RakuAST::Block.new(
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::IntLiteral.new(2)
              )
            )
          )
        ),
        else      => RakuAST::Block.new(
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::IntLiteral.new(3)
              )
            )
          )
        )
      )
    )
    END

# --- while ------------------------------------------------------------------
is Q[while 1 { 2 }].AST.gist, q:to/END/.chomp, 'while -> Statement::Loop::While(condition, body)';
    RakuAST::StatementList.new(
      RakuAST::Statement::Loop::While.new(
        condition => RakuAST::IntLiteral.new(1),
        body      => RakuAST::Block.new(
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::IntLiteral.new(2)
              )
            )
          )
        )
      )
    )
    END

# --- bare loop --------------------------------------------------------------
is Q[loop { 1 }].AST.gist, q:to/END/.chomp, 'bare loop -> Statement::Loop(body)';
    RakuAST::StatementList.new(
      RakuAST::Statement::Loop.new(
        body => RakuAST::Block.new(
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::IntLiteral.new(1)
              )
            )
          )
        )
      )
    )
    END

# --- condition and body carry arbitrary expressions -------------------------
is Q[my $x; while $x { $x.abs }].AST.gist.contains(q:to/FRAG/.chomp), True, 'loop condition/body nest expressions';
      RakuAST::Statement::Loop::While.new(
        condition => RakuAST::Var::Lexical.new("\$x"),
    FRAG
