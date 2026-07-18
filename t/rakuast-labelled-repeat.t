use v6;
use Test;

# RakuAST Phase 2 slice 28 (ADR-0011): labelled `repeat` loops.
# mutsu wraps a labelled repeat/C-style loop in a `Stmt::Label` node (unlike
# `while`/`for`/bare-`loop`, which store the label inline); the converter
# prepends the `labels` field to the inner statement. Expected gist captured
# verbatim from Rakudo; this file passes under BOTH mutsu and raku.

plan 2;

# --- labelled repeat-while --------------------------------------------------
is Q[L: repeat { 1 } while 2].AST.gist, q:to/END/.chomp, 'labelled repeat -> labels first';
    RakuAST::StatementList.new(
      RakuAST::Statement::Loop::RepeatWhile.new(
        labels    => (
          RakuAST::Label.new(
            name => "L"
          ),
        ),
        body      => RakuAST::Block.new(
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::IntLiteral.new(1)
              )
            )
          )
        ),
        condition => RakuAST::IntLiteral.new(2)
      )
    )
    END

# --- the label name is carried through --------------------------------------
is Q[OUTER: repeat { 1 } while 5].AST.gist.contains('name => "OUTER"'), True,
    'labelled repeat carries its Label name';
