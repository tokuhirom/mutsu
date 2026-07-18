use v6;
use Test;

# RakuAST Phase 2 slice 5 (ADR-0011): elsif chains.
# mutsu nests each `elsif` as a single `if` inside the else-branch; this
# flattens the chain into raku's `elsifs` list (Statement::Elsif), with any
# trailing `else` block after the last `elsif`. Expected gists captured
# verbatim from Rakudo; this file passes under BOTH mutsu and raku.

plan 3;

# --- single elsif, no else --------------------------------------------------
is Q[if 1 {2} elsif 3 {4}].AST.gist, q:to/END/.chomp, 'if/elsif -> elsifs list';
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
        elsifs    => (
          RakuAST::Statement::Elsif.new(
            condition => RakuAST::IntLiteral.new(3),
            then      => RakuAST::Block.new(
              body => RakuAST::Blockoid.new(
                RakuAST::StatementList.new(
                  RakuAST::Statement::Expression.new(
                    expression => RakuAST::IntLiteral.new(4)
                  )
                )
              )
            )
          ),
        )
      )
    )
    END

# --- elsif followed by a trailing else --------------------------------------
is Q[if 1 {2} elsif 3 {4} else {5}].AST.gist, q:to/END/.chomp, 'if/elsif/else -> elsifs then else';
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
        elsifs    => (
          RakuAST::Statement::Elsif.new(
            condition => RakuAST::IntLiteral.new(3),
            then      => RakuAST::Block.new(
              body => RakuAST::Blockoid.new(
                RakuAST::StatementList.new(
                  RakuAST::Statement::Expression.new(
                    expression => RakuAST::IntLiteral.new(4)
                  )
                )
              )
            )
          ),
        ),
        else      => RakuAST::Block.new(
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::IntLiteral.new(5)
              )
            )
          )
        )
      )
    )
    END

# --- two elsifs render as two elements of the flat list ---------------------
is Q[if 1 {2} elsif 3 {4} elsif 5 {6} else {7}].AST.gist.comb(/'RakuAST::Statement::Elsif.new'/).elems, 2,
    'two elsifs -> two Statement::Elsif nodes in the list';
