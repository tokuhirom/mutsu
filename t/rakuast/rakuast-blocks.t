use v6;
use Test;

# RakuAST Phase 2 slice 3 (ADR-0011): bare blocks and pointy blocks
# (Block, Blockoid, PointyBlock, Signature, Parameter, ParameterTarget::Var).
# Expected gists captured verbatim from Rakudo; this file passes under BOTH
# mutsu and raku, so raku is the oracle.

plan 6;

# --- bare block -------------------------------------------------------------
is Q[{ 42 }].AST.gist, q:to/END/.chomp, 'bare block -> Block(body => Blockoid)';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Block.new(
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::IntLiteral.new(42)
              )
            )
          )
        )
      )
    )
    END

# --- single-parameter pointy block ------------------------------------------
is Q[-> $x { $x }].AST.gist, q:to/END/.chomp, 'pointy block -> PointyBlock + Signature';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::PointyBlock.new(
          signature => RakuAST::Signature.new(
            parameters => (
              RakuAST::Parameter.new(
                target   => RakuAST::ParameterTarget::Var.new(
                  name => "\$x"
                ),
                optional => False
              ),
            )
          ),
          body      => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::Var::Lexical.new("\$x")
              )
            )
          )
        )
      )
    )
    END

# --- zero-parameter pointy block omits the signature field ------------------
is Q[-> { 1 }].AST.gist, q:to/END/.chomp, 'zero-param pointy block omits signature';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::PointyBlock.new(
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

# --- multi-parameter pointy: a default renders `default =>`, no `optional` ---
# The per-node alignment width differs between the two Parameters (the required
# one pads to "optional" = 8, the defaulted one to "default" = 7).
is Q[-> $a, $b = 7 { $a }].AST.gist, q:to/END/.chomp, 'default param -> default field, alignment per node';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::PointyBlock.new(
          signature => RakuAST::Signature.new(
            parameters => (
              RakuAST::Parameter.new(
                target   => RakuAST::ParameterTarget::Var.new(
                  name => "\$a"
                ),
                optional => False
              ),
              RakuAST::Parameter.new(
                target  => RakuAST::ParameterTarget::Var.new(
                  name => "\$b"
                ),
                default => RakuAST::IntLiteral.new(7)
              ),
            )
          ),
          body      => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::Var::Lexical.new("\$a")
              )
            )
          )
        )
      )
    )
    END

# --- a block body carries a full StatementList (declaration + expression) ----
is Q[{ my $x = 1; $x }].AST.gist, q:to/END/.chomp, 'block body is a multi-statement StatementList';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Block.new(
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::VarDeclaration::Simple.new(
                  sigil       => "\$",
                  desigilname => RakuAST::Name.from-identifier("x"),
                  initializer => RakuAST::Initializer::Assign.new(
                    RakuAST::IntLiteral.new(1)
                  )
                )
              ),
              RakuAST::Statement::Expression.new(
                expression => RakuAST::Var::Lexical.new("\$x")
              )
            )
          )
        )
      )
    )
    END

# --- a method call inside a pointy block body -------------------------------
is Q[-> $x { $x.abs }].AST.gist, q:to/END/.chomp, 'method call nests in pointy block body';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::PointyBlock.new(
          signature => RakuAST::Signature.new(
            parameters => (
              RakuAST::Parameter.new(
                target   => RakuAST::ParameterTarget::Var.new(
                  name => "\$x"
                ),
                optional => False
              ),
            )
          ),
          body      => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyPostfix.new(
                  operand => RakuAST::Var::Lexical.new("\$x"),
                  postfix => RakuAST::Call::Method.new(
                    name => RakuAST::Name.from-identifier("abs")
                  )
                )
              )
            )
          )
        )
      )
    )
    END
