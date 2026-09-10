use v6;
use Test;

# RakuAST Phase 2 slice 8 (ADR-0011): C-style and repeat loops
# (Statement::Loop with setup/condition/increment, Statement::Loop::RepeatWhile).
# Expected gists captured verbatim from Rakudo; this file passes under BOTH
# mutsu and raku.
#
# Note: mutsu desugars `repeat { } until X` to a negated `while` condition, so
# it renders as Loop::RepeatWhile with a `!`-prefixed condition (a documented
# divergence from raku's Loop::RepeatUntil, ADR-0011) and is not pinned here.

plan 4;

# --- C-style loop: setup / condition / increment / body ---------------------
is Q[loop (my $i = 0; $i < 3; $i++) { $i }].AST.gist, q:to/END/.chomp, 'C-style loop -> Statement::Loop(setup, condition, increment, body)';
    RakuAST::StatementList.new(
      RakuAST::Statement::Loop.new(
        setup     => RakuAST::VarDeclaration::Simple.new(
          sigil       => "\$",
          desigilname => RakuAST::Name.from-identifier("i"),
          initializer => RakuAST::Initializer::Assign.new(
            RakuAST::IntLiteral.new(0)
          )
        ),
        condition => RakuAST::ApplyInfix.new(
          left  => RakuAST::Var::Lexical.new("\$i"),
          infix => RakuAST::Infix.new("<"),
          right => RakuAST::IntLiteral.new(3)
        ),
        increment => RakuAST::ApplyPostfix.new(
          operand => RakuAST::Var::Lexical.new("\$i"),
          postfix => RakuAST::Postfix.new(
            operator => "++"
          )
        ),
        body      => RakuAST::Block.new(
          body => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::Var::Lexical.new("\$i")
              )
            )
          )
        )
      )
    )
    END

# --- C-style loop with an un-initialised setup omits the initializer --------
is Q[loop (my $i; $i < 3; $i++) { $i }].AST.gist.contains(q:to/FRAG/.chomp), True, 'no-init setup omits initializer';
        setup     => RakuAST::VarDeclaration::Simple.new(
          sigil       => "\$",
          desigilname => RakuAST::Name.from-identifier("i")
        ),
    FRAG

# --- repeat-while loop: body before condition -------------------------------
is Q[repeat { 1 } while 2].AST.gist, q:to/END/.chomp, 'repeat/while -> Loop::RepeatWhile(body, condition)';
    RakuAST::StatementList.new(
      RakuAST::Statement::Loop::RepeatWhile.new(
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

# --- a bare `loop { }` is unchanged (no setup/condition/increment) -----------
is Q[loop { 1 }].AST.gist.contains('RakuAST::Statement::Loop.new('), True,
    'bare loop still renders as plain Statement::Loop';
