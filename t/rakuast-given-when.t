use v6;
use Test;

# RakuAST Phase 2 slice 18 (ADR-0011): given / when / default.
# `given X { when Y { } default { } }` -> Statement::Given(source, topic Block)
# containing Statement::When(condition, Block) and Statement::Default(Block).
# Expected gists captured verbatim from Rakudo; this file passes under BOTH
# mutsu and raku.

plan 4;

# --- given + when -----------------------------------------------------------
is Q[given 1 { when 2 { 3 } }].AST.gist, q:to/END/.chomp, 'given/when -> Given(source, topic Block) + When';
    RakuAST::StatementList.new(
      RakuAST::Statement::Given.new(
        source => RakuAST::IntLiteral.new(1),
        body   => RakuAST::Block.new(
          implicit-topic => True,
          required-topic => 1,
          body           => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::When.new(
                condition => RakuAST::IntLiteral.new(2),
                body      => RakuAST::Block.new(
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
          )
        )
      )
    )
    END

# --- given topic Block is topic-marked --------------------------------------
is Q[given 1 { when 2 { 3 } }].AST.gist.contains('implicit-topic => True'), True,
    'given body is a topic-taking Block';

# --- default clause ---------------------------------------------------------
is Q[given 1 { default { 4 } }].AST.gist.contains('RakuAST::Statement::Default.new('), True,
    'default -> Statement::Default';

# --- a `when` body is a plain Block (not topic-marked) ----------------------
my $g = Q[given 1 { when 2 { 3 } }].AST.gist;
ok $g.contains('RakuAST::Statement::When.new(')
    && $g.contains('condition => RakuAST::IntLiteral.new(2)')
    && $g.index('RakuAST::Statement::When') < $g.index('IntLiteral.new(3)'),
    'when -> Statement::When(condition, body)';
