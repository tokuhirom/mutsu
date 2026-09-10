use v6;
use Test;

# RakuAST Phase 2 slice 6 (ADR-0011): implicit-topic `for` loops.
# A `for SRC { ... }` with no explicit signature -> Statement::For(mode,
# source, body), where the body is a topic-taking Block marked
# `implicit-topic => True` / `required-topic => 1`. Expected gists captured
# verbatim from Rakudo; this file passes under BOTH mutsu and raku.

plan 3;

# --- for over a Range, implicit topic ---------------------------------------
is Q[for 1..3 { $_ }].AST.gist, q:to/END/.chomp, 'for -> Statement::For with topic Block';
    RakuAST::StatementList.new(
      RakuAST::Statement::For.new(
        mode   => "serial",
        source => RakuAST::ApplyInfix.new(
          left  => RakuAST::IntLiteral.new(1),
          infix => RakuAST::Infix.new(".."),
          right => RakuAST::IntLiteral.new(3)
        ),
        body   => RakuAST::Block.new(
          implicit-topic => True,
          required-topic => 1,
          body           => RakuAST::Blockoid.new(
            RakuAST::StatementList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::Var::Lexical.new("\$_")
              )
            )
          )
        )
      )
    )
    END

# --- the topic marking is present even when the body ignores $_ -------------
is Q[for 1..3 { 5 }].AST.gist.contains(q:to/FRAG/.chomp), True, 'implicit-topic marked regardless of $_ usage';
        body   => RakuAST::Block.new(
          implicit-topic => True,
          required-topic => 1,
    FRAG

# --- for over a declared array ----------------------------------------------
is Q[my @a; for @a { $_ }].AST.gist.contains('source => RakuAST::Var::Lexical.new("\@a")'), True,
    'for over @a -> source is the array variable';
