use v6;
use Test;

# RakuAST Phase 2 slice 11 (ADR-0011): anonymous parameter-less `sub { }`.
# `sub { ... }` (no signature) -> RakuAST::Sub(body => Blockoid), with no
# `name` field. Expected gists captured verbatim from Rakudo; this file passes
# under BOTH mutsu and raku.
#
# Note: `sub ($x) { }` parses to the same internal node as a multi-param pointy
# block, which mutsu cannot distinguish, so a parameterised anonymous sub still
# renders as a PointyBlock (a documented divergence, ADR-0011) and is not
# pinned here.

plan 2;

# --- anonymous sub as an initializer ----------------------------------------
is Q[my $f = sub { 42 }].AST.gist, q:to/END/.chomp, 'anon sub -> Sub(body), no name';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
          sigil       => "\$",
          desigilname => RakuAST::Name.from-identifier("f"),
          initializer => RakuAST::Initializer::Assign.new(
            RakuAST::Sub.new(
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
      )
    )
    END

# --- a bare-statement anonymous sub -----------------------------------------
is Q[sub { 42 }].AST.gist, q:to/END/.chomp, 'bare anon sub statement -> Statement::Expression(Sub)';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Sub.new(
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
