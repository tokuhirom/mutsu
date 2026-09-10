use v6;
use Test;

# RakuAST Phase 2 slice 13 (ADR-0011): class and method declarations
# (RakuAST::Class, RakuAST::Method). Expected gists captured verbatim from
# Rakudo; this file passes under BOTH mutsu and raku. Each test uses a distinct
# class name because `.AST` registers the symbol and raku rejects redeclaration.

plan 3;

# --- empty class ------------------------------------------------------------
is Q[class C1 { }].AST.gist, q:to/END/.chomp, 'class -> Class(name, body => Block)';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Class.new(
          name => RakuAST::Name.from-identifier("C1"),
          body => RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
              RakuAST::StatementList.new()
            )
          )
        )
      )
    )
    END

# --- class with a method ----------------------------------------------------
is Q[class C2 { method m { 1 } }].AST.gist, q:to/END/.chomp, 'method in class body -> Method node';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Class.new(
          name => RakuAST::Name.from-identifier("C2"),
          body => RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
              RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                  expression => RakuAST::Method.new(
                    name => RakuAST::Name.from-identifier("m"),
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
            )
          )
        )
      )
    )
    END

# --- a method with a parameter carries the implicit Type::Setting -----------
my $g = Q[class C3 { method m($x) { $x } }].AST.gist;
ok $g.contains('expression => RakuAST::Method.new(')
    && $g.contains('signature => RakuAST::Signature.new(')
    && $g.contains('type     => RakuAST::Type::Setting.new(')
    && $g.contains('name => "\$x"'),
    'method with a parameter -> Method + Signature + Type::Setting';
