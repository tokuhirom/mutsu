use v6;
use Test;

# RakuAST Phase 2 slice 7 (ADR-0011): named sub declarations.
# `sub NAME (params) { body }` -> Statement::Expression(Sub(name, [signature],
# body)). Sub/method signature parameters carry an implicit
# `type => Type::Setting(Any)` (pointy-block params do not). A parameter-less
# sub omits the `signature` field. Expected gists captured verbatim from
# Rakudo; this file passes under BOTH mutsu and raku.

plan 4;

# --- named sub with one typed-by-default parameter ---------------------------
is Q[sub foo($x) { $x }].AST.gist, q:to/END/.chomp, 'sub -> Sub(name, signature w/ Type::Setting, body)';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Sub.new(
          name      => RakuAST::Name.from-identifier("foo"),
          signature => RakuAST::Signature.new(
            parameters => (
              RakuAST::Parameter.new(
                type     => RakuAST::Type::Setting.new(
                  RakuAST::Name.from-identifier("Any")
                ),
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

# --- parameter-less sub omits the signature field ---------------------------
is Q[sub foo { 42 }].AST.gist, q:to/END/.chomp, 'parameter-less sub omits signature';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Sub.new(
          name => RakuAST::Name.from-identifier("foo"),
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

# --- an empty explicit signature `()` also omits the signature field --------
is Q[sub foo() { 42 }].AST.gist, Q[sub foo { 42 }].AST.gist,
    'empty () signature renders like no signature';

# --- a defaulted parameter renders `default =>`, still typed ----------------
is Q[sub g($x = 5) { $x }].AST.gist.contains(q:to/FRAG/.chomp), True, 'defaulted sub param keeps its Type::Setting';
              RakuAST::Parameter.new(
                type    => RakuAST::Type::Setting.new(
                  RakuAST::Name.from-identifier("Any")
                ),
                target  => RakuAST::ParameterTarget::Var.new(
                  name => "\$x"
                ),
                default => RakuAST::IntLiteral.new(5)
              ),
    FRAG
