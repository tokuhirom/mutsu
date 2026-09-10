use v6;
use Test;

# RakuAST Phase 2 slice 29 (ADR-0011): coercion types `Int()`.
# A `Base()` coercion maps to Type::Coercion(base-type => Type::Simple).
# Expected gists captured verbatim from Rakudo; this file passes under BOTH
# mutsu and raku. A coercion with an explicit target (`Str(Int)`) is deferred.

plan 2;

# --- `Int()` coercion -------------------------------------------------------
is Q[my Int() $x].AST.gist, q:to/END/.chomp, 'Int() -> Type::Coercion(base-type)';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
          type        => RakuAST::Type::Coercion.new(
            base-type => RakuAST::Type::Simple.new(
              RakuAST::Name.from-identifier("Int")
            )
          ),
          sigil       => "\$",
          desigilname => RakuAST::Name.from-identifier("x")
        )
      )
    )
    END

# --- the base type is a nested Type::Simple ---------------------------------
is Q[my Str() $s].AST.gist.contains('base-type => RakuAST::Type::Simple.new('), True,
    'coercion base is a Type::Simple';
