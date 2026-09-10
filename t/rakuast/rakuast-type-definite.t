use v6;
use Test;

# RakuAST Phase 2 slice 20 (ADR-0011): definite types `Int:D` / `Int:U`.
# A `:D`/`:U` smiley makes the `type` field a Type::Definedness wrapping the
# base Type::Simple. Expected gists captured verbatim from Rakudo; this file
# passes under BOTH mutsu and raku. (raku requires an initializer for a `:D`
# variable, so the pinned cases use one.)

plan 3;

# --- `Int:D` -> definite => True --------------------------------------------
is Q[my Int:D $x = 5].AST.gist, q:to/END/.chomp, 'Int:D -> Type::Definedness(definite => True)';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
          type        => RakuAST::Type::Definedness.new(
            base-type => RakuAST::Type::Simple.new(
              RakuAST::Name.from-identifier("Int")
            ),
            definite  => True
          ),
          sigil       => "\$",
          desigilname => RakuAST::Name.from-identifier("x"),
          initializer => RakuAST::Initializer::Assign.new(
            RakuAST::IntLiteral.new(5)
          )
        )
      )
    )
    END

# --- `Int:U` -> definite => False -------------------------------------------
is Q[my Int:U $x].AST.gist.contains('definite  => False'), True, 'Int:U -> definite => False';

# --- the base type is a nested Type::Simple ---------------------------------
my $g = Q[my Str:D $s = "hi"].AST.gist;
ok $g.contains('type        => RakuAST::Type::Definedness.new(')
    && $g.contains('base-type => RakuAST::Type::Simple.new(')
    && $g.contains('RakuAST::Name.from-identifier("Str")')
    && $g.contains('definite  => True'),
    'Str:D -> Definedness over a Type::Simple base';
