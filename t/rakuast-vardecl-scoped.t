use v6;
use Test;

# RakuAST Phase 2 slice 10 (ADR-0011): scoped and typed variable declarations
# (VarDeclaration::Simple gains `scope` and `type => Type::Simple`).
# Expected gists captured verbatim from Rakudo; this file passes under BOTH
# mutsu and raku.

plan 5;

# --- typed `my Int $x` ------------------------------------------------------
is Q[my Int $x = 5].AST.gist, q:to/END/.chomp, 'my Int $x = 5 -> Type::Simple + initializer';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
          type        => RakuAST::Type::Simple.new(
            RakuAST::Name.from-identifier("Int")
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

# --- `our $x` scope ---------------------------------------------------------
is Q[our $x].AST.gist, q:to/END/.chomp, 'our $x -> scope => "our"';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
          scope       => "our",
          sigil       => "\$",
          desigilname => RakuAST::Name.from-identifier("x")
        )
      )
    )
    END

# --- `state $x` scope -------------------------------------------------------
is Q[state $x].AST.gist.contains('scope       => "state"'), True, 'state $x -> scope => "state"';

# --- `our Int $x`: scope then type ------------------------------------------
is Q[our Int $x].AST.gist.contains(q:to/FRAG/.chomp), True, 'our Int $x -> scope then type';
          scope       => "our",
          type        => RakuAST::Type::Simple.new(
            RakuAST::Name.from-identifier("Int")
          ),
    FRAG

# --- plain `my $x` still has neither scope nor type -------------------------
is Q[my $x].AST.gist.contains('scope') || Q[my $x].AST.gist.contains('type '), False,
    'plain my $x omits scope and type';
