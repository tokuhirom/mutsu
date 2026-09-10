use v6;
use Test;

# RakuAST Phase 2 slice 21 (ADR-0011): parameterised types `Array[Int]`.
# A `Base[Arg, ...]` type maps to Type::Parameterized(base-type => Type::Simple,
# args => ArgList(<type args>)). Expected gists captured verbatim from Rakudo;
# this file passes under BOTH mutsu and raku.

plan 3;

# --- `Array[Int]` -----------------------------------------------------------
is Q[my Array[Int] $x].AST.gist, q:to/END/.chomp, 'Array[Int] -> Type::Parameterized';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
          type        => RakuAST::Type::Parameterized.new(
            base-type => RakuAST::Type::Simple.new(
              RakuAST::Name.from-identifier("Array")
            ),
            args      => RakuAST::ArgList.new(
              RakuAST::Type::Simple.new(
                RakuAST::Name.from-identifier("Int")
              )
            )
          ),
          sigil       => "\$",
          desigilname => RakuAST::Name.from-identifier("x")
        )
      )
    )
    END

# --- multi-argument `Hash[Str, Int]` ----------------------------------------
my $h = Q[my Hash[Str, Int] $g].AST.gist;
ok $h.contains('RakuAST::Type::Parameterized.new(')
    && $h.contains('RakuAST::Name.from-identifier("Hash")')
    && $h.contains('RakuAST::Name.from-identifier("Str")')
    && $h.contains('RakuAST::Name.from-identifier("Int")'),
    'Hash[Str, Int] -> two type args in the ArgList';

# --- the base type is a nested Type::Simple ---------------------------------
is Q[my Array[Str] $a].AST.gist.contains('base-type => RakuAST::Type::Simple.new('), True,
    'parameterised base is a Type::Simple';
