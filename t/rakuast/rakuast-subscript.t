use v6;
use Test;

# RakuAST Phase 2 slice 22 (ADR-0011): positional subscripts `@x[EXPR]`.
# `@x[0]` -> ApplyPostfix(operand, postfix => Postcircumfix::ArrayIndex(index =>
# SemiList(Statement::Expression(EXPR)))). Expected gists captured verbatim from
# Rakudo; this file passes under BOTH mutsu and raku.
#
# Note: associative subscripts (`%h{...}` / `%h<...>`) are deferred — mutsu
# cannot distinguish `<k>` (LiteralHashIndex) from `{"k"}` (HashIndex).

plan 3;

# --- literal index ----------------------------------------------------------
is Q[my @x; @x[0]].AST.gist, q:to/END/.chomp, 'positional subscript -> ArrayIndex + SemiList';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::VarDeclaration::Simple.new(
          sigil       => "\@",
          desigilname => RakuAST::Name.from-identifier("x")
        )
      ),
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPostfix.new(
          operand => RakuAST::Var::Lexical.new("\@x"),
          postfix => RakuAST::Postcircumfix::ArrayIndex.new(
            index => RakuAST::SemiList.new(
              RakuAST::Statement::Expression.new(
                expression => RakuAST::IntLiteral.new(0)
              )
            )
          )
        )
      )
    )
    END

# --- variable index ---------------------------------------------------------
my $v = Q[my @x; my $i; @x[$i]].AST.gist;
ok $v.contains('postfix => RakuAST::Postcircumfix::ArrayIndex.new(')
    && $v.contains('index => RakuAST::SemiList.new(')
    && $v.contains('expression => RakuAST::Var::Lexical.new("\$i")'),
    'variable index nests inside the SemiList';

# --- the operand is the indexed variable ------------------------------------
is Q[my @x; @x[0]].AST.gist.contains('operand => RakuAST::Var::Lexical.new("\@x")'), True,
    'subscript operand is the array variable';
