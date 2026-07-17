use v6;
use Test;

# RakuAST Phase 2 (ADR-0011): read-coverage for variables, declarations, and
# infix/prefix/postfix operators. Expected gists captured verbatim from Rakudo.

plan 9;

# --- variable declaration (no initializer) ----------------------------------
is Q[my $x].AST.gist, q:to/END/.chomp, 'VarDeclaration::Simple, $ sigil';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::VarDeclaration::Simple.new(
      sigil       => "\$",
      desigilname => RakuAST::Name.from-identifier("x")
    )
  )
)
END

# --- @ sigil ----------------------------------------------------------------
is Q[my @a].AST.gist, q:to/END/.chomp, 'VarDeclaration::Simple, @ sigil';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::VarDeclaration::Simple.new(
      sigil       => "\@",
      desigilname => RakuAST::Name.from-identifier("a")
    )
  )
)
END

# --- declaration with initializer -------------------------------------------
is Q[my $x = 5].AST.gist, q:to/END/.chomp, 'VarDeclaration::Simple + Initializer::Assign';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::VarDeclaration::Simple.new(
      sigil       => "\$",
      desigilname => RakuAST::Name.from-identifier("x"),
      initializer => RakuAST::Initializer::Assign.new(
        RakuAST::IntLiteral.new(5)
      )
    )
  )
)
END

# --- lexical variable usage -------------------------------------------------
is Q[my $x; $x].AST.gist.contains('RakuAST::Var::Lexical.new("\$x")'), True,
    'Var::Lexical for $ usage';
is Q[my @a; @a].AST.gist.contains('RakuAST::Var::Lexical.new("\@a")'), True,
    'Var::Lexical for @ usage';

# --- infix operator ---------------------------------------------------------
is Q[my $x; $x + 2].AST.gist, q:to/END/.chomp, 'ApplyInfix + Infix';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::VarDeclaration::Simple.new(
      sigil       => "\$",
      desigilname => RakuAST::Name.from-identifier("x")
    )
  ),
  RakuAST::Statement::Expression.new(
    expression => RakuAST::ApplyInfix.new(
      left  => RakuAST::Var::Lexical.new("\$x"),
      infix => RakuAST::Infix.new("+"),
      right => RakuAST::IntLiteral.new(2)
    )
  )
)
END

# --- prefix operator --------------------------------------------------------
is Q[my $x; -$x].AST.gist.contains(q:to/FRAG/.chomp), True, 'ApplyPrefix + Prefix';
    expression => RakuAST::ApplyPrefix.new(
      prefix  => RakuAST::Prefix.new("-"),
      operand => RakuAST::Var::Lexical.new("\$x")
    )
FRAG

# --- postfix operator (named field -> multi-line) ---------------------------
is Q[my $x; $x++].AST.gist.contains(q:to/FRAG/.chomp), True, 'ApplyPostfix + Postfix';
    expression => RakuAST::ApplyPostfix.new(
      operand => RakuAST::Var::Lexical.new("\$x"),
      postfix => RakuAST::Postfix.new(
        operator => "++"
      )
    )
FRAG

# --- nested infix respects precedence ---------------------------------------
is Q[my $x; $x * 2 + 1].AST.gist.contains(q:to/FRAG/.chomp), True, 'nested ApplyInfix';
    expression => RakuAST::ApplyInfix.new(
      left  => RakuAST::ApplyInfix.new(
        left  => RakuAST::Var::Lexical.new("\$x"),
        infix => RakuAST::Infix.new("*"),
        right => RakuAST::IntLiteral.new(2)
      ),
      infix => RakuAST::Infix.new("+"),
      right => RakuAST::IntLiteral.new(1)
    )
FRAG
