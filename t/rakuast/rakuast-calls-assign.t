use v6;
use Test;

# RakuAST Phase 2 slice 2 (ADR-0011): assignment and method calls.
# Expected gists captured verbatim from Rakudo.

plan 8;

# --- scalar assignment carries :item ----------------------------------------
is Q[my $x; $x = 3].AST.gist.contains(q:to/FRAG/.chomp), True, 'scalar assignment -> Assignment.new(:item)';
    expression => RakuAST::ApplyInfix.new(
      left  => RakuAST::Var::Lexical.new("\$x"),
      infix => RakuAST::Assignment.new(:item),
      right => RakuAST::IntLiteral.new(3)
    )
FRAG

# --- array assignment: list form has no adverb, no parens -------------------
is Q[my @a; @a = 1].AST.gist.contains(q:to/FRAG/.chomp), True, 'array assignment -> Assignment.new';
    expression => RakuAST::ApplyInfix.new(
      left  => RakuAST::Var::Lexical.new("\@a"),
      infix => RakuAST::Assignment.new,
      right => RakuAST::IntLiteral.new(1)
    )
FRAG

# --- hash assignment: also the list form ------------------------------------
is Q[my %h; %h = 1].AST.gist.contains('infix => RakuAST::Assignment.new,'), True,
    'hash assignment -> Assignment.new';

# --- method call, no args ---------------------------------------------------
is Q[my $x; $x.abs].AST.gist.contains(q:to/FRAG/.chomp), True, 'no-arg method call -> Call::Method';
    expression => RakuAST::ApplyPostfix.new(
      operand => RakuAST::Var::Lexical.new("\$x"),
      postfix => RakuAST::Call::Method.new(
        name => RakuAST::Name.from-identifier("abs")
      )
    )
FRAG

# --- method call with args --------------------------------------------------
is Q[my $x; $x.round(2)].AST.gist.contains(q:to/FRAG/.chomp), True, 'method call with args -> ArgList';
      postfix => RakuAST::Call::Method.new(
        name => RakuAST::Name.from-identifier("round"),
        args => RakuAST::ArgList.new(
          RakuAST::IntLiteral.new(2)
        )
      )
FRAG

# --- method call on a literal -----------------------------------------------
is Q["hi".uc].AST.gist.contains(q:to/FRAG/.chomp), True, 'method call on a QuotedString operand';
    expression => RakuAST::ApplyPostfix.new(
      operand => RakuAST::QuotedString.new(
        segments   => (
          RakuAST::StrLiteral.new("hi"),
        )
      ),
      postfix => RakuAST::Call::Method.new(
        name => RakuAST::Name.from-identifier("uc")
      )
    )
FRAG

# --- explicit empty parens still render no args -----------------------------
is Q[my $x; $x.abs()].AST.gist.contains('RakuAST::Call::Method.new('), True,
    'method call with empty parens';

# --- chained method calls nest as postfix-on-postfix ------------------------
is Q[my $x; $x.round(2).abs].AST.gist.contains(q:to/FRAG/.chomp), True, 'chained method calls nest';
    expression => RakuAST::ApplyPostfix.new(
      operand => RakuAST::ApplyPostfix.new(
        operand => RakuAST::Var::Lexical.new("\$x"),
        postfix => RakuAST::Call::Method.new(
          name => RakuAST::Name.from-identifier("round"),
          args => RakuAST::ArgList.new(
            RakuAST::IntLiteral.new(2)
          )
        )
      ),
      postfix => RakuAST::Call::Method.new(
        name => RakuAST::Name.from-identifier("abs")
      )
    )
FRAG
