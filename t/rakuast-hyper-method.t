use v6;
use Test;

# RakuAST Phase 2 slice 26 (ADR-0011): hyper method calls `@a>>.method`.
# `@a>>.abs` -> ApplyPostfix(operand, postfix => MetaPostfix::Hyper(
# Call::Method(...))). Expected gists captured verbatim from Rakudo; this file
# passes under BOTH mutsu and raku.

plan 3;

# --- hyper method, no args --------------------------------------------------
is Q[my @a; @a>>.abs].AST.gist.contains(q:to/FRAG/.chomp), True, 'hyper method -> MetaPostfix::Hyper';
        expression => RakuAST::ApplyPostfix.new(
          operand => RakuAST::Var::Lexical.new("\@a"),
          postfix => RakuAST::MetaPostfix::Hyper.new(
            RakuAST::Call::Method.new(
              name => RakuAST::Name.from-identifier("abs")
            )
          )
        )
    FRAG

# --- hyper method with an argument ------------------------------------------
my $r = Q[my @a; @a>>.round(2)].AST.gist;
ok $r.contains('postfix => RakuAST::MetaPostfix::Hyper.new(')
    && $r.contains('RakuAST::Call::Method.new(')
    && $r.contains('RakuAST::Name.from-identifier("round")')
    && $r.contains('args => RakuAST::ArgList.new('),
    'hyper method with args wraps a Call::Method + ArgList';

# --- a plain method call is not hyper ---------------------------------------
is Q[my $x; $x.abs].AST.gist.contains('MetaPostfix::Hyper'), False,
    'plain method call is not wrapped in MetaPostfix::Hyper';
