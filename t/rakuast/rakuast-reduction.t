use v6;
use Test;

# RakuAST Phase 2 slice 25 (ADR-0011): the reduction metaoperator `[+]`.
# `[+] @a` -> Term::Reduce(triangle => False, infix, args); `[\+] @a` sets
# triangle => True. Expected gists captured verbatim from Rakudo; this file
# passes under BOTH mutsu and raku.

plan 3;

# --- ordinary reduction -----------------------------------------------------
is Q[my @a; [+] @a].AST.gist.contains(q:to/FRAG/.chomp), True, '[+] @a -> Term::Reduce';
        expression => RakuAST::Term::Reduce.new(
          triangle => False,
          infix    => RakuAST::Infix.new("+"),
          args     => RakuAST::ArgList.new(
            RakuAST::Var::Lexical.new("\@a")
          )
        )
    FRAG

# --- triangle reduction sets triangle => True -------------------------------
is Q[my @a; [\+] @a].AST.gist.contains('triangle => True'), True, '[\+] @a -> triangle => True';

# --- the infix operator is carried through ----------------------------------
is Q[my @a; [*] @a].AST.gist.contains('infix    => RakuAST::Infix.new("*")'), True,
    '[*] @a -> Infix("*")';
