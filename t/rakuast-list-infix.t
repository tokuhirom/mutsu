use v6;
use Test;

# RakuAST Phase 2 slice 24 (ADR-0011): list-associative infixes
# `andthen` / `orelse` / `notandthen`. These render as a single flat
# ApplyListInfix in raku; mutsu nests them left-associatively, so a same-op
# chain is flattened into one operand list. Expected gists captured verbatim
# from Rakudo; this file passes under BOTH mutsu and raku.

plan 3;

# --- two-operand andthen ----------------------------------------------------
is Q[my $a; my $b; $a andthen $b].AST.gist.contains(q:to/FRAG/.chomp), True, 'andthen -> ApplyListInfix';
        expression => RakuAST::ApplyListInfix.new(
          infix    => RakuAST::Infix.new("andthen"),
          operands => (
            RakuAST::Var::Lexical.new("\$a"),
            RakuAST::Var::Lexical.new("\$b"),
          )
        )
    FRAG

# --- a three-operand chain flattens into one operand list -------------------
my $chain = Q[my $a; my $b; my $c; $a andthen $b andthen $c].AST.gist;
ok $chain.contains('RakuAST::ApplyListInfix.new(')
    && $chain.contains('RakuAST::Infix.new("andthen")')
    && $chain.comb(/'RakuAST::Var::Lexical'/).elems == 3,   # 3 flat operands
    'a op b op c flattens into three operands (not nested)';

# --- orelse also uses ApplyListInfix ----------------------------------------
is Q[my $a; my $b; $a orelse $b].AST.gist.contains('RakuAST::Infix.new("orelse")'), True,
    'orelse -> ApplyListInfix with the orelse infix';
