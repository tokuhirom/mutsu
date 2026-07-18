use v6;
use Test;

# RakuAST Phase 2 slice 23 (ADR-0011): quoted method names `$x."foo"()`.
# A quoted method call -> Call::QuotedMethod, whose name is a QuotedString
# (a string literal) rather than a Name.from-identifier. Expected gists
# captured verbatim from Rakudo; this file passes under BOTH mutsu and raku.

plan 3;

# --- quoted method, no args -------------------------------------------------
my $q = Q[my $x; $x."foo"()].AST.gist;
ok $q.contains('postfix => RakuAST::Call::QuotedMethod.new(')
    && $q.contains('name => RakuAST::QuotedString.new(')
    && $q.contains('RakuAST::StrLiteral.new("foo")')
    && !$q.contains('Name.from-identifier("foo")'),
    'quoted method -> Call::QuotedMethod with a QuotedString name';

# --- quoted method with an argument -----------------------------------------
my $a = Q[my $x; $x."bar"(1)].AST.gist;
ok $a.contains('RakuAST::Call::QuotedMethod.new(')
    && $a.contains('name => RakuAST::QuotedString.new(')
    && $a.contains('RakuAST::StrLiteral.new("bar")')
    && $a.contains('args => RakuAST::ArgList.new('),
    'quoted method with args keeps its ArgList';

# --- a plain (unquoted) method is still Call::Method -------------------------
is Q[my $x; $x.plain].AST.gist.contains('RakuAST::Call::Method.new('), True,
    'plain method call is still Call::Method';
