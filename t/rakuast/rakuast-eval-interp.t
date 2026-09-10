use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST slice 16 (ADR-0011): interpolated strings `"a $x b"`, both directions.
# raku models these as a `QuotedString` with one segment per part (a literal run
# is a `StrLiteral`, an interpolated term keeps its own node). The read side
# (`.AST`) emits that node and the write side (`EVAL`) lowers it back to a
# `StringInterpolation`. Verified against Rakudo; passes under BOTH mutsu and raku.

plan 5;

# --- read side: the gist has one StrLiteral + one Var::Lexical segment -------
my $g = Q[my $x = 5; "val=$x"].AST.gist;
ok $g.contains('RakuAST::QuotedString.new(')
    && $g.contains('RakuAST::StrLiteral.new("val=")')
    && $g.contains('RakuAST::Var::Lexical.new("\$x")'),
    'an interpolated string renders as a QuotedString with segments';

# --- a literal prefix followed by a variable --------------------------------
is EVAL(Q[my $x = 5; "val=$x"].AST), 'val=5',
    'interpolation of a trailing variable';

# --- two variables with literal runs between/around -------------------------
is EVAL(Q[my $a = 2; my $b = 3; "$a+$b"].AST), '2+3',
    'interpolation of two variables';

# --- a single interpolated variable, no literal runs ------------------------
is EVAL(Q[my $x = 42; "$x"].AST), '42',
    'interpolation of a lone variable stringifies it';

# --- a literal run after the variable too -----------------------------------
is EVAL(Q[my $n = 7; "n is $n!"].AST), 'n is 7!',
    'interpolation with literal text on both sides';
