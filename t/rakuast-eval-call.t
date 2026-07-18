use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST Phase 5 slice 4 (ADR-0011): EVAL of listop I/O calls and method calls.
# `say`/`put`/`print`/`note` lower to their statement forms and `$x.method`
# lowers to a MethodCall, so an effectful program runs. Verified against Rakudo;
# this file passes under BOTH mutsu and raku.
#
# Note: the *return value* of a bare trailing `say` differs between mutsu and
# raku (a pre-existing mutsu EVAL behaviour, also seen with a string EVAL), so
# `say` is exercised as a side-effecting middle statement rather than the tail.

plan 4;

# --- method calls (values match) --------------------------------------------
is EVAL(Q[my $x = -5; $x.abs].AST), 5, 'method call: (-5).abs == 5';
is EVAL(Q[my $s = "hi"; $s.uc].AST), 'HI', 'method call: "hi".uc == "HI"';

# --- a method call inside a say (say is a side effect; tail is a value) ------
is EVAL(Q[my $x = 3; say $x; $x * 2].AST), 6,
    'say runs as a side effect; the tail expression is the value';

# --- a chained method call --------------------------------------------------
is EVAL(Q[my $x = -3; $x.abs.Str].AST), '3', 'chained method calls lower and run';
