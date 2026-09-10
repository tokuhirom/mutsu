use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST slice 30 (ADR-0011): the `do { … }` statement prefix, both directions.
# `Expr::DoBlock` <-> `RakuAST::StatementPrefix::Do(Block)`. Verified against
# Rakudo; passes under BOTH mutsu and raku.

plan 5;

# --- read side: `do { … }` renders as StatementPrefix::Do -------------------
ok Q{my $y = 1; do { $y + 2 }}.AST.gist.contains('RakuAST::StatementPrefix::Do.new('),
    'a do block renders as StatementPrefix::Do';

# --- a do block yields its final value --------------------------------------
is EVAL(Q{do { 1 + 2 }}.AST), 3, 'a do block yields its expression value';
is EVAL(Q{my $n = 3; do { $n * $n }}.AST), 9, 'a do block over a variable';

# --- a multi-statement do block ---------------------------------------------
is EVAL(Q{my $x = do { my $a = 5; $a * 2 }; $x}.AST), 10,
    'a multi-statement do block returns its last value';

# --- a do block used inline in an expression --------------------------------
is EVAL(Q{1 + do { 2 * 3 }}.AST), 7, 'a do block composes in an expression';
