use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST slice 31 (ADR-0011): the `try { … }` statement prefix, both directions.
# `Expr::Try` (without a CATCH block) <-> `RakuAST::StatementPrefix::Try(Block)`.
# Verified against Rakudo; passes under BOTH mutsu and raku.

plan 5;

# --- read side: `try { … }` renders as StatementPrefix::Try -----------------
ok Q{my $y = 1; try { $y + 2 }}.AST.gist.contains('RakuAST::StatementPrefix::Try.new('),
    'a try block renders as StatementPrefix::Try';

# --- a successful try yields its value --------------------------------------
is EVAL(Q{try { 1 + 2 }}.AST), 3, 'a successful try yields its value';
is EVAL(Q{my $r = try { 5 * 2 }; $r}.AST), 10, 'a try assigned to a variable';

# --- a multi-statement try --------------------------------------------------
is EVAL(Q{try { my $a = 3; $a + 4 }}.AST), 7, 'a multi-statement try';

# --- a failing try produces an undefined result -----------------------------
nok EVAL(Q{try { (1/0).Int }}.AST).defined,
    'a failing try yields an undefined value (the exception is trapped)';
