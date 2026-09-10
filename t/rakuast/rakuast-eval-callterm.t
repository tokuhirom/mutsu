use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST slice 35 (ADR-0011): calling a term `$f(…)`, both directions.
# `$f(1, 2)` is `ApplyPostfix(operand, Call::Term(args))`; mutsu models it as
# `Expr::CallOn`. The read side emits the `Call::Term` node and the write side
# lowers it back. Verified against Rakudo; passes under BOTH mutsu and raku.
#
# Note: the callable is a bare block `{ … }` (a `$_` closure); named-parameter
# code values (`sub ($x) { … }` / `-> $x { … }`) are a separate construct and
# stay the coverage boundary.

plan 5;

# --- read side: `$f(9)` renders with a Call::Term postfix -------------------
ok Q{my $f = 0; $f(9)}.AST.gist.contains('RakuAST::Call::Term.new('),
    'calling a term renders as ApplyPostfix + Call::Term';

# --- calling a block held in a variable -------------------------------------
is EVAL(Q{my $f = { $_ * 2 }; $f(9)}.AST), 18, 'a block variable is callable';

# --- calling with a different argument --------------------------------------
is EVAL(Q{my $f = { $_ + 100 }; $f(5)}.AST), 105, 'the argument binds the block topic';

# --- a block passed as a callback and invoked -------------------------------
is EVAL(Q{sub twice($g) { $g(3) + $g(4) }; twice({ $_ * 10 })}.AST), 70,
    'a callback block is invoked with each argument';

# --- two calls in an expression ---------------------------------------------
is EVAL(Q{my $f = { $_ * $_ }; $f(3) + $f(4)}.AST), 25, 'two term calls compose';
