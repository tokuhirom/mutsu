use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST `ApplyPostfix` lowering (ADR-0011, the write direction).
#
# The read direction has rendered all of these for a while; `EVAL` handled only
# `Call::Method` (with no dispatch modifier), `Call::Term`, and
# `Postcircumfix::ArrayIndex`. The rest of the postfix cluster now lowers too:
#
#   * `Postfix(operator => "++"/"--")`  -> Expr::PostfixOp
#   * `MetaPostfix::Hyper(Call::Method)` -> Expr::HyperMethodCall
#   * `Call::Method`'s `dispatch` field  -> the `.?` / `.+` / `.*` modifier
#   * `Call::QuotedMethod`               -> a quoted method name
#
# Fixing this surfaced a real lowering bug of its own: the operator-name table
# had no row for `++`/`--`, so a *prefix* `++$x` — which already lowered —
# became `Expr::Unary { op: Ident("++") }`, which the compiler does not treat as
# an increment. `EVAL(Q[my $x = 1; ++$x; say $x].AST)` silently printed 1.
#
# Passes under BOTH mutsu and raku.

plan 12;

# --- postfix increment / decrement ------------------------------------------
is EVAL(Q[my $x = 1; $x++; $x].AST), 2, 'a postfix ++ lowers and mutates';
is EVAL(Q[my $x = 5; $x--; $x].AST), 4, 'a postfix -- lowers and mutates';
is EVAL(Q[my $x = 1; my $y = $x++; $y].AST), 1,
    'a postfix ++ evaluates to the value before the increment';

# --- prefix increment / decrement (the operator-name table bug) -------------
is EVAL(Q[my $x = 1; ++$x; $x].AST), 2, 'a prefix ++ lowers and mutates';
is EVAL(Q[my $x = 5; --$x; $x].AST), 4, 'a prefix -- lowers and mutates';
is EVAL(Q[my $x = 1; my $y = ++$x; $y].AST), 2,
    'a prefix ++ evaluates to the value after the increment';

# --- an increment on an element ---------------------------------------------
is EVAL(Q[my @a = (1, 2); @a[0]++; @a[0]].AST), 2,
    'a postfix ++ on an array element lowers';

# --- a C-style loop stepping with ++ ----------------------------------------
is EVAL(Q[my $s = 0; loop (my $i = 0; $i < 3; $i++) { $s = $s + $i }; $s].AST), 3,
    'a C-style loop whose step is a postfix ++ lowers';

# --- hyper method calls -----------------------------------------------------
is EVAL(Q[my @a = (-1, -2); (@a>>.abs).join(",")].AST), '1,2',
    'a hyper method call lowers';

# --- the .? dispatch modifier -----------------------------------------------
is EVAL(Q[my $x = -5; $x.?abs].AST), 5,
    'a `.?` call on an existing method lowers and dispatches';
ok EVAL(Q[my $x = -5; $x.?no-such-method-here].AST) === Nil,
    'a `.?` call on a missing method lowers and returns Nil';

# --- a quoted method name ---------------------------------------------------
is EVAL(Q[my $x = -5; $x."abs"()].AST), 5, 'a quoted method name lowers';
