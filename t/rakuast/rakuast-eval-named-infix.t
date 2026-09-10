use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST Phase 5 slice 18 (ADR-0011): EVAL of named infix operators.
# raku already renders `$a x $b` as `Infix.new("x")`; the write side now maps any
# named infix (`x`/`xx`/`eq`/`ne`/`lt`/`gt`/`cmp`/...) back to its generic `Ident`
# token, plus `~~` to `SmartMatch`. Verified against Rakudo; passes under BOTH
# mutsu and raku.

plan 7;

# --- string repetition (x) --------------------------------------------------
is EVAL(Q["ab" x 3].AST), 'ababab', 'x repeats a string';

# --- list repetition (xx) ---------------------------------------------------
is EVAL(Q[(1 xx 3).elems].AST), 3, 'xx repeats into a list';

# --- string equality (eq / ne) ----------------------------------------------
is EVAL(Q["a" eq "a"].AST), True, 'eq compares strings for equality';
is EVAL(Q["a" ne "b"].AST), True, 'ne compares strings for inequality';

# --- string ordering (lt) ---------------------------------------------------
is EVAL(Q["a" lt "b"].AST), True, 'lt orders strings';

# --- three-way compare (cmp) ------------------------------------------------
is EVAL(Q[(3 cmp 5).Str].AST), 'Less', 'cmp yields an Order';

# --- smartmatch against a value (~~) ----------------------------------------
is EVAL(Q[my $x = 5; $x ~~ 5].AST), True, '~~ smartmatches against a value';
