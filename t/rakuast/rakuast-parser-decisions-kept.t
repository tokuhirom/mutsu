use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

# Two decisions the parser makes cannot be re-derived from rakudo's tree, so the
# converter keeps them on the node where no renderer shows them:
#
# - where a `*` is primed (`(* + 1).WHAT` primes `* + 1`, while the tree for
#   the postfix chain alone would prime the whole chain), and
# - the source-order number of a main-program `END` phaser.
plan 12;

# --- the priming scope -------------------------------------------------------
is EVAL(Q[(* + 1).WHAT].AST).gist, '(WhateverCode)', 'a parenthesized * + 1 is primed before the postfix';
is EVAL(Q[((* + 1) xx 2).elems].AST), 2, '(* + 1) xx 2 repeats the WhateverCode';
is EVAL(Q[(* ~~ Int).WHAT].AST).gist, '(WhateverCode)', 'a smartmatch with * is primed';
is EVAL(Q[(1..5).map(* * 2).join(",")].AST), '2,4,6,8,10', 'an argument is its own scope';
is EVAL(Q[(* + *)(1, 2)].AST), 3, 'two stars are one scope of arity 2';
is EVAL(Q[my &f = * + 1; f(1)].AST), 2, 'a bound WhateverCode is called';

# --- hidden from the model's text --------------------------------------------
nok Q[(* + 1).WHAT].AST.gist.contains('thunk'), 'the priming scope is not rendered';
nok Q[END { say 1 }].AST.gist.contains('end-index'), 'an END number is not rendered';
nok Q[END { say 1 }].AST.gist.contains('origin'), 'nor a position';

# --- END ordering in an EVAL --------------------------------------------------
# An `END` reached through an EVAL installs where execution reaches it, after the
# ENDs the main program installed up front.
my @order;
END { @order.push('main end') }
EVAL(Q[END { 1 }].AST);
is EVAL(Q[1 + 1].AST), 2, 'an EVAL after an END runs';
ok @order.elems == 0, 'the main END has not run yet';
pass 'END installation does not disturb the main program';
