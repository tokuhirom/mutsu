use Test;
plan 4;

# `True`/`False`/`Inf`/`NaN` are terms, not routines, so calling one is
# X::Undeclared (naming `&name`), the same class `e()`/`pi()`/`tau()`/`i()`
# already got right (t/undeclared-symbol-exception-class.t). Reached only
# through EVAL: the EVAL-time undeclared-routine pre-pass
# (`check_eval_undeclared_routines`, system_eval_names.rs) has its own
# exemption list (`EVAL_KNOWN_ROUTINE_NAMES`) separate from the runtime
# fallback's `CORE_TERM_CONSTANTS`, and it was missing exactly these four --
# so the pre-pass flagged them first, with the wrong class
# (X::Undeclared::Symbols), before the correct runtime path was ever reached.

use MONKEY-SEE-NO-EVAL;

throws-like 'True()', X::Undeclared, 'True() is X::Undeclared', symbol => '&True';
throws-like 'False()', X::Undeclared, 'False() is X::Undeclared', symbol => '&False';
throws-like 'Inf()', X::Undeclared, 'Inf() is X::Undeclared', symbol => '&Inf';
throws-like 'NaN()', X::Undeclared, 'NaN() is X::Undeclared', symbol => '&NaN';
