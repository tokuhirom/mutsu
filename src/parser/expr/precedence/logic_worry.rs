//! The "WhateverCode tested first" worry (#18139).
//!
//! A short-circuiting logical infix (`and`, `or`, `xor`, `andthen`,
//! `orelse`, `notandthen`, `&&`, `||`, `^^`, `//`) does not curry a
//! WhateverCode operand into the whole expression. A WhateverCode as its
//! first operand (`* > 1 and * < 5`) is a closure, which is always true and
//! defined, so the test it reads like never happens. Rakudo says so at compile
//! time; mutsu does too, through the same parse-warning channel as its other
//! "Potential difficulties".

use super::*;

/// Warn when `left`, the first operand of the logical infix spelled
/// `op_text` (whose expression starts at `at`), is a WhateverCode or the
/// Whatever.
// Cost: O(|op_text|).
pub(super) fn warn_uncurried_whatever(left: &Expr, op_text: &str, at: &str) {
    let spelling = op_text.trim();
    // The operand primes on its own (the infix is a thunk barrier, see
    // `whatever::contains_whatever`): a `*` below an operator makes it a
    // WhateverCode; a bare `*` is the Whatever itself, which rakudo names so.
    let bare = super::super::whatever::is_whatever(left);
    let primes = matches!(left, Expr::WhateverCurry(..))
        || (!bare && super::super::whatever::contains_whatever(left));
    let what = if primes {
        "WhateverCode"
    } else if bare {
        "Whatever"
    } else {
        return;
    };
    if matches!(spelling, "min" | "max") || crate::parser::stmt::simple::worries_suppressed() {
        return;
    }
    crate::parser::add_parse_warning(
        format!(
            "Potential difficulties:\n    {what} tested first by {spelling} does not curry \
             the {spelling} expression,\n    and is always true and defined.\n    Try a block \
             using $_ instead if results are not as expected."
        ),
        crate::parser::primary::current_line_number(at),
    );
}
