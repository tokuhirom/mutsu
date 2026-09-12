//! Which `infix:<...>` names rakudo declares as a *routine*, and which it does
//! not.
//!
//! This is the classification [#8006](https://github.com/tokuhirom/mutsu/issues/8006)
//! asked for, and the half of it that lives outside
//! [`super::native_infix_dispatch`]'s type tables. mutsu used to treat every
//! core infix as an extendable `multi`: a user candidate joined the operator's
//! candidate set, and when no user candidate matched, the builtin still
//! answered. That is right for the operators rakudo really does declare
//! ([ADR-0071](../../docs/adr/0071-native-operators-are-dispatch-candidates.md):
//! `multi infix:<+>(Q, Q)` leaves `1 + 2` working), and wrong for the ones it
//! does not declare at all.
//!
//! `infix:<cross>` is the case that surfaced it. rakudo has a core `sub cross`
//! list operator, but **no** `&infix:<cross>` — `(1,2) cross (3,4)` is
//! "Two terms in a row" there. mutsu spells it as an infix as a convenience,
//! reaching the core `sub` of the same *bare* name through
//! `call_infix_fallback`'s last-resort call. So declaring
//! `multi infix:<cross>(Q, Q)` installs a *fresh lexical routine* with nothing
//! behind it: a call its candidates do not accept is `X::Multi::NoMatch`, not a
//! silent fall-through to the list operator.
//!
//! The table below is vendored from rakudo itself — every `&infix:<...>` key of
//! the `CORE::` package, measured on rakudo 2026.07 with
//!
//! ```raku
//! CORE::.keys.grep(*.starts-with("&infix:"))
//! ```
//!
//! It is sorted in byte order so [`rakudo_declares_infix`] can binary-search it.
//! A name missing from it is one a user declaration shadows outright; that
//! includes the list-operator spellings mutsu adds (`cross`, `zip`,
//! `roundrobin`, `sum`, `flat`, ...) and every purely user-defined operator
//! (`infix:<@@>`), which is exactly right — rakudo has no core candidate for
//! those either.
//!
//! Operators that rakudo implements as *syntax* rather than as a routine
//! (`ff`/`fff`, the `R`/`X`/`Z`/hyper metaop modifiers) never reach this
//! classification: they have their own opcodes.

/// Every `&infix:<...>` rakudo's `CORE::` declares, in byte order.
const RAKUDO_CORE_INFIX_NAMES: &[&str] = &[
    "!=",
    "!~~",
    "%",
    "%%",
    "&",
    "&&",
    "(&)",
    "(+)",
    "(-)",
    "(.)",
    "(<)",
    "(<+)",
    "(<=)",
    "(==)",
    "(>)",
    "(>+)",
    "(>=)",
    "(^)",
    "(cont)",
    "(elem)",
    "(|)",
    "*",
    "**",
    "+",
    "+&",
    "+<",
    "+>",
    "+^",
    "+|",
    ",",
    "-",
    "..",
    "...",
    "...^",
    "..^",
    "/",
    "//",
    "<",
    "<=",
    "<=>",
    "=",
    "=:=",
    "==",
    "===",
    "=>",
    "=~",
    "=~=",
    ">",
    ">=",
    "?&",
    "?^",
    "?|",
    "X",
    "Z",
    "^",
    "^..",
    "^...",
    "^...^",
    "^..^",
    "^^",
    "^…",
    "^…^",
    "after",
    "and",
    "andthen",
    "before",
    "but",
    "cmp",
    "coll",
    "div",
    "does",
    "eq",
    "eqv",
    "gcd",
    "ge",
    "gt",
    "lcm",
    "le",
    "leg",
    "lt",
    "max",
    "min",
    "minmax",
    "mod",
    "ne",
    "notandthen",
    "o",
    "or",
    "orelse",
    "unicmp",
    "x",
    "xor",
    "xx",
    "|",
    "||",
    "~",
    "~&",
    "~<",
    "~>",
    "~^",
    "~|",
    "~~",
    "×",
    "÷",
    "…",
    "…^",
    "⇒",
    "∈",
    "∉",
    "∊",
    "∋",
    "∌",
    "∍",
    "−",
    "∖",
    "∘",
    "∩",
    "∪",
    "≅",
    "≠",
    "≡",
    "≢",
    "≤",
    "≥",
    "≼",
    "≽",
    "⊂",
    "⊃",
    "⊄",
    "⊅",
    "⊆",
    "⊇",
    "⊈",
    "⊉",
    "⊍",
    "⊎",
    "⊖",
    "⚛+=",
    "⚛-=",
    "⚛=",
    "⚛−=",
    "⩵",
    "⩶",
];

/// Does rakudo declare `&infix:<op>`? `op` is the bare operator name, without
/// the `infix:<...>` wrapper.
///
/// `false` means the operator has no core candidate set: whatever mutsu answers
/// for it natively is a convenience over a core routine of another name, so a
/// user `infix:<op>` declaration replaces it outright.
pub(crate) fn rakudo_declares_infix(op: &str) -> bool {
    RAKUDO_CORE_INFIX_NAMES.binary_search(&op).is_ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn core_infix_name_table_is_sorted_for_binary_search() {
        assert!(
            RAKUDO_CORE_INFIX_NAMES.windows(2).all(|w| w[0] < w[1]),
            "RAKUDO_CORE_INFIX_NAMES must be sorted in byte order and free of duplicates"
        );
    }

    #[test]
    fn operators_rakudo_declares_are_found() {
        for op in [
            "+", "-", "*", "~", "cmp", "minmax", "min", "max", "X", "Z", "eqv", "but",
        ] {
            assert!(rakudo_declares_infix(op), "rakudo declares infix:<{op}>");
        }
    }

    #[test]
    fn list_operator_spellings_are_not_core_infixes() {
        // `raku -e 'say (1,2) cross (3,4)'` is a syntax error: these are core
        // `sub`s, and mutsu's infix spelling of them is its own convenience.
        for op in [
            "cross",
            "zip",
            "roundrobin",
            "sum",
            "flat",
            "unique",
            "squish",
            "@@",
        ] {
            assert!(
                !rakudo_declares_infix(op),
                "rakudo has no infix:<{op}> routine"
            );
        }
    }
}
