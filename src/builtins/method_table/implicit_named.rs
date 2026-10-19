//! The implicit `*%_` of a built-in method on the routes that reach a
//! routine by name, not through the guard step (ADR-11276 slice 5).
//!
//! Every Raku *method* carries an implicit `*%_`, so a named argument the
//! method does not declare is swallowed and cannot change the answer. The
//! guard step applies it to a call it answers from a row
//! (`dispatch::bind_named`). A call that reaches a routine some other way (the
//! by-name dispatchers, the positional protocol's late rows, a code object's
//! retry) has its arguments read by position, so a named argument it does not
//! bind would be read as data (`"abc".chop(:zzz)` numified the `Pair` to a 0
//! character count; `10.polymod(3, :zzz)` read it as a second modulus). Those
//! routes drop it first, here.
//!
//! The statement of which names a method binds is its rows' `named` lists
//! (and [`RowFlags::ANY_NAMED`](super::RowFlags::ANY_NAMED) for a row that
//! binds every one), the names Rakudo's candidates declare
//! (`scripts/native-method-adverb-survey.raku`). It replaced the hand-kept
//! `accepted_nameds` table, which named the same sets a second time. A method
//! with no row is left alone.

use crate::symbol::Symbol;
use crate::value::{Value, ValueView};

/// Drop the call-site named arguments no built-in row of `method` binds: the
/// implicit `*%_` of a Raku method swallows them. A name some row binds (one
/// that lists it, or binds every name) is kept, for the row or the routine
/// that reads it.
///
/// Returns `None` when there is nothing to do -- no nameds, a method no row is
/// named, or every named bound -- so the overwhelmingly common case costs one
/// tag-level scan and no allocation.
///
/// Named-ness is a call-site property (ADR-0021): only the `Pair` flavour is a
/// named argument, so a *positional* `Pair` (`%h.push((a => 1))`, a `Pair` held
/// in a variable, a `rotor` cycle spec such as `2 => -1`) is never dropped.
// Cost: O(a) with no named argument, a = arguments; otherwise O(a * k * n),
// k = rows named `method`, n = names each declares.
pub(crate) fn strip_undeclared_nameds(method: &str, args: &[Value]) -> Option<Vec<Value>> {
    // Tag probe first: `view()` on a lazy Match argument would materialize it.
    if !args.iter().any(|a| a.is_string_pair_value()) {
        return None;
    }
    let method = Symbol::intern(method);
    if !super::names_any_row(method) {
        return None;
    }
    let kept: Vec<Value> = args
        .iter()
        .filter(|a| {
            if !a.is_string_pair_value() {
                return true;
            }
            match a.view() {
                ValueView::Pair(key, _) => super::some_row_binds_named(method, key.as_str()),
                _ => true,
            }
        })
        .cloned()
        .collect();
    if kept.len() == args.len() {
        None
    } else {
        Some(kept)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_undeclared_method_keeps_every_argument() {
        let args = vec![Value::pair("zzz".to_string(), Value::TRUE)];
        assert!(strip_undeclared_nameds("no-such-builtin-method", &args).is_none());
    }

    #[test]
    fn a_declared_method_drops_only_the_undeclared_named() {
        let args = vec![
            Value::int(2),
            Value::pair("partial".to_string(), Value::TRUE),
            Value::pair("zzz".to_string(), Value::TRUE),
        ];
        let kept = strip_undeclared_nameds("rotor", &args).expect("zzz must go");
        assert_eq!(kept.len(), 2);
        assert!(kept[1].is_string_pair_value());
    }

    #[test]
    fn a_positional_pair_is_not_a_named_argument() {
        // `2 => -1` is a rotor cycle spec, not an adverb: the positional Pair
        // flavour must survive even on a declared method.
        let args = vec![Value::value_pair(Value::int(2), Value::int(-1))];
        assert!(strip_undeclared_nameds("rotor", &args).is_none());
    }

    #[test]
    fn nothing_to_strip_allocates_nothing() {
        let args = vec![Value::int(1), Value::int(2)];
        assert!(strip_undeclared_nameds("chop", &args).is_none());
    }

    #[test]
    fn slurpy_reader_rows_keep_their_established_adverbs() {
        let subst_args = vec![
            Value::pair("global".to_string(), Value::TRUE),
            Value::pair("samecase".to_string(), Value::TRUE),
            Value::pair("qqzz9".to_string(), Value::TRUE),
        ];
        let kept = strip_undeclared_nameds("subst", &subst_args).expect("qqzz9 must go");
        assert_eq!(kept.len(), 2);

        let trans_args = vec![
            Value::pair("squash".to_string(), Value::TRUE),
            Value::pair("complement".to_string(), Value::TRUE),
            Value::pair("qqzz9".to_string(), Value::TRUE),
        ];
        let kept = strip_undeclared_nameds("trans", &trans_args).expect("qqzz9 must go");
        assert_eq!(kept.len(), 2);
    }
}
