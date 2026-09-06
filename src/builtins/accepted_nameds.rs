//! Which named arguments a builtin method accepts.
//!
//! Every Raku *method* carries an implicit `*%_`, so a named argument the
//! method does not declare is swallowed and cannot change the answer. mutsu's
//! builtin methods are dispatched by *arity* rather than by signature, so a
//! named argument occupies a positional slot in that count. `4.log(:base(2))`
//! used to miss the lookup entirely -- that half is fixed, by the implicit-`*%_`
//! retry in `Interpreter::call_method_with_values` (see
//! `news/2026-08/native-methods-honour-the-implicit-slurpy-named.md`).
//!
//! This module closes the other half: the case where the wrong arm *hits*.
//! `"abc".chop(:zzz)` numified the `Pair` to a 0 character count and answered
//! `"abc"`; `10.polymod(3, :zzz)` read it as a second modulus and answered
//! `(1, 3, Inf)`. There is no error to retry on, so the argument list has to be
//! corrected *before* dispatch -- which needs the one thing the arity cascade
//! does not have: a statement of which names each method actually reads.
//!
//! [`native_method_accepted_nameds`] is that statement, and it is deliberately
//! *partial*:
//!
//! - `Some(names)` -- declared. A call-site named argument whose key is not in
//!   `names` is dropped before the builtin layer chooses an arity.
//! - `None` -- undeclared. Nothing is dropped; behaviour is exactly what it was.
//!
//! The default being `None` is what makes the table safe to grow one row at a
//! time: an omission leaves a known-wrong answer wrong, it never invents a new
//! one. The opposite mistake -- declaring a method that really does read an
//! adverb -- makes that adverb stop working, which is loud, and is pinned per
//! declared adverb by `t/native-method-accepted-nameds.t`.
//!
//! The sets are not hand-guessed. They come from Rakudo, via
//! `scripts/native-method-adverb-survey.raku`, which reports the named
//! parameters every candidate of a method declares across every owner type.
//! A routine that validates its own `%_` instead of declaring parameters
//! (`grep`, `first`) accepts names no signature mentions; those are handled by
//! their own validation (they answer `X::Adverb`, as Rakudo does) and are
//! deliberately absent from this table.

use crate::value::{Value, ValueView};

/// The named arguments the builtin implementation of `method` accepts, or
/// `None` if the method has not been surveyed (in which case nothing is
/// dropped). See the module docs for why `None` is the safe default.
pub(crate) fn native_method_accepted_nameds(method: &str) -> Option<&'static [&'static str]> {
    // Every row below is `raku`-derived; see
    // `scripts/native-method-adverb-survey.raku`.
    Some(match method {
        // Surveyed as accepting no named argument at all.
        "AT-KEY" | "AT-POS" | "EXISTS-KEY" | "EXISTS-POS" | "chomp" | "chop" | "combinations"
        | "expmod" | "fmt" | "indent" | "int-bounds" | "join" | "permutations" | "polymod"
        | "roots" | "samecase" | "samemark" | "skip" | "sprintf" | "subbuf" | "subbuf-rw"
        | "tail" | "unimatch" | "uniprops" => &[],
        // Slice 2 of the same survey. `add`/`remove`/`grab` are the mutable
        // QuantHash mutators (`BagHash.add(1, :zzz)` counted the adverb as a
        // second positional and died with an arity error); the rest are the
        // "answers its adverb set only through `%_`" family the survey could
        // previously only report as a lower bound, each confirmed by hand
        // against `raku-doc/doc/Type/` and by probing Rakudo.
        "Array" | "Bool" | "FatRat" | "Int" | "Rat" | "WHICH" | "abs" | "add" | "append"
        | "arity" | "atan2" | "count" | "dd-mm-yyyy" | "gist" | "grab" | "keys" | "kv" | "lazy"
        | "link" | "list" | "mm-dd-yyyy" | "of" | "pairs" | "pop" | "prepend" | "produce"
        | "push" | "reduce" | "remove" | "shift" | "sibling" | "signature" | "splice" | "tree"
        | "unshift" | "values" | "yyyy-mm-dd" => &[],
        "base" => &["no-trailing-zeroes"],
        "minmax" => &["by"],
        "rotor" => &["partial"],
        "classify" | "categorize" => &["as", "into"],
        "classify-list" | "categorize-list" => &["as"],
        "raku" => &["arglist"],
        "Numeric" => &["fail-or-nil"],
        "Str" => &["subscript", "superscript"],
        "unique" => &["as", "expires", "with"],
        // `Any.map` declares these six; `HyperSeq`/`RaceSeq` read `:batch` and
        // `:degree` out of their own `*%options` slurpy.
        "map" => &[
            "batch", "deep", "degree", "duck", "flat", "item", "label", "node",
        ],
        _ => return None,
    })
}

/// Drop the call-site named arguments `method` does not accept.
///
/// Returns `None` when there is nothing to do -- no nameds, an undeclared
/// method, or every named accepted -- so the overwhelmingly common case costs
/// one tag-level scan and no allocation.
///
/// Named-ness is a call-site property (ADR-0021): only the `Pair` flavour is a
/// named argument, so a *positional* `Pair` (`%h.push((a => 1))`, a `Pair` held
/// in a variable, a `rotor` cycle spec such as `2 => -1`) is never dropped.
pub(crate) fn strip_undeclared_nameds(method: &str, args: &[Value]) -> Option<Vec<Value>> {
    // Tag probe first: `view()` on a lazy Match argument would materialize it.
    if !args.iter().any(|a| a.is_string_pair_value()) {
        return None;
    }
    let accepted = native_method_accepted_nameds(method)?;
    let kept: Vec<Value> = args
        .iter()
        .filter(|a| {
            if !a.is_string_pair_value() {
                return true;
            }
            match a.view() {
                ValueView::Pair(key, _) => accepted.contains(&key.as_str()),
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
}
