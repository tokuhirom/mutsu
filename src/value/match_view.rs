//! ADR-0016 P5 seam: accessor helpers for the `Match` representation.
//!
//! Every consumer that reads a Raku `Match` object's internals goes through
//! these `Value` methods instead of pattern-matching the `Instance` shape
//! inline. This is the pure-refactor half of P5: once all consumers use the
//! seam, the representation behind it can swap to a dedicated lazy repr
//! (`ValueRepr::Match`) without touching the consumers again. The inventory
//! of sites is `todo/deep/adr0016-p5-match-consumer-inventory.md`.
//!
//! Accessors return owned `Value`s (an `Arc`/`Gc` bump) or plain scalars —
//! never references into the attribute map — so their signatures survive the
//! repr swap.

use super::*;

impl Value {
    /// Is this value a `Match` instance (regex match object)?
    pub(crate) fn is_match_instance(&self) -> bool {
        matches!(self.view(), ValueView::Instance { class_name, .. } if class_name == "Match")
    }

    /// Read one attribute of a `Match` instance. `None` when `self` is not a
    /// Match or the attribute is absent. The internal funnel every public
    /// accessor below goes through.
    fn match_attr(&self, name: &str) -> Option<Value> {
        match self.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Match" => attributes.as_map().get(name).cloned(),
            _ => None,
        }
    }

    /// The matched text (`.Str`) as a `Value`, usually `Value::Str`.
    pub(crate) fn match_str_value(&self) -> Option<Value> {
        self.match_attr("str")
    }

    /// The match's start offset (`.from`). `None` when not a Match.
    pub(crate) fn match_from(&self) -> Option<i64> {
        self.match_attr("from")?.as_int()
    }

    /// The match's end offset (`.to`). `None` when not a Match.
    pub(crate) fn match_to(&self) -> Option<i64> {
        self.match_attr("to")?.as_int()
    }

    /// The whole subject string the match ran against (`.orig`).
    pub(crate) fn match_orig(&self) -> Option<Value> {
        self.match_attr("orig")
    }

    /// The positional-capture list (`.list`), an array `Value`.
    pub(crate) fn match_list(&self) -> Option<Value> {
        self.match_attr("list")
    }

    /// The named-capture hash (`.hash`), a hash `Value`.
    pub(crate) fn match_named(&self) -> Option<Value> {
        self.match_attr("named")
    }

    /// The `make`-produced value (`.made`/`.ast`).
    pub(crate) fn match_ast(&self) -> Option<Value> {
        self.match_attr("ast")
    }

    /// Whether this is a failed `.subparse` Match (defined but falsy).
    pub(crate) fn match_is_failed(&self) -> bool {
        self.match_attr("__failed_match__")
            .is_some_and(|v| v.truthy())
    }

    /// The original rule name for grammar action dispatch, set on aliased
    /// captures (`<x=rule>`).
    pub(crate) fn match_action_name(&self) -> Option<String> {
        match self.match_attr("action_name")?.view() {
            ValueView::Str(s) => Some(s.to_string()),
            _ => None,
        }
    }
}
