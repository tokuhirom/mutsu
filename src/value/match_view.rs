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
    /// Is this value a `Match` instance (regex match object)? True for both
    /// the lazy repr (`ValueRepr::Match`, checked WITHOUT materializing) and
    /// an eager/rebuilt `Instance("Match")`.
    pub(crate) fn is_match_instance(&self) -> bool {
        if self.0.as_match_node().is_some() {
            return true;
        }
        matches!(self.view(), ValueView::Instance { class_name, .. } if class_name == "Match")
    }

    /// Read one attribute of a `Match` instance. `None` when `self` is not a
    /// Match or the attribute is absent. The internal funnel every public
    /// accessor below goes through. On an unmaterialized lazy Match, scalar
    /// attributes are answered straight from the capture node.
    fn match_attr(&self, name: &str) -> Option<Value> {
        if let Some(node) = self.0.as_match_node() {
            return node.attr(name);
        }
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

    /// A Match equal to `self` with the given attributes replaced — the
    /// rebuild pattern every post-hoc attribute write (`.made`/`ast`,
    /// `actions`, `capture_alias_map`, `orig`, `named`, `list`) uses.
    /// `None` when `self` is not a Match. The rebuilt Match is a fresh
    /// instance (new identity), same as the inline clone-insert-rebuild
    /// sites this replaces.
    pub(crate) fn match_with_attrs(&self, updates: Vec<(&str, Value)>) -> Option<Value> {
        match self.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Match" => {
                let attrs = attributes.as_ref().clone();
                for (k, v) in updates {
                    attrs.insert(k.to_string(), v);
                }
                Some(Value::make_instance(class_name, attrs.to_map()))
            }
            _ => None,
        }
    }

    /// [`Self::match_with_attrs`], but preserving the instance identity —
    /// `Match.make` writes the updated Match back under the SAME id, because
    /// consumers re-read live objects by `(class, id)` match (see the grammar
    /// action walk). `None` when `self` is not a Match.
    pub(crate) fn match_with_attrs_keeping_id(&self, updates: Vec<(&str, Value)>) -> Option<Value> {
        match self.view() {
            ValueView::Instance {
                class_name,
                attributes,
                id,
            } if class_name == "Match" => {
                let attrs = InstanceAttrs::clone(&attributes);
                for (k, v) in updates {
                    attrs.insert(k.to_string(), v);
                }
                Some(Value::instance_parts(
                    class_name,
                    crate::gc::Gc::new(InstanceAttrs::new(class_name, attrs.to_map(), id, false)),
                    id,
                ))
            }
            _ => None,
        }
    }

    /// A Match with `.ast` set, preserving the instance identity (`Match.make`).
    pub(crate) fn match_with_ast_keeping_id(&self, ast: Value) -> Option<Value> {
        self.match_with_attrs_keeping_id(vec![("ast", ast)])
    }
}
