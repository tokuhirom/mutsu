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

/// Internal attribute marking a grammar cursor — a Match whose `class_name` is
/// the grammar's own type rather than `Match` (raku: `Grammar` IS a `Match`
/// subclass, so `G.parse(...).^name` is `G`).
///
/// A live cursor keeps the lazy `ValueRepr::Match`, so the tag probe in
/// [`Value::is_match_instance`] answers for it directly. But the post-hoc
/// attribute rebuilds (`match_with_attrs*` for `.made`/`actions`/
/// `capture_alias_map`, the action walk's write-backs) produce plain eager
/// `Instance`s, and for those the class name is no longer the "is a Match"
/// signal.
///
/// The marker is written ONCE, into `MatchNode::materialize_map`, so every
/// eager derivative inherits it by copying the attribute map — no construction
/// site has to know about cursors. It shares the map with the other internal
/// keys (`silent_caps`, `capture_alias_map`, `reduce_time_vars`,
/// `__failed_match__`) and is never exposed through `.hash`/`.list`.
pub(crate) const CURSOR_MATCH_MARKER: &str = "__grammar_cursor__";

impl Value {
    /// Is this value a `Match` instance (regex match object)? True for both
    /// the lazy repr (`ValueRepr::Match`, checked WITHOUT materializing) and
    /// an eager/rebuilt `Instance("Match")`.
    ///
    /// This is the ONLY correct "is a Match" test — an inline
    /// `class_name == "Match"` is not, because a grammar cursor reports the
    /// grammar's own class (raku: `Grammar` IS a `Match` subclass, so
    /// `G.parse(...).^name` is `G`). Every cursor keeps the lazy repr, so the
    /// tag probe below answers for them without any registry lookup.
    pub(crate) fn is_match_instance(&self) -> bool {
        if self.0.as_match_node().is_some() {
            return true;
        }
        matches!(self.view(), ValueView::Instance { class_name, attributes, .. }
            if class_name == "Match" || attributes.as_map().get(CURSOR_MATCH_MARKER).is_some())
    }

    /// The class name a Match receiver dispatches under: `"Match"` for a plain
    /// regex match, the grammar's own class for a parse cursor. Callers that
    /// used to hardcode `"Match"` for user-override / native-method lookups
    /// pass this instead, so a grammar's own `method made { ... }` wins over
    /// the native `Match.made` the way raku's MRO makes it.
    pub(crate) fn match_dispatch_class(&self) -> &'static str {
        match self.0.as_match_node() {
            Some(node) => node.cursor_class().as_str(),
            None => "Match",
        }
    }

    /// Read one attribute of a `Match` instance. `None` when `self` is not a
    /// Match or the attribute is absent. The internal funnel every public
    /// accessor below goes through. On an unmaterialized lazy Match, scalar
    /// attributes are answered straight from the capture node.
    fn match_attr(&self, name: &str) -> Option<Value> {
        if let Some(node) = self.0.as_match_node() {
            return node.attr(name);
        }
        if !self.is_match_instance() {
            return None;
        }
        match self.view() {
            ValueView::Instance { attributes, .. } => attributes.as_map().get(name).cloned(),
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
        if !self.is_match_instance() {
            return None;
        }
        match self.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } => {
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
        if !self.is_match_instance() {
            return None;
        }
        match self.view() {
            ValueView::Instance {
                class_name,
                attributes,
                id,
            } => {
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
