//! ADR-0016 P5: the lazily materialized `Match` representation.
//!
//! A regex/grammar match no longer eagerly builds a full `Instance("Match")`
//! attribute tree per capture node. The match VALUE is a
//! `ValueRepr::Match(Gc<MatchNode>)` holding the shared subject
//! (`MatchTarget`) and the stored capture node (`Arc<CapNode>`); the
//! Instance-shaped attribute map is materialized once, on first `view()`
//! decode, and memoized. Materialization is ONE level deep: child captures
//! become lazy `Match` values themselves, so a subtree nobody inspects never
//! allocates anything beyond the `CapNode` the matcher already built.
//!
//! ADR-0016 P3: capture nodes no longer store matched text — `.Str` is
//! derived from the recorded span through the shared `MatchTarget`.
//!
//! Consumers are unchanged: `view()` on a lazy Match forces the memoized map
//! and presents `ValueView::Instance` exactly as an eager Match. The seam
//! accessors (`match_view.rs`) answer scalar reads (`.from`, `.Str`, `.made`,
//! ...) straight from the `CapNode` without forcing. Post-hoc attribute
//! writes (`match_with_attrs*`) force and rebuild a plain eager Instance,
//! same as before -- except `.made`, which `make` writes in place into the
//! capture node's shared cell (#12954).

use super::*;
use crate::symbol::wk;
use crate::value::regex_caps::{
    CapNode, MatchTarget, NamedSlot, PosSlot, SILENT_ACTION_MARKER_PREFIX,
};
use std::sync::OnceLock;

/// Interned class symbol for `Match`.
pub(in crate::value) fn match_class_symbol() -> Symbol {
    static SYM: OnceLock<Symbol> = OnceLock::new();
    *SYM.get_or_init(|| Symbol::intern("Match"))
}

/// The class a cursor produced against `target` reports: the grammar's own
/// type for a grammar parse (raku: `Grammar` IS a `Match` subclass and every
/// cursor of a parse is of the invoked grammar's type), plain `Match` for an
/// ordinary regex match.
pub(crate) fn cursor_class_symbol(target: &MatchTarget) -> Symbol {
    target.cursor_class().unwrap_or_else(match_class_symbol)
}

/// The payload of a lazy `Match` value. See the module doc.
pub(crate) struct MatchNode {
    /// The shared subject this match ran against, shared by every node of
    /// the tree. Answers `.orig` and derives `.Str` from the span.
    pub(in crate::value) target: MatchTarget,
    /// The stored capture node this Match presents.
    pub(in crate::value) cap: Arc<CapNode>,
    /// Stable instance identity (same id domain as eager `Instance`s).
    pub(in crate::value) id: u64,
    /// Memoized materialization, built on first `view()` decode.
    attrs: OnceLock<crate::gc::Gc<InstanceAttrs>>,
    /// The memoized positional-capture list (`.list`, `$/[i]`), built on
    /// first read without forcing [`Self::attrs`], which then holds this
    /// same list: a repeated read hands out the same child values either way.
    list: OnceLock<Value>,
    /// The memoized named-capture hash (`named`), on the same terms as
    /// [`Self::list`].
    named: OnceLock<Value>,
}

impl std::fmt::Debug for MatchNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MatchNode")
            .field("id", &self.id)
            .field("from", &self.cap.from)
            .field("to", &self.cap.to)
            .field("forced", &self.attrs.get().is_some())
            .finish()
    }
}

impl MatchNode {
    pub(in crate::value) fn new(cap: Arc<CapNode>, target: MatchTarget) -> Self {
        Self {
            target,
            cap,
            id: next_instance_id(),
            attrs: OnceLock::new(),
            list: OnceLock::new(),
            named: OnceLock::new(),
        }
    }

    /// The memoized attribute node, if already materialized.
    pub(in crate::value) fn forced(&self) -> Option<&crate::gc::Gc<InstanceAttrs>> {
        self.attrs.get()
    }

    /// The class this cursor reports (`Match`, or the grammar's own type).
    pub(crate) fn cursor_class(&self) -> Symbol {
        cursor_class_symbol(&self.target)
    }

    /// Whether this is an ordinary regex match, not a grammar cursor (whose
    /// class is the grammar's own and may override any `Match` method).
    // Cost: O(1).
    pub(crate) fn is_plain(&self) -> bool {
        self.target.cursor_class().is_none()
    }

    /// Force the Instance-shaped materialization (one level deep).
    pub(in crate::value) fn force_attrs(&self) -> &crate::gc::Gc<InstanceAttrs> {
        self.attrs.get_or_init(|| {
            crate::value::regex_caps::stats::record_regex_match_materialization();
            crate::gc::Gc::new(InstanceAttrs::new(
                cursor_class_symbol(&self.target),
                self.materialize_map(),
                self.id,
                true,
            ))
        })
    }

    /// Whether this node can still stand for its capture node: unforced, or
    /// forced under the cursor class its run reports now. A node forced in
    /// mid-parse (a code block or a wrapper read it) was materialized as a
    /// plain `Match` before the parse stamped its grammar class on the run,
    /// so its memoized map no longer reads as the run's cursors do; the
    /// finished tree must not reuse it (ADR-11404).
    // Cost: O(1).
    fn is_current(&self) -> bool {
        self.attrs
            .get()
            .is_none_or(|attrs| attrs.class_name() == cursor_class_symbol(&self.target))
    }

    /// `make`: write the capture node's shared `.made` cell, and mirror it
    /// into the memoized attribute map when this node is already forced, so
    /// both read paths of `.made` agree (#12954).
    // Cost: O(1) expected.
    pub(in crate::value) fn set_made(&self, made: Value) {
        self.cap.made.set(made.clone());
        if let Some(attrs) = self.attrs.get() {
            attrs.insert("ast", made);
        }
    }

    /// Sever the memoized `Gc` edges (cycle-collector reclaim).
    pub(in crate::value) fn take_attrs_for_gc(&mut self) {
        let _ = self.attrs.take();
        let _ = self.list.take();
        let _ = self.named.take();
    }

    /// The memoized positional-capture list, if already built.
    pub(in crate::value) fn memoized_list(&self) -> Option<&Value> {
        self.list.get()
    }

    /// The memoized named-capture hash, if already built.
    pub(in crate::value) fn memoized_named(&self) -> Option<&Value> {
        self.named.get()
    }

    /// The memoized named-capture hash, built from the capture node on first
    /// use. Silent-action captures (a hidden `<.foo>`, under a marker-prefixed
    /// key) are not in it.
    // Cost: O(1) once built; the first read is O(n), n = named captures.
    fn memo_named(&self) -> &Value {
        self.named.get_or_init(|| {
            let mut sub_named: ValueMap = ValueMap::default();
            for (key, slot) in &self.cap.kids().named {
                if key.starts_with(SILENT_ACTION_MARKER_PREFIX) {
                    continue;
                }
                sub_named.insert(key.resolve(), Value::named_slot_value(slot, &self.target));
            }
            Value::hash_bare_values(sub_named)
        })
    }

    /// The positional-capture list (`list`), without forcing the
    /// materialization: the forced map's own list when it is forced, else the
    /// memoized one, built on first read.
    // Cost: O(1) once built; the first read is O(p), p = positional captures.
    pub(in crate::value) fn positional_list(&self) -> Value {
        if let Some(attrs) = self.attrs.get() {
            return attrs
                .as_map()
                .get(wk::match_list())
                .cloned()
                .unwrap_or_else(|| Value::array(Vec::new()));
        }
        self.memo_list().clone()
    }

    /// The memoized list, built from the capture node on first use.
    fn memo_list(&self) -> &Value {
        self.list.get_or_init(|| {
            Value::array(
                self.cap
                    .kids()
                    .positional
                    .iter()
                    .map(|slot| Value::pos_slot_value(slot, &self.target))
                    .collect(),
            )
        })
    }

    /// This node's matched text, derived from the recorded span.
    pub(in crate::value) fn span_text(&self) -> String {
        self.target.span_str(self.cap.from, self.cap.to)
    }

    /// [`Self::span_text`] as a `Str` value, sharing an ASCII subject's buffer.
    fn span_value(&self) -> Value {
        self.target.span_value(self.cap.from, self.cap.to)
    }

    /// Read one attribute, without forcing when it is derivable from the
    /// capture node. `list` and `named` are memoized on their own (and shared
    /// with a later materialization); the other structural attributes
    /// (`silent_caps`, `reduce_time_vars`, `capture_alias_map`) force it.
    pub(in crate::value) fn attr(&self, name: &str) -> Option<Value> {
        if let Some(attrs) = self.attrs.get() {
            return attrs.as_map().get(name).cloned();
        }
        match name {
            "str" => Some(self.span_value()),
            "from" => Some(Value::Int(self.cap.from as i64)),
            "to" => Some(Value::Int(self.cap.to as i64)),
            "orig" => Some(Value::str_arc(Arc::clone(self.target.text()))),
            "ast" => self.cap.made.get(),
            "sym_variant" => self.cap.sym.map(|s| Value::str(s.resolve())),
            "action_name" => self.cap.action_name.map(|s| Value::str(s.resolve())),
            // Post-hoc attributes exist only on REBUILT eager Matches (the
            // rebuild helpers produce plain Instances); a live lazy node
            // never carries them. Answering `None` here rather than falling
            // through keeps a probe for one from forcing the materialization.
            "pos" => self.cap.kids().pos.map(|pos| Value::Int(pos as i64)),
            // `actions` is on the start rule's node of a parse only.
            "actions" => self.cap.kids().actions.clone(),
            "__failed_match__" => None,
            crate::value::match_view::CURSOR_REGEXSUB_ATTR => None,
            "list" => Some(self.memo_list().clone()),
            "named" => Some(self.memo_named().clone()),
            _ => self.force_attrs().as_map().get(name).cloned(),
        }
    }

    /// A lazy child Match sharing this node's subject.
    fn lazy_child(&self, sc: &Arc<CapNode>) -> Value {
        Value::lazy_match(Arc::clone(sc), self.target.clone())
    }

    /// Build this node's attribute map — the pre-P5 `make_subcap_match`, with
    /// recursion replaced by lazy children.
    fn materialize_map(&self) -> AttrMap {
        let cap = &*self.cap;
        if cap.children.is_none() {
            crate::value::regex_caps::stats::record_regex_match_leaf(false);
        }
        let kids = cap.kids();

        // Silent-action captures: hidden `<.foo>` subrule matches (stored
        // under a marker-prefixed key). Absent from `.hash`, but the grammar
        // action walk fires their action methods via `silent_caps`.
        let mut silent_caps_vals: Vec<Value> = Vec::new();
        for (key, slot) in &kids.named {
            if key.starts_with(SILENT_ACTION_MARKER_PREFIX) {
                for sc in &slot.nodes {
                    silent_caps_vals.push(self.lazy_child(sc));
                }
            }
        }

        // The fixed keys below plus the optional ones: sized up front so the
        // map does not rehash as it fills.
        let mut attrs = AttrMap::with_capacity(14);
        // A grammar cursor's class name is the GRAMMAR's, so it can no longer
        // double as the "this Instance is a Match" signal. Record the fact in
        // the attribute map instead: every eager derivative of this Match (the
        // `match_with_attrs*` rebuilds, the action walk's write-backs) copies
        // the map wholesale, so the marker propagates without every one of
        // those construction sites having to know about cursors.
        if self.target.cursor_class().is_some() {
            attrs.insert(
                crate::value::match_view::CURSOR_MATCH_MARKER,
                Value::Bool(true),
            );
        }
        attrs.insert(wk::match_str(), self.span_value());
        attrs.insert(wk::match_from(), Value::Int(cap.from as i64));
        attrs.insert(wk::match_to(), Value::Int(cap.to as i64));
        if let Some(pos) = kids.pos {
            attrs.insert(wk::match_pos(), Value::Int(pos as i64));
        }
        attrs.insert(wk::match_list(), self.memo_list().clone());
        attrs.insert(wk::match_named(), self.memo_named().clone());
        if !silent_caps_vals.is_empty() {
            attrs.insert(wk::match_silent_caps(), Value::real_array(silent_caps_vals));
        }
        attrs.insert(
            wk::match_orig(),
            Value::str_arc(Arc::clone(self.target.text())),
        );
        if let Some(sym) = &cap.sym {
            attrs.insert(wk::match_sym_variant(), Value::str(sym.resolve()));
        }
        if let Some(action_name) = &cap.action_name {
            attrs.insert(wk::match_action_name(), Value::str(action_name.resolve()));
        }
        // This node's `.made`: an inline `{ make … }` from reduce time, or a
        // grammar action's `make` that already ran.
        if let Some(ast) = cap.made.get() {
            attrs.insert(wk::match_ast(), ast);
        }
        // Per-match `:my $*x` values, re-installed around this node's action
        // by the grammar action walk.
        if !kids.regex_vars.is_empty() {
            let vars: ValueMap = kids
                .regex_vars
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            attrs.insert(wk::match_reduce_time_vars(), Value::hash_bare_values(vars));
        }
        if let Some(actions) = &kids.actions {
            attrs.insert("actions", actions.clone());
        }
        if !kids.capture_alias_map.is_empty() {
            let alias_hash: ValueMap = kids
                .capture_alias_map
                .iter()
                .map(|(k, v)| (k.as_str().to_string(), Value::str(v.as_str().to_string())))
                .collect();
            attrs.insert("capture_alias_map", Value::hash_bare_values(alias_hash));
        }
        // The grammar attributes a method wrote on this rule invocation's
        // cursor are the Match's own (`$<t>.inv`). The cursor's positional
        // state (`orig`/`from`/`pos`/`to`) is the Match's, set above.
        if let Some(cursor) = &kids.cursor
            && let ValueView::Instance { attributes, .. } = cursor.view()
        {
            for (key, value) in attributes.as_map().iter() {
                if !crate::sym_in!(*key, "orig" | "from" | "pos" | "to") {
                    attrs.insert(*key, value.clone());
                }
            }
        }
        attrs
    }
}

/// What the grammar-action walk needs of one Match node (see
/// [`Value::match_action_walk_node`]).
///
/// The walk descends only into children that can still dispatch an action:
/// a child whose action already ran at its reduction (ADR-7576) is not listed,
/// and neither is a positional group that is a bare leaf, since a `( )` group
/// is not a rule and has no action of its own. The node's `.hash`/`.list`
/// values, which only the env ceremony around an action call needs, are
/// rendered on demand by [`Self::named_values`] and
/// [`Self::positional_values`].
#[derive(Default)]
pub(crate) struct ActionWalkNode {
    /// The named children still to dispatch, each with its capture name, a
    /// quantified slot flattened to its entries.
    pub(crate) named_children: Vec<(Symbol, Value)>,
    /// The hidden `<.foo>` subrule matches still to dispatch.
    pub(crate) silent: Vec<Value>,
    /// The positional group Matches still to dispatch, flattened.
    pub(crate) positional_children: Vec<Value>,
    /// The node's own `:my $*x` values at its reduction.
    pub(crate) reduce_vars: Vec<(String, Value)>,
    /// The winning `:sym<...>` variant, for a proto-regex candidate.
    pub(crate) sym: Option<Symbol>,
    /// Whether the node has a named-capture hash (every lazy node does; an
    /// eager Match may not).
    pub(crate) has_named_hash: bool,
    captures: WalkCaptures,
}

/// Where [`ActionWalkNode`] renders the node's capture values from.
#[derive(Default)]
enum WalkCaptures {
    /// The capture node of the lazy Match the node was read from, rendered
    /// on demand from that Match.
    Lazy,
    /// The `named` and `list` attributes of an eager Match.
    Eager(Option<Value>, Option<Value>),
    #[default]
    None,
}

impl ActionWalkNode {
    /// The walk's view of an eager Match: its `.hash` and `.list` (as the
    /// attribute map holds them) and its hidden subrule matches.
    // Cost: O(k), k = the node's capture entries; a capture name is interned
    // only for a child still to dispatch.
    pub(crate) fn eager(
        named: Option<Value>,
        positional: Option<Value>,
        silent: Vec<Value>,
    ) -> Self {
        let mut named_children = Vec::new();
        if let Some(ValueView::Hash(map)) = named.as_ref().map(Value::view) {
            for (name, value) in map.iter() {
                // `is_match_instance` first: viewing a lazy child Match would
                // materialize it only to learn that it is not an Array.
                let items: Vec<Value> = if !value.is_match_instance()
                    && let ValueView::Array(items, _) = value.view()
                {
                    items.iter().cloned().collect()
                } else {
                    vec![value.clone()]
                };
                for item in items {
                    if !item.match_node_reduced() {
                        named_children.push((Symbol::intern(name), item));
                    }
                }
            }
        }
        let mut positional_children = Vec::new();
        if let Some(ValueView::Array(items, _)) = positional.as_ref().map(Value::view) {
            for item in items.iter() {
                if item.is_match_instance() {
                    positional_children.push(item.clone());
                } else if let ValueView::Array(inner, _) = item.view() {
                    positional_children.extend(inner.iter().cloned());
                }
            }
        }
        positional_children.retain(|child| !child.match_node_reduced());
        Self {
            named_children,
            silent,
            positional_children,
            has_named_hash: matches!(named.as_ref().map(Value::view), Some(ValueView::Hash(_))),
            captures: WalkCaptures::Eager(named, positional),
            ..Self::default()
        }
    }

    /// The walk node of a lazy Match with nothing left to dispatch below it
    /// (see `Value::match_lazy_walk_is_done`): what
    /// [`Value::match_action_walk_node`] would build for it, without the scan.
    // Cost: O(1), no allocation.
    pub(crate) fn lazy_done(sym: Option<Symbol>) -> Self {
        Self {
            sym,
            has_named_hash: true,
            captures: WalkCaptures::Lazy,
            ..Self::default()
        }
    }

    /// Nothing below this node is left to dispatch.
    pub(crate) fn has_no_children(&self) -> bool {
        self.named_children.is_empty()
            && self.positional_children.is_empty()
            && self.silent.is_empty()
    }

    /// The node's named captures, as `.hash` presents each slot. `match_obj`
    /// is the Match the node was read from.
    // Cost: O(k), k = the node's named capture slots.
    pub(crate) fn named_values(&self, match_obj: &Value) -> Vec<(Symbol, Value)> {
        match &self.captures {
            WalkCaptures::Lazy => match match_obj.0.as_match_node() {
                Some(node) => node
                    .cap
                    .kids()
                    .named
                    .iter()
                    .filter(|(key, _)| !key.starts_with(SILENT_ACTION_MARKER_PREFIX))
                    .map(|(key, slot)| (*key, Value::named_slot_value(slot, &node.target)))
                    .collect(),
                None => Vec::new(),
            },
            WalkCaptures::Eager(named, _) => match named.as_ref().map(Value::view) {
                Some(ValueView::Hash(map)) => map
                    .iter()
                    .map(|(k, v)| (Symbol::intern(k), v.clone()))
                    .collect(),
                _ => Vec::new(),
            },
            WalkCaptures::None => Vec::new(),
        }
    }

    /// The node's positional captures, as `.list` presents each slot.
    /// `match_obj` is the Match the node was read from.
    // Cost: O(k), k = the node's positional capture slots.
    pub(crate) fn positional_values(&self, match_obj: &Value) -> Vec<Value> {
        match &self.captures {
            WalkCaptures::Lazy => match match_obj.0.as_match_node() {
                Some(node) => node
                    .cap
                    .kids()
                    .positional
                    .iter()
                    .map(|slot| Value::pos_slot_value(slot, &node.target))
                    .collect(),
                None => Vec::new(),
            },
            WalkCaptures::Eager(_, positional) => match positional.as_ref().map(Value::view) {
                Some(ValueView::Array(items, _)) => items.iter().cloned().collect(),
                _ => Vec::new(),
            },
            WalkCaptures::None => Vec::new(),
        }
    }
}

/// Whether a positional group's capture node can still dispatch an action:
/// it has captures of its own to descend into, or it was filed under an
/// aliased rule name, and its action has not run yet.
pub(super) fn group_may_dispatch(sc: &CapNode) -> bool {
    (sc.children.is_some() || sc.action_name.is_some() || sc.sym.is_some()) && !sc.reduced.get()
}

/// Leaf Match for a capture slot or quantified-capture entry that carries
/// only its span (no inner capture node).
///
/// It is the same lazy `ValueRepr::Match` over a span-only node that a slot
/// holding a leaf node renders as. Under a grammar parse it has to be lazy
/// anyway: the leaf reports the grammar's cursor class (raku:
/// `G.parse(...)[0].^name` is `G`), and the repr, not the class name, is what
/// tells every consumer "this is a Match". A plain regex used to get an eager
/// `Instance` here instead, which was dearer to build and to read than the
/// node.
///
/// Rendering a span-only slot this way is what lets the compiled engine leave
/// a capture group whose body captures nothing as a span-only slot
/// (`rx_capture_op`'s `CloseCapture`). The node is built when the slot is
/// read, once, rather than on every close of the group while the match
/// backtracks.
// Cost: O(1).
fn span_leaf_match(from: usize, to: usize, target: &MatchTarget) -> Value {
    let caps = crate::value::regex_caps::RegexCaptures {
        from,
        to,
        ..Default::default()
    };
    Value::lazy_match(Arc::new(caps.into_cap_node()), target.clone())
}

impl Value {
    /// Render one positional capture slot exactly as a Match's `.list` exposes
    /// it: `Nil` for an unmatched optional, an Array for a quantified group,
    /// a lazy child Match for a group with its own inner captures, otherwise a
    /// span leaf.
    ///
    /// Shared with the regex engine's *mid-match* variable binding, so `$0`
    /// read from inside an embedded `{ … }` code block is the same `Match` the
    /// finished `$/[0]` will be (raku: `/ (\d) { say $0 } /` prints `｢1｣`, not
    /// the bare string). Building the slot's value directly keeps the parent
    /// cursor lazy — the block may never look at `$/` at all.
    pub(crate) fn pos_slot_value(slot: &PosSlot, target: &MatchTarget) -> Value {
        // An unmatched optional capture (`(x)?` zero match) renders as Nil.
        if slot.nil {
            return Value::Nil;
        }
        if let Some(qlist) = &slot.quantified {
            let arr: Vec<Value> = qlist
                .iter()
                .map(|(qfrom, qto, subcap)| match subcap {
                    Some(sc) => Value::lazy_match(Arc::clone(sc), target.clone()),
                    None => span_leaf_match(*qfrom, *qto, target),
                })
                .collect();
            // A quantified positional capture group (`(x)*`) is a true Array
            // in raku (`(a)+` on "a" gives `$0.raku` = `[...]`, not `(...)`),
            // same as the named-capture case just below (`real_array`).
            return Value::real_array(arr);
        }
        if let Some(subcap) = &slot.subcap {
            return Value::lazy_match(Arc::clone(subcap), target.clone());
        }
        // ADR-0016 P4: every slot carries its span, so a subcap-less leaf
        // renders with its REAL offsets (pre-P4 this was the text-only
        // fallback with fabricated `0..len`).
        span_leaf_match(slot.from, slot.to, target)
    }

    /// Render one named capture slot exactly as a Match's `.hash` exposes it:
    /// a single Match, or an Array when the name was quantified or captured
    /// more than once. Companion of [`Self::pos_slot_value`].
    pub(crate) fn named_slot_value(slot: &NamedSlot, target: &MatchTarget) -> Value {
        let vals: Vec<Value> = slot
            .nodes
            .iter()
            .map(|sc| Value::lazy_match(Arc::clone(sc), target.clone()))
            .collect();
        if vals.len() == 1 && !slot.quantified {
            vals.into_iter().next().unwrap()
        } else {
            // Quantified names (including zero-iteration ones) and multi-entry
            // captures render as arrays.
            Value::real_array(vals)
        }
    }

    /// Eager leaf Match for a TEXT-ONLY capture entry (no recorded span).
    /// ADR-0016 P4 removed the stored text axis, so the matcher never
    /// produces these; it survives only for the exploded text-carrier builder
    /// (`make_match_object_with_captures`) whose sources (transliteration
    /// callbacks, code-block snapshots) genuinely have no offsets. The span
    /// is unrecoverable here, so it is reported as `0..chars` of the captured
    /// text itself.
    pub(crate) fn text_leaf_match(s: &str, target: &MatchTarget) -> Value {
        crate::value::regex_caps::stats::record_regex_match_leaf(true);
        let mut attrs = AttrMap::new();
        attrs.insert("str", Value::str(s.to_string()));
        attrs.insert("from", Value::Int(0));
        attrs.insert("to", Value::Int(s.chars().count() as i64));
        attrs.insert("list", Value::array(Vec::new()));
        attrs.insert("named", Value::hash_bare_values(ValueMap::default()));
        attrs.insert("orig", Value::str_arc(Arc::clone(target.text())));
        Value::make_instance(match_class_symbol(), attrs)
    }
}

impl Value {
    /// The lazy `Match` presenting a stored capture node against the shared
    /// subject: the one already built for `cap` in this engine run when it is
    /// still alive, else a new one (ADR-11404). Every slot and every parent
    /// that holds the same node therefore exposes the same `Match`: a
    /// non-suppressing alias `<val=word>` files one node under both names, so
    /// `$m<val> === $m<word>` (GH #8167), and a node reached through two
    /// parents is forced once instead of being materialized again per parent
    /// (#11404, where that made a wrapped recursion quadratic).
    // Cost: O(1).
    pub(crate) fn lazy_match(cap: Arc<CapNode>, target: MatchTarget) -> Value {
        let node = cap.match_memo.get_or_make(
            |live| live.target.same_run(&target) && live.is_current(),
            || crate::gc::Gc::new(MatchNode::new(Arc::clone(&cap), target.clone())),
        );
        Value::from_repr(ValueRepr::Match(node))
    }

    /// Let this lazy Match present `node` too, when a Match over `node` would
    /// read exactly as this one (`CapNode::presents_like`): a wrapped token's
    /// `callsame` result is then the very Match its caller files as
    /// `$<rule>`, as in rakudo (#12918). Returns whether it was adopted.
    // Cost: O(c), c = the child slots of the two nodes.
    pub(crate) fn adopt_as_match_of(&self, node: &Arc<CapNode>) -> bool {
        let Some(own) = self.0.as_match_node() else {
            return false;
        };
        if !Arc::ptr_eq(&own.cap, node) && !own.cap.presents_like(node) {
            return false;
        }
        match self.0.match_node_weak() {
            Some(weak) => {
                node.match_memo.set(weak);
                // The two nodes now present one Match, so the action that ran
                // for either at the reduction has run for both: the post-parse
                // walk reaches this Match through `own`, the dispatch ran on
                // `node` (#16410).
                if node.reduced.get() {
                    own.cap.reduced.set();
                }
                if own.cap.reduced.get() {
                    node.reduced.set();
                }
                true
            }
            None => false,
        }
    }

    /// A just-built lazy Match whose cursor ended at `pos` although a `)>`
    /// marker put its `.to` earlier (`RegexCaptures::narrowed_pos`). Returns
    /// `self` unchanged when `pos` is `None` or `self` is not a still-lazy
    /// Match.
    // Cost: O(1) when `pos` is `None`; otherwise one capture-node clone.
    pub(crate) fn with_match_cursor_pos(self, pos: Option<usize>) -> Value {
        let Some(pos) = pos else {
            return self;
        };
        let Some(node) = self.0.as_match_node() else {
            return self;
        };
        if node.forced().is_some() {
            return self;
        }
        let mut cap = (*node.cap).clone();
        cap.kids_mut().pos = Some(pos);
        Value::lazy_match(Arc::new(cap), node.target.clone())
    }

    /// The structure the grammar-action walk reads from a node, taken
    /// straight from the capture node of an UNMATERIALIZED lazy Match: the
    /// child slots render exactly as `.hash`/`.list` would expose them (lazy
    /// child Matches), but the node itself is never forced. `None` for an
    /// eager or already-materialized Match; the walk reads its attribute map
    /// instead.
    // Cost: O(k), k = this node's capture slots.
    pub(crate) fn match_action_walk_node(&self) -> Option<ActionWalkNode> {
        let node = self.0.as_match_node()?;
        if node.forced().is_some() {
            return None;
        }
        let mut walk = ActionWalkNode {
            sym: node.cap.sym,
            has_named_hash: true,
            captures: WalkCaptures::Lazy,
            ..ActionWalkNode::default()
        };
        if node.cap.children.is_none() {
            return Some(walk);
        }
        let kids = node.cap.kids();
        for (key, slot) in kids.named.iter() {
            let pending = slot.nodes.iter().filter(|sc| !sc.reduced.get());
            if key.starts_with(SILENT_ACTION_MARKER_PREFIX) {
                walk.silent.extend(pending.map(|sc| node.lazy_child(sc)));
            } else {
                walk.named_children
                    .extend(pending.map(|sc| (*key, node.lazy_child(sc))));
            }
        }
        for slot in &kids.positional {
            if slot.nil {
                continue;
            }
            match &slot.quantified {
                Some(entries) => {
                    for (_, _, sc) in entries {
                        if let Some(sc) = sc
                            && group_may_dispatch(sc)
                        {
                            walk.positional_children.push(node.lazy_child(sc));
                        }
                    }
                }
                None => {
                    if let Some(sc) = &slot.subcap
                        && group_may_dispatch(sc)
                    {
                        walk.positional_children.push(node.lazy_child(sc));
                    }
                }
            }
        }
        walk.reduce_vars = kids
            .regex_vars
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        Some(walk)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::regex_caps::RegexCaptures;

    fn leaf_cap(from: usize, to: usize) -> Arc<CapNode> {
        let caps = RegexCaptures {
            from,
            to,
            ..Default::default()
        };
        Arc::new(caps.into_cap_node())
    }

    #[test]
    fn lazy_match_scalar_reads_do_not_force() {
        let m = Value::lazy_match(leaf_cap(3, 5), MatchTarget::new("xxxab"));
        assert!(m.is_match_instance());
        assert_eq!(m.match_from(), Some(3));
        assert_eq!(m.match_to(), Some(5));
        assert_eq!(
            m.match_str_value().map(|v| v.to_string_value()),
            Some("ab".to_string())
        );
        assert!(m.0.as_match_node().unwrap().forced().is_none());
    }

    #[test]
    fn one_lazy_match_per_node_and_run() {
        let id = |m: &Value| m.0.as_match_node().unwrap().id;
        let cap = leaf_cap(0, 1);
        let target = MatchTarget::new("ab");
        let first = Value::lazy_match(Arc::clone(&cap), target.clone());
        let again = Value::lazy_match(Arc::clone(&cap), target.clone());
        assert_eq!(id(&first), id(&again));
        // Another run over an equal subject is not the same run.
        let other = Value::lazy_match(Arc::clone(&cap), MatchTarget::new("ab"));
        assert_ne!(id(&first), id(&other));
        // A cloned node is a different node.
        let copy = Value::lazy_match(Arc::new((*cap).clone()), target.clone());
        assert_ne!(id(&copy), id(&other));
        // Once nothing holds it, the next request builds a new Match.
        drop((first, again, other));
        let fresh = Value::lazy_match(Arc::clone(&cap), target);
        assert!(fresh.0.as_match_node().unwrap().forced().is_none());
    }

    #[test]
    fn lazy_match_views_as_instance() {
        let m = Value::lazy_match(leaf_cap(3, 5), MatchTarget::new("xxxab"));
        match m.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } => {
                assert_eq!(class_name.as_str(), "Match");
                let map = attributes.as_map();
                assert_eq!(map.get("str").and_then(Value::as_str), Some("ab"));
                assert_eq!(map.get("from").and_then(Value::as_int), Some(3));
            }
            other => panic!("expected Instance view, got {other:?}"),
        }
        assert!(m.0.as_match_node().unwrap().forced().is_some());
        // Post-force scalar reads come from the forced map and stay coherent.
        assert_eq!(m.match_from(), Some(3));
    }

    #[test]
    fn lazy_match_children_stay_lazy_one_level() {
        // parent { named: x => child }, child a leaf with a span.
        let child = leaf_cap(1, 2);
        let mut caps = RegexCaptures {
            from: 0,
            to: 2,
            ..Default::default()
        };
        caps.named.insert(
            Symbol::intern("x"),
            crate::value::regex_caps::NamedSlot {
                nodes: crate::value::regex_caps::CapNodes::one(Arc::clone(&child)),
                quantified: false,
            },
        );
        let parent = Value::lazy_match(Arc::new(caps.into_cap_node()), MatchTarget::new("ab"));
        let named = parent.match_named().expect("named hash");
        let child_val = match named.view() {
            ValueView::Hash(h) => h.map.get("x").cloned().expect("x"),
            other => panic!("expected hash, got {other:?}"),
        };
        assert!(child_val.is_match_instance());
        // The child is itself a lazy, unforced Match.
        assert!(child_val.0.as_match_node().unwrap().forced().is_none());
        assert_eq!(child_val.match_from(), Some(1));
    }
}
