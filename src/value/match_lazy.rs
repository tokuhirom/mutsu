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
//! same as before.

use super::*;
use crate::runtime::{CapNode, MatchTarget, NamedSlot, PosSlot, SILENT_ACTION_MARKER_PREFIX};
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

    /// Force the Instance-shaped materialization (one level deep).
    pub(in crate::value) fn force_attrs(&self) -> &crate::gc::Gc<InstanceAttrs> {
        self.attrs.get_or_init(|| {
            crate::vm::vm_stats::record_regex_match_materialization();
            crate::gc::Gc::new(InstanceAttrs::new(
                cursor_class_symbol(&self.target),
                self.materialize_map(),
                self.id,
                true,
            ))
        })
    }

    /// Sever the memoized `Gc` edge (cycle-collector reclaim).
    pub(in crate::value) fn take_attrs_for_gc(&mut self) {
        let _ = self.attrs.take();
    }

    /// This node's matched text, derived from the recorded span.
    pub(in crate::value) fn span_text(&self) -> String {
        self.target.span_str(self.cap.from, self.cap.to)
    }

    /// Read one attribute, without forcing when it is derivable from the
    /// capture node. Structural attributes (`list`, `named`, `silent_caps`,
    /// `reduce_time_vars`, `capture_alias_map`) force the materialization.
    pub(in crate::value) fn attr(&self, name: &str) -> Option<Value> {
        if let Some(attrs) = self.attrs.get() {
            return attrs.as_map().get(name).cloned();
        }
        match name {
            "str" => Some(Value::str(self.span_text())),
            "from" => Some(Value::Int(self.cap.from as i64)),
            "to" => Some(Value::Int(self.cap.to as i64)),
            "orig" => Some(Value::str_arc(Arc::clone(self.target.text()))),
            "ast" => self.cap.ast.clone(),
            "sym_variant" => self.cap.sym.clone().map(Value::str),
            "action_name" => self.cap.action_name.clone().map(Value::str),
            // Post-hoc attributes exist only on REBUILT eager Matches (the
            // rebuild helpers produce plain Instances); a live lazy node
            // never carries them. Answering `None` here rather than falling
            // through keeps a probe for one from forcing the materialization.
            "actions" | "__failed_match__" | "pos" => None,
            crate::value::match_view::CURSOR_REGEXSUB_ATTR => None,
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
            crate::vm::vm_stats::record_regex_match_leaf(false);
        }
        let kids = cap.kids();

        // One memo for the whole materialization, so two slots that share a
        // capture node share the child `Match` built for it. See
        // [`SharedNodeValues`].
        let mut shared = SharedNodeValues::new();

        let pos_vals: Vec<Value> = kids
            .positional
            .iter()
            .map(|slot| Value::pos_slot_value_shared(slot, &self.target, &mut shared))
            .collect();

        // Silent-action captures: hidden `<.foo>` subrule matches (stored
        // under a marker-prefixed key). Absent from `.hash`, but the grammar
        // action walk fires their action methods via `silent_caps`.
        let mut sub_named: HashMap<String, Value> = HashMap::new();
        let mut silent_caps_vals: Vec<Value> = Vec::new();
        for (key, slot) in &kids.named {
            if key.starts_with(SILENT_ACTION_MARKER_PREFIX) {
                for sc in &slot.nodes {
                    silent_caps_vals.push(self.lazy_child(sc));
                }
                continue;
            }
            sub_named.insert(
                key.resolve(),
                Value::named_slot_value_shared(slot, &self.target, &mut shared),
            );
        }

        let mut attrs = AttrMap::new();
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
        attrs.insert("str", Value::str(self.span_text()));
        attrs.insert("from", Value::Int(cap.from as i64));
        attrs.insert("to", Value::Int(cap.to as i64));
        attrs.insert("list", Value::array(pos_vals));
        attrs.insert("named", Value::hash_bare_values(sub_named));
        if !silent_caps_vals.is_empty() {
            attrs.insert("silent_caps", Value::real_array(silent_caps_vals));
        }
        attrs.insert("orig", Value::str_arc(Arc::clone(self.target.text())));
        if let Some(sym) = &cap.sym {
            attrs.insert("sym_variant", Value::str(sym.clone()));
        }
        if let Some(action_name) = &cap.action_name {
            attrs.insert("action_name", Value::str(action_name.clone()));
        }
        // Inline `{ make … }` value produced by this subrule at reduce time.
        if let Some(ast) = &cap.ast {
            attrs.insert("ast", ast.clone());
        }
        // Per-match `:my $*x` values, re-installed around this node's action
        // by the grammar action walk.
        if !kids.regex_vars.is_empty() {
            let vars: HashMap<String, Value> = kids
                .regex_vars
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            attrs.insert("reduce_time_vars", Value::hash_bare_values(vars));
        }
        if !kids.capture_alias_map.is_empty() {
            let alias_hash: HashMap<String, Value> = kids
                .capture_alias_map
                .iter()
                .map(|(k, v)| (k.as_str().to_string(), Value::str(v.as_str().to_string())))
                .collect();
            attrs.insert("capture_alias_map", Value::hash_bare_values(alias_hash));
        }
        attrs
    }
}

/// Leaf Match for a quantified-capture entry with a recorded span.
///
/// Under a *grammar* parse this has to be a lazy `ValueRepr::Match` rather than
/// the eager `Instance` the plain-regex path uses: the leaf reports the
/// grammar's cursor class (raku: `G.parse(...)[0].^name` is `G`), and the repr
/// — not the class name — is what tells every consumer "this is a Match".
fn span_leaf_match(from: usize, to: usize, target: &MatchTarget) -> Value {
    if target.cursor_class().is_some() {
        let caps = crate::runtime::RegexCaptures {
            from,
            to,
            ..Default::default()
        };
        return Value::lazy_match(Arc::new(caps.into_cap_node()), target.clone());
    }
    let mut attrs = AttrMap::new();
    attrs.insert("str", Value::str(target.span_str(from, to)));
    attrs.insert("from", Value::Int(from as i64));
    attrs.insert("to", Value::Int(to as i64));
    attrs.insert("list", Value::array(Vec::new()));
    attrs.insert("named", Value::hash_bare_values(HashMap::new()));
    attrs.insert("orig", Value::str_arc(Arc::clone(target.text())));
    Value::make_instance(match_class_symbol(), attrs)
}

/// Memo of the lazy child `Match` already built for a given capture node,
/// keyed by `Arc` identity.
///
/// Two capture slots of the SAME parent can share one `Arc<CapNode>`: a
/// non-suppressing alias `<val=word>` files a single capture under both names
/// (`build_named_candidates_from_inner`'s `shared_under_original`, #7576 round
/// 10). Rakudo hands out one `Match` object for both, so `$m<val> === $m<word>`
/// is `True` there; building the child per SLOT minted two `MatchNode`s with
/// two instance ids and made it `False` on the action-less path, while the
/// action-driven path -- which builds its Matches through the reduce walk --
/// kept the sharing. Memoizing per node makes both paths agree (GH #8167).
///
/// The key is only ever compared while every `Arc` it came from is alive in the
/// parent's `CapChildren`, so no freed-address collision is possible; the memo
/// is per materialization and never outlives it.
type SharedNodeValues = std::collections::HashMap<usize, Value>;

/// The lazy child `Match` for `sc`, reusing the one already built for that same
/// capture node. See [`SharedNodeValues`].
fn shared_node_value(
    shared: &mut SharedNodeValues,
    sc: &Arc<CapNode>,
    target: &MatchTarget,
) -> Value {
    shared
        .entry(Arc::as_ptr(sc) as usize)
        .or_insert_with(|| Value::lazy_match(Arc::clone(sc), target.clone()))
        .clone()
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
        Value::pos_slot_value_shared(slot, target, &mut SharedNodeValues::new())
    }

    /// [`Self::pos_slot_value`] sharing one child `Match` per capture node with
    /// the rest of a materialization. See [`SharedNodeValues`].
    fn pos_slot_value_shared(
        slot: &PosSlot,
        target: &MatchTarget,
        shared: &mut SharedNodeValues,
    ) -> Value {
        // An unmatched optional capture (`(x)?` zero match) renders as Nil.
        if slot.nil {
            return Value::Nil;
        }
        if let Some(qlist) = &slot.quantified {
            let arr: Vec<Value> = qlist
                .iter()
                .map(|(qfrom, qto, subcap)| match subcap {
                    Some(sc) => shared_node_value(shared, sc, target),
                    None => span_leaf_match(*qfrom, *qto, target),
                })
                .collect();
            return Value::array(arr);
        }
        if let Some(subcap) = &slot.subcap {
            return shared_node_value(shared, subcap, target);
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
        Value::named_slot_value_shared(slot, target, &mut SharedNodeValues::new())
    }

    /// [`Self::named_slot_value`] sharing one child `Match` per capture node
    /// with the rest of a materialization. See [`SharedNodeValues`].
    fn named_slot_value_shared(
        slot: &NamedSlot,
        target: &MatchTarget,
        shared: &mut SharedNodeValues,
    ) -> Value {
        let vals: Vec<Value> = slot
            .nodes
            .iter()
            .map(|sc| shared_node_value(shared, sc, target))
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
        crate::vm::vm_stats::record_regex_match_leaf(true);
        let mut attrs = AttrMap::new();
        attrs.insert("str", Value::str(s.to_string()));
        attrs.insert("from", Value::Int(0));
        attrs.insert("to", Value::Int(s.chars().count() as i64));
        attrs.insert("list", Value::array(Vec::new()));
        attrs.insert("named", Value::hash_bare_values(HashMap::new()));
        attrs.insert("orig", Value::str_arc(Arc::clone(target.text())));
        Value::make_instance(match_class_symbol(), attrs)
    }
}

impl Value {
    /// Construct a lazy `Match` from a stored capture node and the shared
    /// subject.
    pub(crate) fn lazy_match(cap: Arc<CapNode>, target: MatchTarget) -> Value {
        Value::from_repr(ValueRepr::Match(crate::gc::Gc::new(MatchNode::new(
            cap, target,
        ))))
    }

    /// For a still-lazy Match: a fresh lazy Match carrying `ast` (a `make`
    /// from a grammar action), sharing the subject and cloning the capture
    /// node. `None` when `self` is eager or already materialized — callers
    /// fall back to the eager rebuild. Used by the action walk's leaf fast
    /// path, where the clone is a childless node (cheap).
    pub(crate) fn match_with_ast_lazy(&self, ast: Value) -> Option<Value> {
        let node = self.0.as_match_node()?;
        if node.forced().is_some() {
            return None;
        }
        let mut cap = (*node.cap).clone();
        cap.ast = Some(ast);
        Some(Value::lazy_match(Arc::new(cap), node.target.clone()))
    }

    /// The per-match `:my $*x` values recorded at reduce time, read straight
    /// from the capture node of a still-lazy Match (no materialization).
    /// `None` for eager/materialized Matches or when the rule declared none.
    /// Used by the action walk's leaf fast path, which must re-install them
    /// around the leaf's action (same as the main walk's `reduce_time_vars`).
    pub(crate) fn match_reduce_time_vars_lazy(&self) -> Option<Vec<(String, Value)>> {
        let node = self.0.as_match_node()?;
        if node.forced().is_some() {
            return None;
        }
        let kids = node.cap.kids();
        if kids.regex_vars.is_empty() {
            return None;
        }
        Some(
            kids.regex_vars
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect(),
        )
    }

    /// Non-forcing peek for the grammar-action walk: for an UNMATERIALIZED
    /// lazy Match, report `(is_childless_leaf, sym_variant)` straight from the
    /// capture node. `None` for eager/materialized Matches — the caller falls
    /// back to reading the attribute map.
    pub(crate) fn match_walk_peek(&self) -> Option<(bool, Option<String>)> {
        let node = self.0.as_match_node()?;
        if node.forced().is_some() {
            return None;
        }
        let kids = node.cap.kids();
        let has_named = kids
            .named
            .keys()
            .any(|k| !k.starts_with(SILENT_ACTION_MARKER_PREFIX));
        let has_list = !kids.positional.is_empty();
        let has_silent = kids
            .named
            .iter()
            .any(|(k, slot)| k.starts_with(SILENT_ACTION_MARKER_PREFIX) && !slot.nodes.is_empty());
        Some((!has_named && !has_list && !has_silent, node.cap.sym.clone()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::RegexCaptures;

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
            crate::runtime::NamedSlot {
                nodes: vec![Arc::clone(&child)],
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
