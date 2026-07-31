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
use crate::runtime::{CapNode, MatchTarget, SILENT_ACTION_MARKER_PREFIX};
use std::sync::OnceLock;

/// Interned class symbol for `Match`.
pub(in crate::value) fn match_class_symbol() -> Symbol {
    static SYM: OnceLock<Symbol> = OnceLock::new();
    *SYM.get_or_init(|| Symbol::intern("Match"))
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

    /// Force the Instance-shaped materialization (one level deep).
    pub(in crate::value) fn force_attrs(&self) -> &crate::gc::Gc<InstanceAttrs> {
        self.attrs.get_or_init(|| {
            crate::gc::Gc::new(InstanceAttrs::new(
                match_class_symbol(),
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
            // never carries them.
            "actions" | "__failed_match__" | "pos" => None,
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

        let pos_vals: Vec<Value> = kids
            .positional
            .iter()
            .map(|slot| {
                // An unmatched optional capture (`(x)?` zero match) renders as Nil.
                if slot.nil {
                    return Value::Nil;
                }
                if let Some(qlist) = &slot.quantified {
                    let arr: Vec<Value> = qlist
                        .iter()
                        .map(|(qfrom, qto, subcap)| {
                            if let Some(sc) = subcap {
                                return self.lazy_child(sc);
                            }
                            span_leaf_match(*qfrom, *qto, &self.target)
                        })
                        .collect();
                    return Value::array(arr);
                }
                if let Some(subcap) = &slot.subcap {
                    return self.lazy_child(subcap);
                }
                // ADR-0016 P4: every slot carries its span, so a subcap-less
                // leaf renders with its REAL offsets (pre-P4 this was the
                // text-only fallback with fabricated `0..len`).
                span_leaf_match(slot.from, slot.to, &self.target)
            })
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
            let vals: Vec<Value> = slot.nodes.iter().map(|sc| self.lazy_child(sc)).collect();
            if vals.len() == 1 && !slot.quantified {
                sub_named.insert(key.clone(), vals[0].clone());
            } else {
                // Quantified names (including zero-iteration ones) and
                // multi-entry captures render as arrays.
                sub_named.insert(key.clone(), Value::real_array(vals));
            }
        }

        let mut attrs = AttrMap::new();
        attrs.insert("str", Value::str(self.span_text()));
        attrs.insert("from", Value::Int(cap.from as i64));
        attrs.insert("to", Value::Int(cap.to as i64));
        attrs.insert("list", Value::array(pos_vals));
        attrs.insert("named", Value::hash(sub_named));
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
            attrs.insert("reduce_time_vars", Value::hash(vars));
        }
        if !kids.capture_alias_map.is_empty() {
            let alias_hash: HashMap<String, Value> = kids
                .capture_alias_map
                .iter()
                .map(|(k, v)| (k.clone(), Value::str(v.clone())))
                .collect();
            attrs.insert("capture_alias_map", Value::hash(alias_hash));
        }
        attrs
    }
}

/// Eager leaf Match for a quantified-capture entry with a recorded span.
fn span_leaf_match(from: usize, to: usize, target: &MatchTarget) -> Value {
    let mut attrs = AttrMap::new();
    attrs.insert("str", Value::str(target.span_str(from, to)));
    attrs.insert("from", Value::Int(from as i64));
    attrs.insert("to", Value::Int(to as i64));
    attrs.insert("list", Value::array(Vec::new()));
    attrs.insert("named", Value::hash(HashMap::new()));
    attrs.insert("orig", Value::str_arc(Arc::clone(target.text())));
    Value::make_instance(match_class_symbol(), attrs)
}

impl Value {
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
        attrs.insert("named", Value::hash(HashMap::new()));
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
        let mut caps = RegexCaptures::default();
        caps.from = from;
        caps.to = to;
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
        let mut caps = RegexCaptures::default();
        caps.from = 0;
        caps.to = 2;
        caps.named.insert(
            "x".to_string(),
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
