//! The regex capture accumulator (`RegexCaptures`) and its cold half
//! (`RareCaps`). Moved from `runtime/regex_types.rs` (#10779).

use super::MatchTarget;
use super::NamedCaptureMap;
use super::{OuterBackrefCaps, PosSlot, RegexVarMap};
use crate::symbol::Symbol;
use crate::value::Value;
use rustc_hash::FxHashMap as HashMap;
use std::sync::Arc;

/// The hash-capture map shape (`%<name>=(...)` aliasing in regex).
pub(crate) type HashCaptureMap = HashMap<String, Vec<(String, Option<String>)>>;

/// The capture-alias map shape (`<str=.str_escape>` → original rule name).
///
/// Interned, like [`NamedCaptureMap`]: both halves of an entry are capture
/// names the (memoized) lookup spec already holds as `Symbol`s, and the map is
/// deep-cloned with every `RegexCaptures` the engine clones. Owned `String`s
/// made that clone allocate two per alias per candidate.
pub(crate) type CaptureAliasMap = HashMap<Symbol, Symbol>;

/// The cold half of [`RegexCaptures`], behind one allocation that most
/// accumulators never make.
///
/// The engine constructs, moves, clones and drops a `RegexCaptures` **per
/// match candidate** — millions of times over one grammar parse — while every
/// field in here is written by a minority of patterns: `:my` declarators,
/// capture aliases, `%<name>=` hash captures, a
/// protoregex `:sym<>` win, and the two engine-entry-point links (`target`,
/// `outer_backref`). Keeping them inline made the accumulator 336 bytes, so
/// the per-candidate `memcpy` traffic and three `HashMap` drops were paid by
/// every candidate to carry state almost none of them had
/// ([#7576](https://github.com/tokuhirom/mutsu/issues/7576) item 4). This is
/// the ADR-0016 P2 [`CapNode`](super::CapNode)/[`CapChildren`](super::CapChildren) split applied one level up, to
/// the accumulator instead of the stored node.
///
/// Reach it through the accessors on [`RegexCaptures`]: the `_mut` ones
/// materialize the payload, the read-only ones hand back a shared empty value
/// when it was never allocated.
#[derive(Clone, Default)]
pub(crate) struct RareCaps {
    /// Variables declared via `:my $var = expr;` inside regex.
    /// These are made available to `<{ code }>` closures.
    ///
    /// Shared rather than owned: every inline sub-pattern (a group, an
    /// alternative, a lookaround body) publishes the lexicals in scope to the
    /// store it is about to build, and doing that by value cloned the whole
    /// map per atom match — 1.25% of a YAML parse in `arm_inline_vars_seed`
    /// alone. An `Arc` makes arming and seeding refcount bumps; the copy is
    /// paid only by a level that actually writes a lexical, through
    /// `Arc::make_mut`.
    pub(crate) regex_vars: Option<Arc<RegexVarMap>>,
    /// For aliased captures like `<str=.str_escape>`, maps capture name to
    /// original rule name for grammar action dispatch.
    pub(crate) capture_alias_map: CaptureAliasMap,
    /// Hash captures from `%<name>=(...)` aliasing in regex.
    pub(crate) hash_captures: HashCaptureMap,
    /// The shared subject this match ran against (ADR-0016 P3). Set once by
    /// the engine entry point on the returned top-level accumulator — the
    /// engine itself never touches it. Consumers derive captured text from
    /// recorded spans through it instead of a stored `matched` string.
    pub(crate) target: Option<MatchTarget>,
    /// The enclosing pattern level's captures, for backreference READS only
    /// (see [`OuterBackrefCaps`]). Set once on a nested walk's base store and
    /// never merged, propagated, or published — it is a read-through link to
    /// the parent walk, not a capture of this level.
    pub(crate) outer_backref: Option<Arc<OuterBackrefCaps>>,
    /// The grammar instance the rule invocation that produced this match owned
    /// (see [`CapChildren::cursor`](super::CapChildren::cursor)). Set where a rule invocation returns, from
    /// the compiled engine's frame; carried onto the stored node by
    /// [`RegexCaptures::into_cap_node`].
    pub(crate) cursor: Option<Value>,
    /// The cursor's own span `(start, end)` when a `<(` / `)>` marker
    /// narrowed the match's `.from`/`.to` (see
    /// [`RegexCaptures::finish_span`]). `None` when no marker fired, so the
    /// span is `from`/`to`.
    pub(crate) cursor_span: Option<(usize, usize)>,
}

impl RareCaps {
    /// True when nothing is left worth keeping the allocation for. Checked
    /// after a drain/take so a payload that has been emptied out again does
    /// not make every later clone copy an empty one.
    fn is_empty(&self) -> bool {
        self.regex_vars.as_ref().is_none_or(|vars| vars.is_empty())
            && self.capture_alias_map.is_empty()
            && self.hash_captures.is_empty()
            && self.target.is_none()
            && self.outer_backref.is_none()
            && self.cursor.is_none()
            && self.cursor_span.is_none()
    }
}

#[derive(Clone, Default)]
pub(crate) struct RegexCaptures {
    /// Named captures as span-bearing slots (ADR-0016 P4). Keyed by capture
    /// name; silent-action captures use interned `SILENT_ACTION_MARKER_PREFIX` keys.
    pub(crate) named: NamedCaptureMap,
    /// Positional captures as span-bearing slots (ADR-0016 P4): span, nested
    /// subcaptures, quantified iteration lists, and the Nil marker in one axis.
    pub(crate) positional: Vec<PosSlot>,
    pub(crate) from: usize,
    pub(crate) to: usize,
    pub(crate) capture_start: Option<usize>,
    pub(crate) capture_end: Option<usize>,
    /// Starting position of the match in the input (character index).
    /// Set at the beginning of regex matching to allow code blocks to compute
    /// the matched-so-far text.
    pub(crate) match_from: usize,
    /// The AST value produced by this node's inline `{ make … }` code block(s),
    /// computed at reduce time (`reduce_regex_captures_made`). Carried into the
    /// Match object built by `make_match_object_full_q` so `$<sub>.made` /
    /// `$<sub>».made` resolve in a parent inline action and post-parse. `None`
    /// when the rule ran no `make`.
    pub(crate) ast: Option<Value>,
    /// The winning `:sym<…>` variant and the aliased rule name (see
    /// [`NodeNames`]).
    pub(crate) names: NodeNames,
    /// The cold fields, allocated on first write (see [`RareCaps`]).
    pub(crate) rare: Option<Box<RareCaps>>,
}

/// A capture's two interned names, packed into one word: the winning
/// `:sym<…>` variant, if the match was a proto candidate's, and the original
/// rule name when it was stored under an alias (the empty name: a capture no
/// rule produced). Inline (#10488): a proto return used to clone the variant
/// name into a freshly allocated [`RareCaps`]; packed so the accumulator stays
/// within its size budget (`regex_captures_size_guard`).
#[derive(Clone, Copy)]
pub(crate) struct NodeNames {
    sym: u32,
    action_name: u32,
}

impl NodeNames {
    const NONE: u32 = u32::MAX;

    #[inline]
    fn get(raw: u32) -> Option<Symbol> {
        (raw != Self::NONE).then(|| Symbol::from_raw(raw))
    }

    #[inline]
    fn put(sym: Option<Symbol>) -> u32 {
        sym.map_or(Self::NONE, Symbol::raw)
    }
}

impl Default for NodeNames {
    fn default() -> Self {
        NodeNames {
            sym: Self::NONE,
            action_name: Self::NONE,
        }
    }
}

static EMPTY_REGEX_VARS: std::sync::LazyLock<RegexVarMap> =
    std::sync::LazyLock::new(RegexVarMap::default);
static EMPTY_HASH_CAPTURES: std::sync::LazyLock<HashCaptureMap> =
    std::sync::LazyLock::new(HashCaptureMap::default);

impl RegexCaptures {
    /// The rule name and span of every named capture node in this tree.
    // Cost: O(n), n = nodes in the tree.
    pub(crate) fn tree_spans(&self) -> super::cap_node::SurvivingSpans<'_> {
        let mut out = super::cap_node::SurvivingSpans::default();
        super::cap_node::collect_named_spans(&self.named, &mut out);
        super::cap_node::collect_positional_spans(&self.positional, &mut out);
        out
    }

    /// The cold payload, if this accumulator ever wrote one.
    #[inline]
    pub(crate) fn rare(&self) -> Option<&RareCaps> {
        self.rare.as_deref()
    }

    /// Did this accumulator ever write a cold payload? A `false` answers every
    /// `take_*`/`*_mut` question about [`RareCaps`] at once, so a caller that
    /// would otherwise build, iterate and discard three empty maps can skip
    /// them in one branch.
    #[inline]
    pub(crate) fn has_rare(&self) -> bool {
        self.rare.is_some()
    }

    /// The cold payload, allocating it on first use. Prefer the read-only
    /// accessors on a path that only inspects — this is what turns a
    /// few-word accumulator back into an allocating one.
    #[inline]
    pub(crate) fn rare_mut(&mut self) -> &mut RareCaps {
        self.rare.get_or_insert_with(Default::default)
    }

    /// Drop the payload again if a drain/take emptied it.
    #[inline]
    pub(crate) fn prune_rare(&mut self) {
        if self.rare.as_deref().is_some_and(RareCaps::is_empty) {
            self.rare = None;
        }
    }

    #[inline]
    pub(crate) fn regex_vars(&self) -> &RegexVarMap {
        self.rare()
            .and_then(|rare| rare.regex_vars.as_deref())
            .unwrap_or(&EMPTY_REGEX_VARS)
    }

    /// The shared handle, for publishing these lexicals to an inline
    /// sub-pattern without copying them.
    #[inline]
    pub(crate) fn regex_vars_shared(&self) -> Option<&Arc<RegexVarMap>> {
        self.rare()
            .and_then(|rare| rare.regex_vars.as_ref())
            .filter(|vars| !vars.is_empty())
    }

    /// Adopt an already-shared lexical map wholesale (the inline-sub-pattern
    /// seed). Replaces whatever this accumulator held.
    pub(crate) fn set_regex_vars_shared(&mut self, vars: Option<Arc<RegexVarMap>>) {
        match vars {
            Some(vars) if !vars.is_empty() => self.rare_mut().regex_vars = Some(vars),
            _ => {
                if let Some(rare) = self.rare.as_deref_mut() {
                    rare.regex_vars = None;
                }
                self.prune_rare();
            }
        }
    }

    #[inline]
    pub(crate) fn regex_vars_mut(&mut self) -> &mut RegexVarMap {
        Arc::make_mut(
            self.rare_mut()
                .regex_vars
                .get_or_insert_with(Default::default),
        )
    }

    /// Move the `:my` variable map out, leaving an empty one behind.
    pub(crate) fn take_regex_vars(&mut self) -> RegexVarMap {
        let taken = self
            .rare
            .as_deref_mut()
            .and_then(|rare| rare.regex_vars.take());
        self.prune_rare();
        // Free while this level owned the only handle, which is the usual case
        // — a shared one means an inline sub-pattern is reading it right now.
        taken.map(Arc::unwrap_or_clone).unwrap_or_default()
    }

    /// Merge another accumulator's `:my` variables into this one, without
    /// allocating a payload when there is nothing to merge.
    pub(crate) fn extend_regex_vars<I: IntoIterator<Item = (String, Value)>>(&mut self, vars: I) {
        let mut it = vars.into_iter();
        let Some(first) = it.next() else { return };
        let dst = self.regex_vars_mut();
        dst.insert(first.0, first.1);
        dst.extend(it);
    }

    /// File the grammar instance the rule invocation that produced this match
    /// owned (see [`CapChildren::cursor`](super::CapChildren::cursor)). The payload is allocated only for
    /// an invocation that has one.
    pub(crate) fn set_cursor(&mut self, cursor: Value) {
        self.rare_mut().cursor = Some(cursor);
    }

    /// Take the grammar instance filed by [`Self::set_cursor`], leaving none.
    pub(crate) fn take_cursor(&mut self) -> Option<Value> {
        let taken = self.rare.as_deref_mut().and_then(|rare| rare.cursor.take());
        self.prune_rare();
        taken
    }

    #[inline]
    pub(crate) fn capture_alias_map_mut(&mut self) -> &mut CaptureAliasMap {
        &mut self.rare_mut().capture_alias_map
    }

    /// Merge another accumulator's capture aliases into this one, without
    /// allocating a payload when there is nothing to merge.
    pub(crate) fn extend_capture_alias_map<I: IntoIterator<Item = (Symbol, Symbol)>>(
        &mut self,
        aliases: I,
    ) {
        let mut it = aliases.into_iter();
        let Some(first) = it.next() else { return };
        let dst = self.capture_alias_map_mut();
        dst.insert(first.0, first.1);
        dst.extend(it);
    }

    /// Move the capture-alias map out, leaving an empty one behind.
    pub(crate) fn take_capture_alias_map(&mut self) -> CaptureAliasMap {
        let taken = self
            .rare
            .as_deref_mut()
            .map(|rare| std::mem::take(&mut rare.capture_alias_map))
            .unwrap_or_default();
        self.prune_rare();
        taken
    }

    #[inline]
    pub(crate) fn hash_captures(&self) -> &HashCaptureMap {
        self.rare()
            .map_or(&EMPTY_HASH_CAPTURES, |rare| &rare.hash_captures)
    }

    #[inline]
    pub(crate) fn hash_captures_mut(&mut self) -> &mut HashCaptureMap {
        &mut self.rare_mut().hash_captures
    }

    /// Move the hash-capture map out, leaving an empty one behind.
    pub(crate) fn take_hash_captures(&mut self) -> HashCaptureMap {
        let taken = self
            .rare
            .as_deref_mut()
            .map(|rare| std::mem::take(&mut rare.hash_captures))
            .unwrap_or_default();
        self.prune_rare();
        taken
    }

    /// Fold another accumulator's hash captures into this one (per-name append),
    /// without allocating a payload when there is nothing to fold.
    pub(crate) fn merge_hash_captures(&mut self, other: HashCaptureMap) {
        if other.is_empty() {
            return;
        }
        let dst = self.hash_captures_mut();
        for (k, v) in other {
            dst.entry(k).or_default().extend(v);
        }
    }

    #[inline]
    pub(crate) fn sym(&self) -> Option<Symbol> {
        NodeNames::get(self.names.sym)
    }

    /// Set (or clear) the winning `:sym<>` variant name.
    #[inline]
    pub(crate) fn set_sym(&mut self, sym: Option<Symbol>) {
        self.names.sym = NodeNames::put(sym);
    }

    /// Move the `:sym<>` variant name out.
    #[inline]
    pub(crate) fn take_sym(&mut self) -> Option<Symbol> {
        let sym = self.sym();
        self.names.sym = NodeNames::NONE;
        sym
    }

    /// The aliased-capture rule name.
    #[inline]
    pub(crate) fn action_name(&self) -> Option<Symbol> {
        NodeNames::get(self.names.action_name)
    }

    /// Set (or clear) the aliased-capture rule name.
    #[inline]
    pub(crate) fn set_action_name(&mut self, action_name: Option<Symbol>) {
        self.names.action_name = NodeNames::put(action_name);
    }

    #[inline]
    pub(crate) fn target(&self) -> Option<&MatchTarget> {
        self.rare().and_then(|rare| rare.target.as_ref())
    }

    /// Publish the shared subject on this accumulator (ADR-0016 P3).
    pub(crate) fn set_target(&mut self, target: Option<MatchTarget>) {
        if target.is_none() && self.rare.is_none() {
            return;
        }
        self.rare_mut().target = target;
        self.prune_rare();
    }

    #[inline]
    pub(crate) fn outer_backref(&self) -> Option<&Arc<OuterBackrefCaps>> {
        self.rare().and_then(|rare| rare.outer_backref.as_ref())
    }

    /// Link this level's base store to the enclosing pattern level, for
    /// backreference reads only.
    pub(crate) fn set_outer_backref(&mut self, outer: Option<Arc<OuterBackrefCaps>>) {
        if outer.is_none() && self.rare.is_none() {
            return;
        }
        self.rare_mut().outer_backref = outer;
        self.prune_rare();
    }
}

#[cfg(test)]
mod cap_node_tests {
    use super::*;
    use crate::value::regex_caps::CapNode;

    /// ADR-0016 P2: a stored leaf capture node must stay a handful of words —
    /// that is the entire point of the `CapNode`/`RegexCaptures` split (a
    /// stored leaf used to cost the full ~600-byte accumulator). P3 removed
    /// the stored `matched` text (spans + the shared `MatchTarget` derive it),
    /// shrinking the bound further. If a change pushes `CapNode` past this,
    /// move the new field into `CapChildren`.
    #[test]
    fn cap_node_size_guard() {
        assert!(
            std::mem::size_of::<CapNode>() <= 88,
            "CapNode grew to {} bytes",
            std::mem::size_of::<CapNode>()
        );
    }

    /// A leaf conversion must not allocate a child payload.
    #[test]
    fn into_cap_node_leaf_has_no_children() {
        let caps = RegexCaptures {
            from: 3,
            to: 4,
            ..Default::default()
        };
        let node = caps.into_cap_node();
        assert!(node.children.is_none());
        assert_eq!((node.from, node.to), (3, 4));
    }

    /// #7576 item 4: the engine constructs, moves, clones and drops one of
    /// these per match candidate — millions of times over a grammar parse — so
    /// its size is the per-candidate `memcpy` bill. It was 336 bytes with the
    /// cold fields inline. If a change pushes it past this, the new field
    /// almost certainly belongs in [`RareCaps`].
    #[test]
    fn regex_captures_size_guard() {
        assert!(
            std::mem::size_of::<RegexCaptures>() <= 128,
            "RegexCaptures grew to {} bytes",
            std::mem::size_of::<RegexCaptures>()
        );
    }

    /// The point of the split: an accumulator that took no cold-field write
    /// never allocates the payload, and reading one back gives the empty
    /// value rather than allocating one to look at.
    #[test]
    fn plain_accumulator_allocates_no_rare_payload() {
        let mut caps = RegexCaptures::default();
        assert!(caps.rare().is_none());
        assert!(caps.regex_vars().is_empty());
        assert!(caps.hash_captures().is_empty());
        assert!(caps.sym().is_none());
        assert!(caps.target().is_none());
        assert!(caps.outer_backref().is_none());
        // Clearing an absent field, and merging in nothing, stay allocation-free.
        caps.set_sym(None);
        caps.set_target(None);
        caps.set_outer_backref(None);
        caps.extend_regex_vars(RegexVarMap::default());
        caps.extend_capture_alias_map(CaptureAliasMap::default());
        caps.merge_hash_captures(HashCaptureMap::default());
        assert!(caps.rare().is_none());
        assert!(caps.take_regex_vars().is_empty());
        assert!(caps.rare().is_none());
    }

    /// A write materializes the payload; emptying it again drops it, so a
    /// drained accumulator does not make every later clone copy an empty one.
    #[test]
    fn rare_payload_is_dropped_once_emptied() {
        let mut caps = RegexCaptures::default();
        caps.regex_vars_mut()
            .insert("$x".to_string(), Value::int(1));
        assert!(caps.rare().is_some());
        assert_eq!(caps.take_regex_vars().len(), 1);
        assert!(caps.rare().is_none());
    }
}
