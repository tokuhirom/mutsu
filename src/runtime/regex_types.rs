//! Data types for the regex engine — the parsed-pattern representation
//! (`RegexPattern`/`RegexToken`/`RegexAtom`/…), the capture accumulator
//! (`RegexCaptures`), and the character-class model (`CharClass`/`ClassItem`).
//!
//! Extracted verbatim from `runtime/mod.rs` (2026-07-21 hygiene re-slim). These
//! were parent-module-private structs whose fields the sibling `regex`/
//! `regex_parse*` modules read directly; moving them into their own module
//! widens the previously module-private structs and fields to `pub(crate)` so
//! those siblings keep their access (the whole set is re-exported from
//! `runtime` via `pub(crate) use self::regex_types::*`).

use crate::symbol::Symbol;
use crate::value::Value;
use rustc_hash::FxHashMap as HashMap;
use std::sync::Arc;

/// The named-capture map shape shared by [`RegexCaptures`], [`CapChildren`]
/// and every helper that walks one.
///
/// Fx-hashed, not SipHash-hashed, on purpose: the key is an interned
/// [`Symbol`] (a `u32`), these maps are probed and rebuilt several times per
/// matched capture, and a regex capture name is never adversarial input in the
/// sense SipHash's DoS resistance exists for. A callgrind profile of a YAML
/// parse put `sip::Hasher::write` + `BuildHasher::hash_one` at ~8% of the whole
/// program, with the regex-capture maps among the dominant callers
/// ([#7576](https://github.com/tokuhirom/mutsu/issues/7576)).
pub(crate) type NamedCaptureMap = HashMap<Symbol, NamedSlot>;

/// The `:my $var = …` regex-variable map shape, Fx-hashed for the same
/// reason as [`NamedCaptureMap`].
pub(crate) type RegexVarMap = HashMap<String, Value>;

#[derive(Clone)]
pub(crate) struct RegexPattern {
    pub(crate) tokens: Vec<RegexToken>,
    pub(crate) anchor_start: bool,
    pub(crate) anchor_end: bool,
    pub(crate) ignore_case: bool,
    pub(crate) ignore_mark: bool,
}

/// A single entry in a quantified capture list: (from, to, subcaptures).
/// The matched text derives from the span through the shared subject
/// (ADR-0016 P3).
///
/// The nested sub-captures are held behind an `Arc` so that cloning a parent
/// `RegexCaptures` during backtracking is a refcount bump rather than a deep
/// copy of the whole sub-match tree. A completed sub-match is effectively
/// immutable once stored; the rare post-store tweak (e.g. setting `action_name`)
/// goes through `Arc::make_mut`, which is free while the entry is still unshared.
pub(crate) type QuantifiedCaptureEntry = (usize, usize, Option<Arc<CapNode>>);

/// One positional capture slot (ADR-0016 P4) — the collapse of the five
/// parallel positional vectors (`positional` text ‖ `positional_subcaps` ‖
/// `positional_quantified` ‖ `positional_offsets` ‖ `positional_nil`) into a
/// single axis. The captured text derives from the span through the shared
/// subject; the alignment invariants the parallel vectors asserted in comments
/// are now structural.
#[derive(Clone, Default)]
pub(crate) struct PosSlot {
    /// The recorded span (char offsets, absolute in the engine's subject).
    pub(crate) from: usize,
    pub(crate) to: usize,
    /// Inner captures from a nested group's own pattern run.
    pub(crate) subcap: Option<Arc<CapNode>>,
    /// When the group was quantified (e.g. `(\w)+`), all iteration matches;
    /// the slot then renders as an Array of Match objects. `Some(vec![])`
    /// marks a zero-iteration quantified capture (renders as an empty list).
    pub(crate) quantified: Option<Vec<QuantifiedCaptureEntry>>,
    /// An *unmatched optional* capture (`(x)?` that matched zero times),
    /// rendered as `Nil` rather than an empty Match.
    pub(crate) nil: bool,
    /// A Nil slot inserted only to reserve an alternation branch's width.
    pub(crate) alternation_padding: bool,
}

impl PosSlot {
    /// A plain matched slot: span only.
    pub(crate) fn span(from: usize, to: usize) -> Self {
        PosSlot {
            from,
            to,
            ..Default::default()
        }
    }

    pub(crate) fn alternation_padding() -> Self {
        PosSlot {
            nil: true,
            alternation_padding: true,
            ..Default::default()
        }
    }
}

/// One named capture's entries (ADR-0016 P4) — the collapse of the three
/// parallel named collections (`named` text map ‖ `named_subcaps` ‖
/// `named_quantified`). Every entry is a span-bearing capture node; the
/// captured text derives from the node's span through the shared subject.
#[derive(Clone, Default)]
pub(crate) struct NamedSlot {
    pub(crate) nodes: Vec<Arc<CapNode>>,
    /// The name was captured under a quantifier (or `@<name>=` forced list):
    /// the Match presents it as an Array even for zero or one entries.
    pub(crate) quantified: bool,
}

impl NamedSlot {
    /// A slot holding one span-only leaf entry.
    pub(crate) fn leaf(from: usize, to: usize) -> Self {
        NamedSlot {
            nodes: vec![Arc::new(CapNode {
                from,
                to,
                ..Default::default()
            })],
            quantified: false,
        }
    }

    /// Fold another slot's entries into this one (capture-merge semantics:
    /// entries append, the quantified flag is sticky).
    pub(crate) fn merge(&mut self, other: NamedSlot) {
        self.nodes.extend(other.nodes);
        self.quantified |= other.quantified;
    }
}

/// The enclosing pattern level's captures, as seen by a **backreference inside
/// an inline sub-pattern**. A group / alternation / lookaround body is matched
/// by its own nested engine walk with its own (empty) capture store, so
/// `$0` / `$<name>` written inside one would otherwise resolve against nothing
/// — `/ $<x>=(\w) [ $<x> ] /` failed where raku matches. Each level links to
/// the one outside it, so a backreference at any nesting depth still sees every
/// capture the *same regex* has taken so far. A subrule call (a different
/// regex) deliberately gets `None` instead of a link, so its own backreferences
/// stay scoped to itself.
pub(crate) struct OuterBackrefCaps {
    pub(crate) named: NamedCaptureMap,
    pub(crate) positional: Vec<PosSlot>,
    pub(crate) parent: Option<Arc<OuterBackrefCaps>>,
}

impl OuterBackrefCaps {
    /// The most recent entry recorded for `name` at this level or any enclosing
    /// one (innermost wins, matching the accumulate-then-read order the flat
    /// non-grouped case has).
    pub(crate) fn lookup_named(self: &Arc<Self>, name: &Symbol) -> Option<&Arc<CapNode>> {
        let mut cur: &Arc<Self> = self;
        loop {
            if let Some(node) = cur.named.get(name).and_then(|slot| slot.nodes.last()) {
                return Some(node);
            }
            match cur.parent.as_ref() {
                Some(p) => cur = p,
                None => return None,
            }
        }
    }

    /// The positional slot at `idx` at this level or any enclosing one.
    pub(crate) fn lookup_positional(self: &Arc<Self>, idx: usize) -> Option<&PosSlot> {
        let mut cur: &Arc<Self> = self;
        loop {
            if let Some(slot) = cur.positional.get(idx) {
                return Some(slot);
            }
            match cur.parent.as_ref() {
                Some(p) => cur = p,
                None => return None,
            }
        }
    }
}

/// Prefix marking a `named_subcaps` entry as a *silent action capture*: the
/// match of a silent subrule (`<.foo>`) that is hidden from `.hash` but whose
/// grammar action method (and its descendants') must still fire. The prefix is a
/// control character that can never appear in a real capture name, so marker
/// entries never collide with user captures and are trivially filtered.
pub(crate) const SILENT_ACTION_MARKER_PREFIX: &str = "\u{1}silent\u{1}";

/// An immutable **stored capture node** — what an `Arc<…>` sub-capture is
/// (ADR-0016 P2). Distinct from [`RegexCaptures`], the engine's *mutable
/// accumulator* for the pattern run in progress: a completed sub-match needs
/// only its span, text, dispatch metadata, and (rarely) children, so storing
/// the full 14-collection accumulator per node cost ~600 zeroed bytes per
/// leaf. A leaf `CapNode` (the overwhelmingly common case — e.g. one per
/// matched character in a quantified `<str=space>` run) collapses every child
/// collection into a single `None`.
///
/// The rare post-store mutation (the reduce walk writing `ast`/`regex_vars`)
/// still goes through `Arc::make_mut`, same as before the split.
#[derive(Clone, Default)]
pub(crate) struct CapNode {
    pub(crate) from: usize,
    pub(crate) to: usize,
    /// The winning :sym<> variant name, if this match was from a protoregex.
    pub(crate) sym: Option<String>,
    /// The original rule name when this capture was stored under an alias.
    pub(crate) action_name: Option<String>,
    /// The AST value produced by this node's inline `{ make … }` code block(s),
    /// computed at reduce time. `None` when the rule ran no `make`.
    pub(crate) ast: Option<Value>,
    /// Child captures + per-node reduce state. `None` for a leaf with no
    /// captures, no code blocks, and no metadata — the size win of the split.
    pub(crate) children: Option<Box<CapChildren>>,
}

/// The non-leaf payload of a [`CapNode`]: child captures on both axes plus the
/// per-node reduce-time state. Boxed so a leaf node pays one `None` word.
#[derive(Clone, Default)]
pub(crate) struct CapChildren {
    /// Named captures as span-bearing slots (ADR-0016 P4): capture nodes and
    /// the quantified flag in one axis. Silent-action captures (`<.foo>`)
    /// live under `SILENT_ACTION_MARKER_PREFIX`-prefixed keys and are hidden
    /// from `.hash`.
    /// Capture names stay interned throughout matching and backtracking; only
    /// Match `.hash` materialization resolves them back to user-facing strings.
    pub(crate) named: NamedCaptureMap,
    pub(crate) capture_alias_map: HashMap<String, String>,
    /// Positional captures as span-bearing slots (ADR-0016 P4). Unlike the
    /// pre-P4 parallel vectors, the span survives onto the stored node — the
    /// text-only leaf fallback (fabricated `0..len` offsets) is gone.
    pub(crate) positional: Vec<PosSlot>,
    /// What this rule's own `:my $*x` declarations held at this match's reduce
    /// (see `Interpreter::record_rule_dynvars`).
    pub(crate) regex_vars: HashMap<String, Value>,
}

impl CapNode {
    /// Immutable child access: an empty default when this node is a leaf.
    pub(crate) fn kids(&self) -> &CapChildren {
        static EMPTY: std::sync::OnceLock<CapChildren> = std::sync::OnceLock::new();
        self.children
            .as_deref()
            .unwrap_or_else(|| EMPTY.get_or_init(CapChildren::default))
    }

    /// Mutable child access, materializing the payload on first use.
    pub(crate) fn kids_mut(&mut self) -> &mut CapChildren {
        self.children.get_or_insert_with(Default::default)
    }
}

impl RegexCaptures {
    /// The subject this capture tree was published with (set by the engine
    /// entry point), else one built fresh from `text` (ADR-0016 P3).
    pub(crate) fn target_or_new(&self, text: &str) -> crate::runtime::MatchTarget {
        self.target()
            .cloned()
            .unwrap_or_else(|| crate::runtime::MatchTarget::new(text))
    }

    /// The whole-match text, derived from the recorded span through the
    /// published subject (ADR-0016 P3). Empty when no subject was published —
    /// callers on the engine-entry consumer paths always have one.
    pub(crate) fn matched_text(&self) -> String {
        self.span_text(self.from, self.to)
    }

    /// The text of an arbitrary recorded span through the published subject.
    pub(crate) fn span_text(&self, from: usize, to: usize) -> String {
        self.target()
            .map(|t| t.span_str(from, to))
            .unwrap_or_default()
    }

    /// A positional slot's text through the published subject (ADR-0016 P4).
    pub(crate) fn slot_text(&self, slot: &PosSlot) -> String {
        self.span_text(slot.from, slot.to)
    }

    /// Convert this accumulator into the immutable stored node it describes
    /// (ADR-0016 P2). Consumes the accumulator; drops the accumulator-only
    /// fields nothing reads through a stored node (`hash_captures`,
    /// `positional_slots`, `capture_start`/`capture_end`, `match_from`). The
    /// child payload is allocated only when something would go in it.
    pub(crate) fn into_cap_node(mut self) -> CapNode {
        // Take the cold payload whole: a leaf (the common case) never had one,
        // so the conversion neither allocates nor touches the fields below.
        let rare = self.rare.take().map(|rare| *rare);
        let (capture_alias_map, regex_vars, sym, action_name) = match rare {
            Some(rare) => (
                rare.capture_alias_map,
                rare.regex_vars,
                rare.sym,
                rare.action_name,
            ),
            None => Default::default(),
        };
        let has_children = !self.named.is_empty()
            || !capture_alias_map.is_empty()
            || !self.positional.is_empty()
            || regex_vars.as_ref().is_some_and(|vars| !vars.is_empty());
        let children = has_children.then(|| {
            Box::new(CapChildren {
                named: self.named,
                capture_alias_map,
                positional: self.positional,
                regex_vars: regex_vars.map(Arc::unwrap_or_clone).unwrap_or_default(),
            })
        });
        CapNode {
            from: self.from,
            to: self.to,
            sym,
            action_name,
            ast: self.ast,
            children,
        }
    }
}

/// The hash-capture map shape (`%<name>=(...)` aliasing in regex).
pub(crate) type HashCaptureMap = HashMap<String, Vec<(String, Option<String>)>>;

/// The capture-alias map shape (`<str=.str_escape>` → original rule name).
pub(crate) type CaptureAliasMap = HashMap<String, String>;

/// The cold half of [`RegexCaptures`], behind one allocation that most
/// accumulators never make.
///
/// The engine constructs, moves, clones and drops a `RegexCaptures` **per
/// match candidate** — millions of times over one grammar parse — while every
/// field in here is written by a minority of patterns: `:my` declarators,
/// capture aliases, `%<name>=` hash captures, the pcre2/`:P5` slot axis, a
/// protoregex `:sym<>` win, and the two engine-entry-point links (`target`,
/// `outer_backref`). Keeping them inline made the accumulator 336 bytes, so
/// the per-candidate `memcpy` traffic and three `HashMap` drops were paid by
/// every candidate to carry state almost none of them had
/// ([#7576](https://github.com/tokuhirom/mutsu/issues/7576) item 4). This is
/// the ADR-0016 P2 [`CapNode`]/[`CapChildren`] split applied one level up, to
/// the accumulator instead of the stored node.
///
/// Reach it through the accessors on [`RegexCaptures`]: the `_mut` ones
/// materialize the payload, the read-only ones hand back a shared empty value
/// when it was never allocated.
#[derive(Clone, Default)]
pub(crate) struct RareCaps {
    /// Unnamed capture slots by capture index (for $0, $1, ...) as recorded
    /// spans, where `None` represents an unmatched capture. A separate
    /// numbering axis from `positional` (it has `None` holes where
    /// `positional` has no entry at all); written only by the pcre2/`:P5`
    /// path.
    pub(crate) positional_slots: Vec<Option<(usize, usize)>>,
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
    /// The winning :sym<> variant name, if this match was from a protoregex.
    pub(crate) sym: Option<String>,
    /// For aliased captures like `<str=.str_escape>`, maps capture name to
    /// original rule name for grammar action dispatch.
    pub(crate) capture_alias_map: CaptureAliasMap,
    /// The original rule name when this capture was stored under an alias.
    pub(crate) action_name: Option<String>,
    /// Hash captures from `%<name>=(...)` aliasing in regex.
    pub(crate) hash_captures: HashCaptureMap,
    /// The shared subject this match ran against (ADR-0016 P3). Set once by
    /// the engine entry point on the returned top-level accumulator — the
    /// engine itself never touches it. Consumers derive captured text from
    /// recorded spans through it instead of a stored `matched` string.
    pub(crate) target: Option<crate::runtime::MatchTarget>,
    /// The enclosing pattern level's captures, for backreference READS only
    /// (see [`OuterBackrefCaps`]). Set once on a nested walk's base store and
    /// never merged, propagated, or published — it is a read-through link to
    /// the parent walk, not a capture of this level.
    pub(crate) outer_backref: Option<Arc<OuterBackrefCaps>>,
}

impl RareCaps {
    /// True when nothing is left worth keeping the allocation for. Checked
    /// after a drain/take so a payload that has been emptied out again does
    /// not make every later clone copy an empty one.
    fn is_empty(&self) -> bool {
        self.positional_slots.is_empty()
            && self.regex_vars.as_ref().is_none_or(|vars| vars.is_empty())
            && self.sym.is_none()
            && self.capture_alias_map.is_empty()
            && self.action_name.is_none()
            && self.hash_captures.is_empty()
            && self.target.is_none()
            && self.outer_backref.is_none()
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
    /// The cold fields, allocated on first write (see [`RareCaps`]).
    pub(crate) rare: Option<Box<RareCaps>>,
}

static EMPTY_REGEX_VARS: std::sync::LazyLock<RegexVarMap> =
    std::sync::LazyLock::new(RegexVarMap::default);
static EMPTY_HASH_CAPTURES: std::sync::LazyLock<HashCaptureMap> =
    std::sync::LazyLock::new(HashCaptureMap::default);

impl RegexCaptures {
    /// The cold payload, if this accumulator ever wrote one.
    #[inline]
    pub(crate) fn rare(&self) -> Option<&RareCaps> {
        self.rare.as_deref()
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

    #[inline]
    pub(crate) fn capture_alias_map_mut(&mut self) -> &mut CaptureAliasMap {
        &mut self.rare_mut().capture_alias_map
    }

    /// Merge another accumulator's capture aliases into this one, without
    /// allocating a payload when there is nothing to merge.
    pub(crate) fn extend_capture_alias_map<I: IntoIterator<Item = (String, String)>>(
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
    pub(crate) fn positional_slots(&self) -> &[Option<(usize, usize)>] {
        self.rare().map_or(&[], |rare| &rare.positional_slots)
    }

    #[inline]
    pub(crate) fn positional_slots_mut(&mut self) -> &mut Vec<Option<(usize, usize)>> {
        &mut self.rare_mut().positional_slots
    }

    #[inline]
    pub(crate) fn sym(&self) -> Option<&String> {
        self.rare().and_then(|rare| rare.sym.as_ref())
    }

    /// Set (or clear) the winning `:sym<>` variant name. Clearing an
    /// accumulator that never had a payload does not allocate one.
    pub(crate) fn set_sym(&mut self, sym: Option<String>) {
        if sym.is_none() && self.rare.is_none() {
            return;
        }
        self.rare_mut().sym = sym;
        self.prune_rare();
    }

    /// Move the `:sym<>` variant name out.
    pub(crate) fn take_sym(&mut self) -> Option<String> {
        let taken = self.rare.as_deref_mut().and_then(|rare| rare.sym.take());
        self.prune_rare();
        taken
    }

    /// Set (or clear) the aliased-capture rule name. Clearing an accumulator
    /// that never had a payload does not allocate one.
    pub(crate) fn set_action_name(&mut self, action_name: Option<String>) {
        if action_name.is_none() && self.rare.is_none() {
            return;
        }
        self.rare_mut().action_name = action_name;
        self.prune_rare();
    }

    #[inline]
    pub(crate) fn target(&self) -> Option<&crate::runtime::MatchTarget> {
        self.rare().and_then(|rare| rare.target.as_ref())
    }

    /// Publish the shared subject on this accumulator (ADR-0016 P3).
    pub(crate) fn set_target(&mut self, target: Option<crate::runtime::MatchTarget>) {
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
        assert!(caps.positional_slots().is_empty());
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

        caps.set_sym(Some("foo".to_string()));
        assert_eq!(caps.sym().map(String::as_str), Some("foo"));
        caps.set_sym(None);
        assert!(caps.rare().is_none());
    }
}

#[derive(Clone)]
pub(crate) struct RegexToken {
    pub(crate) atom: RegexAtom,
    pub(crate) quant: RegexQuant,
    pub(crate) named_capture: Option<String>,
    /// Secondary named capture for capturing subrule aliases like `$<alias>=<builtin_class>`.
    /// When set, the matched text is also stored under this name (the original rule name).
    pub(crate) secondary_named_capture: Option<String>,
    /// Hash aliasing: `%<name>=(...)` captures build a hash
    pub(crate) hash_capture: Option<String>,
    /// Array-sigil capture alias (`@<name>=(...)`): forces the named capture
    /// into list context, so even a single (non-quantified) match yields a
    /// one-element List rather than a bare Match. Mirrors Raku's `@`-sigil
    /// declaration semantics for hypothetical capture variables.
    pub(crate) force_list_capture: bool,
    pub(crate) ratchet: bool,
    /// Frugal (non-greedy) quantifier modifier: `*?`, `+?`, `??`
    pub(crate) frugal: bool,
    /// Separator for `%` / `%%` quantifiers, e.g. `<thing> +% ','`. When present,
    /// the quantified atom is matched with `separator.atom` interleaved between
    /// iterations. `allow_trailing` is true for `%%` (an optional trailing
    /// separator is permitted). The separator's own captures are appended as
    /// positional/named captures after the main atom's, matching Raku semantics.
    pub(crate) separator: Option<Box<RegexSeparatorSpec>>,
    /// ADR-0022 Slice 5: true when this token's `RegexAtom::Literal` char
    /// came from interpolating a *non-constant* runtime variable's value
    /// into the pattern text (`interpolate_regex_scalars`), as opposed to a
    /// literal character written directly in the source or interpolated
    /// from a `constant`-declared value (which Rakudo inlines at compile
    /// time and so still participates in LTM ranking like a hand-written
    /// literal — ADR-0022 §2's "non-constant `$var` interpolation" row).
    /// `ltm_atom_mode`'s callers and `ltm_litlen_at` treat a token with this
    /// set as a `Terminate` stopper: it neither extends the declarative
    /// prefix nor contributes to litlen. Always `false` outside
    /// `LTM_DECLARATIVE_MODE` measurement — it does not affect ordinary
    /// matching at all.
    pub(crate) from_runtime_interpolation: bool,
}

#[derive(Clone)]
pub(crate) struct RegexSeparatorSpec {
    /// The separator sub-pattern (matched between iterations). Holding a full
    /// pattern preserves named captures, quantifiers, and other structure of
    /// complex separators such as `$<delim>=<[a..z]>*`.
    pub(crate) pattern: RegexPattern,
    pub(crate) allow_trailing: bool,
}

#[derive(Clone)]
pub(crate) enum RegexAtom {
    Literal(char),
    Named(String),
    Any,
    CharClass(CharClass),
    /// `<.ws>` — Raku's word-boundary-aware whitespace rule:
    /// requires `\s+` between word characters, `\s*` otherwise.
    WsRule,
    Newline,
    NotNewline,
    Group(RegexPattern),
    CaptureGroup(RegexPattern),
    /// A capture-isolated sub-match: matches `RegexPattern` exactly like
    /// `Group` (its own nested captures resolve normally, so a
    /// backreference WITHIN it to its OWN capture still works), but its
    /// positional/named captures are never merged into the caller's numbering
    /// — the caller sees only the matched extent, like a discarded `Match`
    /// object. Used for the `<$var>` / bare-`$var` / `<@var>`-alternation
    /// "stored Regex value" family: Raku gives such a sub-match its own
    /// discarded `Match`, so its captures must not leak into the outer `$/`
    /// (see `todo/tickets/stored-regex-loses-its-defining-scope-lexicals.md`
    /// bug 2, and the Cro::HTTP `http-request-serializer.rakutest` boundary
    /// pattern that needs the INTERNAL backreference to keep working, ruling
    /// out plain capture erasure).
    CaptureIsolatedGroup(RegexPattern),
    Alternation(Vec<RegexPattern>),
    SequentialAlternation(Vec<RegexPattern>),
    /// Conjunction: all branches must match at the same position; longest match wins
    Conjunction(Vec<RegexPattern>),
    ZeroWidth,
    CodeAssertion {
        code: String,
        negated: bool,
        is_assertion: bool,
    },
    /// `<{ code }>` — closure interpolation: evaluate code and match result as regex
    ClosureInterpolation {
        code: String,
    },
    UnicodeProp {
        name: String,
        negated: bool,
        args: Option<String>,
    },
    UnicodePropAssert {
        name: String,
        negated: bool,
    }, // zero-width assertion
    CaptureStartMarker,
    CaptureEndMarker,
    /// `:my $var = expr;` — variable declaration inside a regex
    VarDecl {
        code: String,
    },
    /// Combined character class: <+ xdigit - lower>, matches positive AND NOT negative
    CompositeClass {
        positive: Vec<ClassItem>,
        negative: Vec<ClassItem>,
    },
    /// Lookaround assertion: <?before pattern>, <!before pattern>,
    /// <?after pattern>, <!after pattern>
    Lookaround {
        pattern: RegexPattern,
        negated: bool,
        is_behind: bool,
    },
    /// `<<` or `«` — left word boundary assertion (zero-width)
    LeftWordBoundary,
    /// `>>` or `»` — right word boundary assertion (zero-width)
    RightWordBoundary,
    /// `<?wb>` (word boundary) / `<!wb>` (not a word boundary) — zero-width
    /// assertion at a transition between a word char and a non-word char (either
    /// direction), i.e. `<<` or `>>`.
    WordBoundary {
        negated: bool,
    },
    /// `<?ww>` (within word) / `<!ww>` (not within word) — zero-width assertion
    /// that the position sits *between two word characters*. Unlike
    /// [`RegexAtom::WordBoundary`] this is not the negation of a boundary: both
    /// are false in the middle of a run of non-word characters, because a
    /// position outside the string counts as non-word for either test.
    WithinWord {
        negated: bool,
    },
    /// `^^` — start of line assertion (zero-width)
    StartOfLine,
    /// `$$` — end of line assertion (zero-width)
    EndOfLine,
    /// A mid-pattern `$` — end of *string* assertion (zero-width). Raku's `$` is
    /// always end-of-string; only `$$` is end-of-line. A trailing `$` sets the
    /// pattern's `anchor_end` instead; this atom covers a `$` that is followed by
    /// something else (`^ .* $ { make … }`).
    EndOfString,
    /// `$0`, `$1`, etc. — backreference to positional capture group
    Backref(usize),
    /// `$<name>` — backreference to named capture group
    NamedBackref(String),
    /// Bare `$name` interpolating an in-regex `:my $name …` lexical: a match-time
    /// interpolation of the variable's string value as a literal. The value is
    /// read from `caps.regex_vars` (falling back to `env`) when the atom is
    /// matched, so it reflects assignments made earlier in the same match (e.g.
    /// a captured indentation string). Distinct from `NamedBackref` (which reads
    /// a capture) and from pre-substituted outer-scope `$var` interpolation.
    VarInterp(String),
    /// `<?same>` / `<!same>` — zero-width assertion: adjacent chars are same/different
    SameAssertion {
        negated: bool,
    },
    /// `<at(N)>` — zero-width assertion: match at position N
    AtPosition(usize),
    /// `<~~>` — recursive self-match: match the *enclosing* regex (or, inside a
    /// grammar token/rule, that rule's body) again at the current position. The
    /// payload is the enclosing pattern's source text, which the match-time
    /// parse cache turns straight back into the same shared tree. Like the
    /// `<$var>` sub-match family, the recursive invocation gets its own
    /// discarded `Match`, so its captures do not leak into the caller's `$/`.
    RecurseSelf(Box<str>),
    /// Internal marker used while rewriting `left ~ goal inner`.
    TildeMarker,
    /// Goal matching produced by `~`: match `inner` first, then `goal`,
    /// but preserve capture order as written (`goal` before `inner`).
    GoalMatch {
        goal: RegexPattern,
        inner: RegexPattern,
        goal_text: String,
    },
}

#[derive(Clone)]
pub(crate) enum RegexQuant {
    One,
    ZeroOrMore,
    OneOrMore,
    ZeroOrOne,
    /// `** min..max` — repeat exactly min to max times (max=None means unbounded)
    Repeat(usize, Option<usize>),
    /// `** {code}` — repeat count determined at runtime by evaluating code block
    RepeatCode(String),
}

#[derive(Clone)]
pub(crate) struct CharClass {
    pub(crate) negated: bool,
    pub(crate) items: Vec<ClassItem>,
}

#[derive(Clone)]
pub(crate) enum ClassItem {
    Range(char, char),
    Char(char),
    /// The `.` any-character base of a character-class arithmetic expression
    /// (`<.-[a]-[b]>`, `<.-:letter-:digit>`): a positive item that matches every
    /// character. Raku's class arithmetic is not true set arithmetic — the
    /// positive and negative halves are accumulated separately and a character
    /// matches when it is in the positive half AND NOT in the negative half —
    /// so the universe has to survive as an *item*, not by collapsing the whole
    /// positive half (`<.-[a]+[1]>` still excludes `a`).
    Any,
    Digit,
    NegDigit,
    Word,
    NegWord,
    Space,
    NegSpace,
    HorizSpace,
    NegHorizSpace,
    VertSpace,
    NegVertSpace,
    NotNewline,
    NamedBuiltin(String),
    UnicodePropItem {
        name: String,
        negated: bool,
    },
}
