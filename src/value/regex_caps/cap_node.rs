//! Stored capture nodes (`PosSlot`, `CapNode`, `CapChildren`) and the
//! accumulator → node conversion. Moved from `runtime/regex_types.rs` so
//! `Value`'s lazy `Match` can name them without reaching into the runtime
//! (#10779).

use super::MatchTarget;
use super::NamedCaptureMap;
use super::{CaptureAliasMap, RegexCaptures};
use crate::symbol::Symbol;
use crate::value::Value;
use rustc_hash::FxHashMap as HashMap;
use std::sync::Arc;

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
    #[cfg(test)]
    pub(crate) fn span(from: usize, to: usize) -> Self {
        PosSlot {
            from,
            to,
            ..Default::default()
        }
    }

    /// How many leading slots of `slots` a Match exposes: a trailing unmatched
    /// optional capture (`(x)?` / `[ (x) ]?` that took its zero branch) is
    /// dropped, an interior one kept. Its reserved slot only keeps a *later*
    /// capture's index stable (`(a)? (b)` on "b": `$0` Nil, `$1` ｢b｣); raku's
    /// capture list extends only as far as the last bound slot, so
    /// `"1" ~~ / (\d) (y)? /` has one element. Alternation padding is kept --
    /// the engine resolves static `$N` numbers through it -- see
    /// [`Self::visible_len`] for the user-facing cut that drops it too.
    // Cost: O(t), t = the trailing unbound slots.
    pub(crate) fn bound_len(slots: &[PosSlot]) -> usize {
        slots
            .iter()
            .rposition(|slot| !slot.nil || slot.alternation_padding)
            .map_or(0, |idx| idx + 1)
    }

    /// [`Self::bound_len`] that also drops trailing alternation padding: the
    /// slots a user-visible Match lists (`/ [ (a) | (b) (c) ] (d)? /` on "a"
    /// has one element).
    // Cost: O(t), t = the trailing unbound slots.
    pub(crate) fn visible_len(slots: &[PosSlot]) -> usize {
        slots
            .iter()
            .rposition(|slot| !slot.nil)
            .map_or(0, |idx| idx + 1)
    }

    pub(crate) fn alternation_padding() -> Self {
        PosSlot {
            nil: true,
            alternation_padding: true,
            ..Default::default()
        }
    }

    /// Add what this slot holds to `list`, the entries of a slot that folds
    /// several iterations. A slot an inner quantifier already folded
    /// (`[ [ (\d) ]+ ]+`) contributes each of its entries, not one entry for
    /// itself: a capture group under nested quantifiers is one flat list in
    /// raku, because the groups around it do not capture.
    /// An iteration whose `(x)?` did not match (a Nil slot) contributes
    /// nothing: raku's list holds only the matches (`[ (\d)? x ]+` on "x1xx"
    /// binds `$0` to the one digit).
    // Cost: O(e), e = the entries the slot already holds (one when it holds none).
    pub(crate) fn push_entries_to(&self, list: &mut Vec<QuantifiedCaptureEntry>) {
        match &self.quantified {
            Some(inner) => list.extend(inner.iter().cloned()),
            None if self.nil => {}
            None => list.push((self.from, self.to, self.subcap.clone())),
        }
    }

    /// The slot that holds `list`: its span and sub-Match are the last entry's,
    /// the representative a backreference reads.
    // Cost: O(1) beyond the list.
    pub(crate) fn folded(list: Vec<QuantifiedCaptureEntry>) -> Self {
        let (from, to, subcap) = list
            .last()
            .map(|(from, to, subcap)| (*from, *to, subcap.clone()))
            .unwrap_or((0, 0, None));
        PosSlot {
            from,
            to,
            subcap,
            quantified: Some(list),
            nil: false,
            alternation_padding: false,
        }
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
    /// When a separated quantifier is matching its next atom, the atom's
    /// captures belong in the quantifier's folded positional slots rather
    /// than after them. The range is a slot range of the view the level reads
    /// (`RegexCaptures::inline_capture_view`): every enclosing link's captures
    /// with this link's own folded in, counted after each enclosing fold, so
    /// it points into the outer iteration's slots when the quantifier is
    /// nested in another one. It also says how *this link's* `positional`
    /// folds into its `parent`'s view, since those are the enclosing level's
    /// own captures (`ViewFold`).
    pub(crate) merge_positional: Option<(usize, usize)>,
}

impl OuterBackrefCaps {
    // `append_captures`, the `$/` view of inline code, lives with the rest of
    // the view in `regex_backref_scope`.

    /// The most recent entry recorded for `name` at this level or any enclosing
    /// one (innermost wins, matching the accumulate-then-read order the flat
    /// non-grouped case has).
    pub(crate) fn lookup_named(self: &Arc<Self>, name: &Symbol) -> Option<&Arc<CapNode>> {
        let mut cur: &Arc<Self> = self;
        loop {
            if let Some(node) = cur.named.get(name).and_then(|slot| slot.nodes.last()) {
                return Some(node);
            }
            cur = cur.parent.as_ref()?;
        }
    }

    /// The positional slot at `idx` at this level or any enclosing one.
    pub(crate) fn lookup_positional(self: &Arc<Self>, idx: usize) -> Option<&PosSlot> {
        let mut cur: &Arc<Self> = self;
        loop {
            if let Some(slot) = cur.positional.get(idx) {
                return Some(slot);
            }
            cur = cur.parent.as_ref()?;
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
    pub(crate) sym: Option<Symbol>,
    /// The original rule name when this capture was stored under an alias.
    pub(crate) action_name: Option<Symbol>,
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
    pub(crate) capture_alias_map: CaptureAliasMap,
    /// Positional captures as span-bearing slots (ADR-0016 P4). Unlike the
    /// pre-P4 parallel vectors, the span survives onto the stored node — the
    /// text-only leaf fallback (fabricated `0..len` offsets) is gone.
    pub(crate) positional: Vec<PosSlot>,
    /// What this rule's own `:my $*x` declarations held at this match's reduce
    /// (see `Interpreter::record_rule_dynvars`).
    pub(crate) regex_vars: HashMap<String, Value>,
    /// The grammar instance this rule invocation owned while it ran -- Rakudo's
    /// cursor -- when a method the rule called wrote to it (#9803): its
    /// attributes are the Match's own, so `$<t>.inv` reads what `method acc {
    /// $!inv = True }` stored. `None` for the (overwhelming) invocation that
    /// never touched one.
    pub(crate) cursor: Option<Value>,
    /// The cursor position (`.pos`) when a `)>` marker narrowed `.to` short
    /// of it; `None` when `.pos` is `.to`.
    pub(crate) pos: Option<usize>,
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
    // `inline_capture_view`, the capture state visible to inline regex code,
    // lives with the rest of the view in `regex_backref_scope`.

    pub(crate) fn inline_match_from(&self) -> usize {
        self.match_from
    }

    /// The subject this capture tree was published with (set by the engine
    /// entry point), else one built fresh from `text` (ADR-0016 P3).
    pub(crate) fn target_or_new(&self, text: &str) -> MatchTarget {
        self.target()
            .cloned()
            .unwrap_or_else(|| MatchTarget::new(text))
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
    /// `capture_start`/`capture_end`, `match_from`). The
    /// child payload is allocated only when something would go in it.
    pub(crate) fn into_cap_node(mut self) -> CapNode {
        // Take the cold payload whole: a leaf (the common case) never had one,
        // so the conversion neither allocates nor touches the fields below.
        let rare = self.rare.take().map(|rare| *rare);
        self.settle_numbered_captures();
        self.positional
            .truncate(PosSlot::bound_len(&self.positional));
        let (sym, action_name) = (self.sym(), self.action_name());
        let (capture_alias_map, regex_vars, cursor, cursor_span) = match rare {
            Some(rare) => (
                rare.capture_alias_map,
                rare.regex_vars,
                rare.cursor,
                rare.cursor_span,
            ),
            None => Default::default(),
        };
        // Only a `)>`-narrowed end gives the stored node a `.pos` of its own.
        let pos = cursor_span
            .map(|(_, end)| end)
            .filter(|&end| end != self.to);
        let has_children = !self.named.is_empty()
            || !capture_alias_map.is_empty()
            || !self.positional.is_empty()
            || cursor.is_some()
            || pos.is_some()
            || regex_vars.as_ref().is_some_and(|vars| !vars.is_empty());
        let children = has_children.then(|| {
            Box::new(CapChildren {
                named: self.named,
                capture_alias_map,
                positional: self.positional,
                regex_vars: regex_vars.map(Arc::unwrap_or_clone).unwrap_or_default(),
                cursor,
                pos,
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
