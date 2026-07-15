//! Trail-based capture store for the backtracking regex engine (ADR-0007).
//!
//! One mutable `RegexCaptures` per pattern-level match, owned by the engine.
//! Every mutation goes through `CapStore` methods, which push undo records
//! onto the trail; `mark()` returns the current trail length and `rewind(mark)`
//! pops records back to it, restoring the store to its state at the mark.
//!
//! Atom candidate producers return *deltas* — `RegexCaptures` values built
//! relative to an empty baseline (typically an inner engine run's output plus
//! small additions). The engine applies a candidate with `merge_delta`,
//! descends, and rewinds on backtrack, so per-step capture cost is O(delta)
//! instead of O(accumulated state).
//!
//! Thread-local side channels (`LR_MEMO`/`LR_ACTIVE`, `EAGER_CODE_BLOCKS`,
//! `PENDING_REGEX_ERROR`, goal failures) are deliberately NOT trailed: errors
//! and goal failures persist across backtracks by design.

use super::super::*;

/// Which string-keyed multi-map a `MapVec` undo record refers to.
#[derive(Clone, Copy)]
pub(super) enum MapId {
    Named,
    NamedSub,
    HashCap,
}

/// Saved tails for a positional-block truncation (values moved out, moved
/// back on rewind). Boxed to keep `Undo` small.
pub(super) struct PosTailRec {
    p_at: usize,
    p: Vec<String>,
    sc_at: usize,
    sc: Vec<Option<Arc<RegexCaptures>>>,
    q_at: usize,
    q: Vec<Option<Vec<QuantifiedCaptureEntry>>>,
    off_at: usize,
    off: Vec<(usize, usize)>,
}

pub(super) enum Undo {
    /// Truncate the positional block back to these lengths (undoes appends).
    PosLens {
        p: usize,
        sc: usize,
        q: usize,
        off: usize,
        nil: usize,
    },
    /// Restore previously truncated positional tails (truncate to `*_at`,
    /// then re-extend with the saved values).
    PosTail(Box<PosTailRec>),
    /// Restore a single overwritten positional entry (`$N=` alias writes).
    PosOverwrite {
        idx: usize,
        text: Option<String>,
        sc: Option<Option<Arc<RegexCaptures>>>,
        q: Option<Option<Vec<QuantifiedCaptureEntry>>>,
        off: Option<(usize, usize)>,
    },
    /// Truncate `map[key]` to `len`; remove the key if it was newly created.
    MapVecTrunc {
        which: MapId,
        key: String,
        len: usize,
        present: bool,
    },
    /// Remove a `named_quantified` entry that this frame inserted.
    NamedQuantRemove(String),
    /// Restore a `capture_alias_map` entry (None = remove).
    AliasRestore {
        key: String,
        prev: Option<String>,
    },
    /// Restore a `regex_vars` entry (None = remove).
    RegexVarRestore {
        key: String,
        prev: Option<Value>,
    },
    CodeBlocksLen(usize),
    CaptureStart(Option<usize>),
    CaptureEnd(Option<usize>),
    Sym(Option<String>),
}

/// The engine's single mutable capture store + undo trail.
pub(super) struct CapStore {
    caps: RegexCaptures,
    trail: Vec<Undo>,
}

impl CapStore {
    pub(super) fn new(init: RegexCaptures) -> Self {
        CapStore {
            caps: init,
            trail: Vec::new(),
        }
    }

    /// Read access to the accumulated captures (backrefs, code assertions,
    /// argument evaluation, pos_base reads).
    #[inline]
    pub(super) fn caps(&self) -> &RegexCaptures {
        &self.caps
    }

    /// Clone the accumulated captures — used once per complete match to
    /// materialize an engine result. Nested sub-captures are `Arc`-shared, so
    /// this copies only this pattern level's own state.
    pub(super) fn snapshot(&self) -> RegexCaptures {
        self.caps.clone()
    }

    #[inline]
    pub(super) fn mark(&self) -> usize {
        self.trail.len()
    }

    /// Rewind the store to its state at `mark` by undoing records in reverse.
    pub(super) fn rewind(&mut self, mark: usize) {
        while self.trail.len() > mark {
            let rec = self.trail.pop().expect("trail entry");
            let caps = &mut self.caps;
            match rec {
                Undo::PosLens { p, sc, q, off, nil } => {
                    caps.positional.truncate(p);
                    caps.positional_subcaps.truncate(sc);
                    caps.positional_quantified.truncate(q);
                    caps.positional_offsets.truncate(off);
                    caps.positional_nil.truncate(nil);
                }
                Undo::PosTail(rec) => {
                    let r = *rec;
                    caps.positional.truncate(r.p_at);
                    caps.positional.extend(r.p);
                    caps.positional_subcaps.truncate(r.sc_at);
                    caps.positional_subcaps.extend(r.sc);
                    caps.positional_quantified.truncate(r.q_at);
                    caps.positional_quantified.extend(r.q);
                    caps.positional_offsets.truncate(r.off_at);
                    caps.positional_offsets.extend(r.off);
                }
                Undo::PosOverwrite {
                    idx,
                    text,
                    sc,
                    q,
                    off,
                } => {
                    if let Some(text) = text
                        && idx < caps.positional.len()
                    {
                        caps.positional[idx] = text;
                    }
                    if let Some(sc) = sc
                        && idx < caps.positional_subcaps.len()
                    {
                        caps.positional_subcaps[idx] = sc;
                    }
                    if let Some(q) = q
                        && idx < caps.positional_quantified.len()
                    {
                        caps.positional_quantified[idx] = q;
                    }
                    if let Some(off) = off
                        && idx < caps.positional_offsets.len()
                    {
                        caps.positional_offsets[idx] = off;
                    }
                }
                Undo::MapVecTrunc {
                    which,
                    key,
                    len,
                    present,
                } => match which {
                    MapId::Named => {
                        if !present {
                            caps.named.remove(&key);
                        } else if let Some(v) = caps.named.get_mut(&key) {
                            v.truncate(len);
                        }
                    }
                    MapId::NamedSub => {
                        if !present {
                            caps.named_subcaps.remove(&key);
                        } else if let Some(v) = caps.named_subcaps.get_mut(&key) {
                            v.truncate(len);
                        }
                    }
                    MapId::HashCap => {
                        if !present {
                            caps.hash_captures.remove(&key);
                        } else if let Some(v) = caps.hash_captures.get_mut(&key) {
                            v.truncate(len);
                        }
                    }
                },
                Undo::NamedQuantRemove(key) => {
                    caps.named_quantified.remove(&key);
                }
                Undo::AliasRestore { key, prev } => match prev {
                    Some(v) => {
                        caps.capture_alias_map.insert(key, v);
                    }
                    None => {
                        caps.capture_alias_map.remove(&key);
                    }
                },
                Undo::RegexVarRestore { key, prev } => match prev {
                    Some(v) => {
                        caps.regex_vars.insert(key, v);
                    }
                    None => {
                        caps.regex_vars.remove(&key);
                    }
                },
                Undo::CodeBlocksLen(len) => caps.code_blocks.truncate(len),
                Undo::CaptureStart(prev) => caps.capture_start = prev,
                Undo::CaptureEnd(prev) => caps.capture_end = prev,
                Undo::Sym(prev) => caps.sym = prev,
            }
        }
    }

    /// Record the current positional-block lengths so appends since this point
    /// can be undone by truncation.
    fn record_pos_lens(&mut self) {
        self.trail.push(Undo::PosLens {
            p: self.caps.positional.len(),
            sc: self.caps.positional_subcaps.len(),
            q: self.caps.positional_quantified.len(),
            off: self.caps.positional_offsets.len(),
            nil: self.caps.positional_nil.len(),
        });
    }

    fn record_named_key(&mut self, key: &str) {
        let (len, present) = match self.caps.named.get(key) {
            Some(v) => (v.len(), true),
            None => (0, false),
        };
        self.trail.push(Undo::MapVecTrunc {
            which: MapId::Named,
            key: key.to_string(),
            len,
            present,
        });
    }

    fn record_named_sub_key(&mut self, key: &str) {
        let (len, present) = match self.caps.named_subcaps.get(key) {
            Some(v) => (v.len(), true),
            None => (0, false),
        };
        self.trail.push(Undo::MapVecTrunc {
            which: MapId::NamedSub,
            key: key.to_string(),
            len,
            present,
        });
    }

    fn record_hash_cap_key(&mut self, key: &str) {
        let (len, present) = match self.caps.hash_captures.get(key) {
            Some(v) => (v.len(), true),
            None => (0, false),
        };
        self.trail.push(Undo::MapVecTrunc {
            which: MapId::HashCap,
            key: key.to_string(),
            len,
            present,
        });
    }

    /// Apply a candidate delta (a `RegexCaptures` built relative to an empty
    /// baseline) to the store, recording undo. Merges exactly the fields the
    /// old by-value merge paths handled: named/named_subcaps/named_quantified,
    /// capture_alias_map, the positional block (incl. offsets), code_blocks,
    /// hash_captures, regex_vars, capture markers, and sym.
    /// `positional_nil`/`positional_slots` and the per-level metadata
    /// (matched/from/to/match_from) are intentionally NOT merged.
    pub(super) fn merge_delta(&mut self, mut delta: RegexCaptures) {
        for (k, v) in delta.named.drain() {
            self.record_named_key(&k);
            self.caps.named.entry(k).or_default().extend(v);
        }
        for (k, v) in delta.named_subcaps.drain() {
            self.record_named_sub_key(&k);
            self.caps.named_subcaps.entry(k).or_default().extend(v);
        }
        for k in delta.named_quantified.drain() {
            self.insert_named_quantified(k);
        }
        for (k, v) in delta.capture_alias_map.drain() {
            self.insert_alias(k, v);
        }
        if !delta.positional.is_empty()
            || !delta.positional_subcaps.is_empty()
            || !delta.positional_quantified.is_empty()
            || !delta.positional_offsets.is_empty()
        {
            self.record_pos_lens();
            self.caps.positional.append(&mut delta.positional);
            self.caps
                .positional_subcaps
                .append(&mut delta.positional_subcaps);
            self.caps
                .positional_quantified
                .append(&mut delta.positional_quantified);
            self.caps
                .positional_offsets
                .append(&mut delta.positional_offsets);
        }
        if !delta.code_blocks.is_empty() {
            self.trail
                .push(Undo::CodeBlocksLen(self.caps.code_blocks.len()));
            self.caps.code_blocks.append(&mut delta.code_blocks);
        }
        for (k, v) in delta.hash_captures.drain() {
            self.record_hash_cap_key(&k);
            self.caps.hash_captures.entry(k).or_default().extend(v);
        }
        for (k, v) in delta.regex_vars.drain() {
            let prev = self.caps.regex_vars.insert(k.clone(), v);
            self.trail.push(Undo::RegexVarRestore { key: k, prev });
        }
        if delta.capture_start.is_some() {
            self.trail.push(Undo::CaptureStart(self.caps.capture_start));
            self.caps.capture_start = delta.capture_start;
        }
        if delta.capture_end.is_some() {
            self.trail.push(Undo::CaptureEnd(self.caps.capture_end));
            self.caps.capture_end = delta.capture_end;
        }
        if delta.sym.is_some() {
            self.trail.push(Undo::Sym(self.caps.sym.take()));
            self.caps.sym = delta.sym;
        }
    }

    pub(super) fn push_named(&mut self, key: &str, val: String) {
        self.record_named_key(key);
        self.caps
            .named
            .entry(key.to_string())
            .or_default()
            .push(val);
    }

    pub(super) fn push_named_subcap(&mut self, key: &str, sub: Arc<RegexCaptures>) {
        self.record_named_sub_key(key);
        self.caps
            .named_subcaps
            .entry(key.to_string())
            .or_default()
            .push(sub);
    }

    pub(super) fn insert_named_quantified(&mut self, name: String) {
        if self.caps.named_quantified.insert(name.clone()) {
            self.trail.push(Undo::NamedQuantRemove(name));
        }
    }

    pub(super) fn insert_alias(&mut self, key: String, val: String) {
        let prev = self.caps.capture_alias_map.insert(key.clone(), val);
        self.trail.push(Undo::AliasRestore { key, prev });
    }

    pub(super) fn push_hash_capture(&mut self, key: &str, entry: (String, Option<String>)) {
        self.record_hash_cap_key(key);
        self.caps
            .hash_captures
            .entry(key.to_string())
            .or_default()
            .push(entry);
    }

    /// Append one positional entry (text + subcap + quantified + offsets).
    pub(super) fn push_positional(
        &mut self,
        text: String,
        subcap: Option<Arc<RegexCaptures>>,
        quantified: Option<Vec<QuantifiedCaptureEntry>>,
        offsets: (usize, usize),
    ) {
        self.record_pos_lens();
        self.caps.positional.push(text);
        self.caps.positional_subcaps.push(subcap);
        self.caps.positional_quantified.push(quantified);
        self.caps.positional_offsets.push(offsets);
    }

    /// Truncate the positional text/subcap/quantified vecs to `to`, saving the
    /// removed tails so rewind can restore them (the `$<name>=(...)` alias
    /// surgery drops the group's auto-positional entry). `positional_offsets`
    /// is left untouched, mirroring the old named-alias path.
    pub(super) fn truncate_positional_3(&mut self, to: usize) {
        let p = split_off_clamped(&mut self.caps.positional, to);
        let sc = split_off_clamped(&mut self.caps.positional_subcaps, to);
        let q = split_off_clamped(&mut self.caps.positional_quantified, to);
        self.trail.push(Undo::PosTail(Box::new(PosTailRec {
            p_at: self.caps.positional.len(),
            p,
            sc_at: self.caps.positional_subcaps.len(),
            sc,
            q_at: self.caps.positional_quantified.len(),
            q,
            off_at: self.caps.positional_offsets.len(),
            off: Vec::new(),
        })));
    }

    /// Truncate all four positional vecs to `to` (the `$N=` alias path).
    pub(super) fn truncate_positional_4(&mut self, to: usize) {
        let p = split_off_clamped(&mut self.caps.positional, to);
        let sc = split_off_clamped(&mut self.caps.positional_subcaps, to);
        let q = split_off_clamped(&mut self.caps.positional_quantified, to);
        let off = split_off_clamped(&mut self.caps.positional_offsets, to);
        self.trail.push(Undo::PosTail(Box::new(PosTailRec {
            p_at: self.caps.positional.len(),
            p,
            sc_at: self.caps.positional_subcaps.len(),
            sc,
            q_at: self.caps.positional_quantified.len(),
            q,
            off_at: self.caps.positional_offsets.len(),
            off,
        })));
    }

    /// Overwrite the positional entry at `idx` (`$N=` re-assigning an existing
    /// slot), saving the previous values.
    pub(super) fn overwrite_positional(&mut self, idx: usize, text: String, off: (usize, usize)) {
        let caps = &mut self.caps;
        let prev_text = if idx < caps.positional.len() {
            Some(std::mem::replace(&mut caps.positional[idx], text))
        } else {
            None
        };
        let prev_off = if idx < caps.positional_offsets.len() {
            Some(std::mem::replace(&mut caps.positional_offsets[idx], off))
        } else {
            None
        };
        let prev_sc = if idx < caps.positional_subcaps.len() {
            Some(caps.positional_subcaps[idx].take())
        } else {
            None
        };
        let prev_q = if idx < caps.positional_quantified.len() {
            Some(caps.positional_quantified[idx].take())
        } else {
            None
        };
        self.trail.push(Undo::PosOverwrite {
            idx,
            text: prev_text,
            sc: prev_sc,
            q: prev_q,
            off: prev_off,
        });
    }

    /// Trailed `reserve_nil_capture_slots` (unmatched `(x)?` Nil reservation).
    pub(super) fn reserve_nil(&mut self, stride: usize) {
        if stride == 0 {
            return;
        }
        self.record_pos_lens();
        super::regex_helpers::reserve_nil_capture_slots(&mut self.caps, stride);
    }

    /// Trailed `fold_quantified_captures`: save the unfolded tail, then fold.
    pub(super) fn fold_quantified(&mut self, base_len: usize, stride: usize) {
        if stride == 0 {
            return;
        }
        // Save the whole tail from base_len (clamped per vec) so rewind can
        // restore the unfolded state exactly, including the padding fold adds.
        let p = split_off_clamped(&mut self.caps.positional, base_len);
        let sc = split_off_clamped(&mut self.caps.positional_subcaps, base_len);
        let q = split_off_clamped(&mut self.caps.positional_quantified, base_len);
        let off = split_off_clamped(&mut self.caps.positional_offsets, base_len);
        let rec = PosTailRec {
            p_at: self.caps.positional.len(),
            p,
            sc_at: self.caps.positional_subcaps.len(),
            sc,
            q_at: self.caps.positional_quantified.len(),
            q,
            off_at: self.caps.positional_offsets.len(),
            off,
        };
        // Re-extend with clones for fold to consume; the saved originals are
        // the undo. Rewind truncates each vec to its `*_at` cut point (removing
        // whatever fold produced above it) and re-extends the saved tail,
        // restoring the exact unfolded state.
        self.caps.positional.extend(rec.p.iter().cloned());
        self.caps.positional_subcaps.extend(rec.sc.iter().cloned());
        self.caps
            .positional_quantified
            .extend(rec.q.iter().cloned());
        self.caps.positional_offsets.extend(rec.off.iter().cloned());
        self.trail.push(Undo::PosTail(Box::new(rec)));
        super::regex_helpers::fold_quantified_captures(&mut self.caps, base_len, stride);
    }
}

/// `Vec::split_off` clamped: splitting at an index past the end returns empty.
fn split_off_clamped<T>(v: &mut Vec<T>, at: usize) -> Vec<T> {
    if at >= v.len() {
        Vec::new()
    } else {
        v.split_off(at)
    }
}

#[cfg(test)]
mod tests {
    use super::super::super::RegexCaptures;
    use super::CapStore;

    fn store_with_base() -> CapStore {
        let mut init = RegexCaptures::default();
        init.positional.push("a".to_string());
        init.positional_subcaps.push(None);
        init.positional_quantified.push(None);
        init.positional_offsets.push((0, 1));
        init.named
            .entry("x".to_string())
            .or_default()
            .push("v".to_string());
        CapStore::new(init)
    }

    fn assert_base(store: &CapStore) {
        assert_eq!(store.caps().positional, vec!["a".to_string()]);
        assert_eq!(store.caps().positional_offsets, vec![(0, 1)]);
        assert_eq!(store.caps().named.len(), 1);
        assert_eq!(store.caps().named["x"], vec!["v".to_string()]);
        assert!(store.caps().named_subcaps.is_empty());
        assert!(store.caps().capture_start.is_none());
        assert!(store.caps().sym.is_none());
    }

    #[test]
    fn merge_delta_rewind_roundtrip() {
        let mut store = store_with_base();
        let m = store.mark();
        let mut delta = RegexCaptures::default();
        delta.positional.push("b".to_string());
        delta.positional_subcaps.push(None);
        delta.positional_quantified.push(None);
        delta.positional_offsets.push((1, 2));
        delta
            .named
            .entry("x".to_string())
            .or_default()
            .push("v2".to_string());
        delta
            .named
            .entry("y".to_string())
            .or_default()
            .push("w".to_string());
        delta.named_quantified.insert("y".to_string());
        delta.capture_start = Some(3);
        delta.sym = Some("s".to_string());
        store.merge_delta(delta);
        assert_eq!(store.caps().positional.len(), 2);
        assert_eq!(store.caps().named["x"].len(), 2);
        assert_eq!(store.caps().named["y"], vec!["w".to_string()]);
        assert!(store.caps().named_quantified.contains("y"));
        assert_eq!(store.caps().capture_start, Some(3));
        assert_eq!(store.caps().sym.as_deref(), Some("s"));
        store.rewind(m);
        assert_base(&store);
        assert!(!store.caps().named_quantified.contains("y"));
    }

    #[test]
    fn truncate_and_overwrite_rewind() {
        let mut store = store_with_base();
        let m = store.mark();
        store.push_positional("b".to_string(), None, None, (1, 2));
        store.truncate_positional_4(0);
        assert!(store.caps().positional.is_empty());
        store.push_positional("c".to_string(), None, None, (5, 6));
        store.overwrite_positional(0, "z".to_string(), (9, 9));
        assert_eq!(store.caps().positional, vec!["z".to_string()]);
        store.rewind(m);
        assert_base(&store);
    }

    #[test]
    fn fold_rewind_restores_unfolded() {
        let mut store = store_with_base();
        let m = store.mark();
        // Two iterations of a 1-stride capture: entries at idx 1 and 2.
        store.push_positional("b".to_string(), None, None, (1, 2));
        store.push_positional("c".to_string(), None, None, (2, 3));
        let mf = store.mark();
        store.fold_quantified(1, 1);
        assert_eq!(store.caps().positional.len(), 2); // folded into one slot
        assert!(store.caps().positional_quantified[1].is_some());
        store.rewind(mf);
        assert_eq!(store.caps().positional.len(), 3);
        assert_eq!(store.caps().positional[2], "c");
        assert!(store.caps().positional_quantified[2].is_none());
        store.rewind(m);
        assert_base(&store);
    }

    #[test]
    fn nested_marks_rewind_in_order() {
        let mut store = store_with_base();
        let m1 = store.mark();
        store.push_named("k", "1".to_string());
        let m2 = store.mark();
        store.push_named("k", "2".to_string());
        store.push_named_subcap("k", std::sync::Arc::new(RegexCaptures::default()));
        store.rewind(m2);
        assert_eq!(store.caps().named["k"], vec!["1".to_string()]);
        assert!(store.caps().named_subcaps.is_empty());
        store.rewind(m1);
        assert!(!store.caps().named.contains_key("k"));
    }
}
