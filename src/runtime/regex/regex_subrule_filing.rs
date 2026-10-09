//! Filing a `<subrule>` call's match as a capture of its caller (ADR-10488 D2).
//!
//! A subrule's match becomes one capture node, filed under the capture name
//! (and, for a non-suppressing alias, under the rule's own name too), or under
//! the hidden silent-action marker for a `<.foo>` whose action must still run.
//! The walk wants that filing as a capture *delta* (ADR-0007): a fresh
//! `RegexCaptures` it merges into its store. The compiled engine (ADR-0135)
//! returns from a callee frame straight into the caller's capture level, so it
//! files into the level's store directly; building a delta there allocated a
//! map and a node vector per call only for `merge_delta` to copy them out and
//! free them (#10488). One [`Interpreter::file_named_candidate`] decides the
//! filing; a [`CapSink`] is where it lands.

use super::super::*;
use super::regex_helpers::NamedRegexLookupSpec;
use super::regex_trail::CapStore;
use std::sync::Arc;

/// Where a subrule's filed capture lands: a fresh delta, or a live store
/// whose writes are trailed for backtracking.
pub(super) trait CapSink {
    /// Append `node` under the capture name `key`.
    fn file_node(&mut self, key: Symbol, node: Arc<CapNode>);
    /// Record that the capture `key` was filed under an alias of `rule`.
    fn file_alias(&mut self, key: Symbol, rule: Symbol);
    /// Carry a childless silent subrule's inline `make` value up to the caller.
    fn adopt_ast(&mut self, inner: &mut RegexCaptures);
}

impl CapSink for RegexCaptures {
    // Cost: O(n) amortized, n = the names already filed in the delta.
    fn file_node(&mut self, key: Symbol, node: Arc<CapNode>) {
        self.named.slot_mut(key).nodes.push(node);
    }

    // Cost: O(1) expected.
    fn file_alias(&mut self, key: Symbol, rule: Symbol) {
        self.capture_alias_map_mut().insert(key, rule);
    }

    // Cost: O(1).
    fn adopt_ast(&mut self, inner: &mut RegexCaptures) {
        super::regex_helpers::adopt_inline_ast(self, inner);
    }
}

impl CapSink for CapStore {
    // Cost: O(n) amortized, n = the names the level has filed.
    fn file_node(&mut self, key: Symbol, node: Arc<CapNode>) {
        self.push_named_node_sym(key, node);
    }

    // Cost: O(1) expected.
    fn file_alias(&mut self, key: Symbol, rule: Symbol) {
        self.insert_alias(key, rule);
    }

    // Cost: O(1).
    fn adopt_ast(&mut self, inner: &mut RegexCaptures) {
        if let Some(ast) = inner.ast.take() {
            self.set_ast(ast);
        }
    }
}

impl Interpreter {
    /// Build named regex candidates from inner match results. Inner positions are
    /// already absolute (ADR-0016 P1: the subrule body was matched against the whole
    /// subject starting at `pos`, not a re-slice), so nothing is rebased here.
    /// Wraps each inner match in the appropriate capture structure for the named regex call.
    /// `pos` is the position of the named atom in `chars`. Each candidate is a
    /// capture DELTA relative to an empty baseline (ADR-0007).
    pub(super) fn build_named_candidates_from_inner(
        &mut self,
        inner_matches: Vec<(usize, RegexCaptures)>,
        pos: usize,
        spec: &NamedRegexLookupSpec,
        sym_key: Option<Symbol>,
    ) -> Vec<(usize, RegexCaptures)> {
        inner_matches
            .into_iter()
            .map(|(end, inner_caps)| {
                self.build_named_candidate_from_inner(end, inner_caps, pos, spec, sym_key)
            })
            .collect()
    }

    /// One inner match wrapped as the named call's capture delta
    /// ([`Self::build_named_candidates_from_inner`] for a single end, without
    /// the vectors).
    pub(super) fn build_named_candidate_from_inner(
        &mut self,
        end: usize,
        inner_caps: RegexCaptures,
        pos: usize,
        spec: &NamedRegexLookupSpec,
        sym_key: Option<Symbol>,
    ) -> (usize, RegexCaptures) {
        let mut new_caps = RegexCaptures::default();
        self.file_named_candidate(&mut new_caps, end, inner_caps, pos, spec, sym_key);
        (end, new_caps)
    }

    /// File one inner match of the named call `spec` (matched from `pos` to
    /// `end`) into `sink`.
    // Cost: O(n) amortized, n = the names the sink has filed (the node itself is
    // moved, not copied).
    pub(super) fn file_named_candidate<S: CapSink>(
        &mut self,
        sink: &mut S,
        end: usize,
        inner_caps: RegexCaptures,
        pos: usize,
        spec: &NamedRegexLookupSpec,
        sym_key: Option<Symbol>,
    ) {
        // The name this subrule's match is filed under, with its interned
        // twin. Both come from the (memoized) spec, so filing a capture
        // costs no intern -- see `NamedRegexLookupSpec::capture_sym`.
        let capture = match (spec.capture_name.as_deref(), spec.capture_sym) {
            (Some(name), Some(sym)) => Some((name, sym)),
            _ if !spec.silent => Some((spec.lookup_name.as_str(), spec.lookup_sym)),
            _ => None,
        };
        if let Some((capture_name, capture_sym)) = capture {
            // Apply the subrule's own capture markers (`<(` / `)>`): a token
            // like `token foo { 12345 <( 67890 }` restricts its `<foo>`
            // submatch to `67890`. They are already absolute, and `None` when
            // the subrule used no markers, so this is a no-op otherwise.
            let cs = inner_caps.capture_start.unwrap_or(pos).clamp(pos, end);
            let ce = inner_caps.capture_end.unwrap_or(end).clamp(cs, end);
            let mut subcap = inner_caps;
            subcap.from = cs;
            subcap.to = ce;
            // sym is already set on subcap from raw_out collection loop.
            // Fall back to sym_key parameter for the is_active (seed) path.
            if subcap.sym().is_none() && sym_key.is_some() {
                subcap.set_sym(sym_key);
            }
            // The subrule's own inline `{ … }` code blocks stay ON the subcap
            // (a queryable Match node) rather than bubbling into the parent, so
            // the reduce-time walk (`reduce_regex_captures_made`) can run them
            // once at this node — with `$/` bound to this subrule's Match — and
            // commit the produced `make` value to `subcap.ast`. Bubbling them up
            // (the old behaviour) ran them at the top level with the wrong `$/`
            // and dropped the per-node `.made`.
            // A non-suppressing alias `<name=subrule>` (NOT `<name=.subrule>` /
            // `<name=&subrule>`) installs the capture under BOTH the alias name
            // AND the subrule's own name, matching Rakudo (e.g. `<x=num>` yields
            // `$<x>` and `$<num>`; repeated `<num>`/`<offset=count>` aggregate
            // into a list under the rule name). Both slots share ONE node
            // (see the `shared_under_original` push below).
            let also_under_original = spec.capture_name.is_some()
                && !spec.alias_replaces_original
                && capture_name != spec.lookup_name;
            // For an aliased capture (`<x=rule>`), record the original rule
            // name for grammar action dispatch BEFORE the node is wrapped in
            // an Arc and shared (`record_reduced_subrule` clones the handle):
            // writing it afterwards through `Arc::make_mut` deep-copied the
            // whole descendant subtree for every aliased subrule capture.
            let is_alias = spec.capture_name.is_some() && capture_name != spec.lookup_name;
            if is_alias {
                subcap.set_action_name(Some(spec.lookup_sym));
            }
            let subcap = Arc::new(subcap.into_cap_node());
            // This subrule has just REDUCED. Log it so a parse that fails
            // overall can still run its action, the way Rakudo (which
            // dispatches at reduce time) does — see `REDUCED_SUBRULES`.
            super::regex_helpers::record_reduced_subrule(spec.lookup_sym, &subcap);
            // Both slots reference the SAME node, the way Rakudo stores the
            // same cursor under both names (`$<x> === $<num>` is `True`).
            // Cloning the node here instead — as this did until the
            // exponential-action fix — deep-copied the whole matched
            // subtree per aliased capture AND made the grammar action walk
            // dispatch that subtree twice, once per slot; nested aliases
            // then multiplied, firing a leaf's action 2^depth times (256x
            // on `benchmarks/bench-yaml-parse.raku`).
            let shared_under_original = also_under_original.then(|| Arc::clone(&subcap));
            sink.file_node(capture_sym, subcap);
            if is_alias {
                sink.file_alias(capture_sym, spec.lookup_sym);
            }
            if let Some(orig_subcap) = shared_under_original {
                sink.file_node(spec.lookup_sym, orig_subcap);
            }
        } else if !inner_caps.named.is_empty()
            || self
                .silent_subrule_has_action(spec, inner_caps.sym().or(sym_key).map(|s| s.as_str()))
        {
            // Silent subrule (`<.foo>`) that contains nested captures, or
            // whose OWN action method exists. The subrule is hidden from
            // `.hash`, but its action method must still fire (Rakudo
            // dispatches actions at reduce time regardless of capture), and
            // its nested rules' actions must fire too — with their `.made`
            // set on the SAME nodes the parent action reads
            // (`method header-field { ...$/<field-name>.made... }`). Store the
            // whole subrule match under a HIDDEN MARKER key in `named_subcaps`
            // (the prefix can never be a real capture name). The Match builder
            // routes marker entries into a `silent_caps` attribute instead of
            // `.hash`; the grammar action walk recurses into them. This replaces
            // the older "flatten direct children into the parent" hack, which
            // lost the rule's own action and over-exposed children in `.hash`.
            // A childless one needs the node only for its action: a zero-width
            // `<.end-block>` whose action reports a recovery warning.
            let cs = inner_caps.capture_start.unwrap_or(pos).clamp(pos, end);
            let ce = inner_caps.capture_end.unwrap_or(end).clamp(cs, end);
            let mut subcap = inner_caps;
            subcap.from = cs;
            subcap.to = ce;
            if subcap.sym().is_none() && sym_key.is_some() {
                subcap.set_sym(sym_key);
            }
            subcap.set_action_name(Some(spec.lookup_sym));
            // Keep the silent subrule's inline blocks on its own (marker) node
            // for the reduce-time walk to run once — see the non-silent branch.
            let subcap = Arc::new(subcap.into_cap_node());
            super::regex_helpers::record_reduced_subrule(spec.lookup_sym, &subcap);
            sink.file_node(spec.silent_marker_sym, subcap);
        } else {
            // Childless silent subrule with no action to run (`<.ws>`,
            // `<.CRLF>`, ...): keep the cheap path — just carry its code
            // blocks up. A marker node here would be built, logged for the
            // reduce replay and copied through every backtracking path for
            // nothing; doing it for every `<.ws>` made a 60-row YAMLish parse
            // cost 2.7x the instructions
            // ([#9286](https://github.com/tokuhirom/mutsu/issues/9286)).
            let mut inner_caps = inner_caps;
            sink.adopt_ast(&mut inner_caps);
        }
    }
}
