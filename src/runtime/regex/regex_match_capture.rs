use super::super::*;
use super::regex_helpers::{LTM_DECLARATIVE_MODE, alternation_list_flags, merge_goal_captures};
use super::regex_ltm_fate::ltm_record_fate;
use super::regex_ltm_rank::{LtmAtomMode, ltm_atom_mode};
use super::regex_match_delta::alternation_branch_delta;

impl Interpreter {
    /// Is this atom an inline sub-pattern — part of the *same* regex, just matched
    /// with its own capture store? Those inherit the enclosing regex's `:my`/`:let`
    /// lexicals; a subrule reference (a different regex) must not.
    fn atom_is_inline_subpattern(atom: &RegexAtom) -> bool {
        matches!(
            atom,
            RegexAtom::Group(_)
                | RegexAtom::CaptureGroup(_)
                | RegexAtom::Alternation(_)
                | RegexAtom::SequentialAlternation(_)
                | RegexAtom::Conjunction(_)
                | RegexAtom::Lookaround { .. }
                | RegexAtom::GoalMatch { .. }
        )
    }

    /// Publish (or deliberately withhold) the in-regex lexicals **and** the
    /// enclosing level's captures for the sub-pattern matches this atom is
    /// about to run. See [`super::regex_helpers::INLINE_REGEX_VARS_SEED`] and
    /// [`super::regex_helpers::INLINE_OUTER_CAPS_SEED`].
    ///
    /// Every atom match calls this, and on a grammar that publishes neither
    /// mechanism every call is a no-op: on a 60-row YAMLish parse it ran
    /// 402,202 times and touched a thread-local 507 of them, for 1.63% of the
    /// program between the call, the two seed constructions and their drop glue
    /// ([#7576](https://github.com/tokuhirom/mutsu/issues/7576)). Both "is
    /// anything published" questions are answered without looking at the atom
    /// at all, so they gate an inlined early-out and the real work moves to an
    /// out-of-line cold path.
    #[inline]
    pub(super) fn arm_inline_vars_seed(
        atom: &RegexAtom,
        current_caps: &RegexCaptures,
    ) -> (
        super::regex_helpers::InlineVarsSeed,
        super::regex_helpers::OuterCapsSeed,
    ) {
        use super::regex_helpers::{
            InlineVarsSeed, OuterCapsSeed, any_regex_capture_reader_lowered, inline_capture_scope,
            inline_regex_vars_active,
        };
        // Nothing published and nothing to publish: whichever branch the cold
        // path would take, it arms `InlineVarsSeed::arm(None)` with the slot
        // already empty (inert) and, with no backreference lowered anywhere in
        // the process, `OuterCapsSeed::inert()`.
        if !any_regex_capture_reader_lowered()
            && !inline_regex_vars_active()
            && current_caps.regex_vars_shared().is_none()
            && inline_capture_scope().is_none()
        {
            return (InlineVarsSeed::inert(), OuterCapsSeed::inert());
        }
        Self::arm_inline_vars_seed_cold(atom, current_caps)
    }

    #[inline(never)]
    fn arm_inline_vars_seed_cold(
        atom: &RegexAtom,
        current_caps: &RegexCaptures,
    ) -> (
        super::regex_helpers::InlineVarsSeed,
        super::regex_helpers::OuterCapsSeed,
    ) {
        let inline = Self::atom_is_inline_subpattern(atom);
        let vars = if inline {
            super::regex_helpers::InlineVarsSeed::arm(current_caps.regex_vars_shared())
        } else {
            super::regex_helpers::InlineVarsSeed::arm(None)
        };
        let outer = Self::arm_outer_caps_seed(atom, current_caps).consume_capture_scope();
        (vars, outer)
    }

    /// Is this atom's sub-pattern matched *in the same capture scope* as the
    /// pattern containing it, as far as a backreference is concerned?
    ///
    /// Verified against real `raku`: a non-capturing group, either flavour of
    /// alternation, a conjunction and a `~` goal all see the enclosing level's
    /// captures (`/ $<x>=(\w) [ $<x> ] /` matches "aa"), while a **capturing**
    /// group and a lookaround do NOT — rakudo gives each of those its own
    /// cursor, so `/ $<x>=(\w) ( $<x> ) /` and
    /// `/ $<x>=(\w) <?before $<x>> . /` both fail there. Those two therefore
    /// arm a barrier rather than a read-through, and the barrier also hides the
    /// outer level from anything nested deeper inside them
    /// (`/ $<x>=(\w) ( [ $<x> ] ) /` fails in raku too).
    pub(super) fn atom_shares_backref_scope(atom: &RegexAtom) -> bool {
        matches!(
            atom,
            RegexAtom::Group(_)
                | RegexAtom::Alternation(_)
                | RegexAtom::SequentialAlternation(_)
                | RegexAtom::Conjunction(_)
                | RegexAtom::GoalMatch { .. }
        )
    }

    /// The barrier a subrule call arms before its body's walk. A subrule is a
    /// different regex, so it inherits neither the enclosing regex's `:my`
    /// lexicals nor its captures — including the ones an enclosing same-scope
    /// sub-pattern published for ITS walks, because the continuation (this call)
    /// runs inside that sub-pattern's dynamic extent. The streamed subrule driver
    /// walks the body itself, so it arms this by hand.
    #[inline]
    pub(super) fn arm_subrule_barrier() -> (
        super::regex_helpers::InlineVarsSeed,
        super::regex_helpers::OuterCapsSeed,
    ) {
        use super::regex_helpers::{
            InlineVarsSeed, OuterCapsSeed, any_regex_capture_reader_lowered,
            outer_caps_seed_published,
        };
        let outer = if any_regex_capture_reader_lowered() && outer_caps_seed_published() {
            OuterCapsSeed::arm(None)
        } else {
            OuterCapsSeed::inert()
        };
        (InlineVarsSeed::arm(None), outer.consume_capture_scope())
    }

    /// Does matching this atom start a regex of its own (a different capture
    /// scope from the pattern containing it)?
    fn atom_starts_own_regex(atom: &RegexAtom) -> bool {
        matches!(
            atom,
            RegexAtom::Named(_)
                | RegexAtom::CaptureGroup(_)
                | RegexAtom::CaptureIsolatedGroup(_)
                | RegexAtom::CaptureIsolatedGroupScoped(..)
                | RegexAtom::Lookaround { .. }
                | RegexAtom::CodeAssertion { .. }
                | RegexAtom::VarDecl { .. }
                | RegexAtom::ClosureInterpolation { .. }
                | RegexAtom::CodeInterp { .. }
                | RegexAtom::QqInterp { .. }
                | RegexAtom::RecurseSelf(_)
        )
    }

    /// Backreference read-through for the nested walks this atom will run.
    ///
    /// A same-capture-scope sub-pattern that actually contains a backreference
    /// publishes a snapshot of the captures taken so far, linked to whatever
    /// the enclosing level published; one that contains none leaves the
    /// enclosing seed alone (so a deeper sub-pattern still chains correctly)
    /// and pays nothing. Every other atom — a subrule reference above all —
    /// arms a `None` *barrier*, so a different regex's (or a capturing group's)
    /// `$0` / `$<name>` never resolves against this level's captures.
    fn arm_outer_caps_seed(
        atom: &RegexAtom,
        current_caps: &RegexCaptures,
    ) -> super::regex_helpers::OuterCapsSeed {
        use super::regex_helpers::{
            OuterCapsSeed, any_regex_capture_reader_lowered, atom_contains_backref,
            atom_contains_code, inline_capture_scope, outer_caps_seed_published,
        };
        let capture_scope = inline_capture_scope();
        let reader = any_regex_capture_reader_lowered();
        // An atom that starts a regex of its own — a subrule call above all,
        // but also a capture group, a lookaround, or code that runs a match —
        // never reads the enclosing level's captures. An enclosing same-scope
        // sub-pattern publishes them for ITS nested walks, and the rest of the
        // pattern (this atom included) runs inside that sub-pattern's dynamic
        // extent, so the barrier has to go up whenever one is published: a
        // subrule's `$/` starts at the subrule.
        if reader && Self::atom_starts_own_regex(atom) && outer_caps_seed_published() {
            return OuterCapsSeed::arm(None);
        }
        let needs_backref_scope = reader && atom_contains_backref(atom);
        // A code block inside a same-scope sub-pattern sees the enclosing
        // level's captures and match start, like a backreference does.
        let needs_code_scope =
            reader && Self::atom_shares_backref_scope(atom) && atom_contains_code(atom);
        let needs_capture_scope = capture_scope.is_some() && Self::atom_shares_backref_scope(atom);
        if !needs_backref_scope && !needs_capture_scope && !needs_code_scope {
            return OuterCapsSeed::inert();
        }
        if !Self::atom_shares_backref_scope(atom) {
            return OuterCapsSeed::arm(None);
        }
        if !atom_contains_backref(atom) && !needs_code_scope && capture_scope.is_none() {
            return OuterCapsSeed::inert();
        }
        OuterCapsSeed::arm(Some(std::sync::Arc::new(OuterBackrefCaps {
            named: current_caps.named.clone(),
            positional: current_caps.positional.clone(),
            parent: current_caps.outer_backref().cloned(),
            merge_positional: capture_scope,
            match_from: current_caps.match_from,
        })))
    }

    /// Single-candidate atom matcher: the atom's highest-priority match only.
    /// The returned captures are a DELTA relative to an EMPTY baseline
    /// (ADR-0007); `current_caps` is the engine's accumulated store, passed
    /// for READS only (backrefs, code assertions, code-block contexts).
    /// See the twin wrapper in `regex_match_atom.rs`: a subrule's
    /// dynamically-scoped (`$*`) parameters are established for the duration of
    /// this call and torn down here.
    pub(super) fn regex_match_atom_with_capture_in_pkg(
        &mut self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
        current_caps: &RegexCaptures,
        pkg: Symbol,
        ignore_case: bool,
    ) -> Option<(usize, RegexCaptures)> {
        let mut dyn_saved = None;
        let mut preinstalled_arg_values = None;
        // The single-candidate matcher is used by quantified named subrules
        // (`<part>+`) and has its own direct named-rule path. Give it the same
        // rule frame as the plural matcher so a declaration is scoped during
        // that path too.
        let grammar_frame = match atom {
            RegexAtom::Named(name)
                if !LTM_DECLARATIVE_MODE.with(std::cell::Cell::get)
                    && self
                        .regex_state
                        .grammar_rule_dynvar_decls
                        .contains_key(&name.spec().lookup_name) =>
            {
                let spec = name.spec();
                let arg_values = if spec.arg_exprs.is_empty() {
                    Some(Vec::new())
                } else {
                    self.eval_regex_arg_list(&spec.arg_exprs, current_caps)
                };
                if let Some(arg_values) = arg_values {
                    dyn_saved = self.install_subrule_dynamic_params(spec, pkg, &arg_values);
                    preinstalled_arg_values = Some(arg_values);
                    self.enter_grammar_rule_dynvars(&spec.lookup_name)
                } else {
                    None
                }
            }
            _ => None,
        };
        let mut out = self.regex_match_atom_with_capture_in_pkg_inner(
            atom,
            chars,
            pos,
            current_caps,
            pkg,
            ignore_case,
            &mut dyn_saved,
            preinstalled_arg_values,
        );
        if let Some(frame) = grammar_frame {
            let values = self.exit_grammar_rule_dynvars(frame);
            if let Some((_, caps)) = out.as_mut() {
                Self::attach_grammar_dynvars_to_named_caps(caps, atom, &values);
            }
        }
        if let Some(saved) = dyn_saved {
            self.restore_subrule_dynamic_params(saved);
        }
        out
    }

    #[allow(clippy::too_many_arguments)]
    fn regex_match_atom_with_capture_in_pkg_inner(
        &mut self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
        current_caps: &RegexCaptures,
        pkg: Symbol,
        ignore_case: bool,
        dyn_saved: &mut Option<super::regex_dynparams::SavedDynParams>,
        preinstalled_arg_values: Option<Vec<Value>>,
    ) -> Option<(usize, RegexCaptures)> {
        let _vars_seed = Self::arm_inline_vars_seed(atom, current_caps);

        // An LTM NFA leaf's nested match (ADR-0125): see the identical guard
        // in `regex_match_atom_all_with_capture_in_pkg` (`regex_match_atom.rs`).
        if LTM_DECLARATIVE_MODE.with(std::cell::Cell::get) {
            match ltm_atom_mode(atom) {
                LtmAtomMode::Terminate
                    if !super::regex_ltm_rank::ltm_leading_ws_is_transparent(atom, pos) =>
                {
                    ltm_record_fate(pos);
                    return None;
                }
                LtmAtomMode::Terminate => {}
                LtmAtomMode::TerminateAfter(inner) => {
                    self.ltm_record_lookahead_fates(inner, chars, pos, pkg);
                    return None;
                }
                LtmAtomMode::SkipZeroWidth => {
                    return Some((pos, RegexCaptures::default()));
                }
                LtmAtomMode::Normal => {}
            }
        }

        // Handle zero-width and group atoms before the length check
        match atom {
            RegexAtom::Group(pattern) => {
                // Use capture-aware matching for groups to propagate inner named captures
                return self
                    .regex_match_end_from_caps_in_pkg(pattern, chars, pos, pkg)
                    .map(|(next, mut inner_caps)| {
                        let mut new_caps = RegexCaptures::default();
                        for (k, v) in inner_caps.named.drain() {
                            new_caps.named.slot_mut(k).merge(v);
                        }
                        new_caps.extend_capture_alias_map(inner_caps.take_capture_alias_map());
                        new_caps.positional.append(&mut inner_caps.positional);
                        // Propagate a `<(` / `)>` capture marker set inside the group.
                        if inner_caps.capture_start.is_some() {
                            new_caps.capture_start = inner_caps.capture_start;
                        }
                        if inner_caps.capture_end.is_some() {
                            new_caps.capture_end = inner_caps.capture_end;
                        }
                        // Writes an inline `{ … }` made to the regex's `:my`
                        // lexicals are part of the same lexical scope as the
                        // enclosing pattern, so they leave the group with it.
                        new_caps.extend_regex_vars(inner_caps.take_regex_vars());
                        (next, new_caps)
                    });
            }
            RegexAtom::CaptureIsolatedGroup(pattern) => {
                // Match `pattern` exactly like `Group` — its own captures
                // resolve normally against `pattern`'s own text, so a
                // backreference WITHIN it to its OWN capture still works
                // (verified against real `raku`: Cro's MIME boundary pattern
                // `$<b>=[...] ... $<b>` invoked via `<$var>`) — but discard
                // everything except the match extent: a `<$var>`-family call
                // gets its own discarded `Match` object in Raku, so none of
                // its positional/named captures may reach the caller's
                // numbering. See the variant's doc comment.
                return self
                    .regex_match_end_from_caps_in_pkg(pattern, chars, pos, pkg)
                    .map(|(next, _inner_caps)| (next, RegexCaptures::default()));
            }
            RegexAtom::CaptureIsolatedGroupScoped(pattern, scope) => {
                // Same as `CaptureIsolatedGroup` above, but the interpolated
                // regex closed over its own defining scope (issue #8951) —
                // install it for the duration of this atom's match.
                let saved = self.install_env_scope(scope);
                let result = self
                    .regex_match_end_from_caps_in_pkg(pattern, chars, pos, pkg)
                    .map(|(next, _inner_caps)| (next, RegexCaptures::default()));
                self.uninstall_regex_closure_scope(Some(saved));
                return result;
            }
            RegexAtom::GoalMatch {
                goal,
                inner,
                goal_text,
            } => {
                if let Some((inner_end, inner_caps)) =
                    self.regex_match_end_from_caps_in_pkg(inner, chars, pos, pkg)
                {
                    if let Some((goal_end, goal_caps)) =
                        self.regex_match_end_from_caps_in_pkg(goal, chars, inner_end, pkg)
                    {
                        let new_caps = merge_goal_captures(goal_caps, inner_caps);
                        return Some((goal_end, new_caps));
                    }
                    Self::record_goal_failure(goal_text, inner_end);
                }
                return None;
            }
            RegexAtom::Alternation(alternatives) => {
                // ADR-0022 §4.4(b): explore all alternatives, keep the one
                // ranked best by (prefix_len desc, litlen desc), ties broken
                // by declaration order (iterating in written order and only
                // replacing on a STRICT rank improvement keeps the earlier
                // branch on a tie — no index needs to travel alongside the
                // key). Replaces the old "longest end wins" rule.
                let mut best: Option<((usize, usize), usize, RegexCaptures)> = None;
                let flags = alternation_list_flags(alternatives);
                for alt in alternatives {
                    let matched = self.regex_match_end_from_caps_in_pkg(alt, chars, pos, pkg);
                    if let Some((next, inner_caps)) = matched {
                        let new_caps = alternation_branch_delta(&flags, inner_caps);
                        let rank = self.ltm_branch_rank_key(alt, chars, pos, pkg);
                        let replace = best
                            .as_ref()
                            .map(|(best_rank, _, _)| rank > *best_rank)
                            .unwrap_or(true);
                        if replace {
                            best = Some((rank, next, new_caps));
                        }
                    }
                }
                return best.map(|(_, next, caps)| (next, caps));
            }
            RegexAtom::Conjunction(_) => {
                // ALL branches must match the SAME substring (end at the same
                // position). Captures from EVERY branch are merged (Raku keeps
                // all captures from each side of `&` / `&&`), in written order.
                // Delegate to the multi-end variant and take the highest-priority
                // (longest) result.
                return self
                    .regex_match_atom_all_with_capture_in_pkg(
                        atom,
                        chars,
                        pos,
                        current_caps,
                        pkg,
                        ignore_case,
                    )
                    .into_iter()
                    .next_back();
            }
            RegexAtom::SequentialAlternation(alternatives) => {
                let flags = alternation_list_flags(alternatives);
                for alt in alternatives {
                    if let Some((next, inner_caps)) =
                        self.regex_match_end_from_caps_in_pkg(alt, chars, pos, pkg)
                    {
                        return Some((next, alternation_branch_delta(&flags, inner_caps)));
                    }
                }
                return None;
            }
            RegexAtom::ZeroWidth
            | RegexAtom::UnicodePropAssert { .. }
            | RegexAtom::LeftWordBoundary
            | RegexAtom::RightWordBoundary
            | RegexAtom::WordBoundary { .. }
            | RegexAtom::WithinWord { .. }
            | RegexAtom::StartOfLine
            | RegexAtom::EndOfLine
            | RegexAtom::EndOfString
            | RegexAtom::WsRule
            | RegexAtom::SameAssertion { .. }
            | RegexAtom::RecurseSelf(_)
            | RegexAtom::AtPosition(_) => {
                return self
                    .regex_match_atom_in_pkg(atom, chars, pos, pkg, ignore_case)
                    .map(|next| (next, RegexCaptures::default()));
            }
            RegexAtom::Lookaround {
                pattern,
                negated,
                is_behind,
            } => {
                let mut inner_vars = crate::runtime::RegexVarMap::default();
                let matched = if *is_behind {
                    let mut found = false;
                    // A start earlier than `pos` minus the most the pattern can
                    // consume cannot end at `pos`, so the search begins there
                    // rather than at 0 — what made one look-behind O(pos) and a
                    // per-line look-behind O(n^2) (#7576).
                    let floor =
                        super::regex_lookbehind::lookbehind_start_floor(pattern, chars, pos);
                    for start in floor..=pos {
                        if let Some((end, mut inner)) =
                            self.regex_match_end_from_caps_in_pkg(pattern, chars, start, pkg)
                            && end == pos
                        {
                            inner_vars = inner.take_regex_vars();
                            found = true;
                            break;
                        }
                    }
                    found
                } else {
                    match self.regex_match_end_from_caps_in_pkg(pattern, chars, pos, pkg) {
                        Some((_, mut inner)) => {
                            inner_vars = inner.take_regex_vars();
                            true
                        }
                        None => false,
                    }
                };
                let pass = if *negated { !matched } else { matched };
                return if pass {
                    // A lookaround consumes nothing and publishes no captures, but a
                    // `{ … }` inside it did run: its writes to the enclosing regex's
                    // `:my` lexicals are real and must survive (YAMLish's `root-block`
                    // measures the indent in a `<?before … { … } >` and matches it
                    // afterwards).
                    let mut new_caps = RegexCaptures::default();
                    new_caps.extend_regex_vars(inner_vars);
                    Some((pos, new_caps))
                } else {
                    None
                };
            }
            RegexAtom::CaptureGroup(pattern) => {
                // Match the inner pattern and capture the matched text
                if let Some((end, inner_caps)) =
                    self.regex_match_end_from_caps_in_pkg(pattern, chars, pos, pkg)
                {
                    let mut new_caps = RegexCaptures::default();
                    // Named captures appearing inside a positional capture group
                    // belong to that group's sub-Match (`$/[0]<name>`), NOT to the
                    // parent Match's top-level named captures (`$/<name>`). They are
                    // therefore preserved only in the slot's subcap below, and
                    // are intentionally NOT merged into the parent `named` map.
                    // Store inner captures as subcaptures of this group
                    let mut subcap = inner_caps.clone();
                    subcap.from = pos;
                    subcap.to = end;
                    new_caps.positional.push(PosSlot {
                        from: pos,
                        to: end,
                        subcap: Some(std::sync::Arc::new(subcap.into_cap_node())),
                        ..Default::default()
                    });
                    return Some((end, new_caps));
                }
                return None;
            }
            RegexAtom::CodeAssertion { .. } => {
                return self.regex_code_atom(atom, chars, pos, current_caps, pkg);
            }
            RegexAtom::CodeInterp { code, list } => {
                return self
                    .regex_code_interp_ends(code, *list, chars, pos, current_caps, pkg, ignore_case)
                    .pop();
            }
            RegexAtom::ClosureInterpolation { .. }
            | RegexAtom::CaptureStartMarker
            | RegexAtom::CaptureEndMarker
            | RegexAtom::Backref(_)
            | RegexAtom::NamedBackref(_)
            | RegexAtom::VarInterp(_)
            | RegexAtom::QqInterp { .. } => {
                return self.regex_leaf_atom(atom, chars, pos, current_caps, pkg, ignore_case);
            }
            RegexAtom::VarDecl { code } => {
                return self.regex_var_decl_atom(code, chars, pos, current_caps);
            }
            _ => {}
        }
        if let RegexAtom::Named(name) = atom {
            let spec = name.spec().clone();
            // Symbolic indirect subrule `<::(EXPR)>`: evaluate EXPR to obtain the
            // dynamic rule name, then dispatch as if it were `<NAME>` so that
            // builtin character classes and user-defined tokens both resolve.
            if spec.lookup_name == "::" && spec.arg_exprs.len() == 1 {
                let val = self.eval_regex_expr_value(&spec.arg_exprs[0], current_caps)?;
                let dyn_atom = RegexAtom::Named(val.to_string_value().into());
                return self
                    .regex_match_atom_all_with_capture_in_pkg(
                        &dyn_atom,
                        chars,
                        pos,
                        current_caps,
                        pkg,
                        ignore_case,
                    )
                    .into_iter()
                    .last();
            }
            let preinstalled = preinstalled_arg_values.is_some();
            let arg_values = if let Some(values) = preinstalled_arg_values {
                values
            } else if spec.arg_exprs.is_empty() {
                Vec::new()
            } else {
                self.eval_regex_arg_list(&spec.arg_exprs, current_caps)?
            };
            // Establish the subrule's `$*`-twigil parameters for the whole
            // resolve-and-match (see `regex_dynparams`); the wrapper restores.
            if !preinstalled {
                *dyn_saved = self.install_subrule_dynamic_params(&spec, pkg, &arg_values);
            }
            // Resolve + parse the candidates once (memoized for the
            // argument-less common case). Patterns are matched in place with
            // `self` against the whole `chars` starting at `pos` (ADR-0016 P1) —
            // an earlier implementation built a fresh scratch sub-interpreter
            // (plus a tail-text `String`) per candidate per call, and the one
            // after that re-sliced to `&chars[pos..]`, which made every inner
            // offset slice-relative and forced a deep rebase of the whole
            // capture subtree afterwards.
            let (candidates, _raw_empty) = self.parsed_subrule_candidates(&spec, pkg, &arg_values);
            if !candidates.is_empty() {
                let mut best: Option<(usize, RegexCaptures)> = None;
                let mut best_sym: Option<String> = None;
                for (parsed, sub_pkg, sym_key) in candidates.iter() {
                    // One rule invocation per candidate (#9803).
                    self.enter_rule_cursor();
                    let matched =
                        self.regex_match_end_from_caps_in_pkg(parsed, chars, pos, *sub_pkg);
                    let cursor = self.leave_rule_cursor();
                    if let Some((inner_end, mut inner_caps)) = matched {
                        if let Some(cursor) = cursor {
                            inner_caps.set_cursor(cursor);
                        }
                        let better = best
                            .as_ref()
                            .map(|(best_end, _)| inner_end > *best_end)
                            .unwrap_or(true);
                        if better {
                            inner_caps.from = pos;
                            inner_caps.to = inner_end;
                            best = Some((inner_end, inner_caps));
                            best_sym = sym_key.clone();
                        }
                    }
                }
                // Wrap the winning inner match via the shared builder (the same
                // one the all-candidates path uses), so capture markers
                // (`<( )>`), alias double-registration and the silent-subrule
                // action channel behave identically on both paths.
                if let Some((inner_end, mut inner_caps)) = best {
                    if best_sym.is_some() {
                        inner_caps.set_sym(best_sym.as_deref().map(Symbol::intern));
                    }
                    return self
                        .build_named_candidates_from_inner(
                            vec![(inner_end, inner_caps)],
                            pos,
                            &spec,
                            None,
                        )
                        .pop();
                }
                return None;
            }
            return self.regex_builtin_named(&spec, chars, pos, pkg);
        }
        if pos >= chars.len() {
            return None;
        }
        self.regex_match_atom_in_pkg(atom, chars, pos, pkg, ignore_case)
            .map(|next| (next, RegexCaptures::default()))
    }
}
