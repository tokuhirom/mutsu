use super::*;

impl Interpreter {
    /// Suggest lexically-declared variable names close to an undeclared
    /// `symbol` (sigiled, e.g. `$Foo`). Candidate names from `declared` are
    /// normalized to sigiled form (scalars are stored without `$`; arrays/hashes
    /// keep their sigil). Matching is sigil-insensitive on the name part so that
    /// e.g. `$barf` suggests `@barf`. Returns suggestions sorted by edit distance.
    pub(super) fn suggest_declared_vars(symbol: &str, declared: &HashSet<String>) -> Vec<String> {
        use crate::runtime::did_you_mean::levenshtein_distance;
        let sigiled = |name: &str| -> String {
            if name.starts_with(['$', '@', '%', '&']) {
                name.to_string()
            } else {
                format!("${}", name)
            }
        };
        // Compare on the name part (without sigil) so a differing sigil counts
        // as a near match rather than excluding the candidate.
        let strip_sigil = |s: &str| s.trim_start_matches(['$', '@', '%', '&']).to_string();
        let target = strip_sigil(symbol);
        let max_distance = if target.len() <= 3 {
            1
        } else if target.len() <= 6 {
            2
        } else {
            3
        };
        let mut scored: Vec<(usize, String)> = Vec::new();
        for cand in declared {
            let cand_sigiled = sigiled(cand);
            if cand_sigiled == symbol {
                continue;
            }
            // Self is already excluded above by the full-name comparison, so a
            // distance of 0 here means a same-name-different-sigil candidate
            // (e.g. `$barf` -> `@barf`), which is a valid suggestion.
            let dist = levenshtein_distance(&target, &strip_sigil(&cand_sigiled));
            if dist <= max_distance {
                scored.push((dist, cand_sigiled));
            }
        }
        scored.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)));
        scored.into_iter().map(|(_, s)| s).collect()
    }

    /// Suggest close routine names (built-in functions + user-defined subs) for
    /// an undeclared routine `name`. Used for X::Undeclared::Symbols
    /// `.routine_suggestion`.
    pub(crate) fn suggest_routine_names(&self, name: &str) -> Vec<String> {
        let mut candidates: Vec<String> = crate::runtime::builtins::BUILTIN_FUNCTION_NAMES
            .iter()
            .map(|s| s.to_string())
            .collect();
        candidates.extend(self.registry().functions.keys().map(|s| s.resolve()));
        // Phasers are suggested for a case-typo'd routine name (`begin` →
        // "Did you mean 'BEGIN'?"), matching rakudo.
        candidates.extend(
            crate::runtime::undeclared_routines::PHASER_SUGGESTION_NAMES
                .iter()
                .map(|s| s.to_string()),
        );
        Self::suggest_from_candidates(name, &candidates)
    }

    /// [`suggest_routine_names`](Self::suggest_routine_names) plus names the
    /// caller knows are routines but the registry does not hold — the
    /// compilation unit's own `sub` declarations, as collected by the
    /// CHECK-time walker (`runtime/undeclared_routines.rs`). Rakudo suggests
    /// those, and without them `sub greeting {}; greetng()` reports the typo
    /// with no way to see what was meant.
    pub(crate) fn suggest_routine_names_including(
        &self,
        name: &str,
        extra: &HashSet<String>,
    ) -> Vec<String> {
        let mut candidates = Self::static_routine_candidates(extra);
        candidates.extend(self.registry().functions.keys().map(|s| s.resolve()));
        Self::suggest_from_candidates(name, &candidates)
    }

    /// The suggestion candidates that need no interpreter: the built-in routine
    /// names, the phasers, and `extra` (the compilation unit's own routines).
    /// Shared with [`suggest_routine_names_including`](Self::suggest_routine_names_including)
    /// so the two paths cannot drift apart.
    fn static_routine_candidates(extra: &HashSet<String>) -> Vec<String> {
        let mut candidates: Vec<String> = extra.iter().cloned().collect();
        candidates.extend(
            crate::runtime::builtins::BUILTIN_FUNCTION_NAMES
                .iter()
                .map(|s| s.to_string()),
        );
        candidates.extend(
            crate::runtime::undeclared_routines::PHASER_SUGGESTION_NAMES
                .iter()
                .map(|s| s.to_string()),
        );
        candidates
    }

    /// [`suggest_routine_names_including`](Self::suggest_routine_names_including)
    /// for the analysis frontend, which has no interpreter to read a registry
    /// from (ADR-0065 S2).
    pub(crate) fn static_routine_suggestions(name: &str, extra: &HashSet<String>) -> Vec<String> {
        Self::suggest_from_candidates(name, &Self::static_routine_candidates(extra))
    }

    /// Suggest close type names (registered classes/roles) for an undeclared
    /// type `name`. Used for X::Undeclared::Symbols `.type_suggestion`.
    pub(crate) fn suggest_type_names(&self, name: &str) -> Vec<String> {
        let mut candidates: Vec<String> = self.registry().classes.keys().cloned().collect();
        candidates.extend(CORE_TYPE_NAMES.iter().map(|s| s.to_string()));
        Self::suggest_from_candidates(name, &candidates)
    }

    pub(crate) fn suggest_from_candidates(name: &str, candidates: &[String]) -> Vec<String> {
        use crate::runtime::did_you_mean::levenshtein_distance;
        // Rakudo accepts a candidate whose Levenshtein distance from the typo
        // is at most `chars div 3` (e.g. a 4-char name tolerates 1 edit, a
        // 9-char name 3 edits), with a floor of 1 for names of length >= 3.
        let max_distance =
            (name.chars().count() / 3).max(if name.chars().count() >= 3 { 1 } else { 0 });
        let mut scored: Vec<(usize, String)> = Vec::new();
        let mut seen = HashSet::new();
        for cand in candidates {
            if cand == name || !seen.insert(cand.clone()) {
                continue;
            }
            // A pure case variant (`begin` vs `BEGIN`) counts as distance 1,
            // like rakudo's case-insensitive-leaning suggestion metric.
            let dist = if cand.eq_ignore_ascii_case(name) {
                1
            } else {
                levenshtein_distance(name, cand)
            };
            if dist > 0 && dist <= max_distance {
                scored.push((dist, cand.clone()));
            }
        }
        scored.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)));
        scored.into_iter().map(|(_, s)| s).collect()
    }
}
