//! Which of the frame's locals a `~~` must publish into `env` before its RHS
//! runs (#9169, #9293).
//!
//! The regex engine resolves a pattern's interpolated variables (`/a$x/`,
//! `<$rule>`, `<&f>`) *by name* against `env` while it matches, and so does
//! the code it embeds (`{ }`, `<?{ }>`) and a substitution's replacement (a
//! `qq` quote evaluated per match) -- readers the compile-time free-variable
//! analysis cannot see, so a slot-only local's env entry may be stale when
//! they run. The `~~` op used to answer that by re-broadcasting every local
//! slot of the frame on every match: O(L) per `$x ~~ Int`, whatever the RHS.
//!
//! Every other reader is already covered without it: a routine, method or
//! closure the RHS reaches reads its free variables through the per-store
//! mirror or a shared cell, exactly as it does for a plain call `f($x)`, which
//! pays no pre-sync either. So only the source text the engine evaluates by
//! name needs the publish, and that text can name only the identifiers spelled
//! in it.
//!
//! Text that looks a name up *indirectly* (`EVAL`, `::($n)`, `MY::`) is no
//! different from the same lookup written outside a regex: the chunk holding
//! it is marked reflective when it is finalized
//! ([`regex_source_has_indirect_lookup`], read by
//! `CompiledCode::scan_reflective_name_access`), which keeps every store
//! mirroring into `env` -- the mechanism a plain `EVAL '$x'` already relies
//! on. So no match publishes the whole frame any more.

use super::*;

/// How deep [`Interpreter::smartmatch_value_sync`] follows nested containers
/// before it starts remembering the mutable ones it has visited, so a
/// self-referential structure (`@a[0] = @a`) cannot recurse forever.
const VALUE_SYNC_TRACK_DEPTH: usize = 64;

/// Add `more` to `have`, skipping duplicates.
fn merge_names(have: &mut Vec<String>, more: Vec<String>) {
    for n in more {
        if !have.contains(&n) {
            have.push(n);
        }
    }
}

impl Interpreter {
    /// What must be published before the RHS compiled into `[rhs_start,
    /// rhs_end)` runs: the variables every regex literal, and every
    /// `s///`/`S///` pattern and replacement, in it can name
    /// ([`regex_pattern_var_names`], [`subst_replacement_var_names`]). A
    /// `tr///` interpolates nothing. Any other op needs nothing: a routine,
    /// method or closure it reaches reads its free variables the way it does
    /// for a plain call. A regex the RHS only *computes* (`$x ~~ $re`) is
    /// covered after the RHS has run, by [`Self::smartmatch_value_sync`].
    // Cost: O(r + p), r = ops of the RHS, p = total source length of its regex
    // literals and substitutions.
    pub(super) fn smartmatch_rhs_sync(
        code: &CompiledCode,
        rhs_start: usize,
        rhs_end: usize,
    ) -> Vec<String> {
        let const_str = |idx: u32| match code.constants.get(idx as usize).map(Value::view) {
            Some(ValueView::Str(s)) => s.to_string(),
            _ => String::new(),
        };
        let mut names = Vec::new();
        for op in code.ops.get(rhs_start..rhs_end).unwrap_or(&[]) {
            let const_idx = match op {
                OpCode::LoadConst(idx) => *idx,
                OpCode::LoadRegexClosure { const_idx, .. } => *const_idx,
                OpCode::Subst {
                    pattern_idx,
                    replacement_idx,
                    ..
                }
                | OpCode::NonDestructiveSubst {
                    pattern_idx,
                    replacement_idx,
                    ..
                } => {
                    merge_names(
                        &mut names,
                        regex_pattern_var_names(&const_str(*pattern_idx)),
                    );
                    merge_names(
                        &mut names,
                        subst_replacement_var_names(&const_str(*replacement_idx)),
                    );
                    continue;
                }
                _ => continue,
            };
            match code.constants.get(const_idx as usize).map(Value::view) {
                Some(ValueView::Regex(pattern)) => {
                    merge_names(&mut names, regex_pattern_var_names(pattern.as_str()))
                }
                Some(ValueView::RegexWithAdverbs(adverbs)) => merge_names(
                    &mut names,
                    regex_pattern_var_names(adverbs.pattern.as_str()),
                ),
                _ => {}
            }
        }
        names
    }

    /// What must be published before the smartmatch itself runs `left`
    /// against the RHS *value* `right`: a regex value names the variables in
    /// its pattern; a junction, a list, a pair, a capture or a mixin is
    /// searched for the regexes it holds (the match visits those elements
    /// anyway), and so is a hash when `left` is a hash (`Hash ~~ Hash` matches
    /// value against value; any other `~~ %h` is a key lookup and runs no
    /// regex). Any other value (a type object, a scalar, a range, an instance
    /// with a user `ACCEPTS`, a callable) reads nothing by a runtime-parsed
    /// name.
    ///
    /// Nor does a value whose elements need user code to reach: the match
    /// never runs that code. A lazy list RHS answers `False` without being
    /// reified (as in Rakudo), and a nested `Proxy` or unforced lazy thunk is
    /// compared as it stands. The op FETCHes / forces a *top-level* one before
    /// calling this (Rakudo decontainerizes the RHS too), and a forced thunk's
    /// cached value is searched like any other.
    // Cost: O(k + p), k = elements reachable in `right` (the match visits them
    // too), p = total pattern length of the regexes among them; O(1) for a
    // non-container value.
    pub(super) fn smartmatch_value_sync(left: &Value, right: &Value) -> Vec<String> {
        let left_is_hash = left.with_deref(|l| matches!(l.view(), ValueView::Hash(_)));
        let mut names = Vec::new();
        let mut seen = std::collections::HashSet::new();
        Self::value_sync_at(right, left_is_hash, 0, &mut seen, &mut names);
        names
    }

    fn value_sync_at(
        value: &Value,
        left_is_hash: bool,
        depth: usize,
        seen: &mut std::collections::HashSet<usize>,
        names: &mut Vec<String>,
    ) {
        // Past the tracking depth, visit each mutable container once: only an
        // `Array`, a `Hash` or a container cell can close a cycle.
        let first_visit = |seen: &mut std::collections::HashSet<usize>, addr: usize| {
            depth <= VALUE_SYNC_TRACK_DEPTH || seen.insert(addr)
        };
        let each = |items: &mut dyn Iterator<Item = &Value>,
                    seen: &mut std::collections::HashSet<usize>,
                    names: &mut Vec<String>| {
            for item in items {
                Self::value_sync_at(item, left_is_hash, depth + 1, seen, names);
            }
        };
        match value.view() {
            ValueView::Regex(pattern) => {
                merge_names(names, regex_pattern_var_names(pattern.as_str()))
            }
            ValueView::RegexWithAdverbs(adverbs) => {
                merge_names(names, regex_pattern_var_names(adverbs.pattern.as_str()))
            }
            ValueView::Routine {
                captured_regex: Some(inner),
                ..
            } => Self::value_sync_at(inner, left_is_hash, depth + 1, seen, names),
            ValueView::Junction { values, .. } => each(&mut values.iter(), seen, names),
            ValueView::Array(items, _) => {
                if first_visit(seen, crate::gc::Gc::as_ptr(&items) as usize) {
                    each(&mut items.iter(), seen, names)
                }
            }
            ValueView::Seq(body) | ValueView::HyperSeq(body) | ValueView::RaceSeq(body) => {
                each(&mut body.iter(), seen, names)
            }
            ValueView::Slip(items) => each(&mut items.iter(), seen, names),
            ValueView::Hash(map) if left_is_hash => {
                if first_visit(seen, crate::gc::Gc::as_ptr(&map) as usize) {
                    each(&mut map.values(), seen, names)
                }
            }
            ValueView::Pair(_, v) => Self::value_sync_at(v, left_is_hash, depth + 1, seen, names),
            ValueView::ValuePair(_, v) => {
                Self::value_sync_at(v, left_is_hash, depth + 1, seen, names)
            }
            ValueView::Capture { positional, named } => {
                each(&mut positional.iter().chain(named.values()), seen, names)
            }
            ValueView::Mixin(inner, _) => {
                Self::value_sync_at(inner, left_is_hash, depth + 1, seen, names)
            }
            ValueView::Scalar(inner) => {
                Self::value_sync_at(inner, left_is_hash, depth + 1, seen, names)
            }
            ValueView::VarRef { value, .. } => {
                Self::value_sync_at(value, left_is_hash, depth + 1, seen, names)
            }
            ValueView::ContainerRef(cell) | ValueView::ContainerView(cell) => {
                if first_visit(seen, crate::gc::Gc::as_ptr(&cell) as usize) {
                    let inner = value.deref_container();
                    Self::value_sync_at(&inner, left_is_hash, depth + 1, seen, names)
                }
            }
            ValueView::LazyThunk(thunk) => {
                let cached = thunk.cache.lock().ok().and_then(|c| c.clone());
                if let Some(v) = cached {
                    Self::value_sync_at(&v, left_is_hash, depth + 1, seen, names)
                }
            }
            _ => {}
        }
    }

    /// Publish the locals `names` selected (env-key spellings).
    // Cost: O(n), n = names.
    pub(super) fn apply_smartmatch_sync(&mut self, code: &CompiledCode, names: &[String]) {
        if !names.is_empty() {
            self.sync_regex_interpolation_env_for_names(code, names);
        }
    }
}

/// Whether a regex pattern embeds code (`{ }`, `<{ }>`, `<?{ }>`, `:my`),
/// which the scan then reads as code.
fn pattern_has_code(pattern: &str) -> bool {
    pattern.contains('{')
        || pattern.contains(":my")
        || pattern.contains(":our")
        || pattern.contains(":let")
        || pattern.contains(":temp")
}

/// Every local a regex pattern can name, as env keys (see
/// [`scan_source_names`]). A pattern embedding code is scanned as code: every
/// bare identifier in it is a candidate too.
// Cost: O(p), p = pattern length.
pub(super) fn regex_pattern_var_names(pattern: &str) -> Vec<String> {
    scan_source_names(pattern, pattern_has_code(pattern)).0
}

/// Every local a substitution's replacement can name. The replacement is a
/// `qq` quote: interpolated variables, and embedded `{ }` code.
// Cost: O(p), p = replacement length.
pub(super) fn subst_replacement_var_names(replacement: &str) -> Vec<String> {
    scan_source_names(replacement, replacement.contains('{')).0
}

/// Whether a regex pattern (`replacement` false) or a substitution's
/// replacement (`replacement` true) embeds code that looks a name up
/// *indirectly* -- an [`INDIRECT_LOOKUP_NAMES`] word, `::(...)` or `::<...>`.
/// No scan of the text can bound what such code reads, so the chunk holding
/// it is treated like one that calls `EVAL` directly (see
/// `CompiledCode::scan_reflective_name_access`).
// Cost: O(p), p = source length; O(1)-ish for text that embeds no code.
pub(crate) fn regex_source_has_indirect_lookup(src: &str, replacement: bool) -> bool {
    let code = if replacement {
        src.contains('{')
    } else {
        pattern_has_code(src)
    };
    code && scan_source_names(src, true).1
}

/// Names that make embedded code look another name up *indirectly*, which no
/// scan of the source text can bound: string evaluation, the call-frame
/// introspection API, and the pseudo-packages that expose a whole lexical
/// scope (`MY::<$x>`, `OUTER::`, `CALLER::`, ...).
const INDIRECT_LOOKUP_NAMES: &[&str] = &[
    "EVAL",
    "EVALFILE",
    "callframe",
    "MY",
    "OUR",
    "OUTER",
    "OUTERS",
    "CALLER",
    "CALLERS",
    "DYNAMIC",
    "LEXICAL",
    "UNIT",
    "SETTING",
    "CORE",
    "CLIENT",
];

/// Every env key the source text `src` (a regex pattern, or a `qq`
/// replacement) can read by name.
///
/// Deliberately a **superset**: an identifier after a sigil (`$`, `@`, `%`,
/// `&`, with an optional twigil) contributes every prefix that ends at a `-` /
/// `'` / `::` boundary as well as its full spelling (the evaluator decides
/// where `$a-b` ends, not this scan), and an `<ident` / `<.ident` / `<&ident`
/// subrule contributes `&ident`. With `code` set (the text embeds code) every
/// bare identifier contributes both its plain spelling (a sigilless variable,
/// `self`) and `&ident` (a lexical routine), and the second result reports an
/// indirect lookup -- an [`INDIRECT_LOOKUP_NAMES`] word, `::(...)` or
/// `::<...>` -- whose target no scan can know. Publishing a name nobody
/// reads is harmless; missing one would let a by-name reader see a stale env
/// value.
// Cost: O(p), p = source length.
fn scan_source_names(src: &str, code: bool) -> (Vec<String>, bool) {
    let mut indirect = code && (src.contains("::(") || src.contains("::<"));
    let chars: Vec<(usize, char)> = src.char_indices().collect();
    let is_ident = |c: char| c.is_alphanumeric() || c == '_';
    let is_ident_start = |c: char| c.is_alphabetic() || c == '_';
    let at = |k: usize| chars.get(k).map(|&(_, c)| c);
    let byte_at = |k: usize| chars.get(k).map_or(src.len(), |&(b, _)| b);
    let mut names: Vec<String> = Vec::new();
    let mut push = |name: String| {
        if !names.contains(&name) {
            names.push(name);
        }
    };
    let mut i = 0;
    while i < chars.len() {
        let c = chars[i].1;
        let (sigil, start) = match c {
            '$' | '@' | '%' | '&' => (Some(c), i + 1),
            '<' => (None, i + 1),
            // A bare identifier, in code: a word start not glued onto an
            // identifier to its left.
            _ if code && is_ident_start(c) && !(i > 0 && is_ident(chars[i - 1].1)) => (None, i),
            _ => {
                i += 1;
                continue;
            }
        };
        // An optional twigil (`$*x`, `$!x`, `$.x`, `$?x`, `$^x`, `$:x`) or,
        // after `<`, a subrule prefix (`<.x>`, `<&x>`, `<?x>`, `<!x>`).
        let mut name_start = start;
        let mut twigil = String::new();
        if name_start != i
            && let Some(t) = at(name_start)
            && matches!(t, '*' | '!' | '.' | '?' | '^' | ':' | '&')
            && at(name_start + 1).is_some_and(is_ident_start)
        {
            twigil.push(t);
            name_start += 1;
        }
        if !at(name_start).is_some_and(is_ident_start) {
            i += 1;
            continue;
        }
        let mut j = name_start;
        let mut ends: Vec<usize> = Vec::new();
        while let Some(ch) = at(j) {
            if is_ident(ch) {
                j += 1;
                continue;
            }
            // `-`/`'` continue an identifier only when a letter follows.
            if matches!(ch, '-' | '\'') && at(j + 1).is_some_and(char::is_alphabetic) {
                ends.push(j);
                j += 1;
                continue;
            }
            // `A::B`: a package-qualified name.
            if ch == ':' && at(j + 1) == Some(':') && at(j + 2).is_some_and(is_ident_start) {
                ends.push(j);
                j += 2;
                continue;
            }
            break;
        }
        ends.push(j);
        for end in ends {
            let ident = &src[byte_at(name_start)..byte_at(end)];
            if code && sigil.is_none() && INDIRECT_LOOKUP_NAMES.contains(&ident) {
                indirect = true;
            }
            match sigil {
                // A scalar's env key carries no sigil; a twigil stays.
                Some('$') => push(format!("{twigil}{ident}")),
                Some(s) => push(format!("{s}{twigil}{ident}")),
                // A word in code: a sigilless variable or a routine.
                None if code => {
                    push(ident.to_string());
                    push(format!("&{ident}"));
                }
                // A subrule (`<ident>`) may be a lexical regex/routine.
                None => push(format!("&{ident}")),
            }
        }
        i = j;
    }
    (names, indirect)
}

#[cfg(test)]
mod tests {
    use super::{
        regex_pattern_var_names, regex_source_has_indirect_lookup, subst_replacement_var_names,
    };

    fn names(p: &str) -> Vec<String> {
        regex_pattern_var_names(p)
    }

    #[test]
    fn a_plain_pattern_names_nothing() {
        assert!(names("abc \\d+ [x|y]*").is_empty());
    }

    #[test]
    fn interpolations_are_named_with_every_prefix() {
        let n = names("a $x b @arr %h &f $*dyn $a-b");
        for want in ["x", "@arr", "%h", "&f", "*dyn", "a", "a-b"] {
            assert!(n.iter().any(|x| x == want), "missing {want} in {n:?}");
        }
    }

    #[test]
    fn subrules_are_named_as_code_vars() {
        let n = names("<word> <.ws> <&helper> <$rx>");
        for want in ["&word", "&ws", "&helper", "rx"] {
            assert!(n.iter().any(|x| x == want), "missing {want} in {n:?}");
        }
    }

    #[test]
    fn embedded_code_names_its_words() {
        let n = names("a { $x++; helper(@list) } b");
        for want in ["x", "@list", "helper", "&helper"] {
            assert!(n.iter().any(|x| x == want), "missing {want} in {n:?}");
        }
        let n = names("<?{ $lim > self.size }> :my $y = n");
        for want in ["lim", "self", "y", "n", "size"] {
            assert!(n.iter().any(|x| x == want), "missing {want} in {n:?}");
        }
    }

    #[test]
    fn non_ascii_identifiers_are_whole() {
        let n = names("a $caf\u{e9}x b");
        assert!(n.iter().any(|x| x == "caf\u{e9}x"), "{n:?}");
    }

    #[test]
    fn package_qualified_names_keep_every_prefix() {
        let n = names("{ Foo::Bar.new($x) }");
        for want in ["Foo", "Foo::Bar", "x"] {
            assert!(n.iter().any(|x| x == want), "missing {want} in {n:?}");
        }
    }

    #[test]
    fn indirect_lookup_is_reported_and_spelled_names_kept() {
        assert!(regex_source_has_indirect_lookup("a { EVAL '$x' } b", false));
        assert!(regex_source_has_indirect_lookup("<?{ MY::<$x> }>", false));
        assert!(regex_source_has_indirect_lookup("{ ::('$x') }", false));
        assert!(regex_source_has_indirect_lookup("{ callframe(0) }", false));
        assert!(regex_source_has_indirect_lookup("<{ EVAL $y }>", true));
        // Outside code the same words are literal text.
        assert!(!regex_source_has_indirect_lookup("EVAL MY", false));
        assert!(!regex_source_has_indirect_lookup("$x { $y }", false));
        // The directly spelled names are still collected.
        let n = names("<?{ $0 eq EVAL($w) }>");
        assert!(n.iter().any(|x| x == "w"), "{n:?}");
    }

    #[test]
    fn replacements_name_their_interpolations() {
        let n = subst_replacement_var_names("<$pre$0{ $count++ }>");
        for want in ["pre", "count"] {
            assert!(n.iter().any(|x| x == want), "missing {want} in {n:?}");
        }
    }
}
