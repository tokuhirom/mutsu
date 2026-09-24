//! Which of the frame's locals a `~~` must publish into `env` before its RHS
//! runs (#9169).
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
//! in it -- unless it looks a name up indirectly (`EVAL`, `::($n)`, `MY::`),
//! the one case still left to a whole-frame publish.

use super::*;

/// What `~~` has to publish into `env` for the regex engine.
pub(super) enum RhsSync {
    /// Only these locals (env-key spellings: `x` for `$x`, `@a`, `%h`, `&f`);
    /// empty when nothing is needed.
    Names(Vec<String>),
    /// Every local slot: embedded code that looks a name up indirectly
    /// (`EVAL`, `::($name)`, a pseudo-package), or an RHS value whose regexes
    /// cannot be reached without running user code (a lazy list, a `Proxy`).
    Full,
}

impl RhsSync {
    fn merge_names(self, more: Option<Vec<String>>) -> RhsSync {
        match (self, more) {
            (RhsSync::Names(mut have), Some(more)) => {
                for n in more {
                    if !have.contains(&n) {
                        have.push(n);
                    }
                }
                RhsSync::Names(have)
            }
            _ => RhsSync::Full,
        }
    }

    fn merge_pattern(self, pattern: &str) -> RhsSync {
        self.merge_names(regex_pattern_var_names(pattern))
    }

    fn merge(self, other: RhsSync) -> RhsSync {
        match other {
            RhsSync::Names(names) => self.merge_names(Some(names)),
            RhsSync::Full => RhsSync::Full,
        }
    }
}

/// How deep [`Interpreter::smartmatch_value_sync`] follows nested containers
/// before it gives up and asks for the whole-frame publish (a
/// self-referential structure would otherwise never end).
const VALUE_SYNC_MAX_DEPTH: usize = 64;

impl Interpreter {
    /// What must be published before the RHS compiled into `[rhs_start,
    /// rhs_end)` runs: the variables every regex literal, and every
    /// `s///`/`S///` pattern and replacement, in it can name
    /// ([`regex_pattern_var_names`], [`subst_replacement_var_names`]), or
    /// [`RhsSync::Full`] when one of them looks a name up indirectly. A
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
    ) -> RhsSync {
        let const_str = |idx: u32| match code.constants.get(idx as usize).map(Value::view) {
            Some(ValueView::Str(s)) => s.to_string(),
            _ => String::new(),
        };
        let mut sync = RhsSync::Names(Vec::new());
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
                    sync = sync
                        .merge_pattern(&const_str(*pattern_idx))
                        .merge_names(subst_replacement_var_names(&const_str(*replacement_idx)));
                    if matches!(sync, RhsSync::Full) {
                        return sync;
                    }
                    continue;
                }
                _ => continue,
            };
            sync = match code.constants.get(const_idx as usize).map(Value::view) {
                Some(ValueView::Regex(pattern)) => sync.merge_pattern(pattern.as_str()),
                Some(ValueView::RegexWithAdverbs(adverbs)) => {
                    sync.merge_pattern(adverbs.pattern.as_str())
                }
                _ => sync,
            };
            if matches!(sync, RhsSync::Full) {
                return sync;
            }
        }
        sync
    }

    /// What must be published before the smartmatch itself runs `left`
    /// against the RHS *value* `right`: a regex value names the variables in
    /// its pattern; a junction, a list, a pair, a capture or a mixin is
    /// searched for the regexes it holds (the match visits those elements
    /// anyway), and so is a hash when `left` is a hash (`Hash ~~ Hash` matches
    /// value against value; any other `~~ %h` is a key lookup and runs no
    /// regex). Any other value (a type object, a scalar, a range, an instance
    /// with a user `ACCEPTS`, a callable) reads nothing by a runtime-parsed
    /// name. Only a lazy list or a `Proxy`, whose elements cannot be reached
    /// without running user code, keeps the whole-frame publish.
    // Cost: O(k + p), k = elements reachable in `right` (the match visits them
    // too), p = total pattern length of the regexes among them; O(1) for a
    // non-container value.
    pub(super) fn smartmatch_value_sync(left: &Value, right: &Value) -> RhsSync {
        let left_is_hash = left.with_deref(|l| matches!(l.view(), ValueView::Hash(_)));
        Self::value_sync_at(right, left_is_hash, 0)
    }

    fn value_sync_at(value: &Value, left_is_hash: bool, depth: usize) -> RhsSync {
        if depth > VALUE_SYNC_MAX_DEPTH {
            return RhsSync::Full;
        }
        let each = |items: &mut dyn Iterator<Item = &Value>| {
            let mut sync = RhsSync::Names(Vec::new());
            for item in items {
                sync = sync.merge(Self::value_sync_at(item, left_is_hash, depth + 1));
                if matches!(sync, RhsSync::Full) {
                    break;
                }
            }
            sync
        };
        match value.view() {
            ValueView::Regex(pattern) => RhsSync::Names(Vec::new()).merge_pattern(pattern.as_str()),
            ValueView::RegexWithAdverbs(adverbs) => {
                RhsSync::Names(Vec::new()).merge_pattern(adverbs.pattern.as_str())
            }
            ValueView::Routine {
                captured_regex: Some(inner),
                ..
            } => Self::value_sync_at(inner, left_is_hash, depth + 1),
            ValueView::Junction { values, .. } => each(&mut values.iter()),
            ValueView::Array(items, _) => each(&mut items.iter()),
            ValueView::Seq(body) | ValueView::HyperSeq(body) | ValueView::RaceSeq(body) => {
                each(&mut body.iter())
            }
            ValueView::Slip(items) => each(&mut items.iter()),
            ValueView::Hash(map) if left_is_hash => each(&mut map.iter().map(|(_, v)| v)),
            ValueView::Pair(_, v) => Self::value_sync_at(v, left_is_hash, depth + 1),
            ValueView::ValuePair(_, v) => Self::value_sync_at(v, left_is_hash, depth + 1),
            ValueView::Capture { positional, named } => {
                each(&mut positional.iter().chain(named.iter().map(|(_, v)| v)))
            }
            ValueView::Mixin(inner, _) => Self::value_sync_at(inner, left_is_hash, depth + 1),
            ValueView::Scalar(inner) => Self::value_sync_at(inner, left_is_hash, depth + 1),
            ValueView::VarRef { value, .. } => Self::value_sync_at(value, left_is_hash, depth + 1),
            ValueView::ContainerRef(..) | ValueView::ContainerView(..) => {
                let inner = value.deref_container();
                if matches!(
                    inner.view(),
                    ValueView::ContainerRef(..) | ValueView::ContainerView(..)
                ) {
                    return RhsSync::Full;
                }
                Self::value_sync_at(&inner, left_is_hash, depth + 1)
            }
            ValueView::LazyList(..) | ValueView::Proxy { .. } | ValueView::LazyThunk(..) => {
                RhsSync::Full
            }
            _ => RhsSync::Names(Vec::new()),
        }
    }

    /// Carry out a [`RhsSync`] decision.
    // Cost: O(n) for `Names`, n = names; O(L) for `Full`, L = local slots.
    pub(super) fn apply_smartmatch_sync(&mut self, code: &CompiledCode, sync: &RhsSync) {
        match sync {
            RhsSync::Names(names) if names.is_empty() => {}
            RhsSync::Names(names) => self.sync_regex_interpolation_env_for_names(code, names),
            RhsSync::Full => self.sync_regex_interpolation_env_from_locals(code),
        }
    }
}

/// Every local a regex pattern can name, as env keys, or `None` when it
/// embeds code that looks a name up indirectly (see [`source_var_names`]).
/// A pattern embedding code (`{ }`, `<{ }>`, `<?{ }>`, `:my`) is scanned as
/// code: every bare identifier in it is a candidate too.
// Cost: O(p), p = pattern length.
pub(super) fn regex_pattern_var_names(pattern: &str) -> Option<Vec<String>> {
    let has_code = pattern.contains('{')
        || pattern.contains(":my")
        || pattern.contains(":our")
        || pattern.contains(":let")
        || pattern.contains(":temp");
    source_var_names(pattern, has_code)
}

/// Every local a substitution's replacement can name. The replacement is a
/// `qq` quote: interpolated variables, and embedded `{ }` code.
// Cost: O(p), p = replacement length.
pub(super) fn subst_replacement_var_names(replacement: &str) -> Option<Vec<String>> {
    source_var_names(replacement, replacement.contains('{'))
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
/// `self`) and `&ident` (a lexical routine), and the scan answers `None` for
/// an indirect lookup -- an [`INDIRECT_LOOKUP_NAMES`] word, `::(...)` or
/// `::<...>` -- whose target no scan can know. Publishing a name nobody
/// reads is harmless; missing one would let a by-name reader see a stale env
/// value.
// Cost: O(p), p = source length.
fn source_var_names(src: &str, code: bool) -> Option<Vec<String>> {
    if code && (src.contains("::(") || src.contains("::<")) {
        return None;
    }
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
                return None;
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
    Some(names)
}

#[cfg(test)]
mod tests {
    use super::{regex_pattern_var_names, subst_replacement_var_names};

    fn names(p: &str) -> Vec<String> {
        regex_pattern_var_names(p).expect("no embedded code")
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
    fn indirect_lookup_is_unbounded() {
        assert!(regex_pattern_var_names("a { EVAL '$x' } b").is_none());
        assert!(regex_pattern_var_names("<?{ MY::<$x> }>").is_none());
        assert!(regex_pattern_var_names("{ ::('$x') }").is_none());
        assert!(regex_pattern_var_names("{ callframe(0) }").is_none());
        // Outside code the same words are literal text.
        assert!(regex_pattern_var_names("EVAL MY").is_some());
    }

    #[test]
    fn replacements_name_their_interpolations() {
        let n = subst_replacement_var_names("<$pre$0{ $count++ }>").unwrap();
        for want in ["pre", "count"] {
            assert!(n.iter().any(|x| x == want), "missing {want} in {n:?}");
        }
    }
}
