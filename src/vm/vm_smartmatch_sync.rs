//! Which of the frame's locals a `~~` must publish into `env` before its RHS
//! runs (#9169).
//!
//! The regex engine resolves a pattern's interpolated variables (`/a$x/`,
//! `<$rule>`, `<&f>`) *by name* against `env` while it matches -- a reader the
//! compile-time free-variable analysis cannot see, so a slot-only local's env
//! entry may be stale when it runs. The `~~` op used to answer that by
//! re-broadcasting every local slot of the frame on every match: O(L) per
//! `$x ~~ Int`, whatever the RHS.
//!
//! Every other reader is already covered without it: a routine, method or
//! closure the RHS reaches reads its free variables through the per-store
//! mirror or a shared cell, exactly as it does for a plain call `f($x)`, which
//! pays no pre-sync either. So only a regex needs the publish, and a regex
//! without embedded code can name only the variables spelled in its pattern.

use super::*;

/// What `~~` has to publish into `env` for the regex engine.
pub(super) enum RhsSync {
    /// Only these locals (env-key spellings: `x` for `$x`, `@a`, `%h`, `&f`);
    /// empty when nothing is needed.
    Names(Vec<String>),
    /// Every local slot: by-name reads that cannot be bounded statically
    /// (a regex embedding code, a destructive `s///`/`tr///`).
    Full,
}

impl RhsSync {
    fn merge_pattern(self, pattern: &str) -> RhsSync {
        match (self, regex_pattern_var_names(pattern)) {
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
}

impl Interpreter {
    /// What must be published before the RHS compiled into `[rhs_start,
    /// rhs_end)` runs: the variables every regex literal in it can name
    /// ([`regex_pattern_var_names`]), or [`RhsSync::Full`] when one of them
    /// embeds code or the RHS runs a destructive `s///`/`tr///`. Any other op
    /// needs nothing: a routine, method or closure it reaches reads its free
    /// variables the way it does for a plain call. A regex the RHS only
    /// *computes* (`$x ~~ $re`) is covered after the RHS has run, by
    /// [`Self::smartmatch_value_sync`].
    // Cost: O(r + p), r = ops of the RHS, p = total pattern length of its regex
    // literals.
    pub(super) fn smartmatch_rhs_sync(
        code: &CompiledCode,
        rhs_start: usize,
        rhs_end: usize,
    ) -> RhsSync {
        let mut sync = RhsSync::Names(Vec::new());
        for op in code.ops.get(rhs_start..rhs_end).unwrap_or(&[]) {
            let const_idx = match op {
                OpCode::LoadConst(idx) => *idx,
                OpCode::LoadRegexClosure { const_idx, .. } => *const_idx,
                OpCode::Subst { .. }
                | OpCode::NonDestructiveSubst { .. }
                | OpCode::Transliterate { .. } => return RhsSync::Full,
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

    /// What must be published before the smartmatch itself runs against the
    /// RHS *value* `right`: a regex value names the variables in its pattern;
    /// a junction or a collection may hold regexes of its own, so it keeps the
    /// whole-frame publish; any other value (a type object, a scalar, a range,
    /// an instance with a user `ACCEPTS`, a callable) reads nothing by a
    /// runtime-parsed name.
    // Cost: O(p) for a regex value, p = pattern length; O(1) otherwise.
    pub(super) fn smartmatch_value_sync(right: &Value) -> RhsSync {
        match right.view() {
            ValueView::Regex(pattern) => RhsSync::Names(Vec::new()).merge_pattern(pattern.as_str()),
            ValueView::RegexWithAdverbs(adverbs) => {
                RhsSync::Names(Vec::new()).merge_pattern(adverbs.pattern.as_str())
            }
            ValueView::Junction { .. }
            | ValueView::Array(..)
            | ValueView::Hash(..)
            | ValueView::Seq(..)
            | ValueView::HyperSeq(..)
            | ValueView::RaceSeq(..)
            | ValueView::Slip(..)
            | ValueView::LazyList(..)
            | ValueView::Mixin(..)
            | ValueView::Capture { .. }
            | ValueView::Proxy { .. }
            | ValueView::Scalar(..)
            | ValueView::ContainerRef(..)
            | ValueView::ContainerView(..) => RhsSync::Full,
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

/// Every local a regex pattern can name, as env keys, or `None` when the
/// pattern embeds code (`{ }`, `<{ }>`, `<?{ }>`, `:my`) whose reads cannot be
/// bounded from the source text.
///
/// Deliberately a **superset**: an identifier after a sigil (`$`, `@`, `%`,
/// `&`, with an optional twigil) contributes every prefix that ends at a `-` /
/// `'` boundary as well as its full spelling (the engine decides where
/// `$a-b` ends, not this scan), and an `<ident` / `<.ident` / `<&ident`
/// subrule contributes `&ident`. Publishing a name nobody reads is harmless;
/// missing one would let the engine read a stale env value.
// Cost: O(p), p = pattern length.
pub(super) fn regex_pattern_var_names(pattern: &str) -> Option<Vec<String>> {
    if pattern.contains('{') || pattern.contains(":my") || pattern.contains(":our") {
        return None;
    }
    let bytes = pattern.as_bytes();
    let is_ident = |b: u8| b.is_ascii_alphanumeric() || b == b'_';
    let mut names: Vec<String> = Vec::new();
    let mut push = |name: String| {
        if !names.contains(&name) {
            names.push(name);
        }
    };
    let mut i = 0;
    while i < bytes.len() {
        let b = bytes[i];
        let (sigil, start) = match b {
            b'$' | b'@' | b'%' | b'&' => (Some(b), i + 1),
            b'<' => (None, i + 1),
            _ => {
                i += 1;
                continue;
            }
        };
        // An optional twigil (`$*x`, `$!x`, `$.x`, `$?x`, `$^x`, `$:x`) or,
        // after `<`, a subrule prefix (`<.x>`, `<&x>`, `<?x>`, `<!x>`).
        let mut name_start = start;
        let mut twigil = "";
        if let Some(&t) = bytes.get(name_start)
            && matches!(t, b'*' | b'!' | b'.' | b'?' | b'^' | b':' | b'&')
            && bytes.get(name_start + 1).is_some_and(|&c| is_ident(c))
        {
            twigil = &pattern[name_start..name_start + 1];
            name_start += 1;
        }
        if !bytes
            .get(name_start)
            .is_some_and(|&c| c.is_ascii_alphabetic() || c == b'_')
        {
            i += 1;
            continue;
        }
        let mut j = name_start;
        let mut ends: Vec<usize> = Vec::new();
        while j < bytes.len() {
            if is_ident(bytes[j]) {
                j += 1;
                continue;
            }
            // `-`/`'` continue an identifier only when a letter follows.
            if matches!(bytes[j], b'-' | b'\'')
                && bytes.get(j + 1).is_some_and(|c| c.is_ascii_alphabetic())
            {
                ends.push(j);
                j += 1;
                continue;
            }
            break;
        }
        ends.push(j);
        for end in ends {
            let ident = &pattern[name_start..end];
            match sigil {
                // A scalar's env key carries no sigil; a twigil stays.
                Some(b'$') => push(format!("{twigil}{ident}")),
                Some(s) => push(format!("{}{twigil}{ident}", s as char)),
                // A subrule (`<ident>`) may be a lexical regex/routine.
                None => {
                    push(format!("&{ident}"));
                }
            }
        }
        i = j;
    }
    Some(names)
}

#[cfg(test)]
mod tests {
    use super::regex_pattern_var_names;

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
    fn embedded_code_is_unbounded() {
        assert!(regex_pattern_var_names("a { $x++ } b").is_none());
        assert!(regex_pattern_var_names("<?{ $x > 1 }>").is_none());
        assert!(regex_pattern_var_names(":my $y = 1; a").is_none());
    }
}
