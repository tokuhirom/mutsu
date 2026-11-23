//! Plain `$var` atoms read at match time (ADR-12917, direction 2).
//!
//! ```raku
//! my $x = "a"; say ~("xb" ~~ / x { $x = "b" } $x /);   # xb
//! ```
//!
//! Rakudo reads an interpolated scalar when the cursor reaches it, so a code
//! block earlier in the pattern can change what the atom matches. Splicing the
//! value into the pattern text before the match starts cannot see that write.
//! A pattern reads every plain scalar it names at match time: a code block can
//! assign it, a loop re-enters atoms written before the block, and without a
//! block one body per pattern replaces a pattern text per value.
//! Those scalars are declared as match-time lexicals (the same declaration block a
//! rule's parameters use, see `regex_rule_params`), which the interpolation
//! pre-pass leaves in the text and the structural parser lowers to a
//! `RegexAtom::VarInterp`.

/// The plain scalar names the pattern `text` writes, in order of first use, and
/// whether it contains a code block. A name a `=` follows is an alias
/// (`$x=ATOM`): the variable is still read, and the interpolation pre-pass
/// drops the alias's atom, which is left out here.
// Cost: O(n), n = the text's length.
pub(crate) fn scalars_in_pattern(text: &str) -> (Vec<String>, bool) {
    let (names, saw_block) = scan_scalars(text);
    (names.into_iter().map(|(name, _)| name).collect(), saw_block)
}

/// Every plain scalar name in order of first use, flagged by whether it was
/// written after a code block, and whether the text had a code block at all.
fn scan_scalars(text: &str) -> (Vec<(String, bool)>, bool) {
    if !text.contains('$') {
        return (Vec::new(), false);
    }
    let chars: Vec<char> = text.chars().collect();
    let mut names: Vec<(String, bool)> = Vec::new();
    let mut seen_block = false;
    let mut i = 0usize;
    while i < chars.len() {
        match chars[i] {
            '\\' => i += 2,
            '\'' | '"' => {
                let quote = chars[i];
                i += 1;
                while i < chars.len() && chars[i] != quote {
                    if chars[i] == '\\' {
                        i += 1;
                    }
                    i += 1;
                }
                i += 1;
            }
            '#' => {
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
            }
            // `<<` is the left word boundary, not a `<...>` assertion.
            '<' if chars.get(i + 1) == Some(&'<') => i += 2,
            '<' if matches!(chars.get(i + 1), Some('[' | '-' | '+')) => {
                // A character class: its braces and dollars are plain members.
                while i < chars.len() && chars[i] != '>' {
                    if chars[i] == '\\' {
                        i += 1;
                    }
                    i += 1;
                }
                i += 1;
            }
            '<' if chars.get(i + 1) == Some(&'$') => {
                // `<$var>` interpolates its value as regex source, with
                // semantics of its own (#13029, #13042), read where the
                // parse-time `<$var>` arm reads it -- unless a code block
                // precedes it, which may assign the variable: then it is read
                // when the cursor reaches it, as Rakudo does (#14510).
                let start = i + 2;
                while i < chars.len() && chars[i] != '>' {
                    i += 1;
                }
                let name: String = chars[start..i.min(chars.len())].iter().collect();
                let plain = name
                    .chars()
                    .next()
                    .is_some_and(|c| c.is_alphabetic() || c == '_')
                    && name.chars().all(|c| c.is_alphanumeric() || c == '_' || c == '-');
                if seen_block && plain {
                    match names.iter_mut().find(|(n, _)| *n == name) {
                        Some(entry) => entry.1 = true,
                        None => names.push((name, true)),
                    }
                }
                i += 1;
            }
            '{' => {
                seen_block = true;
                let mut depth = 1usize;
                i += 1;
                while i < chars.len() && depth > 0 {
                    match chars[i] {
                        '\\' => i += 1,
                        '{' => depth += 1,
                        '}' => depth -= 1,
                        _ => {}
                    }
                    i += 1;
                }
            }
            '$' if chars.get(i + 1).is_some_and(|c| c.is_alphabetic() || *c == '_')
                || (chars.get(i + 1) == Some(&'^')
                    && chars.get(i + 2).is_some_and(|c| c.is_alphabetic() || *c == '_')) =>
            {
                let start = i + 1;
                // A placeholder (`$^a`) keeps its twigil in the name.
                let mut j = if chars[start] == '^' { start + 1 } else { start };
                while j < chars.len() {
                    let ch = chars[j];
                    let kebab = ch == '-'
                        && chars
                            .get(j + 1)
                            .is_some_and(|n| n.is_alphabetic() || *n == '_');
                    // `$Pkg::var`: a package-qualified scalar.
                    let qualified = ch == ':'
                        && chars.get(j + 1) == Some(&':')
                        && chars
                            .get(j + 2)
                            .is_some_and(|n| n.is_alphabetic() || *n == '_');
                    if qualified {
                        j += 2;
                    } else if ch.is_alphanumeric() || ch == '_' || kebab {
                        j += 1;
                    } else {
                        break;
                    }
                }
                let name: String = chars[start..j].iter().collect();
                // The atom of a variable alias (`$x=$re`) is dropped and an
                // in-angle `<a=$re>` is a call of its own, so neither is read
                // as an interpolation. The atom of a sigil alias (`$<a>=$re`,
                // `$0=$re`) is: the parser lowers it to a call of the value
                // (`regex_alias_var_call`).
                let mut b = i;
                while b > 0 && chars[b - 1].is_whitespace() {
                    b -= 1;
                }
                if b > 0 && chars[b - 1] == '=' && !matches!(chars.get(b.wrapping_sub(2)), Some('=' | '!' | '<')) {
                    let mut a = b - 1;
                    while a > 0 && chars[a - 1].is_whitespace() {
                        a -= 1;
                    }
                    let sigil_alias = a > 0
                        && (chars[a - 1] == '>' || {
                            let mut d = a;
                            while d > 0 && chars[d - 1].is_ascii_digit() {
                                d -= 1;
                            }
                            d < a && d > 0 && chars[d - 1] == '$'
                        });
                    if !sigil_alias {
                        i = j;
                        continue;
                    }
                }
                match names.iter_mut().find(|(n, _)| *n == name) {
                    // Written after a block anywhere: late.
                    Some(entry) => entry.1 |= seen_block,
                    None => names.push((name, seen_block)),
                }
                i = j;
            }
            _ => i += 1,
        }
    }
    (names, seen_block)
}

impl super::super::Interpreter {
    /// Whether a `$name` should be read at match time: the variable is bound
    /// to anything but a hash. `VarInterp` matches a plain value or a callable
    /// as its string, a list as its string, a junction as an alternation, a
    /// regex as a call frame and a type object as nothing, exactly as the
    /// splice did (#14510). An unbound name keeps the pre-pass, which resolves
    /// it through the package chain.
    // Cost: O(1).
    pub(crate) fn late_scalar_reads_at_match_time(&self, name: &str, allow_regex: bool) -> bool {
        use crate::value::ValueView;
        let value = super::regex_helpers::interp_closure_scope_get(name)
            .or_else(|| self.env.get(name).cloned())
            .or_else(|| self.env.get(&format!("${name}")).cloned())
            // A package-qualified scalar (`$Pkg::v`) lives in its package.
            .or_else(|| {
                crate::qualified::is_qualified_str(name)
                    .then(|| self.regex_package_chain_var_fallback(&format!("${name}")))
                    .flatten()
            })
            .map(crate::value::Value::into_deref);
        value.is_some_and(|v| match v.view() {
            // A hash is rejected by the interpolation pre-pass
            // (`X::Syntax::Reserved`), at parse time as rakudo does.
            ValueView::Hash(_) => false,
            ValueView::Regex(_) | ValueView::RegexWithAdverbs(_) => allow_regex,
            _ => true,
        })
    }
}

impl super::super::Interpreter {
    /// `pattern` with every plain scalar it names declared as a match-time
    /// lexical (`regex_rule_params::add_runtime_names`), together with
    /// `names`, so the structural parser lowers each to a `VarInterp` read
    /// where the cursor reaches it instead of the interpolation pre-pass
    /// splicing its current value (ADR-12917, direction 2). A `constant` is
    /// inlined at compile time, so it stays spliced, as does a value
    /// `VarInterp` does not match the way the splice does (a callable, an
    /// unbound name).
    // Cost: O(n + k), n = the pattern's length, k = the scalars it names (one
    // env lookup each).
    pub(crate) fn with_match_time_scalars<'a>(
        &self,
        pattern: &'a str,
        mut names: Vec<String>,
    ) -> std::borrow::Cow<'a, str> {
        let (pattern_scalars, _) = scalars_in_pattern(pattern);
        for name in pattern_scalars {
            if self.late_scalar_reads_at_match_time(&name, true)
                && !self.is_compile_time_constant_scalar(&name)
                && !names.contains(&name)
            {
                names.push(name);
            }
        }
        if names.is_empty() {
            std::borrow::Cow::Borrowed(pattern)
        } else {
            std::borrow::Cow::Owned(super::regex_rule_params::add_runtime_names(&names, pattern))
        }
    }

    /// [`Self::parse_regex`] for a match entry point: the pattern's plain
    /// scalars are read at match time ([`Self::with_match_time_scalars`]).
    // Cost: `with_match_time_scalars`, plus the (cached) parse.
    pub(crate) fn parse_regex_for_match(
        &self,
        pattern: &str,
    ) -> Option<std::sync::Arc<crate::runtime::regex_types::RegexPattern>> {
        self.parse_regex(&self.with_match_time_scalars(pattern, Vec::new()))
    }

    /// Whether an interpolated `$name` may hold text the case-folding match
    /// has to expand. A Regex object never does: it keeps its own adverbs, so
    /// the enclosing literal's `:i` does not fold it, and folding the subject
    /// for it would break its exact-case literals (`m:i{^$re}` with
    /// `$re = rx{A}` matches "A").
    // Cost: O(1).
    pub(crate) fn interp_var_may_fold(&self, name: &str) -> bool {
        use crate::value::ValueView;
        let value = self
            .env
            .get(name)
            .or_else(|| self.env.get(&format!("${name}")))
            .cloned()
            .map(crate::value::Value::into_deref);
        !value.is_some_and(|v| match v.view() {
            ValueView::Regex(_) => true,
            ValueView::RegexWithAdverbs(a) => !a.ignore_case,
            _ => false,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::scalars_in_pattern;

    #[test]
    fn every_scalar_of_a_pattern_is_read_at_match_time() {
        assert_eq!(scalars_in_pattern("$a { 1 } $b").0, ["a", "b"]);
        assert_eq!(scalars_in_pattern("$a $b").0, ["a", "b"]);
        // An alias reads its variable; its atom is dropped.
        assert_eq!(scalars_in_pattern("$a=$b").0, ["a"]);
        assert_eq!(scalars_in_pattern("$^a $P::v").0, ["^a", "P::v"]);
        assert_eq!(scalars_in_pattern("<<$n>>").0, ["n"]);
        assert!(scalars_in_pattern("<a=$b>").0.is_empty());
        assert_eq!(scalars_in_pattern("$<a>=$b").0, ["b"]);
        assert_eq!(scalars_in_pattern("$0 = $b").0, ["b"]);
        assert!(scalars_in_pattern("<$b> 'x'").0.is_empty());
        assert_eq!(scalars_in_pattern("x { 1 } <$b>").0, ["b"]);
        assert!(scalars_in_pattern("{ $b } '$q' \\$e <[$f]>").0.is_empty());
        assert_eq!(scalars_in_pattern("$a { 1 } $a $c").0, ["a", "c"]);
    }
}
