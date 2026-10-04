//! Matching [`RegexAtom::CodeInterp`]: a `$( code )` / `@( code )`
//! contextualizer interpolation, evaluated when the atom is matched.
//!
//! The interpolation pre-pass (`Interpreter::interpolate_regex_scalars`) used
//! to evaluate these while it built the pattern text, in a scratch
//! interpreter: the regex parser only has `&self`, so it could not run code
//! on the caller (#10157). The pre-pass now leaves the code in the text, the
//! structural parser lowers it to a `CodeInterp` atom, and the matcher —
//! which holds `&mut self` — runs it here through
//! [`Interpreter::run_regex_sub_eval`]. That is also when Rakudo runs it: the
//! atom compiles to code evaluated at the cursor, so it sees the match state
//! and the regex's installed closure scope.

use super::super::*;

/// The index of the `)` closing the contextualizer paren at `chars[open]`
/// (`$(` / `@(`), or `None` when it is unterminated. Parens nest; a paren
/// inside a quoted string of the code (`$(")")`) is text. The pre-pass and
/// the structural parser both find the code's end here, so they agree on it.
// Cost: O(n), n = the code's length.
pub(in crate::runtime) fn code_interp_close(chars: &[char], open: usize) -> Option<usize> {
    let mut depth = 0usize;
    let mut quote: Option<char> = None;
    let mut i = open;
    while i < chars.len() {
        let c = chars[i];
        match quote {
            Some(q) => {
                if c == '\\' {
                    i += 1;
                } else if c == q {
                    quote = None;
                }
            }
            None => match c {
                '\'' | '"' => quote = Some(c),
                '(' => depth += 1,
                ')' => {
                    depth -= 1;
                    if depth == 0 {
                        return Some(i);
                    }
                }
                _ => {}
            },
        }
        i += 1;
    }
    None
}

/// The atom for a double-quoted regex literal whose text embeds `$( code )`
/// interpolations: `segments` holds each literal run with the code that
/// follows it, `tail` the literal text after the last one. One `Group`, so a
/// quantifier after the closing quote binds to the whole literal.
// Cost: O(s + t), s = the segments' total length, t = the tail's.
pub(in crate::runtime) fn dq_code_interp_atom(
    segments: Vec<(String, String)>,
    tail: String,
    ignore_case: bool,
) -> RegexAtom {
    let token = |atom: RegexAtom, from_runtime_interpolation: bool| RegexToken {
        atom,
        quant: RegexQuant::One,
        named_capture: None,
        hash_capture: None,
        secondary_named_capture: None,
        force_list_capture: false,
        ratchet: false,
        frugal: false,
        separator: None,
        from_runtime_interpolation,
        subrule_call_capture: false,
    };
    let mut tokens = Vec::new();
    let push_literal = |tokens: &mut Vec<RegexToken>, text: String| {
        if !text.is_empty() {
            let atom = crate::runtime::regex_parse::regex_single_quote_atom(text, ignore_case);
            tokens.push(token(atom, false));
        }
    };
    for (literal, code) in segments {
        push_literal(&mut tokens, literal);
        tokens.push(token(
            RegexAtom::CodeInterp {
                code: code.into(),
                list: false,
            },
            true,
        ));
    }
    push_literal(&mut tokens, tail);
    RegexAtom::Group(RegexPattern {
        tokens,
        anchor_start: false,
        anchor_end: false,
        ignore_case,
        ignore_mark: false,
        derived: Default::default(),
    })
}

impl Interpreter {
    /// The env a regex code interpolation (`<{ code }>`, `$( code )`,
    /// `@( code )`) runs over: the match-time env (`make_regex_eval_env`)
    /// with the module file-scope lexicals `code` names seeded in, and `$_`
    /// bound to the match target.
    ///
    /// A module's file-scope lexicals are resolved by the normal compiled
    /// variable reader through the module/unit lexical stores rather than by
    /// a plain env lookup, so a module routine's regex fragments are seeded
    /// explicitly. Match-local `:my`/`:let` values keep precedence. The topic
    /// is set after `make_regex_eval_env`, which installs the `:my`/`:let`
    /// lexicals, so it wins over them — unless the regex captured its
    /// defining scope's `$_` (`install_regex_closure_scope` pinned it, #9610).
    ///
    /// Cost: O(e + v), e = the env size (the copy-on-write clone is O(1), the
    /// capture bindings are O(captures)), v = the variables `code` names.
    pub(super) fn regex_code_interp_env(
        &self,
        code: &str,
        caps: &RegexCaptures,
        target: &str,
    ) -> crate::env::Env {
        let mut env = self.make_regex_eval_env(caps);
        let regex_local_names: std::collections::HashSet<String> =
            caps.regex_vars().keys().cloned().collect();
        for name in crate::opcode::CompiledCode::regex_code_interpolated_var_names(code) {
            if regex_local_names.contains(&name) {
                continue;
            }
            if let Some(value) = self.get_env_with_main_alias(&name) {
                env.insert(name, value);
            }
        }
        if self.topic_state.regex_topic_pinned == 0 {
            env.insert("_".to_string(), Value::str(target.to_string()));
        } else if let Some(topic) = self.env.get("_") {
            env.insert("_".to_string(), topic.clone());
        }
        env
    }

    /// The regex source a [`RegexAtom::CodeInterp`] atom matches as at this
    /// point of the match: `code` run on this interpreter, its result as an
    /// escaped literal (`list: false`) or as an alternation over its elements
    /// (`list: true`), prefixed with `:i` under `ignore_case`. `None` when the
    /// code throws; the error is parked in `PENDING_REGEX_ERROR` for the match
    /// entry point to re-raise.
    ///
    /// Cost: O(n + r) plus the code's own run, n = the subject's length (the
    /// topic string), r = the result's rendered length.
    pub(super) fn regex_code_interp_pattern(
        &mut self,
        code: &str,
        list: bool,
        caps: &RegexCaptures,
        chars: &[char],
        ignore_case: bool,
    ) -> Option<String> {
        let target: String = chars.iter().collect();
        let env = self.regex_code_interp_env(code, caps, &target);
        let (stmts, id) = self.parse_regex_code_cached_with_id(code)?;
        let value = match self.run_regex_sub_eval(env, None, |interp| {
            interp.eval_block_value_cached(&stmts, id)
        }) {
            Ok(v) => v,
            Err(e) => match e.return_value {
                Some(v) => v,
                None => {
                    crate::runtime::regex_parse::PENDING_REGEX_ERROR.with(|slot| {
                        *slot.borrow_mut() = Some(e);
                    });
                    return None;
                }
            },
        };
        let mut pattern = String::from(if ignore_case { ":i " } else { "" });
        if list {
            let alts = Self::regex_alternation_sources(&value);
            Self::push_regex_interpolated_alternation(&mut pattern, &alts);
        } else if let ValueView::Regex(_) | ValueView::RegexWithAdverbs(_) = value.view() {
            // A `Regex` value is matched as a pattern, as `<$re>` does.
            pattern.push_str(&Self::regex_alternation_sources(&value)[0]);
        } else {
            pattern.push_str(&Self::escape_regex_scalar_literal(&value.to_string_value()));
        }
        Some(pattern)
    }

    /// The alternatives an interpolated list contributes to a regex: a
    /// `Regex` element as its own source, anything else as an escaped
    /// literal. A non-list value is a one-element list.
    ///
    /// Cost: O(k + r), k = the elements, r = their rendered length.
    pub(in crate::runtime) fn regex_alternation_sources(value: &Value) -> Vec<String> {
        let render = |elt: &Value| match elt.view() {
            ValueView::Regex(pat) => pat.to_string(),
            ValueView::RegexWithAdverbs(a) => a.pattern.to_string(),
            _ => Self::escape_regex_scalar_literal(&elt.to_string_value()),
        };
        match value.view() {
            ValueView::Array(arr, _) => arr.iter().map(render).collect(),
            ValueView::Seq(items) => items.iter().map(render).collect(),
            ValueView::Slip(items) => items.iter().map(render).collect(),
            _ => vec![render(value)],
        }
    }

    /// The pattern a [`RegexAtom::CodeInterp`] atom matches at `pos`: its code
    /// run once, where the cursor reaches it ([`Self::regex_code_interp_pattern`]),
    /// and the result parsed. Under `MUTSU_RX_DIFF` the run is recorded and
    /// replayed like every other call-out (ADR-0135 D6). `None` when the code
    /// throws (the error is parked for the match entry point) or the result
    /// does not parse.
    ///
    /// Cost: O(n + r) plus the code's run, n = the subject's length, r = the
    /// result's rendered length (the parse is cached by source).
    pub(in crate::runtime) fn regex_code_interp_parsed(
        &mut self,
        code: &str,
        list: bool,
        chars: &[char],
        current_caps: &RegexCaptures,
        ignore_case: bool,
    ) -> Option<std::sync::Arc<RegexPattern>> {
        let source =
            self.regex_code_interp_pattern(code, list, current_caps, chars, ignore_case)?;
        self.parse_regex(&source)
    }

    /// Every end of an interpolated pattern `parsed` at `pos`, lowest priority
    /// first, each with an empty capture delta: the interpolated pattern is a
    /// regex of its own, and rakudo keeps none of its captures
    /// (`"ab" ~~ / @( rx{ (\w) } ) b /` has no `$0`). Every end is
    /// computed up front; the compiled engine runs the pattern as a frame
    /// instead, resumed on demand, and comes here only for one that declines.
    ///
    /// Cost: the pattern's all-ends match at `pos`, plus O(e) for the e ends.
    pub(in crate::runtime) fn regex_code_interp_pattern_ends(
        &mut self,
        parsed: &RegexPattern,
        chars: &[char],
        pos: usize,
        pkg: Symbol,
    ) -> Vec<(usize, RegexCaptures)> {
        let mut out: Vec<(usize, RegexCaptures)> = self
            .regex_match_ends_from_caps_in_pkg(parsed, chars, pos, pkg)
            .into_iter()
            .map(|(end, _)| (end, RegexCaptures::default()))
            .collect();
        out.reverse();
        out
    }

    /// Every end of the pattern a `<{ ... }>` / `<@(...)>` atom interpolates at
    /// `pos`, for the position-only matcher.
    ///
    /// Cost: one run of the code, plus the interpolated pattern's all-ends
    /// match at `pos`.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn regex_code_interp_ends_unrecorded(
        &mut self,
        code: &str,
        list: bool,
        chars: &[char],
        pos: usize,
        current_caps: &RegexCaptures,
        pkg: Symbol,
        ignore_case: bool,
    ) -> Vec<(usize, RegexCaptures)> {
        let Some(source) =
            self.regex_code_interp_pattern(code, list, current_caps, chars, ignore_case)
        else {
            return Vec::new();
        };
        let Some(parsed) = self.parse_regex(&source) else {
            return Vec::new();
        };
        self.regex_code_interp_pattern_ends(&parsed, chars, pos, pkg)
    }
}
