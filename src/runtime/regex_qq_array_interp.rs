//! `@name` inside a double-quoted regex literal (`/"x @a[]"/`). The literal
//! follows qq-string rules: a bare `@a` is literal text, `@a[]` / `@a{}` /
//! `@a<>` (a zen slice) interpolate the space-joined elements. Outside the
//! literal a bare `@a` is an alternation over the elements instead, which is
//! what `interpolate_regex_scalars`' `@` arm does.

use super::*;
use crate::runtime::meta_ns::MetaNs;
use std::cell::Cell;

impl Interpreter {
    /// Handle the `@` at `chars[at]` (inside a double-quoted regex literal),
    /// appending to `out` and returning the position after it, or `None` to
    /// leave it to the caller's ordinary `@name` handling.
    // Cost: O(n + |@name|), n = the name's length.
    pub(super) fn interpolate_qq_array_in_regex(
        &self,
        chars: &[char],
        at: usize,
        out: &mut String,
    ) -> Option<usize> {
        let name_start = at + 1;
        if !chars
            .get(name_start)
            .is_some_and(|c| c.is_alphabetic() || *c == '_')
        {
            return None;
        }
        let mut j = name_start;
        while j < chars.len() && (chars[j].is_alphanumeric() || matches!(chars[j], '_' | '-')) {
            j += 1;
        }
        match (chars.get(j), chars.get(j + 1)) {
            (Some('['), Some(']')) | (Some('{'), Some('}')) | (Some('<'), Some('>')) => {
                let bare_name: String = chars[name_start..j].iter().collect();
                let sigiled_name = format!("@{bare_name}");
                let value = super::regex::regex_helpers::interp_closure_scope_get(&sigiled_name)
                    .or_else(|| self.env.get(&sigiled_name).cloned())
                    .unwrap_or(Value::NIL)
                    .into_descalarized()
                    .into_deref();
                let joined = match value.view() {
                    ValueView::Array(arr, _) => join_str(arr.iter()),
                    ValueView::Seq(items) => join_str(items.iter()),
                    ValueView::Slip(items) => join_str(items.iter()),
                    _ => value.to_string_value(),
                };
                Self::push_value_as_regex_pattern(&Value::str(joined), out);
                Some(j + 2)
            }
            // A subscripted `@a[0]` / `@a{'k'}` or a `"@a.join(',')"` call is
            // lowered to a compiled qq thunk (`splice_regex_qq_thunk_result`)
            // and reaches here only as a `QqInterp` atom's fallback parse,
            // for a pattern that has no thunk (a runtime-built pattern).
            (Some('[' | '{' | '<'), _) => None,
            // A bare `@name` is literal text in a qq string (so is `@a.foo`
            // without a trailing call).
            _ => {
                out.push('@');
                Some(at + 1)
            }
        }
    }
}

impl Interpreter {
    /// Handle the double-quoted atom opening at `chars[at]` when the
    /// compiler lowers such an atom to a qq thunk (`crate::regex_qq_atoms`),
    /// returning the position after the closing quote. When the regex's
    /// installed scope holds the thunk's string result, the result is
    /// appended to `out` as a single-quoted literal; otherwise the atom is
    /// copied verbatim, for the structural parser to lower to a match-time
    /// [`RegexAtom::QqInterp`]. `None` leaves the atom to the text scan: it
    /// is not one a thunk evaluates, or this is that atom's fallback parse.
    // Cost: O(n + |r|), n = the pattern's length (quote-state scan), r = the result.
    pub(super) fn splice_regex_qq_thunk_result(
        &self,
        chars: &[char],
        at: usize,
        out: &mut String,
    ) -> Option<usize> {
        if REGEX_QQ_FALLBACK_PARSE.with(Cell::get) {
            return None;
        }
        let close = crate::regex_qq_atoms::dq_atom_close(chars, at)?;
        let body: String = chars[at + 1..close].iter().collect();
        if !crate::regex_qq_atoms::body_wants_thunk(&body)
            || super::regex_parse::is_inside_regex_quote_literal(chars, at)
        {
            return None;
        }
        let key = MetaNs::RegexQq.key(Symbol::intern(&body));
        // A `<$re>`-interpolated regex being re-parsed under its own scope
        // (`RegexInterpClosureScopeGuard`) holds its thunk there, unevaluated;
        // a result in `env` under the same key belongs to the enclosing match.
        let own_scope_thunk =
            super::regex::regex_helpers::interp_closure_scope_get(&key.resolve()).is_some();
        let result = (!own_scope_thunk)
            .then(|| self.env.get_sym(key))
            .flatten()
            .filter(|r| matches!(r.view(), ValueView::Str(_)));
        let Some(result) = result else {
            out.extend(chars[at..=close].iter());
            return Some(close + 1);
        };
        let ValueView::Str(text) = result.view() else {
            unreachable!("filtered to a Str above");
        };
        out.push('\'');
        for c in text.chars() {
            if matches!(c, '\\' | '\'') {
                out.push('\\');
            }
            out.push(c);
        }
        out.push('\'');
        Some(close + 1)
    }
}

impl Interpreter {
    /// A class- or role-body `token`/`rule` declaration's raw body with its
    /// `"..."` atoms' qq thunks (built by running `qq_thunk_chunks` — see
    /// [`crate::opcode::CompiledTokenDeclPlan::qq_thunk_chunks`] — now, in
    /// the declaring scope) put on the body's regex value, the way a regex
    /// literal carries them. `None` when the declaration has none.
    // Cost: O(t + b) plus the chunks' own runs, t = thunks, b = body statements.
    pub(crate) fn token_body_with_qq_thunks(
        &mut self,
        raw_body: &[Stmt],
        qq_thunk_chunks: &[(Symbol, crate::opcode::CompiledDeclExpr)],
    ) -> Result<Option<Vec<Stmt>>, RuntimeError> {
        if qq_thunk_chunks.is_empty() {
            return Ok(None);
        }
        let mut thunks = crate::value::ValueMap::default();
        for (key, chunk) in qq_thunk_chunks {
            let thunk = self.run_decl_expr(chunk)?;
            thunks.insert(key.resolve().to_string(), thunk);
        }
        let mut body = raw_body.to_vec();
        for stmt in body.iter_mut() {
            if let Stmt::Expr(Expr::Literal(v)) = stmt {
                *v = with_scope_entries(v, &thunks);
            }
        }
        Ok(Some(body))
    }
}

/// `regex` with `entries` added to the scope it closed over.
// Cost: O(s + e), s = the existing scope's size, e = entries.
fn with_scope_entries(regex: &Value, entries: &crate::value::ValueMap) -> Value {
    let merged = |existing: Option<&crate::value::ValueMap>| {
        let mut scope = existing.cloned().unwrap_or_default();
        for (k, v) in entries.iter() {
            scope.insert(k.clone(), v.clone());
        }
        Some(std::sync::Arc::new(scope))
    };
    match regex.view() {
        ValueView::Regex(p) => Value::regex_closure(
            std::sync::Arc::clone(&p),
            merged(regex.regex_closure_scope().as_deref()),
            regex.regex_signature(),
            None,
            regex.regex_captured_topic(),
        ),
        ValueView::RegexWithAdverbs(a) => {
            let mut adv = a.clone();
            adv.captured = merged(a.captured.as_deref());
            Value::regex_with_adverbs(adv)
        }
        _ => regex.clone(),
    }
}

/// The `"..."` atom opening with `opener` (already consumed) at the head of
/// `rest`, when the structural parser lowers it to a match-time
/// [`RegexAtom::QqInterp`]: its body, and how many more chars of `rest` it
/// spans (through the closing quote).
// Cost: O(n), n = the rest of the pattern (collected to scan for the closer).
pub(super) fn regex_qq_interp_body(
    opener: char,
    rest: &std::iter::Peekable<std::str::Chars<'_>>,
) -> Option<(String, usize)> {
    if REGEX_QQ_FALLBACK_PARSE.with(Cell::get) {
        return None;
    }
    let chars: Vec<char> = std::iter::once(opener).chain(rest.clone()).collect();
    let close = crate::regex_qq_atoms::dq_atom_close(&chars, 0)?;
    let body: String = chars[1..close].iter().collect();
    crate::regex_qq_atoms::body_wants_thunk(&body).then_some((body, close))
}

impl Interpreter {
    /// Build the [`RegexAtom::QqInterp`] for the `"..."` atom `body` opened
    /// by `opener` (see [`regex_qq_interp_body`]). Its fallback is the
    /// atom's text-scan reading — what the atom meant before its thunk
    /// existed, still right where none does — parsed now, which reads `env`,
    /// so the enclosing parse is not memoized.
    // Cost: O(b) plus the fallback's parse, b = the body's length.
    pub(super) fn regex_qq_interp_atom(
        &self,
        opener: char,
        body: &str,
        ignore_case: bool,
    ) -> Option<RegexAtom> {
        let closer = if opener == '"' { '"' } else { '\u{201D}' };
        let text = format!(
            "{}{opener}{body}{closer}",
            if ignore_case { ":i " } else { "" }
        );
        crate::runtime::regex_parse::PARSE_CONSULTED_AMBIENT_STATE.with(|f| f.set(true));
        let prev = REGEX_QQ_FALLBACK_PARSE.with(|f| f.replace(true));
        let fallback =
            self.parse_regex_uncached(&text, crate::runtime::regex_parse::RegexParseMode::Match);
        REGEX_QQ_FALLBACK_PARSE.with(|f| f.set(prev));
        Some(RegexAtom::QqInterp {
            key: MetaNs::RegexQq.key(Symbol::intern(body)),
            fallback: Box::new(fallback?),
        })
    }
}

thread_local! {
    /// Set while [`Interpreter::regex_qq_interp_atom`] parses an atom's
    /// fallback, so that parse reads the atom as text instead of lowering it
    /// to a `QqInterp` again.
    static REGEX_QQ_FALLBACK_PARSE: Cell<bool> = const { Cell::new(false) };

    /// The `"..."` qq thunks (`crate::regex_qq_atoms`) whose results are
    /// installed right now, innermost last, each with its result. One match
    /// can install the same regex's scope more than once (the VM's
    /// smartmatch op, then `smart_match` itself); a nested install of a
    /// thunk that is already active reuses its result, so the thunk runs
    /// once per match as in Rakudo.
    static ACTIVE_REGEX_QQ: std::cell::RefCell<Vec<(Value, Value)>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

impl Interpreter {
    /// Evaluate a `"..."` atom's compiled qq thunk for a scope install and
    /// mark it active until the matching [`Self::end_regex_qq_thunk`].
    /// Returns the string result, or `None` (nothing marked) when the thunk
    /// throws — the pre-pass then falls back to its own reading of the atom.
    // Cost: O(a) plus the thunk's own run, a = active thunks (nesting depth).
    pub(crate) fn eval_regex_qq_thunk(&mut self, thunk: &Value) -> Option<Value> {
        let active = ACTIVE_REGEX_QQ.with(|a| {
            a.borrow()
                .iter()
                .rev()
                .find(|(t, _)| t.same_binding(thunk))
                .map(|(_, r)| r.clone())
        });
        let result = match active {
            Some(r) => r,
            None => {
                let r = self.call_sub_value(thunk.clone(), Vec::new(), false).ok()?;
                Value::str(r.to_string_value())
            }
        };
        ACTIVE_REGEX_QQ.with(|a| a.borrow_mut().push((thunk.clone(), result.clone())));
        Some(result)
    }

    /// Undo one [`Self::eval_regex_qq_thunk`] (installs nest strictly, so
    /// the innermost entry is the one being uninstalled).
    // Cost: O(1).
    pub(crate) fn end_regex_qq_thunk() {
        ACTIVE_REGEX_QQ.with(|a| a.borrow_mut().pop());
    }
}

fn join_str<'a>(items: impl Iterator<Item = &'a Value>) -> String {
    items
        .map(|v| v.to_string_value())
        .collect::<Vec<_>>()
        .join(" ")
}
