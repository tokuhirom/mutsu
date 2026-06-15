//! Native `.subst` over a `Str` invocant with a simple pattern/replacement.
//!
//! `"...".subst(/pat/, "repl")` previously always fell back to the interpreter's
//! `dispatch_subst` (see docs/vm-decoupling.md, lever A; `.subst` is the
//! highest-frequency residual method fallback). The substitution *engine* —
//! regex matching (`regex_find_first_from_with_captures`), `$0`/`$1` capture
//! expansion (`expand_capture_refs`) and Match-object construction — already
//! lives VM-side because the operator form `s/.../.../ ` compiles to
//! `OpCode::Subst` and runs natively in `exec_subst_op`. This routes the common
//! `.subst` *method* call through the same building blocks instead of the
//! interpreter, falling back for anything that needs the interpreter's richer
//! handling (closure replacements, regex-with-adverbs objects, Perl5 regex, the
//! `:nth`/`:x`/`:c`/`:p`/`:ii`/`:mm`/`:ss` adverbs, non-`Str` invocants, …).
//!
//! The output is built to match `dispatch_subst` byte-for-byte (including
//! mutsu's literal-`$0` expansion in string replacements and its `$/` policy:
//! set on a single non-global match, left untouched on a global match, set to
//! `Nil` on no match), so this is a pure decoupling change with no behavioral
//! difference.

use super::*;
use crate::value::Value;
use std::collections::HashMap;

impl VM {
    /// Try to run `target.subst(pattern, replacement, :g?)` natively. Returns
    /// `Some(result)` when handled in the VM, `None` to fall back to the
    /// interpreter unchanged.
    pub(super) fn try_native_subst(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if method != "subst" {
            return None;
        }
        // Only a plain string invocant. Other Cool types stringify in the
        // interpreter; keep the native path conservative.
        let Value::Str(text_arc) = target else {
            return None;
        };
        let text = text_arc.to_string();

        // Split args into positionals and adverbs. Only `:g`/`:global` are
        // understood here; any other adverb (`:nth`/`:x`/`:c`/`:p`/`:ii`/…)
        // means fall back, because their selection semantics live in
        // `dispatch_subst`.
        let mut positional: Vec<&Value> = Vec::new();
        let mut global = false;
        for arg in args {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "g" | "global" => global = value.truthy(),
                    _ => return None,
                }
            } else {
                positional.push(arg);
            }
        }
        // `.subst` takes a pattern and an optional replacement; anything else is
        // unusual enough to defer.
        if positional.is_empty() || positional.len() > 2 {
            return None;
        }
        let pattern = positional[0];
        let replacement_val = positional.get(1).copied();
        // Only literal string (or absent) replacements. A closure replacement
        // needs `eval_subst_replacement`'s block call; numbers/other values are
        // rare enough to defer.
        let replacement_str = match replacement_val {
            None => String::new(),
            Some(Value::Str(s)) => s.to_string(),
            _ => return None,
        };

        match pattern {
            // Plain (non-adverb, non-Perl5) regex literal: `.subst(/pat/, ...)`.
            Value::Regex(pat) => {
                Some(self.native_subst_regex(&text, &pat.to_string(), &replacement_str, global))
            }
            // Literal string pattern: pure string replacement, never touches `$/`.
            Value::Str(pat) => {
                let pat = pat.as_str();
                if pat.is_empty() {
                    // Empty-pattern semantics (match between every char) differ
                    // from `str::replace`; let the interpreter handle it.
                    return None;
                }
                let result = if global {
                    text.replace(pat, &replacement_str)
                } else {
                    text.replacen(pat, &replacement_str, 1)
                };
                Some(Ok(Value::str(result)))
            }
            _ => None,
        }
    }

    /// Native regex `.subst`. Mirrors the non-adverb path of `dispatch_subst`'s
    /// `Value::Regex` branch.
    fn native_subst_regex(
        &mut self,
        text: &str,
        pat: &str,
        replacement_str: &str,
        global: bool,
    ) -> Result<Value, RuntimeError> {
        // Collect non-overlapping matches with their positional captures, using
        // the same iterative walk the operator form uses in `exec_subst_op`.
        let mut matches: Vec<(usize, usize, Vec<String>)> = Vec::new();
        let mut pos = 0usize;
        while let Some((start, end, caps)) = self
            .interpreter
            .regex_find_first_from_with_captures(pat, text, pos)
        {
            pos = if end > start { end } else { start + 1 };
            matches.push((start, end, caps));
        }

        if matches.is_empty() {
            // No match: `$/` becomes Nil and the original string is returned.
            self.env_mut().insert("/".to_string(), Value::Nil);
            return Ok(Value::str(text.to_string()));
        }

        // A non-global `.subst` replaces only the first match and sets `$/` to
        // that match; a global `.subst` replaces every match and leaves `$/`
        // untouched (matching `dispatch_subst`).
        let selected: &[(usize, usize, Vec<String>)] =
            if global { &matches } else { &matches[..1] };

        let chars: Vec<char> = text.chars().collect();
        let mut result = String::with_capacity(text.len());
        let mut last_end = 0usize;
        for (start, end, caps) in selected {
            result.extend(chars[last_end..*start].iter());
            // Expand `$0`/`$1`/… only when the match actually captured groups,
            // matching `eval_subst_replacement` (a literal `$0` with no capture
            // group is kept verbatim).
            if caps.is_empty() {
                result.push_str(replacement_str);
            } else {
                result.push_str(&crate::vm::vm_string_regex_ops::expand_capture_refs(
                    replacement_str,
                    caps,
                ));
            }
            last_end = *end;
        }
        result.extend(chars[last_end..].iter());

        if !global {
            let (start, end, caps) = &matches[0];
            let matched: String = chars[*start..*end].iter().collect();
            let match_obj = Value::make_match_object_full(
                matched,
                *start as i64,
                *end as i64,
                caps,
                &HashMap::new(),
                &HashMap::new(),
                &[],
                &[],
                Some(text),
            );
            self.env_mut().insert("/".to_string(), match_obj);
        }

        Ok(Value::str(result))
    }
}
