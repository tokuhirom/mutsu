//! A parameterized subrule call's argument list read without the evaluator
//! when every argument is a bare scalar variable or a literal constant
//! (#12917): `<element($indent, 0)>`, `<value=map($new-indent)>`.
//!
//! `eval_regex_arg_list` otherwise compiles the whole list as a Capture and
//! runs it in a scratch evaluation env, about 28k instructions per call; a
//! YAMLish `load-yaml` of 60 rows makes ~300 such calls, all of this shape.

use super::super::*;
use crate::value::ValueView;

impl Interpreter {
    /// `name` of an argument spelled as a bare scalar variable, `$name`, and
    /// nothing else: `$p-1` (a kebab `-` followed by a digit is NOT part of a
    /// raku identifier) is an expression.
    // Cost: O(n), n = the length of `src`.
    pub(super) fn bare_scalar_var_name(src: &str) -> Option<&str> {
        let name = src.strip_prefix('$')?;
        let mut chars = name.chars().peekable();
        let first = chars.next()?;
        if !(first.is_alphabetic() || first == '_') {
            return None;
        }
        while let Some(c) = chars.next() {
            let kebab = c == '-' && chars.peek().is_some_and(|n| n.is_alphabetic() || *n == '_');
            if !(c.is_alphanumeric() || c == '_' || kebab) {
                return None;
            }
        }
        Some(name)
    }

    /// The values of an argument list whose every argument is a bare scalar
    /// variable or a literal constant (`<element($indent, 0)>`, YAMLish's
    /// whole shape), read without the evaluator; `None` for any other list,
    /// which `eval_regex_arg_list` compiles as a Capture. Every argument is
    /// positional, so the Capture would hold exactly these values in order.
    /// A variable read is the one `eval_regex_expr_value` makes for a lone
    /// `$name` (the in-regex lexicals first, then the caller's env); only a
    /// value of a plain scalar kind is taken as is, since a Capture itemizes
    /// a container where a bare read would not.
    // Cost: O(a * n), a = arguments, n = the length of each (a cached parse
    // per literal).
    pub(super) fn eval_regex_arg_list_simple(
        &mut self,
        exprs: &[String],
        caps: &RegexCaptures,
    ) -> Option<Vec<Value>> {
        let mut out = Vec::with_capacity(exprs.len());
        for expr in exprs {
            let trimmed = expr.trim();
            let value = if let Some(name) = Self::bare_scalar_var_name(trimmed) {
                let lookup = |k: &str| caps.regex_vars().get(k).or_else(|| self.env.get(k)).cloned();
                let value = lookup(name).or_else(|| lookup(trimmed))?;
                if !Self::is_plain_scalar_value(&value) {
                    return None;
                }
                super::regex_arg_purity::note_named_read(name);
                value
            } else {
                if trimmed.is_empty() || trimmed.starts_with(':') {
                    return None;
                }
                let (stmts, _) = self.parse_regex_code_cached_with_id(&format!("({trimmed});"))?;
                Self::constant_stmt_value(&stmts)?
            };
            out.push(value);
        }
        Some(out)
    }

    /// Is `v` of a scalar kind a literal token can produce (the kinds
    /// `constant_stmt_value` admits)?
    // Cost: O(1).
    pub(super) fn is_plain_scalar_value(v: &Value) -> bool {
        matches!(
            v.view(),
            ValueView::Int(_)
                | ValueView::BigInt(_)
                | ValueView::Num(_)
                | ValueView::Str(_)
                | ValueView::Bool(_)
                | ValueView::Rat(..)
                | ValueView::FatRat(..)
                | ValueView::BigRat(..)
                | ValueView::Complex(..)
                | ValueView::Nil
        )
    }
}
