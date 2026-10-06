//! `<::(EXPR)>`: a `<subrule>` call whose rule name is computed per call
//! (ADR-0135 §8, Slice E). The name is evaluated where the cursor reaches the
//! call, then the call is matched as `<name>` would be: a rule's candidates
//! through the growing-seed loop, a grammar method on the calling frame's
//! cursor, or a builtin.

use crate::ast::{Expr, Stmt};
use crate::runtime::Interpreter;
use crate::runtime::regex_types::{NamedAtom, RegexCaptures};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::{Value, ValueView};

impl Interpreter {
    /// Every end of the symbolic call `name` (`<::(EXPR)>`) at `pos`, LOWEST
    /// PRIORITY FIRST, as an eager call's are. `caps` is what `EXPR` sees;
    /// `cursor` is the calling frame's grammar instance, for a method call.
    /// `options` is `(first_only, ignore_case)`.
    // Cost: one run of `EXPR`, then the resolved call's: the growing-seed
    // loop's for a rule, the method's for a method, O(1) for a builtin.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn rx_symbolic_call_ends(
        &mut self,
        name: &NamedAtom,
        chars: &[char],
        pos: usize,
        caps: &RegexCaptures,
        pkg: Symbol,
        cursor: impl FnOnce(&mut Interpreter) -> Value,
        options: (bool, bool),
    ) -> Vec<(usize, RegexCaptures)> {
        let Some(expr) = name.spec().arg_exprs.first() else {
            return Vec::new();
        };
        let constant = self.symbolic_name_is_constant(expr);
        let Some(value) = self.eval_regex_expr_value(expr, caps) else {
            return Vec::new();
        };
        let resolved = value.to_string_value();
        // A name written out in the regex (`<::("x")>`) is a call of that rule,
        // filed under its name. A computed one (`<::($n)>`) is looked up on the
        // cursor at run time and files nothing, like `<.x>`: rakudo gives
        // `<::($n)>` no capture of its own (an alias still captures it).
        let called = NamedAtom::from(if constant || resolved.starts_with('.') {
            resolved
        } else {
            format!(".{resolved}")
        });
        let spec = called.spec();
        let (candidates, raw_empty) = self.parsed_subrule_candidates(spec, pkg, &[]);
        if !candidates.is_empty() {
            return self.subrule_seed_ends(spec, &candidates, chars, pos, pkg, &[], false, options);
        }
        if raw_empty && self.subrule_names_user_method(spec, pkg) {
            let invocant = cursor(self);
            return self
                .regex_grammar_method_end(&spec.lookup_name, chars, pos, pkg, &[], invocant)
                .map(|end| (end, RegexCaptures::default()))
                .into_iter()
                .collect();
        }
        self.regex_builtin_named(spec, chars, pos, pkg)
            .into_iter()
            .collect()
    }

    /// The rule name a symbolic call (`<::("alpha")>`) spells out, when its
    /// argument is a single plain string literal; `None` for anything that has
    /// to be evaluated to be known. A static answer, for the analyses that run
    /// before a match (which names a quantifier turns into lists).
    // Cost: O(n), n = the length of the argument text.
    pub(in crate::runtime) fn symbolic_call_written_name(
        spec: &crate::runtime::regex::regex_helpers::NamedRegexLookupSpec,
    ) -> Option<String> {
        let arg = spec.arg_exprs.first()?.trim();
        let quote = arg.chars().next().filter(|c| matches!(c, '"' | '\''))?;
        let body = arg.strip_prefix(quote)?.strip_suffix(quote)?;
        let plain = !body.is_empty()
            && !body.contains(['"', '\'', '\\', '$', '@', '%', '&', '{', '}', '\n']);
        plain.then(|| body.to_string())
    }

    /// Whether the name expression of a symbolic call is known where the regex
    /// is written: a string literal, or a `~` of them (rakudo folds those, so
    /// `<::("a" ~ "b")>` names `ab` outright). Anything that has to run to find
    /// the name — a variable, an interpolated string, a method call — is not.
    // Cost: O(1) for a bare `$var` (no parse); otherwise one memoized parse of the
    // argument text, then O(e), e = the nodes of the name expression.
    pub(in crate::runtime) fn symbolic_name_is_constant(&self, expr_src: &str) -> bool {
        let trimmed = expr_src.trim();
        if trimmed.starts_with('$') {
            return false;
        }
        let Some((stmts, _)) = self.parse_regex_code_cached_with_id(&format!("({trimmed});"))
        else {
            return false;
        };
        let mut root = None;
        for stmt in stmts.iter() {
            match stmt {
                Stmt::SetLine(_) => {}
                Stmt::Expr(expr) if root.is_none() => root = Some(expr),
                _ => return false,
            }
        }
        let Some(root) = root else {
            return false;
        };
        let mut pending = vec![root];
        while let Some(expr) = pending.pop() {
            match expr {
                Expr::Grouped(inner) => pending.push(inner),
                Expr::Literal(v) if matches!(v.view(), ValueView::Str(_)) => {}
                Expr::Binary {
                    left,
                    op: TokenKind::Tilde,
                    right,
                } => {
                    pending.push(left);
                    pending.push(right);
                }
                _ => return false,
            }
        }
        true
    }
}
