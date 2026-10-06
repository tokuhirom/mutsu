//! `<::(EXPR)>`: a `<subrule>` call whose rule name is computed per call
//! (ADR-0135 §8, Slice E). The name is evaluated where the cursor reaches the
//! call, then the call is matched as `<name>` would be: a rule's candidates
//! through the growing-seed loop, a grammar method on the calling frame's
//! cursor, or a builtin.

use crate::runtime::Interpreter;
use crate::runtime::regex_types::{NamedAtom, RegexCaptures};
use crate::symbol::Symbol;
use crate::value::Value;

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
        let Some(value) = self.eval_regex_expr_value(expr, caps) else {
            return Vec::new();
        };
        let called = NamedAtom::from(value.to_string_value());
        let spec = called.spec();
        let (candidates, raw_empty) = self.parsed_subrule_candidates(spec, pkg, &[]);
        if candidates.dispatch_failed() {
            self.park_protoless_dispatch_error(&spec.lookup_name, pkg);
            return Vec::new();
        }
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
}
