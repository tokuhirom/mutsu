//! One pair of operands through a built-in infix operator called as a routine
//! or a code value (`&[cmp]`, `&infix:<+>`): the user-override probes, then the
//! operator's own body. Split out of `call_infix_routine_in` so the common
//! two-operand call of a plain operator reaches it without the prologue that
//! only list-associative, meta and set operators need (#15847).
use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;
use std::sync::OnceLock;

/// The lookup keys of one plain binary operator, interned once for the process
/// (interned ids are global and append-only, so a `OnceLock` is sound across
/// threads).
pub(super) struct PlainInfix {
    /// `infix:<op>`.
    name: &'static str,
    /// `&infix:<op>`, the env key a lexical or imported callable binds under.
    amp_name: &'static str,
    syms: OnceLock<(Symbol, Symbol)>,
}

impl PlainInfix {
    // Cost: O(1) after the first call.
    fn syms(&self) -> (Symbol, Symbol) {
        *self
            .syms
            .get_or_init(|| (Symbol::intern(self.name), Symbol::intern(self.amp_name)))
    }
}

macro_rules! plain_infix_table {
    ($($op:literal),* $(,)?) => {
        /// The plain binary operator `op`, or `None` for every operator whose
        /// call needs `call_infix_routine_in`'s prologue (hyper and assignment
        /// metaops, set operators, `x`/`xx`, the sequence and short-circuit
        /// operators, anything not listed).
        // Cost: O(1), a string match.
        pub(super) fn plain_infix(op: &str) -> Option<&'static PlainInfix> {
            match op {
                $($op => {
                    static ENTRY: PlainInfix = PlainInfix {
                        name: concat!("infix:<", $op, ">"),
                        amp_name: concat!("&infix:<", $op, ">"),
                        syms: OnceLock::new(),
                    };
                    Some(&ENTRY)
                })*
                _ => None,
            }
        }
    };
}

plain_infix_table!(
    "cmp", "<=>", "==", "!=", "<", ">", "<=", ">=", "eq", "ne", "lt", "gt", "le", "ge", "leg",
    "+", "-", "*", "/", "%", "**", "~", "===", "before", "after", "min", "max",
);

impl Interpreter {
    /// A plain two-operand builtin operator called through its routine value
    /// (`my $c = &[cmp]; $c(1, 2)`): the result, or `None` when the call has to
    /// take the full ladder -- another arity or operator, a Junction operand
    /// (autothreaded there), a non-CORE package, or any user routine or `proto`
    /// carrying the name.
    // Cost: O(1) probes plus the operator body.
    pub(crate) fn try_plain_infix_routine_value(
        &mut self,
        package: Symbol,
        name: Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if args.len() != 2 || !crate::qualified::is_global_package(package) {
            return None;
        }
        let name_str = name.as_str();
        let op = name_str.strip_prefix("infix:<")?.strip_suffix('>')?;
        plain_infix(op)?;
        if args
            .iter()
            .any(|v| matches!(v.view(), ValueView::Junction { .. }))
            || self.fn_base_name_registered_sym(name_str, name)
            || self.has_proto_cached_sym(name_str, name)
        {
            return None;
        }
        Some(self.call_infix_routine(op, args))
    }

    /// `acc op rhs` for the built-in operator `op`: a user candidate, a
    /// routine or a lexical `&infix:<op>` callable in scope wins, else the
    /// operator's own body. `plain` carries the pre-built keys of a plain
    /// operator; `None` builds them here.
    // Cost: O(1) probes when nothing overrides `op`, plus the operator body.
    pub(super) fn infix_pair(
        &mut self,
        op: &str,
        plain: Option<&'static PlainInfix>,
        acc: Value,
        rhs: &Value,
        core_only: bool,
    ) -> Result<Value, RuntimeError> {
        let mut acc = acc;
        let built;
        let infix_name: &str = match plain {
            Some(p) => p.name,
            None => {
                // Built by hand: `format!` costs ~0.5k Ir per call.
                let mut name = String::with_capacity(op.len() + 8);
                name.push_str("infix:<");
                name.push_str(op);
                name.push('>');
                built = name;
                &built
            }
        };
        'pair: {
            if !core_only {
                if let Some(value) = self.try_user_infix(infix_name, &acc, rhs)? {
                    acc = value;
                    break 'pair;
                }
                let (name_sym, amp_sym) = match plain {
                    Some(p) => p.syms(),
                    None => (
                        Symbol::intern(infix_name),
                        Symbol::intern(&format!("&{}", infix_name)),
                    ),
                };
                // The base-name negative gate (#5574): with no registry key of
                // this name and no `proto`, no user routine can answer, which is
                // what `resolve_function_with_types` would start by concluding.
                if self.fn_base_name_registered_sym(infix_name, name_sym)
                    || self.has_proto_cached_sym(infix_name, name_sym)
                {
                    let pair_args = vec![acc.clone(), rhs.clone()];
                    if let Some(def) = self.resolve_function_with_types(infix_name, &pair_args)? {
                        crate::trace::trace_log!(
                            "call",
                            "call_infix_routine dispatch def: {}",
                            infix_name
                        );
                        acc = self.call_routine_def(&def, pair_args)?;
                        break 'pair;
                    }
                }
                if let Some(callable) = self.env.get_sym(amp_sym).cloned()
                    && !self.declined_captured_family(infix_name, &callable)
                    && matches!(
                        callable.view(),
                        ValueView::Sub(_)
                            | ValueView::WeakSub(_)
                            | ValueView::Instance { .. }
                            | ValueView::Mixin(..)
                    )
                {
                    crate::trace::trace_log!(
                        "call",
                        "call_infix_routine dispatch callable env: {}",
                        infix_name
                    );
                    acc = self.eval_call_on_value(callable, vec![acc, rhs.clone()])?;
                    break 'pair;
                }
            }
            crate::trace::trace_log!(
                "call",
                "call_infix_routine fallback reduce/eval: {}",
                infix_name
            );
            // Smart match needs the full interpreter for regex dispatch
            if op == "~~" {
                acc = Value::truth(self.smart_match(&acc, rhs));
                break 'pair;
            }
            if op == "!~~" {
                acc = Value::truth(!self.smart_match(&acc, rhs));
                break 'pair;
            }
            // `eqv` likewise needs the full interpreter, not the pure
            // `apply_reduction_op` fold: the lazy-iterable rules, `Proxy`
            // element FETCH, and the Seq reify/consume protocol (the source of
            // `X::Seq::Consumed`) all live in `eqv_values`, and the routine form
            // `&infix:<eqv>($a, $b)` must behave exactly like the `a eqv b`
            // operator. The real `Test.rakumod`'s `cmp-ok` reaches `eqv` only
            // through this routine form.
            if op == "eqv" {
                acc = if core_only {
                    self.core_eqv_values(acc, rhs.clone())?
                } else {
                    self.eqv_values(acc, rhs.clone())?
                };
                break 'pair;
            }
            // The numeric-comparison family likewise needs the full
            // interpreter, not the pure `apply_reduction_op` fold plus this
            // function's own `coerce_infix_operand_numeric` bridge: that
            // bridge is a *reimplementation* of the operator's coercion rules
            // (Inf/-Inf-valued Rat/FatRat, exact BigInt equality, SetHash/Set
            // structural comparison, a user subclass of Int, ...) and drifts
            // from the real operator body in `vm_comparison_ops.rs` /
            // `vm_comparison_order_ops.rs`. `&infix:<==>($a, $b)` — exactly
            // what the real `Test.rakumod`'s `cmp-ok` reaches through — must
            // behave identically to `$a == $b`.
            if let Some(result) = match op {
                "==" => Some(self.num_eq_values(acc.clone(), rhs.clone())?),
                "!=" => Some(self.num_ne_values(acc.clone(), rhs.clone())?),
                "<" => Some(self.num_lt_values(acc.clone(), rhs.clone())?),
                ">" => Some(self.num_gt_values(acc.clone(), rhs.clone())?),
                "<=" => Some(self.num_le_values(acc.clone(), rhs.clone())?),
                ">=" => Some(self.num_ge_values(acc.clone(), rhs.clone())?),
                "<=>" => Some(self.spaceship_values(acc.clone(), rhs.clone())?),
                _ => None,
            } {
                acc = result;
                break 'pair;
            }
            // The string comparisons, `leg`, `before`/`after`, `===`/`!==` and
            // `min`/`max` have one body each, shared with their opcode and the
            // metaop leaf (`vm_operator_values.rs`, #9447).
            if Self::is_comparison_family_op(op)
                && let Some(result) = self.comparison_family_values(op, &acc, rhs, core_only)
            {
                acc = result?;
                break 'pair;
            }
            // `cmp` has structural Range/list/Inf handling in the operator
            // implementation. Once user-defined candidates decline, the
            // routine form must use that same native candidate instead of
            // reducing through the generic numeric bridge. This is the path
            // used by `cmp-ok` and by an explicitly called `&infix:<cmp>`.
            if op == "cmp" {
                let lhs = acc.unwrap_varref().deref_container().deitemize_element();
                let rhs = rhs.unwrap_varref().deref_container().deitemize_element();
                acc = self.cmp_value(lhs, rhs)?;
                break 'pair;
            }
            let mut lhs = acc.clone();
            let mut rhs = rhs.clone();
            // `cmp` is value-oriented, but an argument that came from a
            // scalar variable or positional access can still carry the
            // transient VarRef/ContainerRef wrappers here. Strip those
            // before the numeric bridge so both Instant operands take the
            // same path; otherwise only the variable side is Numeric'ed and
            // the array-element side falls through to representation text.
            if op == "cmp" {
                lhs = lhs.unwrap_varref().deref_container().deitemize_element();
                rhs = rhs.unwrap_varref().deref_container().deitemize_element();
            }
            // The string operators take the same operand coercion as their
            // opcode (a user `Str`/`Stringy`, a `utf8` decode, a `Proxy`
            // FETCH, and the Blob rules), so `&infix:<eq>($obj, "b")` and
            // `.sort(&infix:<leg>)` agree with `$obj eq "b"` / `$obj leg "a"`.
            // The pure table below only knows `.gist` for an object.
            if matches!(op, "eq" | "ne" | "lt" | "gt" | "le" | "ge" | "leg") {
                (lhs, rhs) = self.coerce_str_compare_operands(lhs, rhs)?;
            } else if matches!(op, "~" | "~|" | "~^" | "~&") {
                lhs = self.coerce_stringy_operand(lhs)?;
                rhs = self.coerce_stringy_operand(rhs)?;
            }
            if self.infix_uses_numeric_bridge(op) {
                // The arithmetic ops evaluate to a lazy X::Str::Numeric Failure
                // on a non-numeric string, like their opcodes
                // (`coerce_numeric_bridge_pair_strict`); the generic comparators
                // (cmp/before/after/min/max) compare them as strings.
                if Self::infix_is_strictly_numeric(op)
                    && let Some(failure) = crate::runtime::utils::str_numeric_operand_failure(&lhs)
                        .or_else(|| crate::runtime::utils::str_numeric_operand_failure(&rhs))
                {
                    acc = failure;
                    break 'pair;
                }
                lhs = self.coerce_infix_operand_numeric(lhs)?;
                rhs = self.coerce_infix_operand_numeric(rhs)?;
            }
            if let Ok(value) = Self::apply_reduction_op(op, &lhs, &rhs) {
                acc = value;
            } else {
                let (expr_op, expr_left, expr_right) =
                    if let Some(inner) = op.strip_prefix('R').filter(|inner| !inner.is_empty()) {
                        // Runtime autogen for callable reverse meta-ops (&infix:<Rop>):
                        // evaluate the inner operator with swapped operands.
                        (inner, rhs.clone(), acc)
                    } else {
                        (op, acc, rhs.clone())
                    };
                let expr_args = vec![expr_left.clone(), expr_right.clone()];
                let expr_infix_name = format!("infix:<{}>", expr_op);
                if core_only {
                    return Err(RuntimeError::new(format!(
                        "No core candidate of {expr_infix_name} accepts these arguments"
                    )));
                }
                if let Some(def) = self.resolve_function_with_types(&expr_infix_name, &expr_args)? {
                    crate::trace::trace_log!(
                        "call",
                        "call_infix_routine fallback dispatch def: {}",
                        expr_infix_name
                    );
                    acc = self.call_routine_def(&def, expr_args)?;
                    break 'pair;
                }
                if let Some(callable) = self.env.get(&format!("&{}", expr_infix_name)).cloned()
                    && !self.declined_captured_family(&expr_infix_name, &callable)
                    && matches!(
                        callable.view(),
                        ValueView::Sub(_)
                            | ValueView::WeakSub(_)
                            | ValueView::Instance { .. }
                            | ValueView::Mixin(..)
                    )
                {
                    crate::trace::trace_log!(
                        "call",
                        "call_infix_routine fallback dispatch callable env: {}",
                        expr_infix_name
                    );
                    acc = self.eval_call_on_value(callable, expr_args)?;
                    break 'pair;
                }
                acc = self.eval_block_value(&[Stmt::Expr(Self::build_infix_expr(
                    expr_op, expr_left, expr_right,
                ))])?;
            }
        }
        Ok(acc)
    }
}
