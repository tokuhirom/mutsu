//! One body per comparison-family infix operator, shared by every form.
//!
//! `a OP b` compiles to a dedicated opcode, while `[OP]`, `»OP«`, `ZOP`/`XOP`
//! reach the operator through `eval_infix_leaf`, and `&infix:<OP>(a, b)`
//! through `call_infix_routine`. Those two used to fall back to the pure
//! `apply_reduction_op` table, which carried its own copy of each operator and
//! drifted from the opcode: `[before] 10, 9` compared the numbers as strings
//! (True, where `10 before 9` is False), and `[eq] Any, ""` stringified the
//! type object differently from `Any eq ""` (#9447).
//!
//! Each function here is the operator. The opcode handler pops its operands
//! and calls it; the metaop leaf and the routine form call it directly. This is
//! the same arrangement `num_eq_values` and friends already have for the
//! numeric comparisons (AGENTS.md: a primitive has exactly one
//! implementation).

use super::*;

/// The six string comparisons.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum StrCmp {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

impl StrCmp {
    const ALL: [StrCmp; 6] = [
        StrCmp::Eq,
        StrCmp::Ne,
        StrCmp::Lt,
        StrCmp::Gt,
        StrCmp::Le,
        StrCmp::Ge,
    ];

    /// The comparison an operator spelling names, if it is one of the six.
    pub(crate) fn from_op(op: &str) -> Option<Self> {
        Some(match op {
            "eq" => StrCmp::Eq,
            "ne" => StrCmp::Ne,
            "lt" => StrCmp::Lt,
            "gt" => StrCmp::Gt,
            "le" => StrCmp::Le,
            "ge" => StrCmp::Ge,
            _ => return None,
        })
    }

    fn holds(self, ord: std::cmp::Ordering) -> bool {
        use std::cmp::Ordering::*;
        match self {
            StrCmp::Eq => ord == Equal,
            StrCmp::Ne => ord != Equal,
            StrCmp::Lt => ord == Less,
            StrCmp::Gt => ord == Greater,
            StrCmp::Le => ord != Greater,
            StrCmp::Ge => ord != Less,
        }
    }
}

/// One junction-free string comparison; `K` indexes [`StrCmp::ALL`]. `ne` is
/// never threaded itself (it negates the threaded `eq`), so `K` is never 1.
fn str_cmp_leaf<const K: usize>(
    _: &mut Interpreter,
    l: Value,
    r: Value,
) -> Result<Value, RuntimeError> {
    let kind = StrCmp::ALL[K];
    if kind == StrCmp::Eq {
        if Interpreter::is_buf_value(&l) && Interpreter::is_buf_value(&r) {
            return Ok(Value::truth(
                Interpreter::extract_buf_bytes(&l) == Interpreter::extract_buf_bytes(&r),
            ));
        }
        return Ok(Value::truth(crate::builtins::str_prim::str_eq(
            &l.str_context_cow(),
            &r.str_context_cow(),
        )));
    }
    let ord = match Interpreter::blob_ordering(&l, &r)? {
        Some(ord) => ord,
        None => crate::builtins::str_prim::str_order(&l.str_context_cow(), &r.str_context_cow()),
    };
    Ok(Value::truth(kind.holds(ord)))
}

impl Interpreter {
    /// `eq ne lt gt le ge`: both operands coerced as `.Stringy` (a user
    /// `Str`, a `utf8` decode, a `Proxy` FETCH), junctions autothreaded, and
    /// two Blobs compared bytewise. `ne` is `!eq`: it autothreads `eq` and
    /// negates the collapsed result, so it always answers a plain Bool.
    // Cost: O(p), p = common prefix; `eq`/`ne` are O(1) when the lengths differ
    // (a plain Str operand is borrowed by `str_context_cow`, any other is
    // stringified first).
    pub(crate) fn str_cmp_values(
        &mut self,
        kind: StrCmp,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        let (left, right) = self.coerce_str_compare_operands(left, right)?;
        // `eval_binary_with_junctions` takes a plain `fn`, so the comparison is
        // picked here rather than captured.
        let leaf: fn(&mut Interpreter, Value, Value) -> Result<Value, RuntimeError> = match kind {
            StrCmp::Eq | StrCmp::Ne => str_cmp_leaf::<0>,
            StrCmp::Lt => str_cmp_leaf::<2>,
            StrCmp::Gt => str_cmp_leaf::<3>,
            StrCmp::Le => str_cmp_leaf::<4>,
            StrCmp::Ge => str_cmp_leaf::<5>,
        };
        let result = self.eval_binary_with_junctions(left, right, leaf)?;
        Ok(if kind == StrCmp::Ne {
            Value::truth(!result.truthy())
        } else {
            result
        })
    }

    /// `before` / `after`: the generic `cmp` order -- numbers numerically,
    /// Blobs bytewise, everything else by the same rules as `cmp`.
    // Cost: O(1) for scalars; O(p) for strings, p = common prefix.
    pub(crate) fn before_after_values(
        &mut self,
        is_before: bool,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        let blob_ord = Self::blob_ordering(&left, &right)?;
        let (left, right) = self
            .coerce_numeric_bridge_pair(left.clone(), right.clone())
            .unwrap_or((left, right));
        let ord = blob_ord.unwrap_or_else(|| Self::spaceship_ordering(&left, &right));
        Ok(Value::truth(if is_before {
            ord == std::cmp::Ordering::Less
        } else {
            ord == std::cmp::Ordering::Greater
        }))
    }

    /// `===` (and `!==` when `negate`): `$a.WHICH eq $b.WHICH`, so a class
    /// that overrides `WHICH` decides its own identity. `!==` autothreads
    /// `===` and negates the collapsed result.
    // Cost: O(t1 + t2), t = elements of a list-shaped operand counted
    // recursively to depth 16 (`warm_which_identity` visits each one looking for
    // a user `WHICH`), O(1) for scalars. Rakudo: O(1) -- see #9172.
    pub(crate) fn identical_values(
        &mut self,
        negate: bool,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        self.warm_which_identity(&left);
        self.warm_which_identity(&right);
        let result = self.eval_binary_with_junctions(left, right, |_, l, r| {
            Ok(Value::truth(runtime::values_identical(&l, &r)))
        })?;
        Ok(if negate {
            Value::truth(!result.truthy())
        } else {
            result
        })
    }

    /// `min` / `max`: a user `infix:<min>`/`infix:<max>` candidate first
    /// (ADR-0071); an `Any` type object yields the other operand; a Failure
    /// operand is passed through; otherwise the `cmp` order decides, the left
    /// operand winning a tie.
    // Cost: O(1) for scalars; O(p) for strings, p = common prefix.
    pub(crate) fn min_max_values(
        &mut self,
        is_min: bool,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        let name = if is_min { "infix:<min>" } else { "infix:<max>" };
        if let Some(result) = self.try_user_infix(name, &left, &right)? {
            return Ok(result);
        }
        if matches!(left.view(), ValueView::Package(name) if name == "Any") {
            return Ok(right);
        }
        if matches!(right.view(), ValueView::Package(name) if name == "Any") {
            return Ok(left);
        }
        let is_failure = |v: &Value| matches!(v.view(), ValueView::Instance { class_name, .. } if class_name == "Failure");
        if is_failure(&left) {
            return Ok(left);
        }
        if is_failure(&right) {
            return Ok(right);
        }
        let ord = cmp_values(&left, &right);
        let keep_left = if is_min { ord.is_le() } else { ord.is_ge() };
        Ok(if keep_left { left } else { right })
    }

    /// Whether `op` is one of the operators [`Interpreter::comparison_family_values`]
    /// owns.
    pub(crate) fn is_comparison_family_op(op: &str) -> bool {
        StrCmp::from_op(op).is_some()
            || matches!(
                op,
                "leg" | "before" | "after" | "===" | "!==" | "min" | "max"
            )
    }

    /// The shared body for a comparison-family operator spelled `op`, or
    /// `None` when `op` is not one of them. The metaop leaf and the routine
    /// form both come here, so every spelling of these operators runs the
    /// function its opcode runs.
    pub(crate) fn comparison_family_values(
        &mut self,
        op: &str,
        left: &Value,
        right: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
        // A metaop hands over list elements ref-preserving (`($a,) Zeq ...`
        // wraps `$a` so `=:=` can see the container); these are all value
        // operators, so read through to the value first, as the opcode's
        // operands already are.
        let decont = |v: &Value| v.unwrap_varref().deref_container();
        let (l, r) = (decont(left), decont(right));
        if let Some(kind) = StrCmp::from_op(op) {
            return Some(self.str_cmp_values(kind, l, r));
        }
        Some(match op {
            "leg" => self.str_leg(l, r),
            "before" => self.before_after_values(true, l, r),
            "after" => self.before_after_values(false, l, r),
            "===" => self.identical_values(false, l, r),
            "!==" => self.identical_values(true, l, r),
            "min" => self.min_max_values(true, l, r),
            "max" => self.min_max_values(false, l, r),
            _ => return None,
        })
    }
}
