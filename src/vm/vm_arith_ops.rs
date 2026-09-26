use super::*;
use crate::token_kind::MetaAssignIdentity;
use num_traits::ToPrimitive;

/// Whether the assignment metaop must seed this LHS with the operator's
/// zero-argument value. Mirrors rakudo's `nqp::isconcrete` test: type objects
/// (and `Nil`, which resets a container to its type object) are seeded; every
/// concrete value — including a `Failure` — is left alone.
fn is_meta_assign_undefined(v: &Value) -> bool {
    match v.view() {
        ValueView::Package(_) | ValueView::Nil => true,
        ValueView::ContainerRef(cell) => is_meta_assign_undefined(&cell.lock().unwrap()),
        _ => false,
    }
}

/// Replace an undefined `$x` with the zero-argument value of the operator in
/// `$x OP= $y`, or throw for the operators that have none. `identity` is `None`
/// wherever METAOP_ASSIGN semantics do not apply (a literal `$x = $x OP $y`, or
/// an operator with no identity of its own such as `~`), in which case the
/// value passes through untouched.
pub(super) fn seed_meta_assign_identity(
    value: Value,
    identity: Option<MetaAssignIdentity>,
) -> Result<Value, RuntimeError> {
    let Some(identity) = identity else {
        return Ok(value);
    };
    if !is_meta_assign_undefined(&value) {
        return Ok(value);
    }
    match identity {
        MetaAssignIdentity::Zero => Ok(Value::int(0)),
        MetaAssignIdentity::One => Ok(Value::int(1)),
        MetaAssignIdentity::EmptyStr => Ok(Value::str(String::new())),
        MetaAssignIdentity::NoZeroArgDiv => Err(RuntimeError::no_zero_arg_meaning("infix:</>")),
        MetaAssignIdentity::NoZeroArgMod => Err(RuntimeError::no_zero_arg_meaning("infix:<%>")),
        MetaAssignIdentity::EmptySet => Ok(Value::set(Default::default())),
        MetaAssignIdentity::EmptyBag => Ok(Value::bag(Default::default())),
    }
}

impl Interpreter {
    /// Execute a compiler-proven native integer `+`, `-`, or `*`. Native
    /// declarations use machine-width registers for arithmetic: signed values
    /// wrap as `i64`, unsigned values as `u64`; `int8`/`uint8` and the other
    /// narrow declarations are narrowed later by their destination store.
    /// The arithmetic itself is `runtime::nqp_native`'s, shared with
    /// `nqp::add_i` and friends, TRIR and the JIT (ADR-0118).
    pub(super) fn exec_native_int_arithmetic_op(
        &mut self,
        op: crate::opcode::CompoundBaseOp,
        unsigned: bool,
    ) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let op_name = match op {
            crate::opcode::CompoundBaseOp::Add => "infix:<+>",
            crate::opcode::CompoundBaseOp::Sub => "infix:<->",
            crate::opcode::CompoundBaseOp::Mul => "infix:<*>",
            _ => unreachable!("only native integer +, -, and * are emitted"),
        };
        // Native candidates remain overridable by a user-declared infix. Put
        // the operands back and use the ordinary path so its junction and
        // dispatch semantics stay unchanged.
        if self.user_infix_override(op_name) {
            self.stack.push(left);
            self.stack.push(right);
            match op {
                crate::opcode::CompoundBaseOp::Add => self.exec_add_op()?,
                crate::opcode::CompoundBaseOp::Sub => self.exec_sub_op()?,
                crate::opcode::CompoundBaseOp::Mul => self.exec_mul_op()?,
                _ => unreachable!(),
            }
            return Ok(());
        }

        use crate::runtime::nqp_native;
        let result = if unsigned {
            let left = match left.view() {
                ValueView::Int(value) => u64::try_from(value).ok(),
                ValueView::BigInt(value) => value.to_u64(),
                _ => None,
            };
            let right = match right.view() {
                ValueView::Int(value) => u64::try_from(value).ok(),
                ValueView::BigInt(value) => value.to_u64(),
                _ => None,
            };
            match (left, right) {
                (Some(left), Some(right)) => Some(Value::int(match op {
                    crate::opcode::CompoundBaseOp::Add => nqp_native::add_u(left, right),
                    crate::opcode::CompoundBaseOp::Sub => nqp_native::sub_u(left, right),
                    crate::opcode::CompoundBaseOp::Mul => nqp_native::mul_u(left, right),
                    _ => unreachable!(),
                })),
                _ => None,
            }
        } else {
            let left_value = match left.view() {
                ValueView::Int(value) => Some(value),
                ValueView::BigInt(value) => value.to_i64(),
                _ => None,
            };
            let right_value = match right.view() {
                ValueView::Int(value) => Some(value),
                ValueView::BigInt(value) => value.to_i64(),
                _ => None,
            };
            match (left_value, right_value) {
                (Some(left), Some(right)) => Some(Value::int(match op {
                    crate::opcode::CompoundBaseOp::Add => nqp_native::add_i(left, right),
                    crate::opcode::CompoundBaseOp::Sub => nqp_native::sub_i(left, right),
                    crate::opcode::CompoundBaseOp::Mul => nqp_native::mul_i(left, right),
                    _ => unreachable!(),
                })),
                _ => None,
            }
        };
        if let Some(result) = result {
            self.stack.push(result);
            return Ok(());
        }

        // A value changed shape between compilation and execution. Preserve
        // the ordinary arithmetic fallback rather than silently applying a
        // native conversion to an incompatible value.
        self.stack.push(left);
        self.stack.push(right);
        match op {
            crate::opcode::CompoundBaseOp::Add => self.exec_add_op()?,
            crate::opcode::CompoundBaseOp::Sub => self.exec_sub_op()?,
            crate::opcode::CompoundBaseOp::Mul => self.exec_mul_op()?,
            _ => unreachable!(),
        }
        Ok(())
    }

    /// Whether a user-declared `sub infix:<op>` is in scope for `canon`
    /// (e.g. `"infix:<+>"`). The set is empty in the overwhelming common
    /// case, keeping tight numeric loops free of any registry lookup.
    ///
    /// Returns false when the code *currently executing* was compiled from a
    /// module file rather than the main script: user infix ops are lexically
    /// scoped to their compilation unit and must not override operators inside
    /// an imported module (e.g. Test.rakumod's `$num_of_tests_run + 1`).
    ///
    /// The question is deliberately about the executing compilation unit, NOT
    /// about how the VM got there. A dynamic "are we anywhere inside a module
    /// call" depth counter answers a different question and gets the common
    /// callback case backwards: `Test.rakumod`'s `lives-ok { $a + $b }`
    /// invokes a block that was compiled in the *main script*, so the script's
    /// own `sub infix:<+>` must still apply inside it.
    #[inline]
    pub(crate) fn user_infix_override(&self, canon: &str) -> bool {
        if self.user_declared_infix_ops.is_empty() {
            return false;
        }
        let Some(files) = self.user_declared_infix_ops.get(canon) else {
            return false;
        };
        // Empty == provenance unknown (an export, or a declaration we could
        // not attribute to a unit): visible everywhere, as before.
        files.is_empty() || self.declaring_unit_is_in_scope(files)
    }

    /// Whether any of `files` (the compilation units that declared an
    /// operator) is the unit currently executing, or an ancestor of it through
    /// the `EVAL` chain — `EVAL` compiles in its caller's lexical scope, so an
    /// operator declared in the enclosing unit is in scope inside the EVAL,
    /// while one declared by the EVAL'd code is scoped to that EVAL unit.
    ///
    /// Only reached once a user-declared infix of this name exists, so the env
    /// lookup stays off ordinary arithmetic.
    fn declaring_unit_is_in_scope(&self, files: &HashSet<Symbol>) -> bool {
        self.unit_chain_contains(self.current_unit, files)
    }

    /// METAOP_ASSIGN identity substitution (`$x OP= $y` with an undefined `$x`).
    ///
    /// Rakudo applies the base infix to the operator's zero-argument value
    /// rather than to the type object the container holds, so `my Int $a;
    /// $a += 1` is `0 + 1` and `$a *= 5` is `1 * 5`. Only a *type object* (a
    /// non-concrete value) is substituted — a `Failure` is concrete and stays
    /// on the stack so the following operator throws it, matching rakudo.
    pub(super) fn exec_meta_assign_identity_op(
        &mut self,
        identity: MetaAssignIdentity,
    ) -> Result<(), RuntimeError> {
        let Some(top) = self.stack.last() else {
            return Ok(());
        };
        if !is_meta_assign_undefined(top) {
            return Ok(());
        }
        *self.stack.last_mut().unwrap() = seed_meta_assign_identity(Value::NIL, Some(identity))?;
        Ok(())
    }

    pub(super) fn exec_add_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // A user-declared `infix:<+>` sub can override even native Int/Num
        // addition (roast S06-operator-overloading/infix.t), so the native
        // fast paths below must be skipped whenever one is in scope.
        let has_override = self.user_infix_override("infix:<+>");
        // Fast path: Int + Int (most common case in numeric loops)
        if !has_override
            && let Some(a) = left.as_int()
            && let Some(b) = right.as_int()
            && let Some(result) = a.checked_add(b)
        {
            self.stack.push(Value::int(result));
            return Ok(());
        }
        // Fast path: Num + Num
        if !has_override
            && let Some(a) = left.as_num()
            && let Some(b) = right.as_num()
        {
            self.stack.push(Value::num(a + b));
            return Ok(());
        }
        // Fast path: big-integer addition. Two `Int`/`BigInt` operands can be
        // neither a junction, a temporal value, a numeric type object nor a
        // numeric-looking string, so the junction/coercion wrapper below has
        // nothing to contribute for them, and its per-op cost is not small: on
        // the growing-Fibonacci loop of
        // `news/2026-09/bigint-arith-borrowed-operands.md` it was ~180M of the
        // 560M instructions this opcode spent over 100k iterations.
        if !has_override && let Some(result) = crate::builtins::arith::big_int_add(&left, &right) {
            self.stack.push(result);
            return Ok(());
        }
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            if let Some(result) = vm.try_user_infix("infix:<+>", &l, &r)? {
                return Ok(result);
            }
            if let Some(result) = vm.range_instance_arithmetic("+", &l, &r)? {
                return Ok(result);
            }
            if crate::builtins::arith::is_temporal_operand(&l)
                || crate::builtins::arith::is_temporal_operand(&r)
            {
                return crate::builtins::arith_add(l, r);
            }
            let (l, r) = match vm.coerce_numeric_bridge_pair_strict(l, r)? {
                Ok(pair) => pair,
                Err(failure) => return Ok(failure),
            };
            crate::builtins::arith_add(l, r)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_sub_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // A user `infix:<->` overrides the native fast paths, same as `+`.
        let has_override = self.user_infix_override("infix:<->");
        // Fast path: Int - Int
        if !has_override
            && let Some(a) = left.as_int()
            && let Some(b) = right.as_int()
            && let Some(result) = a.checked_sub(b)
        {
            self.stack.push(Value::int(result));
            return Ok(());
        }
        // Fast path: Num - Num
        if !has_override
            && let Some(a) = left.as_num()
            && let Some(b) = right.as_num()
        {
            self.stack.push(Value::num(a - b));
            return Ok(());
        }
        // Fast path: big-integer subtraction -- see the note in `exec_add_op`.
        if !has_override && let Some(result) = crate::builtins::arith::big_int_sub(&left, &right) {
            self.stack.push(result);
            return Ok(());
        }
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            if let Some(result) = vm.try_user_infix("infix:<->", &l, &r)? {
                return Ok(result);
            }
            if let Some(result) = vm.range_instance_arithmetic("-", &l, &r)? {
                return Ok(result);
            }
            if crate::builtins::arith::is_temporal_operand(&l)
                || crate::builtins::arith::is_temporal_operand(&r)
            {
                return Ok(crate::builtins::arith_sub(l, r));
            }
            let (l, r) = match vm.coerce_numeric_bridge_pair_strict(l, r)? {
                Ok(pair) => pair,
                Err(failure) => return Ok(failure),
            };
            Ok(crate::builtins::arith_sub(l, r))
        })?;
        self.stack.push(result);
        Ok(())
    }

    /// When the user has overloaded the list-associative comma operator
    /// (`sub infix:<,> {...}`), a bare value-list expression (`5, 5`) dispatches
    /// to that sub with ALL comma operands at once (raku's `infix:<,>` is
    /// list-associative), instead of building a `List`. Returns `true` when the
    /// call was made (result pushed); `false` when no override applies, in which
    /// case the operands are left on the stack for normal list construction.
    ///
    /// Guarded on the (normally empty) `user_declared_infix_ops` set so ordinary
    /// list construction pays only a set-emptiness check. Argument-list commas
    /// (`f(a, b)`) are compiled directly, not through `MakeArray`, so they are
    /// unaffected — matching raku, where `infix:<,>` overloads value lists only.
    pub(super) fn try_comma_overload(&mut self, n: u32) -> Result<bool, RuntimeError> {
        if n < 2
            || self.user_declared_infix_ops.is_empty()
            || !self.user_declared_infix_ops.contains_key("infix:<,>")
        {
            return Ok(false);
        }
        let n = n as usize;
        let start = self.stack.len() - n;
        let args: Vec<Value> = self.stack.drain(start..).collect();
        if let Some(def) = loan_env!(self, resolve_function_with_types("infix:<,>", &args)) {
            let empty_fns = CompiledFns::default();
            let result = self.compile_and_call_function_def(&def, args, &empty_fns)?;
            self.stack.push(result);
            Ok(true)
        } else {
            // No candidate matched the operand count: restore the stack and let
            // normal list construction proceed.
            for a in args {
                self.stack.push(a);
            }
            Ok(false)
        }
    }

    /// Try to dispatch a binary operation to a user-defined infix operator.
    /// Returns Some(result) if a user-defined candidate matched, None otherwise.
    pub(crate) fn try_user_infix(
        &mut self,
        op_name: &str,
        left: &Value,
        right: &Value,
    ) -> Result<Option<Value>, RuntimeError> {
        // Fast bail when no `sub infix:<op>` is in scope. Without this, every
        // arithmetic op that falls off the Int/Num fast paths (Rat, mixed-type,
        // etc.) pays a full `resolve_function_with_types` -- which builds ~30
        // `format!` lookup keys and probes the registry -- only to get `None`.
        // `user_declared_infix_ops` is populated for every registered/imported
        // `infix:<...>` sub, so an op absent from it can never resolve to a user
        // infix; the set is empty in the overwhelming common case.
        if !self.user_infix_override(op_name) {
            return Ok(None);
        }
        // Set operators are also used by the tree-walking carrier path. That
        // path preserves VarRef/Scalar wrappers so a raw parameter can alias
        // its caller, but operator dispatch must match the values, not those
        // call-site containers. The bytecode set-op path performs the same
        // decontainerization before reaching this helper.
        let dispatch_arg = |value: &Value| {
            crate::runtime::types::unwrap_varref_value(value.clone()).into_descalarized()
        };
        let args = if matches!(
            op_name,
            "infix:<(elem)>"
                | "infix:<(cont)>"
                | "infix:<(&)>"
                | "infix:<∩>"
                | "infix:<(|)>"
                | "infix:<∪>"
        ) {
            vec![dispatch_arg(left), dispatch_arg(right)]
        } else {
            vec![left.clone(), right.clone()]
        };
        // A custom EXPORT hook can return a materialized `&infix:<op>` value
        // whose candidates were private to the exporting compilation unit.
        // They are intentionally captured in the Sub, not left in the global
        // registry. Dispatch that captured family directly here; resolving the
        // operator by name would otherwise find the core candidate and lose the
        // exported overload. A non-matching captured candidate must decline so
        // the ordinary core operator can still handle recursive calls on values
        // such as `Order` made from inside the exported candidate itself.
        let captured_candidates =
            self.env()
                .get(&format!("&{op_name}"))
                .and_then(|value| match value.view() {
                    ValueView::Sub(data) => data
                        .env
                        .get("__mutsu_multi_dispatch_candidates")
                        .and_then(|value| value.as_list_items().map(ToOwned::to_owned)),
                    _ => None,
                });
        if let Some(candidates) = captured_candidates {
            for candidate in &candidates {
                if let ValueView::Sub(candidate_data) = candidate.view()
                    && self
                        .bind_function_args_values(
                            &candidate_data.param_defs,
                            &candidate_data.params,
                            &args,
                        )
                        .is_ok()
                {
                    let result = self.call_sub_value(candidate.clone(), args, false)?;
                    return Ok(Some(result));
                }
            }
            return Ok(None);
        }
        if let Some(def) = loan_env!(self, resolve_function_with_types(op_name, &args)) {
            // The native implementation is a *candidate*, not a fallback
            // (ADR-0071): a user `multi infix:<+>($a, $b)` joins the operator's
            // candidate set and only takes the call when it out-narrows the
            // core set. Reporting "no user candidate" here leaves the native
            // path to run, which is exactly what rakudo's core candidate does.
            if self.core_infix_candidate_wins(op_name, &def, left, right) {
                return Ok(None);
            }
            let empty_fns = CompiledFns::default();
            let result = self.compile_and_call_function_def(&def, args, &empty_fns)?;
            return Ok(Some(result));
        }
        Ok(None)
    }

    pub(super) fn exec_mul_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // A user `infix:<*>` overrides the native fast paths, same as `+`.
        let has_override = self.user_infix_override("infix:<*>");
        // Fast path: Int * Int
        if !has_override
            && let Some(a) = left.as_int()
            && let Some(b) = right.as_int()
            && let Some(result) = a.checked_mul(b)
        {
            self.stack.push(Value::int(result));
            return Ok(());
        }
        // Fast path: Num * Num
        if !has_override
            && let Some(a) = left.as_num()
            && let Some(b) = right.as_num()
        {
            self.stack.push(Value::num(a * b));
            return Ok(());
        }
        // Fast path: big-integer multiplication -- see the note in `exec_add_op`.
        if !has_override && let Some(result) = crate::builtins::arith::big_int_mul(&left, &right) {
            self.stack.push(result);
            return Ok(());
        }
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            if let Some(result) = vm.try_user_infix("infix:<*>", &l, &r)? {
                return Ok(result);
            }
            if let Some(result) = vm.range_instance_arithmetic("*", &l, &r)? {
                return Ok(result);
            }
            let (l, r) = match vm.coerce_numeric_bridge_pair_strict(l, r)? {
                Ok(pair) => pair,
                Err(failure) => return Ok(failure),
            };
            Ok(crate::builtins::arith_mul(l, r))
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_div_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            if let Some(result) = vm.try_user_infix("infix:</>", &l, &r)? {
                return Ok(result);
            }
            if let Some(result) = vm.range_instance_arithmetic("/", &l, &r)? {
                return Ok(result);
            }
            let (l, r) = match vm.coerce_numeric_bridge_pair_strict(l, r)? {
                Ok(pair) => pair,
                Err(failure) => return Ok(failure),
            };
            crate::builtins::arith_div(l, r)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_mod_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            // `infix:<%>` is a `multi` like every other operator, so a user
            // candidate that out-narrows the core set takes the call — this was
            // the only arithmetic opcode that never consulted one, so
            // `multi infix:<%>(P $a, P $b)` was unreachable for `P.new % P.new`.
            if let Some(result) = vm.try_user_infix("infix:<%>", &l, &r)? {
                return Ok(result);
            }
            // Duration % Real => Duration. Handle before numeric coercion strips
            // the Duration wrapper down to a bare number.
            if crate::builtins::arith::is_temporal_operand(&l) {
                return crate::builtins::arith_mod(l, r);
            }
            let (l, r) = match vm.coerce_numeric_bridge_pair_strict(l, r)? {
                Ok(pair) => pair,
                Err(failure) => return Ok(failure),
            };
            crate::builtins::arith_mod(l, r)
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_pow_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // A user `infix:<**>` overrides the native fast path, same as `+`.
        let has_override = self.user_infix_override("infix:<**>");
        // Fast path: Int ** small non-negative Int
        if !has_override
            && let Some(base) = left.as_int()
            && let Some(exp) = right.as_int()
            && (0..=30).contains(&exp)
        {
            let mut result: i64 = 1;
            let mut overflow = false;
            for _ in 0..exp {
                if let Some(r) = result.checked_mul(base) {
                    result = r;
                } else {
                    overflow = true;
                    break;
                }
            }
            if !overflow {
                self.stack.push(Value::int(result));
                return Ok(());
            }
        }
        let result = self.eval_binary_with_junctions(left, right, |vm, l, r| {
            if let Some(result) = vm.try_user_infix("infix:<**>", &l, &r)? {
                return Ok(result);
            }
            let (l, r) = match vm.coerce_numeric_bridge_pair_strict(l, r)? {
                Ok(pair) => pair,
                Err(failure) => return Ok(failure),
            };
            Ok(crate::builtins::arith_pow(l, r))
        })?;
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_negate_op(&mut self) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap();
        // Type objects: `Mu` has no `prefix:<->` candidate (hard error). A class
        // with a user `Numeric` method dispatches even on the bare type object,
        // then negates. Every other type object warns and resumes with its
        // negated per-type numeric *zero* (`-Int` → 0, `-Num` → -0e0,
        // `-Complex` → 0-0i), matching rakudo — the negate twin of prefix `+`.
        if let ValueView::Package(name) = val.view() {
            let n = name.resolve();
            if n == "Mu" {
                return Err(RuntimeError::new(format!(
                    "Cannot resolve caller prefix:<->({}:U); none of these signatures matches:\n    (\\a)",
                    n
                )));
            }
            let cn = n;
            if self.has_user_method(&cn, "Numeric") {
                let caller_code = self.current_code;
                let result = self.try_compiled_method_or_interpret(val.clone(), "Numeric", vec![]);
                self.reconcile_caller_after_internal_dispatch(caller_code);
                if let Ok(result) = result {
                    self.stack.push(crate::builtins::arith_negate(result)?);
                    return Ok(());
                }
            }
            let neg_zero =
                crate::builtins::arith_negate(Interpreter::type_object_numeric_zero(&cn))?;
            let resumed = self.warn_type_object_numeric_context_resume(&cn, neg_zero)?;
            self.stack.push(resumed);
            return Ok(());
        }
        // For strings, first coerce to numeric (preserving Rat/Complex types and
        // producing X::Str::Numeric for invalid strings), then negate the result.
        if let Some(s) = val.as_str() {
            let trimmed = s.trim();
            if trimmed.is_empty() {
                self.stack.push(Value::int(0));
                return Ok(());
            }
            if let Some(numeric) = crate::runtime::str_numeric::parse_raku_str_to_numeric(trimmed) {
                self.stack.push(crate::builtins::arith_negate(numeric)?);
            } else {
                return Err(RuntimeError::typed_msg(
                    "X::Str::Numeric",
                    format!(
                        "Cannot convert string to number: base-10 number must begin with valid digits or '.' in '{}'",
                        s
                    ),
                ));
            }
            return Ok(());
        }
        let val = self.coerce_numeric_bridge_value(val)?;
        self.stack.push(crate::builtins::arith_negate(val)?);
        Ok(())
    }

    pub(super) fn exec_int_bit_neg_op(&mut self) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap();
        let val = self.coerce_numeric_bridge_value(val)?;
        self.stack.push(crate::builtins::int_bitneg(&val));
        Ok(())
    }

    pub(super) fn exec_bool_bit_neg_op(&mut self) {
        let val = self.stack.pop().unwrap();
        self.stack.push(Value::truth(!val.truthy()));
    }

    pub(super) fn exec_str_bit_neg_op(&mut self) {
        let val = self.stack.pop().unwrap();
        if Self::is_buf_value(&val) {
            let bytes = Self::extract_buf_bytes(&val);
            let negated: Vec<Value> = bytes.iter().map(|b| Value::int((!b) as i64)).collect();
            // Determine result type: if input is utf8, result is utf8; otherwise Buf
            let result_type = if let ValueView::Instance { class_name, .. } = val.view() {
                class_name.resolve()
            } else {
                "Buf".to_string()
            };
            self.stack.push(crate::value::value_buf::make_buf(
                Symbol::intern(&result_type),
                negated,
            ));
        } else {
            let s = crate::runtime::utils::coerce_to_str(&val);
            let negated: Vec<u8> = s.as_bytes().iter().map(|b| !b).collect();
            let result = String::from_utf8_lossy(&negated).into_owned();
            self.stack.push(Value::str(result));
        }
    }
}
