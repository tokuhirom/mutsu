//! The NQP container tests `nqp::iscont`, `nqp::iscont_i` / `_n` / `_s` and
//! `nqp::isrwcont` (ADR-11553 §3.2, #12673).
//!
//! They ask about the operand's *binding*, which a value op never sees: the
//! `nqp::` layer decontainerizes its operands. So the compiler keeps what it
//! knows about the operand:
//!
//! - a variable declared with a native type answers statically: a `my int $i`
//!   or a native `is rw` / `is copy` / `is raw` parameter is a native container
//!   of that kind, a plain native parameter (`sub f(int $i)`) binds a value;
//! - an explicit `$x.VAR` operand is a container *object* passed as a value:
//!   only `iscont` is true for it;
//! - any other variable, element or attribute is compiled as `.VAR`, the
//!   container mutsu already answers for it, and a bare expression as itself
//!   (a first-class reference such as `nqp::atposref_i`'s result is a value);
//!   the runtime op classifies that, and gets the variable's name to ask
//!   whether its binding is writable (`isrwcont`) or, for an attribute, what
//!   native type it was declared with.

use super::*;

/// The native kind a declared type stores: `'i'`, `'n'` or `'s'`.
// Cost: O(1).
fn native_kind_of_type(constraint: &str) -> Option<char> {
    let base = constraint
        .strip_suffix(":D")
        .or_else(|| constraint.strip_suffix(":U"))
        .or_else(|| constraint.strip_suffix(":_"))
        .unwrap_or(constraint);
    if crate::runtime::native_types::is_native_int_type(base) {
        Some('i')
    } else if matches!(base, "num" | "num32" | "num64") {
        Some('n')
    } else if base == "str" {
        Some('s')
    } else {
        None
    }
}

impl Compiler {
    /// Compile `nqp::iscont*` / `nqp::isrwcont` with one operand; `false`
    /// leaves any other shape to the ordinary call path.
    // Cost: O(1) at run time (a literal, or a `.VAR` plus one classification op).
    pub(super) fn compile_nqp_container_test(&mut self, name: &str, arg: &Expr) -> bool {
        let op = name
            .strip_prefix(crate::symbol::NQP_OP_PREFIX)
            .unwrap_or(name);
        // `nqp::iscont(my $v := EXPR)` asks about the variable the bind
        // declares: run the declaration, then test the variable's binding
        // (Hash2Class's `nqp::iscont(my $value := nqp::atkey(..))`, #15106).
        let declared;
        let arg = if let Expr::DoStmt(stmt) = arg
            && let Stmt::VarDecl {
                name: var,
                bind: Some(_),
                ..
            } = &**stmt
        {
            self.compile_expr(arg);
            self.code.emit(OpCode::Pop);
            declared = Expr::Var(var.clone());
            &declared
        } else {
            arg
        };
        if let Expr::Var(var) = arg
            && let Some(kind) = self
                .local_types
                .get(var)
                .and_then(|t| native_kind_of_type(t))
        {
            let container = !self.native_ro_params.contains(var);
            let answer = match op {
                "iscont" | "isrwcont" => container,
                _ => container && op.ends_with(kind),
            };
            let idx = self.code.add_constant(Value::int(i64::from(answer)));
            self.code.emit(OpCode::LoadConst(idx));
            return true;
        }
        // An explicit `.VAR` hands over the container *object* as a value:
        // rakudo answers `iscont` for it but neither the native-kind tests nor
        // `isrwcont` (`nqp::isrwcont($x.VAR)` is 0).
        if let Expr::MethodCall {
            name: method,
            args: margs,
            ..
        } = arg
            && method.as_str() == "VAR"
            && margs.is_empty()
            && op != "iscont"
        {
            self.compile_expr(arg);
            self.code.emit(OpCode::Pop);
            let idx = self.code.add_constant(Value::int(0));
            self.code.emit(OpCode::LoadConst(idx));
            return true;
        }
        let (operand, hint) = match arg {
            Expr::Var(var) | Expr::BareWord(var) => (Self::var_method_call(arg), Some(var.clone())),
            Expr::Index { .. } => (Self::var_method_call(arg), None),
            _ => (arg.clone(), None),
        };
        let mut args = vec![operand];
        if let Some(hint) = hint {
            args.push(Expr::Literal(Value::str(hint)));
        }
        self.try_compile_nqp_value_op(name, &args)
    }

    /// `expr.VAR`.
    fn var_method_call(expr: &Expr) -> Expr {
        Expr::MethodCall {
            target: Box::new(expr.clone()),
            name: crate::symbol::Symbol::intern("VAR"),
            args: Vec::new(),
            modifier: None,
            quoted: false,
            sugar: false,
        }
    }
}
