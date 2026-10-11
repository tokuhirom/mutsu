//! A native `str` stored into a native int container: rakudo compiles
//! `my int $width = $format` (both natives) to `nqp::coerce_si`, which parses
//! a leading integer ("8d" → 8, "abc" → 0) instead of raising, the "p5
//! semantics" core code such as `Telemetry` relies on (#9824). A boxed `Str`
//! source still raises: the coercion is decided by the two declared types.

use super::*;

impl Compiler {
    /// Whether a store of `value` into `target` takes `nqp::coerce_si`:
    /// `target` is declared with a native int type and `value` reads a
    /// variable declared `str`.
    // Cost: O(1) at compile time (two declared-type lookups).
    pub(super) fn native_str_to_int_coercion(&self, target: &str, value: &Expr) -> bool {
        let source_is_native_str = match value {
            Expr::Var(source) => self.local_types.get(source).map(String::as_str) == Some("str"),
            // `my int $h = my str $ = $line.substr(1,2)`: the inner declaration
            // is itself a native `str` (the "fast Str to int conversion" idiom
            // of IRC::Log::Colabti).
            Expr::DoStmt(stmt) => matches!(
                &**stmt,
                Stmt::VarDecl { type_constraint: Some(tc), .. } if tc == "str"
            ),
            _ => false,
        };
        source_is_native_str
            && self
                .local_types
                .get(target)
                .is_some_and(|tc| crate::runtime::native_types::is_native_int_type(tc))
    }

    /// Emit the `nqp::coerce_si` of the str on the stack.
    // Cost: O(1) at compile time.
    pub(super) fn emit_native_str_to_int(&mut self) {
        let id =
            crate::runtime::nqp_op_ids::nqp_op_id("coerce_si").expect("coerce_si is an nqp op");
        self.code.emit(OpCode::NqpOp { id, arity: 1 });
    }
}
