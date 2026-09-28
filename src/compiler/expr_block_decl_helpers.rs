use super::*;

impl Compiler {
    /// The constraint string an expression-position container declaration
    /// (`(my Int @c)`, `$(my Int %{Int})`) registers via `SetVarType`, or
    /// `None` when nothing should be tagged. Boxed element types only —
    /// native `int`/`num`/`str` change the storage and would panic in the
    /// auto-vivify. For an *anonymous* object hash (`my Int %{Int}` as an
    /// rvalue / `$(...)` itemization / EVAL round-trip) this keeps the FULL
    /// constraint including the `{KeyType}` half, so the container's key type
    /// survives and `(my Int %{Int})` round-trips through `.raku.EVAL`. A
    /// *named* object hash (`my Int %j{Cool}`) tags only the VALUE-type half:
    /// re-applying the key-type half would mark the value `.WHICH`-keyed
    /// while a raw-binding mutation (`gen my Int %j{Cool}` → `sub gen(\h){
    /// h{$_}=... }`) can still store plain string keys, so adverbs
    /// (`%j<b>:k`) would miss.
    pub(super) fn expr_decl_settable_constraint(
        &self,
        name: &str,
        type_constraint: &Option<String>,
    ) -> Option<String> {
        if !(name.starts_with('@') || name.starts_with('%')) {
            return None;
        }
        let tc = type_constraint.as_ref()?;
        let resolved_tc = self.resolve_type_alias_constraint(tc);
        let is_native_value_type = if name.starts_with('@') {
            crate::runtime::native_types::is_native_array_element_type(&resolved_tc)
        } else {
            self.is_native_type_constraint(&resolved_tc)
        };
        if is_native_value_type {
            return None;
        }
        let value_tc = if name.starts_with('%') && !name.contains("__ANON_HASH__") {
            resolved_tc.split('{').next().unwrap_or(&resolved_tc)
        } else {
            &resolved_tc
        };
        // Bare `my %h{KeyType}` (no value type): nothing to tag.
        (!value_tc.is_empty()).then(|| value_tc.to_string())
    }

    /// Compile DoStmt expression (do { ... }, do if, do for, etc.).
    /// Compile a `let`/`temp` statement so it leaves its value on the stack.
    ///
    /// `let $x = 42` is an assignment, so its value is the assigned one — this
    /// runs the save plus the assignment and then pushes the temporized
    /// variable. Used both for `(temp $x = 42)` in genuine expression position
    /// and for a block-final `let`/`temp`, whose value is the block's value and
    /// therefore decides whether the frame's `let` saves are kept or restored
    /// (#7646).
    pub(super) fn compile_let_stmt_as_value(&mut self, stmt: &Stmt, name: &str) {
        self.compile_stmt(stmt);
        // Reuse the same logic as Expr::ArrayVar/HashVar/Var compilation.
        if let Some(stripped) = name.strip_prefix('@') {
            self.compile_expr(&Expr::ArrayVar(stripped.to_string()));
        } else if let Some(stripped) = name.strip_prefix('%') {
            self.compile_expr(&Expr::HashVar(stripped.to_string()));
        } else {
            self.compile_expr(&Expr::Var(name.to_string()));
        }
    }
}
