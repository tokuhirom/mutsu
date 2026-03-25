use super::*;
use crate::ast::{CallArg, make_anon_sub};
use crate::symbol::Symbol;

impl Compiler {
    pub(super) fn is_normalized_stmt_call_name(name: &str) -> bool {
        matches!(
            name,
            "shift"
                | "pop"
                | "push"
                | "unshift"
                | "append"
                | "prepend"
                | "splice"
                | "undefine"
                | "VAR"
                | "indir"
        ) || crate::parser::is_imported_function(name)
    }

    pub(super) fn rewrite_stmt_call_args(name: &str, args: &[CallArg]) -> Vec<CallArg> {
        let rewrites_needed = matches!(
            name,
            "lives-ok" | "dies-ok" | "throws-like" | "warns-like" | "doesn't-warn" | "is_run"
        );
        if !rewrites_needed {
            return args.to_vec();
        }
        let mut positional_index = 0usize;
        args.iter()
            .map(|arg| match arg {
                CallArg::Positional(expr) => {
                    let rewritten = if matches!(
                        name,
                        "lives-ok" | "dies-ok" | "throws-like" | "warns-like" | "doesn't-warn"
                    ) && positional_index == 0
                    {
                        match expr {
                            Expr::Block(body) => make_anon_sub(body.clone()),
                            _ => expr.clone(),
                        }
                    } else if name == "is_run" && positional_index == 1 {
                        Self::rewrite_hash_block_values(expr)
                    } else {
                        expr.clone()
                    };
                    positional_index += 1;
                    CallArg::Positional(rewritten)
                }
                CallArg::Named { name, value } => CallArg::Named {
                    name: name.clone(),
                    value: value.clone(),
                },
                CallArg::Slip(expr) => CallArg::Slip(expr.clone()),
                CallArg::Invocant(expr) => CallArg::Invocant(expr.clone()),
            })
            .collect()
    }

    /// Rewrite block values inside a hash literal to anonymous subs.
    /// Used for `is_run`'s expectation hash: `{ out => { ... } }`.
    pub(super) fn rewrite_hash_block_values(expr: &Expr) -> Expr {
        if let Expr::Hash(pairs) = expr {
            let rewritten_pairs = pairs
                .iter()
                .map(|(name, value)| {
                    let rewritten_value = value.as_ref().map(|v| {
                        if let Expr::Block(body) = v {
                            make_anon_sub(body.clone())
                        } else {
                            v.clone()
                        }
                    });
                    (name.clone(), rewritten_value)
                })
                .collect();
            Expr::Hash(rewritten_pairs)
        } else {
            expr.clone()
        }
    }

    pub(super) fn has_phasers(stmts: &[Stmt]) -> bool {
        stmts
            .iter()
            .any(|s| matches!(s, Stmt::Phaser { kind, .. } if matches!(kind, PhaserKind::Enter | PhaserKind::Leave | PhaserKind::Keep | PhaserKind::Undo | PhaserKind::First | PhaserKind::Next | PhaserKind::Last | PhaserKind::Pre | PhaserKind::Post)))
    }

    /// Check if a block body contains placeholder variables ($^a, $^b, etc.)
    pub(super) fn has_block_placeholders(stmts: &[Stmt]) -> bool {
        for stmt in stmts {
            if Self::stmt_has_placeholder(stmt) {
                return true;
            }
        }
        false
    }

    /// Compile a method call argument. Named args (AssignExpr) are
    /// compiled as Pair values so they survive VM execution.
    pub(super) fn compile_method_arg(&mut self, arg: &Expr) {
        if let Expr::AssignExpr { name, expr } = arg {
            // `foo(arg = 1)` in method-call argument position is treated as a named
            // argument only for sigilless identifiers. Sigiled targets (`$x = ...`,
            // `@x = ...`, `%x = ...`) are real assignment expressions.
            if name.starts_with('$')
                || name.starts_with('@')
                || name.starts_with('%')
                || name.starts_with('&')
            {
                self.compile_expr(arg);
                if Self::needs_decont(arg) {
                    self.code.emit(OpCode::Decont);
                }
            } else {
                self.compile_expr(&Expr::Literal(Value::str(name.clone())));
                self.compile_expr(expr);
                self.code.emit(OpCode::MakePair);
            }
        } else {
            self.compile_expr(arg);
            if Self::needs_decont(arg) {
                self.code.emit(OpCode::Decont);
            }
        }
    }

    /// Check if an expression produces an array value that needs decontainerization
    /// for slurpy flattening at call sites.
    fn needs_decont(expr: &Expr) -> bool {
        match expr {
            Expr::ArrayVar(_) => true,
            // Assignment to @-variable returns an array
            Expr::AssignExpr { name, .. } => name.starts_with('@'),
            // VarDecl/Assign in expression position (my @a = ...)
            Expr::DoStmt(stmt) => match stmt.as_ref() {
                Stmt::VarDecl { name, .. } | Stmt::Assign { name, .. } => name.starts_with('@'),
                _ => false,
            },
            _ => false,
        }
    }

    /// Compile a function-call positional argument.
    /// Variable-like args are wrapped with source-name metadata so sigilless
    /// parameters (`\x`) can bind as writable aliases.
    pub(super) fn compile_call_arg(&mut self, arg: &Expr) {
        self.compile_expr(arg);
        if Self::needs_decont(arg) {
            self.code.emit(OpCode::Decont);
        }
        let source_name = match arg {
            Expr::Var(n) => Some(n.clone()),
            Expr::ArrayVar(n) => Some(format!("@{}", n)),
            Expr::HashVar(n) => Some(format!("%{}", n)),
            Expr::CodeVar(n) => Some(format!("&{}", n)),
            Expr::BareWord(n) => Some(n.clone()),
            _ => None,
        };
        if let Some(name) = source_name {
            let name_idx = self.code.add_constant(Value::str(name));
            self.code.emit(OpCode::WrapVarRef(name_idx));
        }
    }

    pub(super) fn stmt_has_placeholder(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Expr(e) | Stmt::Return(e) | Stmt::Die(e) | Stmt::Fail(e) | Stmt::Take(e) => {
                Self::expr_has_placeholder(e)
            }
            Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => {
                Self::expr_has_placeholder(expr)
            }
            Stmt::Say(es) | Stmt::Put(es) | Stmt::Print(es) | Stmt::Note(es) => {
                es.iter().any(Self::expr_has_placeholder)
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                ..
            } => {
                Self::expr_has_placeholder(cond)
                    || then_branch.iter().any(Self::stmt_has_placeholder)
                    || else_branch.iter().any(Self::stmt_has_placeholder)
            }
            Stmt::Block(stmts) => stmts.iter().any(Self::stmt_has_placeholder),
            _ => false,
        }
    }

    pub(super) fn expr_has_placeholder(expr: &Expr) -> bool {
        match expr {
            Expr::Var(name) => name.starts_with('^'),
            Expr::CodeVar(name) => name.starts_with('^'),
            Expr::Binary { left, right, .. } => {
                Self::expr_has_placeholder(left) || Self::expr_has_placeholder(right)
            }
            Expr::Unary { expr, .. } => Self::expr_has_placeholder(expr),
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                Self::expr_has_placeholder(cond)
                    || Self::expr_has_placeholder(then_expr)
                    || Self::expr_has_placeholder(else_expr)
            }
            Expr::Call { args, .. } => args.iter().any(Self::expr_has_placeholder),
            Expr::MethodCall { target, args, .. }
            | Expr::DynamicMethodCall { target, args, .. }
            | Expr::HyperMethodCall { target, args, .. }
            | Expr::HyperMethodCallDynamic { target, args, .. } => {
                Self::expr_has_placeholder(target) || args.iter().any(Self::expr_has_placeholder)
            }
            Expr::Index { target, index } | Expr::IndexAssign { target, index, .. } => {
                Self::expr_has_placeholder(target) || Self::expr_has_placeholder(index)
            }
            Expr::CallOn { target, args } => {
                Self::expr_has_placeholder(target) || args.iter().any(Self::expr_has_placeholder)
            }
            Expr::StringInterpolation(parts)
            | Expr::ArrayLiteral(parts)
            | Expr::BracketArray(parts, _)
            | Expr::CaptureLiteral(parts) => parts.iter().any(Self::expr_has_placeholder),
            _ => false,
        }
    }

    /// Check for placeholder variable conflicts in a block/sub body.
    /// Returns a Value to die with if a conflict is found.
    /// `decl_kind` is Some("sub") for named subs, None for blocks.
    pub(super) fn check_placeholder_conflicts(
        &self,
        params: &[String],
        body: &[Stmt],
        decl_kind: Option<&str>,
    ) -> Option<Value> {
        use crate::ast::{bare_precedes_placeholder, has_var_decl};
        for param in params {
            let bare_name = if let Some(b) = param.strip_prefix("&^") {
                b
            } else if let Some(b) = param.strip_prefix('^') {
                b
            } else {
                continue;
            };
            // Check for `my $name` in the same scope → X::Redeclaration
            if has_var_decl(body, bare_name) {
                return Some(Value::str(format!(
                    "X::Redeclaration: Redeclaration of symbol '$^{}'",
                    bare_name
                )));
            }
            // Check if bare var precedes placeholder in the body
            if bare_precedes_placeholder(body, bare_name) {
                // If outer scope has this variable → X::Placeholder::NonPlaceholder
                if self.local_map.contains_key(bare_name) {
                    let decl = decl_kind.unwrap_or("block");
                    let message = format!(
                        "'${}' has already been used as a non-placeholder in the surrounding {}, \
                         so you will confuse the reader if you suddenly declare $^{} here",
                        bare_name, decl, bare_name
                    );
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert(
                        "variable_name".to_string(),
                        Value::str(format!("${}", bare_name)),
                    );
                    attrs.insert(
                        "placeholder".to_string(),
                        Value::str(format!("$^{}", bare_name)),
                    );
                    attrs.insert("decl".to_string(), Value::str(decl.to_string()));
                    attrs.insert("message".to_string(), Value::str(message));
                    return Some(Value::make_instance(
                        Symbol::intern("X::Placeholder::NonPlaceholder"),
                        attrs,
                    ));
                } else {
                    // No outer declaration → X::Undeclared
                    return Some(Value::str(format!(
                        "X::Undeclared: Variable '${}' is not declared. \
                         Did you mean '$^{}'?",
                        bare_name, bare_name
                    )));
                }
            }
        }
        None
    }

    /// Check for assignment to native-typed read-only parameters inside a
    /// sub/method/block body. Returns an X::Assignment::RO::Comp error value
    /// if such an assignment is found.
    pub(crate) fn check_native_readonly_param_assignment(
        param_defs: &[crate::ast::ParamDef],
        body: &[Stmt],
    ) -> Option<Value> {
        // Build set of native-typed param names that are NOT `is rw` or `is copy`
        let readonly_native_params: std::collections::HashSet<&str> = param_defs
            .iter()
            .filter(|pd| {
                let is_native = pd.type_constraint.as_deref().is_some_and(|c| {
                    matches!(
                        c,
                        "int"
                            | "int8"
                            | "int16"
                            | "int32"
                            | "int64"
                            | "uint"
                            | "uint8"
                            | "uint16"
                            | "uint32"
                            | "uint64"
                            | "num"
                            | "num32"
                            | "num64"
                            | "str"
                    )
                });
                let has_rw_or_copy = pd
                    .traits
                    .iter()
                    .any(|t| t == "rw" || t == "copy" || t == "raw");
                is_native && !has_rw_or_copy
            })
            .map(|pd| pd.name.as_str())
            .collect();
        if readonly_native_params.is_empty() {
            return None;
        }
        fn scan_stmts(
            stmts: &[Stmt],
            readonly: &std::collections::HashSet<&str>,
        ) -> Option<String> {
            for stmt in stmts {
                if let Some(name) = scan_stmt(stmt, readonly) {
                    return Some(name);
                }
            }
            None
        }
        fn scan_stmt(stmt: &Stmt, readonly: &std::collections::HashSet<&str>) -> Option<String> {
            match stmt {
                Stmt::Assign { name, .. } => {
                    if readonly.contains(name.as_str()) {
                        return Some(format!("${}", name));
                    }
                }
                Stmt::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    if let Some(n) = scan_stmts(then_branch, readonly) {
                        return Some(n);
                    }
                    if let Some(n) = scan_stmts(else_branch, readonly) {
                        return Some(n);
                    }
                }
                Stmt::For { body, .. }
                | Stmt::While { body, .. }
                | Stmt::Loop { body, .. }
                | Stmt::Block(body)
                | Stmt::SyntheticBlock(body)
                | Stmt::Default(body)
                | Stmt::Catch(body)
                | Stmt::Control(body) => {
                    if let Some(n) = scan_stmts(body, readonly) {
                        return Some(n);
                    }
                }
                Stmt::Given { body, .. } | Stmt::When { body, .. } => {
                    if let Some(n) = scan_stmts(body, readonly) {
                        return Some(n);
                    }
                }
                _ => {}
            }
            None
        }
        if let Some(var_name) = scan_stmts(body, &readonly_native_params) {
            let msg = format!("Cannot assign to readonly variable {}", var_name);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("variable".to_string(), Value::str(var_name));
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            return Some(Value::make_instance(
                crate::symbol::Symbol::intern("X::Assignment::RO::Comp"),
                attrs,
            ));
        }
        None
    }
}
