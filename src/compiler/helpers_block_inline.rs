use super::*;
use crate::symbol::Symbol;

impl Compiler {
    /// Compile a block inline (for blocks without placeholders).
    pub(super) fn compile_block_inline(&mut self, stmts: &[Stmt]) {
        let saved = self.push_dynamic_scope_lexical();
        if stmts.is_empty() {
            self.code.emit(OpCode::LoadNil);
            self.pop_dynamic_scope_lexical(saved);
            return;
        }
        // If the block contains CATCH/CONTROL, wrap in implicit try
        if Self::has_catch_or_control(stmts) {
            self.compile_try(stmts, &None);
            self.pop_dynamic_scope_lexical(saved);
            return;
        }
        // Hoist sub declarations
        self.hoist_sub_decls(stmts, true);
        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;
            if is_last {
                match stmt {
                    // MarkSigillessReadonly at block-final position: compile
                    // the mark statement, then load the variable's value so
                    // `my \x = 42` used in expression context returns 42.
                    Stmt::MarkSigillessReadonly(name) => {
                        self.compile_stmt(stmt);
                        self.compile_expr(&Expr::BareWord(name.clone()));
                        self.pop_dynamic_scope_lexical(saved);
                        return;
                    }
                    Stmt::Expr(expr) => {
                        self.compile_expr(expr);
                        // Don't emit Pop — leave value on stack as block's return value
                        self.pop_dynamic_scope_lexical(saved);
                        return;
                    }
                    Stmt::Block(inner) | Stmt::SyntheticBlock(inner) => {
                        // Nested bare blocks in final position should keep flowing
                        // their final value outward.
                        self.compile_block_inline(inner);
                        self.pop_dynamic_scope_lexical(saved);
                        return;
                    }
                    Stmt::Given { .. } => {
                        // given block pushes succeed value onto stack
                        self.compile_stmt(stmt);
                        self.pop_dynamic_scope_lexical(saved);
                        return;
                    }
                    Stmt::Whenever { supply, .. } => {
                        // `do whenever ...` should produce the created Tap in expression context.
                        self.compile_stmt(stmt);
                        if let Expr::Var(name) = supply {
                            self.compile_expr(&Expr::Var(name.clone()));
                        } else {
                            self.code.emit(OpCode::LoadNil);
                        }
                        self.pop_dynamic_scope_lexical(saved);
                        return;
                    }
                    Stmt::SubDecl { name, .. } => {
                        self.compile_stmt(stmt);
                        self.compile_expr(&Expr::CodeVar(name.resolve()));
                        self.pop_dynamic_scope_lexical(saved);
                        return;
                    }
                    Stmt::Call { name, args } => {
                        let positional: Option<Vec<Expr>> = args
                            .iter()
                            .map(|arg| match arg {
                                crate::ast::CallArg::Positional(expr) => Some(expr.clone()),
                                _ => None,
                            })
                            .collect();
                        if let Some(positional_args) = positional {
                            self.compile_expr(&Expr::Call {
                                name: *name,
                                args: positional_args,
                            });
                            self.pop_dynamic_scope_lexical(saved);
                            return;
                        }
                    }
                    Stmt::VarDecl {
                        name,
                        expr,
                        is_dynamic: ast_is_dynamic,
                        ..
                    } => {
                        // my $x = expr in block-final position: declare and return value
                        let is_dynamic = *ast_is_dynamic || self.var_is_dynamic(name);
                        let name_idx = self.code.add_constant(Value::Str(name.clone().into()));
                        self.code.emit(OpCode::SetVarDynamic {
                            name_idx,
                            dynamic: is_dynamic,
                        });
                        self.compile_expr(expr);
                        self.code.emit(OpCode::Dup);
                        self.emit_set_named_var(name);
                        self.pop_dynamic_scope_lexical(saved);
                        return;
                    }
                    Stmt::Assign { name, expr, .. } => {
                        // $x = expr in block-final position: assign and return value
                        self.compile_expr(expr);
                        self.code.emit(OpCode::Dup);
                        self.emit_set_named_var(name);
                        self.pop_dynamic_scope_lexical(saved);
                        return;
                    }
                    Stmt::Phaser {
                        kind: PhaserKind::Begin | PhaserKind::Check | PhaserKind::Init,
                        body,
                    } => {
                        // Phasers in block-final position should leave their
                        // last expression value on the stack (e.g. when used
                        // inside string interpolation blocks).
                        self.compile_block_inline(body);
                        self.pop_dynamic_scope_lexical(saved);
                        return;
                    }
                    _ => {}
                }
            }
            self.compile_stmt(stmt);
        }
        // If last statement wasn't an expression, push Nil
        self.code.emit(OpCode::LoadNil);
        self.pop_dynamic_scope_lexical(saved);
    }

    /// Check if assigning a numeric literal to a typed variable produces a
    /// compile-time `X::Syntax::Number::LiteralType` error.  Returns `Some(error_value)`
    /// when a mismatch is detected, `None` otherwise.
    pub(super) fn check_literal_type_mismatch(&self, var_name: &str, expr: &Expr) -> Option<Value> {
        // Only check plain assignment of literal values
        let (lit_type, lit_repr) = match expr {
            Expr::Literal(Value::Int(n)) => ("Int", format!("{}", n)),
            Expr::Literal(Value::Num(n)) => ("Num", format!("{:?}", n)),
            Expr::Literal(Value::Rat(n, d)) => {
                let r = *n as f64 / *d as f64;
                ("Rat", format!("{}", r))
            }
            Expr::Literal(Value::Complex(re, im)) => ("Complex", format!("<{}+{}i>", re, im)),
            _ => return None,
        };

        // Look up the variable's type constraint
        let constraint = self.local_types.get(var_name)?;
        let base = constraint
            .strip_suffix(":D")
            .or_else(|| constraint.strip_suffix(":U"))
            .unwrap_or(constraint);

        // All numeric types (boxed and native) for cross-checking
        let is_numeric_constraint =
            matches!(base, "Int" | "Num" | "Rat" | "Complex" | "int" | "num");
        if !is_numeric_constraint {
            return None;
        }

        // Check if the literal type matches the constraint
        let matches = match base {
            "Int" | "int" => lit_type == "Int",
            "Num" | "num" => lit_type == "Num",
            "Rat" => lit_type == "Rat",
            "Complex" => lit_type == "Complex",
            _ => true,
        };

        if matches {
            return None;
        }

        // Build the error message matching Raku's format
        let is_native = base == "int" || base == "num";
        let message = if is_native {
            format!(
                "Cannot assign a literal of type {} ({}) to a native variable of type {}. \
                 You can declare the variable to be of type Real, or try to coerce the value with {}.{} or {}({})",
                lit_type,
                lit_repr,
                base,
                lit_repr,
                if base == "int" { "Int" } else { "Num" },
                if base == "int" { "Int" } else { "Num" },
                lit_repr,
            )
        } else {
            let hint = match (base, lit_type) {
                ("Num", "Int") => format!(", or just write the value as {}e0", lit_repr),
                ("Int", "Num") => String::new(),
                ("Rat", "Int") => format!(", or just write the value as {}.0", lit_repr),
                _ => String::new(),
            };
            format!(
                "Cannot assign a literal of type {} ({}) to a variable of type {}. \
                 You can declare the variable to be of type Real, or try to coerce the value with {}.{} or {}({}){}",
                lit_type, lit_repr, base, lit_repr, base, base, lit_repr, hint,
            )
        };

        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(message));
        Some(Value::make_instance(
            Symbol::intern("X::Syntax::Number::LiteralType"),
            attrs,
        ))
    }

    /// Check a block body for heredoc interpolations referencing variables that
    /// are declared inside the block but not visible in the outer scope (where
    /// the heredoc terminator physically appears in Raku source).
    /// Returns Some(error_value) if an undeclared variable is found.
    pub(super) fn check_heredoc_scope_errors(&self, body: &[Stmt]) -> Option<Value> {
        // Collect variable names declared in this block body (my $x, etc.)
        let mut block_locals: std::collections::HashSet<String> = std::collections::HashSet::new();
        for stmt in body {
            if let Stmt::VarDecl { name, .. } = stmt {
                block_locals.insert(name.clone());
            }
        }
        if block_locals.is_empty() {
            return None;
        }
        // Recursively find HeredocInterpolation nodes in the body
        for stmt in body {
            if let Some(var_name) = self.find_heredoc_in_stmt(stmt, &block_locals) {
                let msg = format!(
                    "Variable '${}' is not declared. \
                     Perhaps you forgot a 'sub' if this was intended to be part of a signature?",
                    var_name
                );
                return Some(Value::str(msg));
            }
        }
        None
    }

    /// Search a statement for HeredocInterpolation nodes that reference block-local
    /// variables not visible in the outer scope.
    fn find_heredoc_in_stmt(
        &self,
        stmt: &Stmt,
        block_locals: &std::collections::HashSet<String>,
    ) -> Option<String> {
        match stmt {
            Stmt::Expr(expr) => self.find_heredoc_in_expr(expr, block_locals),
            Stmt::Say(exprs) | Stmt::Print(exprs) | Stmt::Note(exprs) => {
                for expr in exprs {
                    if let Some(name) = self.find_heredoc_in_expr(expr, block_locals) {
                        return Some(name);
                    }
                }
                None
            }
            Stmt::Return(expr) | Stmt::Die(expr) | Stmt::Fail(expr) => {
                self.find_heredoc_in_expr(expr, block_locals)
            }
            _ => None,
        }
    }

    /// Search an expression for HeredocInterpolation nodes that reference
    /// block-local variables not visible in the outer scope.
    fn find_heredoc_in_expr(
        &self,
        expr: &Expr,
        block_locals: &std::collections::HashSet<String>,
    ) -> Option<String> {
        match expr {
            Expr::HeredocInterpolation(content) => {
                let resolved = crate::parser::interpolate_heredoc_content(content);
                Self::find_undeclared_heredoc_var(&resolved, block_locals, &self.local_map)
            }
            // Recurse into subexpressions that might contain a heredoc
            Expr::Call { args, .. } => {
                for arg in args {
                    if let Some(name) = self.find_heredoc_in_expr(arg, block_locals) {
                        return Some(name);
                    }
                }
                None
            }
            Expr::MethodCall { target, args, .. } => {
                if let Some(name) = self.find_heredoc_in_expr(target, block_locals) {
                    return Some(name);
                }
                for arg in args {
                    if let Some(name) = self.find_heredoc_in_expr(arg, block_locals) {
                        return Some(name);
                    }
                }
                None
            }
            _ => None,
        }
    }

    /// Find a variable in a heredoc interpolation expression that is declared
    /// only in the sub scope (sub_locals) but not in the outer scope (outer_map).
    fn find_undeclared_heredoc_var(
        expr: &Expr,
        sub_locals: &std::collections::HashSet<String>,
        outer_map: &HashMap<String, u32>,
    ) -> Option<String> {
        match expr {
            Expr::Var(name) => {
                // Skip dynamic/compile-time/special variables
                if name.starts_with('*')
                    || name.starts_with('?')
                    || name.starts_with("DYNAMIC::")
                    || name.starts_with('~')
                {
                    return None;
                }
                // If the variable is declared in the sub but not in the outer scope,
                // it's not visible at the heredoc terminator position.
                if sub_locals.contains(name) && !outer_map.contains_key(name.as_str()) {
                    return Some(name.clone());
                }
                None
            }
            Expr::StringInterpolation(parts) => {
                for part in parts {
                    if let Some(name) =
                        Self::find_undeclared_heredoc_var(part, sub_locals, outer_map)
                    {
                        return Some(name);
                    }
                }
                None
            }
            Expr::MethodCall { target, args, .. } => {
                if let Some(name) = Self::find_undeclared_heredoc_var(target, sub_locals, outer_map)
                {
                    return Some(name);
                }
                for arg in args {
                    if let Some(name) =
                        Self::find_undeclared_heredoc_var(arg, sub_locals, outer_map)
                    {
                        return Some(name);
                    }
                }
                None
            }
            Expr::Block(stmts) => {
                for stmt in stmts {
                    if let Stmt::Expr(e) = stmt
                        && let Some(name) =
                            Self::find_undeclared_heredoc_var(e, sub_locals, outer_map)
                    {
                        return Some(name);
                    }
                }
                None
            }
            _ => None,
        }
    }
}
