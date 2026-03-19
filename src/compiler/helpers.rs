use super::*;
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;

/// Convert a `TokenKind` to its operator name string for runtime dispatch.
pub(super) fn token_kind_to_op_name(op: &TokenKind) -> String {
    match op {
        TokenKind::Ident(name) => name.clone(),
        TokenKind::Plus => "+".to_string(),
        TokenKind::Minus => "-".to_string(),
        TokenKind::Star => "*".to_string(),
        TokenKind::StarStar => "**".to_string(),
        TokenKind::Slash => "/".to_string(),
        TokenKind::Percent => "%".to_string(),
        TokenKind::Tilde => "~".to_string(),
        TokenKind::Bang => "!".to_string(),
        TokenKind::Question => "?".to_string(),
        TokenKind::Caret => "^".to_string(),
        TokenKind::Ampersand => "&".to_string(),
        TokenKind::Pipe => "|".to_string(),
        TokenKind::Comma => ",".to_string(),
        TokenKind::EqEq => "==".to_string(),
        TokenKind::BangEq => "!=".to_string(),
        TokenKind::Lt => "<".to_string(),
        TokenKind::Lte => "<=".to_string(),
        TokenKind::Gt => ">".to_string(),
        TokenKind::Gte => ">=".to_string(),
        TokenKind::AndAnd => "&&".to_string(),
        TokenKind::OrOr => "||".to_string(),
        TokenKind::XorXor => "^^".to_string(),
        TokenKind::SmartMatch => "~~".to_string(),
        TokenKind::BangTilde => "!~".to_string(),
        TokenKind::LtEqGt => "<=>".to_string(),
        TokenKind::EqEqEq => "===".to_string(),
        TokenKind::BangEqEqEq => "!===".to_string(),
        TokenKind::DotDot => "..".to_string(),
        TokenKind::DotDotDot => "...".to_string(),
        TokenKind::BitAnd => "+&".to_string(),
        TokenKind::BitOr => "+|".to_string(),
        TokenKind::BitXor => "+^".to_string(),
        TokenKind::BitShiftLeft => "+<".to_string(),
        TokenKind::BitShiftRight => "+>".to_string(),
        TokenKind::IntBitNeg => "+^".to_string(),
        TokenKind::BoolBitNeg => "?^".to_string(),
        TokenKind::StrBitNeg => "~^".to_string(),
        TokenKind::PercentPercent => "%%".to_string(),
        TokenKind::BangPercentPercent => "!%%".to_string(),
        TokenKind::SlashSlash => "//".to_string(),
        TokenKind::OrElse => "orelse".to_string(),
        TokenKind::AndThen => "andthen".to_string(),
        TokenKind::NotAndThen => "notandthen".to_string(),
        TokenKind::OrWord => "or".to_string(),
        TokenKind::BangBang => "!!".to_string(),
        TokenKind::QuestionQuestion => "??".to_string(),
        TokenKind::SetUnion => "(|)".to_string(),
        TokenKind::SetAddition => "(+)".to_string(),
        TokenKind::SetIntersect => "(&)".to_string(),
        TokenKind::SetMultiply => "(.)".to_string(),
        TokenKind::SetDiff => "(-)".to_string(),
        TokenKind::SetSymDiff => "(^)".to_string(),
        TokenKind::SetElem => "(elem)".to_string(),
        TokenKind::SetCont => "(cont)".to_string(),
        TokenKind::SetSubset => "(<=)".to_string(),
        TokenKind::SetSuperset => "(>=)".to_string(),
        TokenKind::SetStrictSubset => "(<)".to_string(),
        TokenKind::SetStrictSuperset => "(>)".to_string(),
        TokenKind::PlusPlus => "++".to_string(),
        TokenKind::MinusMinus => "--".to_string(),
        _ => format!("{:?}", op),
    }
}

impl Compiler {
    fn normalize_dynamic_scope_name(name: &str) -> String {
        name.trim_start_matches(['$', '@', '%', '&']).to_string()
    }

    pub(super) fn push_dynamic_scope_lexical(
        &mut self,
    ) -> (bool, Option<std::collections::HashSet<String>>) {
        (self.dynamic_scope_all, self.dynamic_scope_names.clone())
    }

    pub(super) fn pop_dynamic_scope_lexical(
        &mut self,
        saved: (bool, Option<std::collections::HashSet<String>>),
    ) {
        self.dynamic_scope_all = saved.0;
        self.dynamic_scope_names = saved.1;
    }

    pub(super) fn apply_dynamic_scope_pragma(&mut self, arg: Option<&Expr>) {
        match arg {
            None => {
                self.dynamic_scope_all = true;
                self.dynamic_scope_names = None;
            }
            Some(Expr::ArrayLiteral(items)) => {
                let mut names = std::collections::HashSet::new();
                for item in items {
                    if let Expr::Literal(Value::Str(s)) = item {
                        names.insert(Self::normalize_dynamic_scope_name(s));
                    }
                }
                self.dynamic_scope_all = false;
                self.dynamic_scope_names = Some(names);
            }
            Some(Expr::Literal(Value::Str(s))) => {
                let mut names = std::collections::HashSet::new();
                names.insert(Self::normalize_dynamic_scope_name(s));
                self.dynamic_scope_all = false;
                self.dynamic_scope_names = Some(names);
            }
            Some(_) => {
                self.dynamic_scope_all = false;
                self.dynamic_scope_names = Some(std::collections::HashSet::new());
            }
        }
    }

    /// Check if a variable name is a dynamic variable with a package-like name (contains ::).
    pub(super) fn is_dynamic_package_var(name: &str) -> bool {
        let stripped = name.trim_start_matches(['$', '@', '%', '&']);
        if let Some(after_star) = stripped.strip_prefix('*') {
            after_star.contains("::")
        } else {
            false
        }
    }

    /// Emit X::Dynamic::Package error for a dynamic variable with :: in name.
    pub(super) fn emit_dynamic_package_error(&mut self, name: &str) {
        let symbol = Self::dynamic_var_symbol(name);
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("symbol".to_string(), Value::str(symbol));
        let err = Value::make_instance(Symbol::intern("X::Dynamic::Package"), attrs);
        let idx = self.code.add_constant(err);
        self.code.emit(OpCode::LoadConst(idx));
        self.code.emit(OpCode::Die);
    }

    /// Reconstruct the full symbol name (with sigil) from the internal name.
    pub(super) fn dynamic_var_symbol(name: &str) -> String {
        // If name starts with a sigil (@, %, &), it already has the sigil
        if name.starts_with('@') || name.starts_with('%') || name.starts_with('&') {
            name.to_string()
        } else if name.starts_with('*') {
            // $* variable — sigil $ was stripped
            format!("${}", name)
        } else {
            format!("${}", name)
        }
    }

    pub(super) fn var_is_dynamic(&self, name: &str) -> bool {
        if self.dynamic_scope_all {
            return true;
        }
        let Some(names) = &self.dynamic_scope_names else {
            return false;
        };
        names.contains(&Self::normalize_dynamic_scope_name(name))
    }

    /// Parse CALLER:: prefix(es) from a variable name.
    /// Returns (bare_name, depth) where depth is the number of CALLER:: levels.
    /// E.g. "CALLER::a" -> ("a", 1), "CALLER::CALLER::a" -> ("a", 2).
    pub(crate) fn parse_caller_prefix(name: &str) -> Option<(String, usize)> {
        let mut remaining = name;
        let mut depth = 0;
        while let Some(rest) = remaining.strip_prefix("CALLER::") {
            depth += 1;
            remaining = rest;
        }
        if depth > 0 {
            Some((remaining.to_string(), depth))
        } else {
            None
        }
    }

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
            .any(|s| matches!(s, Stmt::Phaser { kind, .. } if matches!(kind, PhaserKind::Enter | PhaserKind::Leave | PhaserKind::Keep | PhaserKind::Undo | PhaserKind::First | PhaserKind::Next | PhaserKind::Last)))
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

    /// Compile a SubDecl body to a CompiledFunction and store it.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn compile_sub_body(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[crate::ast::ParamDef],
        return_type: Option<&String>,
        body: &[Stmt],
        multi: bool,
        state_group: Option<&str>,
        is_rw: bool,
        is_raw: bool,
    ) {
        // Before compiling the sub body, check for heredoc interpolations
        // that reference variables not visible at the outer scope (where the
        // heredoc terminator physically appears in Raku).
        if let Some(err) = self.check_heredoc_scope_errors(body) {
            let idx = self.code.add_constant(err);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die);
            return;
        }
        let sink_last_expr = return_type
            .map(|s| Self::is_definite_return_spec(s))
            .unwrap_or(false);
        let mut sub_compiler = Compiler::new();
        sub_compiler.is_routine = true;
        let arity = param_defs
            .iter()
            .filter(|p| !p.named && (!p.slurpy || p.name == "_capture"))
            .count();
        let state_scope = if multi {
            if let Some(group) = state_group {
                format!("{}::&{}/shared:{}", self.current_package, name, group)
            } else {
                let type_sig: Vec<String> = param_defs
                    .iter()
                    .filter(|pd| !pd.named && (!pd.slurpy || pd.name == "_capture"))
                    .map(|pd| pd.type_constraint.clone().unwrap_or_default())
                    .collect();
                if !type_sig.is_empty() {
                    format!(
                        "{}::&{}/{}:{}",
                        self.current_package,
                        name,
                        arity,
                        type_sig.join(",")
                    )
                } else {
                    format!("{}::&{}/{}", self.current_package, name, arity)
                }
            }
        } else {
            format!("{}::&{}/{}", self.current_package, name, arity)
        };
        sub_compiler.set_current_package(state_scope);
        // Pre-allocate locals for parameters
        for param in params {
            sub_compiler.alloc_local(param);
        }
        // Also allocate from param_defs in case param names differ
        for pd in param_defs {
            if !pd.name.is_empty() {
                sub_compiler.alloc_local(&pd.name);
            }
        }
        // Hoist sub declarations within the sub body
        sub_compiler.hoist_sub_decls(body);
        // If sub body contains CATCH/CONTROL, wrap in implicit try
        if Self::has_catch_or_control(body) {
            sub_compiler.compile_try(body, &None);
            sub_compiler.code.emit(OpCode::Pop);
            if sink_last_expr {
                let nil_idx = sub_compiler.code.add_constant(Value::Nil);
                sub_compiler.code.emit(OpCode::LoadConst(nil_idx));
            }
        } else if Self::has_block_enter_leave_phasers(body) {
            let idx = sub_compiler.code.emit(OpCode::BlockScope {
                pre_end: 0,
                enter_end: 0,
                body_end: 0,
                keep_start: 0,
                undo_start: 0,
                post_start: 0,
                end: 0,
            });
            // PRE phasers (forward order, before ENTER)
            Self::compile_pre_phasers(&mut sub_compiler, body);
            sub_compiler.code.patch_block_pre_end(idx);
            let mut body_main: Vec<Stmt> = Vec::new();
            for stmt in body.iter() {
                match stmt {
                    Stmt::Phaser {
                        kind: PhaserKind::Enter,
                        body,
                    } => {
                        for inner in body {
                            sub_compiler.compile_stmt(inner);
                        }
                    }
                    Stmt::Phaser {
                        kind:
                            PhaserKind::Leave
                            | PhaserKind::Keep
                            | PhaserKind::Undo
                            | PhaserKind::Pre
                            | PhaserKind::Post,
                        ..
                    } => {}
                    _ => body_main.push(stmt.clone()),
                }
            }
            sub_compiler.code.patch_block_enter_end(idx);
            Self::compile_routine_body_stmts(&mut sub_compiler, &body_main, sink_last_expr);
            sub_compiler.code.patch_block_body_end(idx);
            sub_compiler.code.patch_block_keep_start(idx);
            for stmt in body.iter().rev() {
                if let Stmt::Phaser { kind, body } = stmt
                    && matches!(kind, PhaserKind::Leave | PhaserKind::Keep)
                {
                    for inner in body {
                        sub_compiler.compile_stmt(inner);
                    }
                }
            }
            sub_compiler.code.patch_block_undo_start(idx);
            for stmt in body.iter().rev() {
                if let Stmt::Phaser { kind, body } = stmt
                    && matches!(kind, PhaserKind::Leave | PhaserKind::Undo)
                {
                    for inner in body {
                        sub_compiler.compile_stmt(inner);
                    }
                }
            }
            // POST phasers (reverse order, after LEAVE)
            sub_compiler.code.patch_block_post_start(idx);
            Self::compile_post_phasers(&mut sub_compiler, body);
            sub_compiler.code.patch_loop_end(idx);
        } else if sink_last_expr {
            Self::compile_routine_body_stmts(&mut sub_compiler, body, true);
        } else {
            Self::compile_routine_body_stmts(&mut sub_compiler, body, false);
        }

        let fingerprint = crate::ast::function_body_fingerprint(params, param_defs, body);
        let key = if multi {
            let type_sig: Vec<String> = param_defs
                .iter()
                .filter(|pd| !pd.named && (!pd.slurpy || pd.name == "_capture"))
                .map(|pd| pd.type_constraint.clone().unwrap_or_default())
                .collect();
            format!(
                "{}::{}{}",
                self.current_package,
                name,
                if !type_sig.is_empty() {
                    format!("/{}:{}", arity, type_sig.join(","))
                } else {
                    format!("/{}", arity)
                }
            )
        } else {
            // Include arity and fingerprint in key to avoid collisions between
            // same-named subs with different bodies in different scopes
            format!(
                "{}::{}/{}#{:x}",
                self.current_package, name, arity, fingerprint
            )
        };

        let cf = CompiledFunction {
            code: sub_compiler.code,
            params: params.to_vec(),
            param_defs: param_defs.to_vec(),
            return_type: return_type.cloned(),
            fingerprint,
            // Named subs with no params and no param_defs that don't use @_/%_ have
            // explicit empty signature :() and should reject any arguments.
            empty_sig: params.is_empty()
                && param_defs.is_empty()
                && !Self::body_uses_legacy_args(body),
            is_rw,
            is_raw,
        };
        self.compiled_functions.insert(key, cf);
    }

    fn compile_routine_body_stmts(
        sub_compiler: &mut Compiler,
        body: &[Stmt],
        sink_last_expr: bool,
    ) {
        if sink_last_expr {
            for stmt in body {
                sub_compiler.compile_stmt(stmt);
            }
            let nil_idx = sub_compiler.code.add_constant(Value::Nil);
            sub_compiler.code.emit(OpCode::LoadConst(nil_idx));
            return;
        }
        // Compile body statements; last Stmt::Expr should NOT emit Pop (implicit return)
        for (i, stmt) in body.iter().enumerate() {
            let is_last = i == body.len() - 1;
            if is_last {
                match stmt {
                    Stmt::Expr(expr) => {
                        sub_compiler.compile_expr(expr);
                        // Don't emit Pop — leave value on stack as implicit return
                        continue;
                    }
                    Stmt::If {
                        cond,
                        then_branch,
                        else_branch,
                        binding_var,
                    } if binding_var.is_none() => {
                        sub_compiler.compile_if_value(cond, then_branch, else_branch);
                        continue;
                    }
                    Stmt::Block(stmts) | Stmt::SyntheticBlock(stmts) => {
                        // Bare blocks in final statement position auto-execute and
                        // produce their final value.
                        sub_compiler.compile_block_inline(stmts);
                        continue;
                    }
                    Stmt::VarDecl { name, .. } => {
                        sub_compiler.compile_stmt(stmt);
                        // VarDecl as last statement returns the variable value
                        if let Some(&slot) = sub_compiler.local_map.get(name) {
                            sub_compiler.code.emit(OpCode::GetLocal(slot));
                        } else {
                            sub_compiler.emit_nil_value();
                        }
                        continue;
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
                            sub_compiler.compile_expr(&Expr::Call {
                                name: *name,
                                args: positional_args,
                            });
                            continue;
                        }
                    }
                    _ => {}
                }
            }
            sub_compiler.compile_stmt(stmt);
        }
    }

    /// Compile a closure body to a `CompiledCode` (not stored in compiled_functions).
    /// Used for Lambda, AnonSub, AnonSubParams, and BlockClosure.
    pub(crate) fn compile_closure_body(
        &mut self,
        params: &[String],
        param_defs: &[crate::ast::ParamDef],
        body: &[Stmt],
    ) -> CompiledCode {
        self.compile_closure_body_with_routine_flag(params, param_defs, body, false)
    }

    pub(crate) fn compile_routine_closure_body(
        &mut self,
        params: &[String],
        param_defs: &[crate::ast::ParamDef],
        body: &[Stmt],
    ) -> CompiledCode {
        self.compile_closure_body_with_routine_flag(params, param_defs, body, true)
    }

    fn compile_closure_body_with_routine_flag(
        &mut self,
        params: &[String],
        param_defs: &[crate::ast::ParamDef],
        body: &[Stmt],
        is_routine: bool,
    ) -> CompiledCode {
        let mut sub_compiler = Compiler::new();
        sub_compiler.is_routine = is_routine;
        // Give closures a unique package name so their state variables don't
        // collide with state variables in the enclosing code that happen to
        // share the same variable name.
        let closure_id = STATE_COUNTER.fetch_add(1, Ordering::Relaxed);
        let closure_package = format!("{}::&<closure>/{}", self.current_package, closure_id);
        sub_compiler.set_current_package(closure_package);
        // Pre-allocate locals for parameters
        for param in params {
            sub_compiler.alloc_local(param);
        }
        for pd in param_defs {
            if !pd.name.is_empty() {
                sub_compiler.alloc_local(&pd.name);
            }
        }
        // Hoist sub declarations within the closure body
        sub_compiler.hoist_sub_decls(body);
        // If body contains CATCH/CONTROL, wrap in implicit try
        if Self::has_catch_or_control(body) {
            sub_compiler.compile_try(body, &None);
            sub_compiler.code.emit(OpCode::Pop);
        } else if Self::has_block_enter_leave_phasers(body) {
            // Body has ENTER/LEAVE/KEEP/UNDO/PRE/POST — wrap in BlockScope
            let idx = sub_compiler.code.emit(OpCode::BlockScope {
                pre_end: 0,
                enter_end: 0,
                body_end: 0,
                keep_start: 0,
                undo_start: 0,
                post_start: 0,
                end: 0,
            });
            Self::compile_pre_phasers(&mut sub_compiler, body);
            sub_compiler.code.patch_block_pre_end(idx);
            // ENTER phasers
            for stmt in body.iter() {
                if let Stmt::Phaser {
                    kind: PhaserKind::Enter,
                    body: ph_body,
                } = stmt
                {
                    for inner in ph_body {
                        sub_compiler.compile_stmt(inner);
                    }
                }
            }
            sub_compiler.code.patch_block_enter_end(idx);
            // Body (excluding phasers)
            let body_stmts: Vec<&Stmt> = body
                .iter()
                .filter(|s| {
                    !matches!(
                        s,
                        Stmt::Phaser {
                            kind: PhaserKind::Enter
                                | PhaserKind::Leave
                                | PhaserKind::Keep
                                | PhaserKind::Undo
                                | PhaserKind::Pre
                                | PhaserKind::Post,
                            ..
                        }
                    )
                })
                .collect();
            for (i, stmt) in body_stmts.iter().enumerate() {
                let is_last = i == body_stmts.len() - 1;
                if is_last && let Stmt::Expr(expr) = stmt {
                    sub_compiler.compile_expr(expr);
                    sub_compiler.code.emit(OpCode::SetTopic);
                    continue;
                }
                sub_compiler.compile_stmt(stmt);
            }
            sub_compiler.code.patch_block_body_end(idx);
            sub_compiler.code.patch_block_keep_start(idx);
            for s in body.iter().rev() {
                if let Stmt::Phaser {
                    kind,
                    body: ph_body,
                } = s
                    && matches!(kind, PhaserKind::Leave | PhaserKind::Keep)
                {
                    for inner in ph_body {
                        sub_compiler.compile_stmt(inner);
                    }
                }
            }
            sub_compiler.code.patch_block_undo_start(idx);
            for s in body.iter().rev() {
                if let Stmt::Phaser {
                    kind,
                    body: ph_body,
                } = s
                    && matches!(kind, PhaserKind::Leave | PhaserKind::Undo)
                {
                    for inner in ph_body {
                        sub_compiler.compile_stmt(inner);
                    }
                }
            }
            sub_compiler.code.patch_block_post_start(idx);
            Self::compile_post_phasers(&mut sub_compiler, body);
            sub_compiler.code.patch_loop_end(idx);
        } else {
            // Compile body; last Stmt::Expr leaves value on stack (implicit return)
            for (i, stmt) in body.iter().enumerate() {
                let is_last = i == body.len() - 1;
                if is_last {
                    match stmt {
                        Stmt::Expr(expr) => {
                            sub_compiler.compile_expr(expr);
                            continue;
                        }
                        Stmt::If {
                            cond,
                            then_branch,
                            else_branch,
                            binding_var,
                        } if binding_var.is_none() => {
                            sub_compiler.compile_if_value(cond, then_branch, else_branch);
                            continue;
                        }
                        Stmt::Block(stmts) | Stmt::SyntheticBlock(stmts) => {
                            if Self::has_block_placeholders(stmts) {
                                sub_compiler.compile_stmt(&Stmt::Die(Expr::Literal(Value::str(
                                    "Implicit placeholder parameters are not available in bare nested blocks"
                                        .to_string(),
                                ))));
                            } else {
                                sub_compiler.compile_block_inline(stmts);
                            }
                            continue;
                        }
                        Stmt::VarDecl { name, .. } => {
                            sub_compiler.compile_stmt(stmt);
                            // VarDecl as last statement returns the variable value
                            if let Some(&slot) = sub_compiler.local_map.get(name) {
                                sub_compiler.code.emit(OpCode::GetLocal(slot));
                            } else {
                                sub_compiler.emit_nil_value();
                            }
                            continue;
                        }
                        _ => {}
                    }
                }
                sub_compiler.compile_stmt(stmt);
            }
        }
        // Transfer any compiled functions from the closure to the parent
        for (k, v) in sub_compiler.compiled_functions {
            self.compiled_functions.insert(k, v);
        }
        sub_compiler.code
    }

    /// Check if the body uses @_ or %_ legacy argument variables.
    fn body_uses_legacy_args(body: &[Stmt]) -> bool {
        let body_str = format!("{:?}", body);
        body_str.contains("\"@_\"") || body_str.contains("\"%_\"")
    }

    fn is_definite_return_spec(spec: &str) -> bool {
        let s = spec.trim();
        if s.is_empty() {
            return false;
        }
        if s.starts_with('$')
            || s.starts_with('\"')
            || s.starts_with('\'')
            || s.chars().next().is_some_and(|c| c.is_ascii_digit())
            || (s.starts_with('-') && s[1..].chars().next().is_some_and(|c| c.is_ascii_digit()))
        {
            return true;
        }
        matches!(s, "Nil" | "True" | "False" | "Empty" | "pi" | "e" | "tau")
            || s.chars().next().is_some_and(|c| c.is_ascii_lowercase())
    }

    fn emit_nil_value(&mut self) {
        let nil_idx = self.code.add_constant(Value::Nil);
        self.code.emit(OpCode::LoadConst(nil_idx));
    }

    fn compile_stmts_value(&mut self, stmts: &[Stmt]) {
        let saved = self.push_dynamic_scope_lexical();
        if stmts.is_empty() {
            self.emit_nil_value();
            self.pop_dynamic_scope_lexical(saved);
            return;
        }
        // If the block contains CATCH/CONTROL, wrap in implicit try so
        // exceptions are handled (any Raku block can act as a try block).
        if Self::has_catch_or_control(stmts) {
            self.compile_try(stmts, &None);
            self.pop_dynamic_scope_lexical(saved);
            return;
        }
        for (i, stmt) in stmts.iter().enumerate() {
            let is_last = i == stmts.len() - 1;
            if is_last {
                match stmt {
                    Stmt::Expr(expr) => self.compile_expr(expr),
                    Stmt::If {
                        cond,
                        then_branch,
                        else_branch,
                        binding_var,
                    } if binding_var.is_none() => {
                        self.compile_if_value(cond, then_branch, else_branch)
                    }
                    Stmt::Block(inner) | Stmt::SyntheticBlock(inner) => {
                        self.compile_block_inline(inner)
                    }
                    Stmt::VarDecl { name, .. } => {
                        self.compile_stmt(stmt);
                        // VarDecl returns the variable value (like Raku)
                        if let Some(&slot) = self.local_map.get(name) {
                            self.code.emit(OpCode::GetLocal(slot));
                        } else {
                            self.emit_nil_value();
                        }
                    }
                    _ => {
                        self.compile_stmt(stmt);
                        self.emit_nil_value();
                    }
                }
            } else {
                self.compile_stmt(stmt);
            }
        }
        self.pop_dynamic_scope_lexical(saved);
    }

    pub(super) fn compile_if_value(
        &mut self,
        cond: &Expr,
        then_branch: &[Stmt],
        else_branch: &[Stmt],
    ) {
        // Check for heredoc scope violations before compiling
        if let Some(err) = self.check_heredoc_scope_errors(then_branch) {
            let idx = self.code.add_constant(err);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die);
            return;
        }
        if let Some(err) = self.check_heredoc_scope_errors(else_branch) {
            let idx = self.code.add_constant(err);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die);
            return;
        }
        self.compile_expr(cond);
        let jump_else = self.code.emit(OpCode::JumpIfFalse(0));
        self.compile_stmts_value(then_branch);
        let jump_end = self.code.emit(OpCode::Jump(0));
        self.code.patch_jump(jump_else);
        if else_branch.is_empty() {
            let empty_idx = self.code.add_constant(Value::slip(vec![]));
            self.code.emit(OpCode::LoadConst(empty_idx));
        } else {
            self.compile_stmts_value(else_branch);
        }
        self.code.patch_jump(jump_end);
    }

    /// Check if a list of statements contains a CATCH or CONTROL block.
    pub(super) fn has_catch_or_control(stmts: &[Stmt]) -> bool {
        stmts
            .iter()
            .any(|s| matches!(s, Stmt::Catch(_) | Stmt::Control(_)))
    }

    pub(super) fn body_mutates_topic(stmts: &[Stmt]) -> bool {
        fn expr_mutates_topic(expr: &Expr) -> bool {
            match expr {
                Expr::AssignExpr { name, .. } => name == "_",
                Expr::Unary { expr, .. } => expr_mutates_topic(expr),
                Expr::Binary { left, right, .. } => {
                    expr_mutates_topic(left) || expr_mutates_topic(right)
                }
                Expr::MethodCall { target, args, .. } => {
                    expr_mutates_topic(target) || args.iter().any(expr_mutates_topic)
                }
                Expr::Call { args, .. } => args.iter().any(expr_mutates_topic),
                Expr::IndexAssign {
                    target,
                    index,
                    value,
                } => {
                    expr_mutates_topic(target)
                        || expr_mutates_topic(index)
                        || expr_mutates_topic(value)
                }
                _ => false,
            }
        }

        fn stmt_mutates_topic(stmt: &Stmt) -> bool {
            match stmt {
                Stmt::Assign { name, .. } => name == "_",
                Stmt::Expr(expr) => expr_mutates_topic(expr),
                Stmt::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    super::Compiler::body_mutates_topic(then_branch)
                        || super::Compiler::body_mutates_topic(else_branch)
                }
                Stmt::While { body, .. }
                | Stmt::Block(body)
                | Stmt::SyntheticBlock(body)
                | Stmt::Catch(body)
                | Stmt::Control(body)
                | Stmt::When { body, .. }
                | Stmt::Given { body, .. }
                | Stmt::Default(body) => super::Compiler::body_mutates_topic(body),
                Stmt::For { body, .. } => super::Compiler::body_mutates_topic(body),
                _ => false,
            }
        }

        stmts.iter().any(stmt_mutates_topic)
    }

    /// Compile a block body, automatically wrapping in implicit try if it contains
    /// CATCH or CONTROL blocks. This should be used for any block context (bare blocks,
    /// if branches, loop bodies, sub bodies) to ensure CATCH/CONTROL are not silently ignored.
    pub(super) fn compile_body_with_implicit_try(&mut self, stmts: &[Stmt]) {
        let saved = self.push_dynamic_scope_lexical();
        if Self::has_catch_or_control(stmts) {
            self.compile_try(stmts, &None);
            self.code.emit(OpCode::Pop);
        } else {
            for s in stmts {
                self.compile_stmt(s);
            }
        }
        self.pop_dynamic_scope_lexical(saved);
    }

    /// Compile Expr::Try { body, catch } to TryCatch opcode.
    pub(super) fn compile_try(&mut self, body: &[Stmt], catch: &Option<Vec<Stmt>>) {
        let saved = self.push_dynamic_scope_lexical();
        // Separate CATCH/CONTROL blocks from body.
        let mut main_stmts = Vec::new();
        let mut catch_stmts = catch.clone();
        let mut control_stmts: Option<Vec<Stmt>> = None;
        for stmt in body {
            if let Stmt::Catch(catch_body) = stmt {
                catch_stmts = Some(catch_body.clone());
            } else if let Stmt::Control(control_body) = stmt {
                control_stmts = Some(control_body.clone());
            } else {
                main_stmts.push(stmt.clone());
            }
        }
        let has_explicit_catch = catch_stmts.is_some();
        // Emit TryCatch placeholder.
        let try_idx = self.code.emit(OpCode::TryCatch {
            catch_start: 0,
            control_start: 0,
            body_end: 0,
            explicit_catch: has_explicit_catch,
        });
        // Compile main body (last Stmt::Expr/Call leaves value on stack)
        let mut main_leaves_value = false;
        if Self::has_block_enter_leave_phasers(&main_stmts) {
            self.compile_stmt(&Stmt::Block(main_stmts.clone()));
            self.compile_expr(&Expr::Var("_".to_string()));
            main_leaves_value = true;
        } else {
            for (i, stmt) in main_stmts.iter().enumerate() {
                let is_last = i == main_stmts.len() - 1;
                if is_last {
                    if let Stmt::Expr(expr) = stmt {
                        self.compile_expr(expr);
                        main_leaves_value = true;
                        continue;
                    } else if let Stmt::Call { name, args } = stmt {
                        let rewritten_args = Self::rewrite_stmt_call_args(&name.resolve(), args);
                        let positional_only = rewritten_args
                            .iter()
                            .all(|arg| matches!(arg, CallArg::Positional(_)));

                        if positional_only {
                            let expr_args: Vec<Expr> = rewritten_args
                                .iter()
                                .filter_map(|arg| match arg {
                                    CallArg::Positional(expr) => Some(expr.clone()),
                                    _ => None,
                                })
                                .collect();
                            self.compile_expr(&Expr::Call {
                                name: *name,
                                args: expr_args,
                            });
                            main_leaves_value = true;
                            continue;
                        }

                        let has_slip = rewritten_args
                            .iter()
                            .any(|arg| matches!(arg, CallArg::Slip(_)));
                        if has_slip {
                            let mut regular_count = 0u32;
                            for arg in &rewritten_args {
                                match arg {
                                    CallArg::Positional(expr) => {
                                        self.compile_call_arg(expr);
                                        regular_count += 1;
                                    }
                                    CallArg::Named {
                                        name: n,
                                        value: Some(expr),
                                    } => {
                                        self.compile_expr(&Expr::Literal(Value::str(n.clone())));
                                        self.compile_expr(expr);
                                        self.code.emit(OpCode::MakePair);
                                        regular_count += 1;
                                    }
                                    CallArg::Named {
                                        name: n,
                                        value: None,
                                    } => {
                                        self.compile_expr(&Expr::Literal(Value::str(n.clone())));
                                        self.compile_expr(&Expr::Literal(Value::Bool(true)));
                                        self.code.emit(OpCode::MakePair);
                                        regular_count += 1;
                                    }
                                    CallArg::Slip(_) | CallArg::Invocant(_) => {}
                                }
                            }
                            for arg in &rewritten_args {
                                if let CallArg::Slip(expr) = arg {
                                    self.compile_expr(expr);
                                }
                            }
                            let name_idx = self.code.add_constant(Value::str(name.resolve()));
                            self.code.emit(OpCode::CallFuncSlip {
                                name_idx,
                                regular_arity: regular_count,
                                arg_sources_idx: None,
                            });
                            main_leaves_value = true;
                            continue;
                        }

                        for arg in &rewritten_args {
                            match arg {
                                CallArg::Positional(expr) => self.compile_call_arg(expr),
                                CallArg::Named {
                                    name,
                                    value: Some(expr),
                                } => {
                                    self.compile_expr(&Expr::Literal(Value::str(name.clone())));
                                    self.compile_expr(expr);
                                    self.code.emit(OpCode::MakePair);
                                }
                                CallArg::Named { name, value: None } => {
                                    self.compile_expr(&Expr::Literal(Value::str(name.clone())));
                                    self.compile_expr(&Expr::Literal(Value::Bool(true)));
                                    self.code.emit(OpCode::MakePair);
                                }
                                CallArg::Slip(_) | CallArg::Invocant(_) => unreachable!(),
                            }
                        }
                        let name_idx = self.code.add_constant(Value::str(name.resolve()));
                        let arg_sources_idx = rewritten_args
                            .iter()
                            .map(|arg| match arg {
                                CallArg::Positional(expr) => Some(expr),
                                _ => None,
                            })
                            .collect::<Option<Vec<&Expr>>>()
                            .and_then(|exprs| {
                                let owned: Vec<Expr> = exprs.into_iter().cloned().collect();
                                self.add_arg_sources_constant(&owned)
                            });
                        self.code.emit(OpCode::CallFunc {
                            name_idx,
                            arity: rewritten_args.len() as u32,
                            arg_sources_idx,
                        });
                        main_leaves_value = true;
                        continue;
                    }
                }
                self.compile_stmt(stmt);
            }
        }
        if !main_leaves_value {
            self.code.emit(OpCode::LoadNil);
        }
        // Jump over catch/control on success.
        let jump_end = self.code.emit(OpCode::Jump(0));
        // Patch catch_start.
        self.code.patch_try_catch_start(try_idx);
        // Compile catch block.
        let mut jump_after_catch = None;
        if let Some(ref catch_body) = catch_stmts {
            for stmt in catch_body {
                self.compile_stmt(stmt);
            }
            if control_stmts.is_some() {
                jump_after_catch = Some(self.code.emit(OpCode::Jump(0)));
            }
        }
        // catch result is Nil
        self.code.emit(OpCode::LoadNil);
        // Patch control_start.
        self.code.patch_try_control_start(try_idx);
        // Compile control block.
        if let Some(ref control_body) = control_stmts {
            for stmt in control_body {
                self.compile_stmt(stmt);
            }
            // control result is Nil
            self.code.emit(OpCode::LoadNil);
        }
        // Patch body_end and jump targets.
        self.code.patch_try_body_end(try_idx);
        self.code.patch_jump(jump_end);
        if let Some(j) = jump_after_catch {
            self.code.patch_jump(j);
        }
        self.pop_dynamic_scope_lexical(saved);
    }

    pub(super) fn compile_do_block_expr(&mut self, body: &[Stmt], label: &Option<String>) {
        // If the do block contains CATCH/CONTROL, compile as try so exceptions are handled.
        if Self::has_catch_or_control(body) {
            self.compile_try(body, &None);
            return;
        }
        // If the do block contains ENTER/LEAVE/KEEP/UNDO phasers, wrap in
        // DoBlockExpr + BlockScope so phaser semantics are preserved.
        if Self::has_block_enter_leave_phasers(body) {
            let do_idx = self.code.emit(OpCode::DoBlockExpr {
                body_end: 0,
                label: label.clone(),
                scope_isolate: false,
            });
            let saved = self.push_dynamic_scope_lexical();
            let idx = self.code.emit(OpCode::BlockScope {
                pre_end: 0,
                enter_end: 0,
                body_end: 0,
                keep_start: 0,
                undo_start: 0,
                post_start: 0,
                end: 0,
            });
            // PRE phasers
            Self::compile_pre_phasers(self, body);
            self.code.patch_block_pre_end(idx);
            // ENTER phasers
            for s in body {
                if let Stmt::Phaser {
                    kind: PhaserKind::Enter,
                    body: ph_body,
                } = s
                {
                    for inner in ph_body {
                        self.compile_stmt(inner);
                    }
                }
            }
            self.code.patch_block_enter_end(idx);
            // Body (filter out phasers)
            let body_stmts: Vec<&Stmt> = body
                .iter()
                .filter(|s| {
                    !matches!(
                        s,
                        Stmt::Phaser {
                            kind: PhaserKind::Enter
                                | PhaserKind::Leave
                                | PhaserKind::Keep
                                | PhaserKind::Undo
                                | PhaserKind::Pre
                                | PhaserKind::Post,
                            ..
                        }
                    )
                })
                .collect();
            for (i, s) in body_stmts.iter().enumerate() {
                let is_last = i == body_stmts.len() - 1;
                if is_last {
                    self.compile_last_stmt_as_topic(s);
                } else {
                    self.compile_stmt(s);
                }
            }
            self.code.patch_block_body_end(idx);
            // KEEP phasers
            self.code.patch_block_keep_start(idx);
            for s in body.iter().rev() {
                if let Stmt::Phaser {
                    kind,
                    body: ph_body,
                } = s
                    && matches!(kind, PhaserKind::Leave | PhaserKind::Keep)
                {
                    for inner in ph_body {
                        self.compile_stmt(inner);
                    }
                }
            }
            // UNDO phasers
            self.code.patch_block_undo_start(idx);
            for s in body.iter().rev() {
                if let Stmt::Phaser {
                    kind,
                    body: ph_body,
                } = s
                    && matches!(kind, PhaserKind::Leave | PhaserKind::Undo)
                {
                    for inner in ph_body {
                        self.compile_stmt(inner);
                    }
                }
            }
            // POST phasers
            self.code.patch_block_post_start(idx);
            Self::compile_post_phasers(self, body);
            self.code.patch_loop_end(idx);
            self.pop_dynamic_scope_lexical(saved);
            self.code.patch_body_end(do_idx);
            return;
        }
        let idx = self.code.emit(OpCode::DoBlockExpr {
            body_end: 0,
            label: label.clone(),
            scope_isolate: false,
        });
        self.compile_block_inline(body);
        self.code.patch_body_end(idx);
    }

    pub(super) fn compile_do_block_expr_scoped(&mut self, body: &[Stmt], label: &Option<String>) {
        let idx = self.code.emit(OpCode::DoBlockExpr {
            body_end: 0,
            label: label.clone(),
            scope_isolate: true,
        });
        self.compile_block_inline(body);
        self.code.patch_body_end(idx);
    }

    pub(super) fn compile_do_if_expr(
        &mut self,
        cond: &Expr,
        then_branch: &[Stmt],
        else_branch: &[Stmt],
    ) {
        self.compile_expr(cond);
        let jump_else = self.code.emit(OpCode::JumpIfFalse(0));
        self.compile_block_inline(then_branch);
        let jump_end = self.code.emit(OpCode::Jump(0));
        self.code.patch_jump(jump_else);

        if else_branch.is_empty() {
            let empty_idx = self.code.add_constant(Value::slip(vec![]));
            self.code.emit(OpCode::LoadConst(empty_idx));
        } else if else_branch.len() == 1 {
            if let Stmt::If {
                cond: inner_cond,
                then_branch: inner_then,
                else_branch: inner_else,
                ..
            } = &else_branch[0]
            {
                self.compile_do_if_expr(inner_cond, inner_then, inner_else);
            } else {
                self.compile_block_inline(else_branch);
            }
        } else {
            self.compile_block_inline(else_branch);
        }
        self.code.patch_jump(jump_end);
    }

    fn compile_collected_loop_body(&mut self, body: &[Stmt]) {
        self.compile_stmts_value(body);
    }

    /// Compile `do for` expression: like a for loop but collects each iteration result.
    pub(super) fn compile_do_for_expr(
        &mut self,
        iterable: &Expr,
        param: &Option<String>,
        param_def: &Option<crate::ast::ParamDef>,
        params: &[String],
        body: &[Stmt],
        label: &Option<String>,
    ) {
        // Parser currently lowers labeled `do { ... }` / labeled bare blocks into
        // a dummy single-iteration `for Nil` with a label. Preserve block semantics
        // here so control flow like `LABEL.leave(...)` returns the block value.
        if param.is_none()
            && params.is_empty()
            && matches!(
                iterable,
                Expr::ArrayLiteral(items)
                    if items.len() == 1
                        && matches!(items[0], Expr::Literal(Value::Nil))
            )
        {
            self.compile_do_block_expr(body, label);
            return;
        }

        let (_pre_stmts, mut loop_body, _post_stmts) =
            self.expand_loop_phasers(body, label.as_deref());
        let param_idx = param
            .as_ref()
            .map(|p| self.code.add_constant(Value::str(p.clone())));
        let bind_stmts = Self::build_for_bind_stmts(param, param_def, param_idx, params);
        if !bind_stmts.is_empty() {
            let mut merged = bind_stmts;
            merged.extend(loop_body);
            loop_body = merged;
        }
        let has_rw = param_def
            .as_ref()
            .is_some_and(|def| def.traits.iter().any(|t| t == "rw"));
        let arity = if !params.is_empty() {
            params.len() as u32
        } else {
            1
        };
        let normalized_iterable = Self::normalize_for_iterable(iterable);
        self.compile_expr(&normalized_iterable);
        if let Some(source_name) = Self::for_iterable_source_name(iterable) {
            let source_idx = self.code.add_constant(Value::str(source_name));
            self.code.emit(OpCode::TagContainerRef(source_idx));
        }
        let param_local = param
            .as_ref()
            .and_then(|p| self.local_map.get(p.as_str()).copied());
        let loop_idx = self.code.emit(OpCode::ForLoop {
            param_idx,
            param_local,
            body_end: 0,
            label: label.clone(),
            arity,
            collect: true,
            restore_topic: true,
            threaded: false,
            is_rw: has_rw,
            do_writeback: has_rw,
            rw_param_names: Vec::new(),
            kv_mode: false,
        });
        self.compile_collected_loop_body(&loop_body);
        self.code.patch_loop_end(loop_idx);
    }

    /// Compile `do while` / `do until` expression: collect each iteration value.
    pub(super) fn compile_do_while_expr(
        &mut self,
        cond: &Expr,
        body: &[Stmt],
        label: &Option<String>,
    ) {
        let (pre_stmts, loop_body, post_stmts) = self.expand_loop_phasers(body, label.as_deref());
        for stmt in &pre_stmts {
            self.compile_stmt(stmt);
        }
        let loop_idx = self.code.emit(OpCode::WhileLoop {
            cond_end: 0,
            body_end: 0,
            label: label.clone(),
            collect: true,
            isolate_topic: Self::body_mutates_topic(&loop_body),
        });
        self.compile_expr(cond);
        self.code.patch_while_cond_end(loop_idx);
        self.compile_collected_loop_body(&loop_body);
        self.code.patch_loop_end(loop_idx);
        for stmt in &post_stmts {
            self.compile_stmt(stmt);
        }
    }

    /// Compile `do loop (...) { ... }` expression: collect each iteration value.
    pub(super) fn compile_do_loop_expr(
        &mut self,
        init: &Option<Box<Stmt>>,
        cond: &Option<Expr>,
        step: &Option<Expr>,
        body: &[Stmt],
        label: &Option<String>,
    ) {
        let (pre_stmts, loop_body, post_stmts) = self.expand_loop_phasers(body, label.as_deref());
        if let Some(init_stmt) = init {
            self.compile_stmt(init_stmt);
        }
        for stmt in &pre_stmts {
            self.compile_stmt(stmt);
        }
        let loop_idx = self.code.emit(OpCode::CStyleLoop {
            cond_end: 0,
            step_start: 0,
            body_end: 0,
            label: label.clone(),
            collect: true,
        });
        if let Some(cond_expr) = cond {
            self.compile_expr(cond_expr);
        } else {
            self.code.emit(OpCode::LoadTrue);
        }
        self.code.patch_cstyle_cond_end(loop_idx);
        self.compile_collected_loop_body(&loop_body);
        self.code.patch_cstyle_step_start(loop_idx);
        if let Some(step_expr) = step {
            self.compile_expr(step_expr);
            self.code.emit(OpCode::Pop);
        }
        self.code.patch_loop_end(loop_idx);
        for stmt in &post_stmts {
            self.compile_stmt(stmt);
        }
    }

    pub(super) fn do_if_branch_supported(stmts: &[Stmt]) -> bool {
        if Self::has_phasers(stmts) {
            return false;
        }
        for stmt in stmts {
            match stmt {
                Stmt::Given { .. } => return false,
                Stmt::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    if !Self::do_if_branch_supported(then_branch)
                        || !Self::do_if_branch_supported(else_branch)
                    {
                        return false;
                    }
                }
                _ => {}
            }
        }
        true
    }

    pub(super) fn has_block_enter_leave_phasers(stmts: &[Stmt]) -> bool {
        stmts.iter().any(|s| {
            matches!(
                s,
                Stmt::Phaser {
                    kind: PhaserKind::Enter
                        | PhaserKind::Leave
                        | PhaserKind::Keep
                        | PhaserKind::Undo
                        | PhaserKind::Pre
                        | PhaserKind::Post,
                    ..
                }
            )
        })
    }

    /// Check if a statement list contains `let` or `temp` statements (not inside sub/lambda bodies).
    pub(super) fn has_let_deep(stmts: &[Stmt]) -> bool {
        for s in stmts {
            match s {
                Stmt::Let { .. } | Stmt::TempMethodAssign { .. } => return true,
                Stmt::Block(inner) => {
                    if Self::has_let_deep(inner) {
                        return true;
                    }
                }
                Stmt::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    if Self::has_let_deep(then_branch) || Self::has_let_deep(else_branch) {
                        return true;
                    }
                }
                Stmt::Expr(expr) => {
                    if Self::expr_has_let_deep(expr) {
                        return true;
                    }
                }
                Stmt::Call { args, .. } => {
                    for arg in args {
                        if let crate::ast::CallArg::Positional(expr) = arg
                            && Self::expr_has_let_deep(expr)
                        {
                            return true;
                        }
                    }
                }
                _ => {}
            }
        }
        false
    }

    /// Check if a statement list contains actual `let` (not `temp`) statements.
    /// Used to decide whether the block's return value matters for save/restore.
    pub(super) fn has_real_let_deep(stmts: &[Stmt]) -> bool {
        for s in stmts {
            match s {
                Stmt::Let { is_temp: false, .. } => return true,
                Stmt::Block(inner) => {
                    if Self::has_real_let_deep(inner) {
                        return true;
                    }
                }
                Stmt::If {
                    then_branch,
                    else_branch,
                    ..
                } => {
                    if Self::has_real_let_deep(then_branch) || Self::has_real_let_deep(else_branch)
                    {
                        return true;
                    }
                }
                Stmt::Expr(expr) => {
                    if Self::expr_has_real_let_deep(expr) {
                        return true;
                    }
                }
                Stmt::Call { args, .. } => {
                    for arg in args {
                        if let crate::ast::CallArg::Positional(expr) = arg
                            && Self::expr_has_real_let_deep(expr)
                        {
                            return true;
                        }
                    }
                }
                _ => {}
            }
        }
        false
    }

    /// Check if an expression contains actual `let` (not `temp`) deep inside.
    fn expr_has_real_let_deep(expr: &Expr) -> bool {
        match expr {
            Expr::DoBlock { body, .. } => Self::has_real_let_deep(body),
            Expr::Try { body, .. } => Self::has_real_let_deep(body),
            Expr::Call { args, .. } => args.iter().any(Self::expr_has_real_let_deep),
            Expr::MethodCall { args, target, .. }
            | Expr::DynamicMethodCall { args, target, .. }
            | Expr::HyperMethodCall { args, target, .. }
            | Expr::HyperMethodCallDynamic { args, target, .. } => {
                Self::expr_has_real_let_deep(target)
                    || args.iter().any(Self::expr_has_real_let_deep)
            }
            _ => false,
        }
    }

    /// Check if a block directly contains a `use`/`no` statement (non-recursive).
    pub(super) fn has_use_stmt(stmts: &[Stmt]) -> bool {
        stmts
            .iter()
            .any(|s| matches!(s, Stmt::Use { .. } | Stmt::Import { .. } | Stmt::No { .. }))
    }

    pub(super) fn expr_has_let_deep(expr: &Expr) -> bool {
        match expr {
            Expr::DoBlock { body, .. } => Self::has_let_deep(body),
            Expr::Try { body, .. } => Self::has_let_deep(body),
            Expr::Call { args, .. } => args.iter().any(Self::expr_has_let_deep),
            Expr::MethodCall { args, target, .. }
            | Expr::DynamicMethodCall { args, target, .. }
            | Expr::HyperMethodCall { args, target, .. }
            | Expr::HyperMethodCallDynamic { args, target, .. } => {
                Self::expr_has_let_deep(target) || args.iter().any(Self::expr_has_let_deep)
            }
            _ => false,
        }
    }

    pub(super) fn next_tmp_name(&mut self, prefix: &str) -> String {
        let name = format!("${}{}", prefix, self.tmp_counter);
        self.tmp_counter += 1;
        name
    }

    fn next_targets_current_loop(
        next_label: &Option<String>,
        current_loop_label: Option<&str>,
        in_nested_loop: bool,
    ) -> bool {
        match next_label {
            Some(lbl) => current_loop_label == Some(lbl.as_str()),
            None => !in_nested_loop,
        }
    }

    fn rewrite_next_targets_in_stmt(
        stmt: &Stmt,
        current_loop_label: Option<&str>,
        next_ph: &[Stmt],
        in_nested_loop: bool,
    ) -> Stmt {
        match stmt {
            Stmt::Next(label)
                if Self::next_targets_current_loop(label, current_loop_label, in_nested_loop) =>
            {
                let mut wrapped = next_ph.to_vec();
                wrapped.push(stmt.clone());
                Stmt::SyntheticBlock(wrapped)
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                binding_var,
            } => Stmt::If {
                cond: cond.clone(),
                then_branch: Self::rewrite_next_targets_in_stmts(
                    then_branch,
                    current_loop_label,
                    next_ph,
                    in_nested_loop,
                ),
                else_branch: Self::rewrite_next_targets_in_stmts(
                    else_branch,
                    current_loop_label,
                    next_ph,
                    in_nested_loop,
                ),
                binding_var: binding_var.clone(),
            },
            Stmt::Block(body) => Stmt::Block(Self::rewrite_next_targets_in_stmts(
                body,
                current_loop_label,
                next_ph,
                in_nested_loop,
            )),
            Stmt::SyntheticBlock(body) => {
                Stmt::SyntheticBlock(Self::rewrite_next_targets_in_stmts(
                    body,
                    current_loop_label,
                    next_ph,
                    in_nested_loop,
                ))
            }
            Stmt::Label { name, stmt } => Stmt::Label {
                name: name.clone(),
                stmt: Box::new(Self::rewrite_next_targets_in_stmt(
                    stmt,
                    current_loop_label,
                    next_ph,
                    in_nested_loop,
                )),
            },
            Stmt::While { cond, body, label } => Stmt::While {
                cond: cond.clone(),
                body: Self::rewrite_next_targets_in_stmts(body, current_loop_label, next_ph, true),
                label: label.clone(),
            },
            Stmt::For {
                iterable,
                param,
                param_def,
                params,
                body,
                label,
                mode,
                rw_block,
            } => Stmt::For {
                iterable: iterable.clone(),
                param: param.clone(),
                param_def: param_def.clone(),
                params: params.clone(),
                body: Self::rewrite_next_targets_in_stmts(body, current_loop_label, next_ph, true),
                label: label.clone(),
                mode: *mode,
                rw_block: *rw_block,
            },
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                repeat,
                label,
            } => Stmt::Loop {
                init: init.clone(),
                cond: cond.clone(),
                step: step.clone(),
                body: Self::rewrite_next_targets_in_stmts(body, current_loop_label, next_ph, true),
                repeat: *repeat,
                label: label.clone(),
            },
            other => other.clone(),
        }
    }

    fn rewrite_next_targets_in_stmts(
        stmts: &[Stmt],
        current_loop_label: Option<&str>,
        next_ph: &[Stmt],
        in_nested_loop: bool,
    ) -> Vec<Stmt> {
        stmts
            .iter()
            .map(|stmt| {
                Self::rewrite_next_targets_in_stmt(
                    stmt,
                    current_loop_label,
                    next_ph,
                    in_nested_loop,
                )
            })
            .collect()
    }

    pub(super) fn expand_loop_phasers(
        &mut self,
        body: &[Stmt],
        label: Option<&str>,
    ) -> (Vec<Stmt>, Vec<Stmt>, Vec<Stmt>) {
        if !Self::has_phasers(body) {
            return (Vec::new(), body.to_vec(), Vec::new());
        }

        let mut enter_ph = Vec::new();
        let mut leave_ph = Vec::new();
        let mut keep_ph = Vec::new();
        let mut undo_ph = Vec::new();
        let mut first_ph = Vec::new();
        let mut next_ph = Vec::new();
        let mut last_ph = Vec::new();
        let mut body_main = Vec::new();
        for stmt in body {
            if let Stmt::Phaser { kind, body } = stmt {
                match kind {
                    PhaserKind::Enter => enter_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::Leave => leave_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::Keep => keep_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::Undo => undo_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::First => first_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::Next => next_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::Last => last_ph.push(Stmt::Block(body.clone())),
                    _ => body_main.push(stmt.clone()),
                }
            } else {
                body_main.push(stmt.clone());
            }
        }

        let first_var = self.next_tmp_name("__mutsu_loop_first_");
        let ran_var = self.next_tmp_name("__mutsu_loop_ran_");
        let result_var = if keep_ph.is_empty() && undo_ph.is_empty() {
            None
        } else {
            Some(self.next_tmp_name("__mutsu_loop_result_"))
        };

        let mut pre = vec![
            Stmt::VarDecl {
                name: first_var.clone(),
                expr: Expr::Literal(Value::Bool(true)),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            },
            Stmt::VarDecl {
                name: ran_var.clone(),
                expr: Expr::Literal(Value::Bool(false)),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            },
        ];
        if let Some(result_var) = result_var.clone() {
            pre.push(Stmt::VarDecl {
                name: result_var,
                expr: Expr::Literal(Value::Nil),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            });
        }

        let mut loop_body = Vec::new();
        loop_body.push(Stmt::Assign {
            name: ran_var.clone(),
            expr: Expr::Literal(Value::Bool(true)),
            op: AssignOp::Assign,
        });
        // NEXT phasers run in LIFO (reverse declaration) order per Raku spec
        next_ph.reverse();
        let body_main = if next_ph.is_empty() {
            body_main
        } else {
            Self::rewrite_next_targets_in_stmts(&body_main, label, &next_ph, false)
        };

        // FIRST runs before ENTER on the first iteration (per Raku spec)
        if !first_ph.is_empty() {
            let mut then_branch = first_ph;
            then_branch.push(Stmt::Assign {
                name: first_var.clone(),
                expr: Expr::Literal(Value::Bool(false)),
                op: AssignOp::Assign,
            });
            loop_body.push(Stmt::If {
                cond: Expr::Var(first_var.clone()),
                then_branch,
                else_branch: Vec::new(),
                binding_var: None,
            });
        }
        loop_body.extend(enter_ph);
        if let Some(result_var) = result_var.clone() {
            if let Some((last, prefix)) = body_main.split_last() {
                loop_body.extend(prefix.iter().cloned());
                match last {
                    Stmt::Expr(expr) => loop_body.push(Stmt::Assign {
                        name: result_var.clone(),
                        expr: expr.clone(),
                        op: AssignOp::Assign,
                    }),
                    other => {
                        loop_body.push(other.clone());
                        loop_body.push(Stmt::Assign {
                            name: result_var.clone(),
                            expr: Expr::Literal(Value::Nil),
                            op: AssignOp::Assign,
                        });
                    }
                }
            } else {
                loop_body.push(Stmt::Assign {
                    name: result_var.clone(),
                    expr: Expr::Literal(Value::Nil),
                    op: AssignOp::Assign,
                });
            }
        } else {
            loop_body.extend(body_main);
        }
        // next_ph was already reversed above for LIFO order
        loop_body.extend(next_ph);
        loop_body.extend(leave_ph);
        if let Some(result_var) = result_var.clone() {
            if !keep_ph.is_empty() || !undo_ph.is_empty() {
                loop_body.push(Stmt::If {
                    cond: Expr::Var(result_var.clone()),
                    then_branch: keep_ph,
                    else_branch: undo_ph,
                    binding_var: None,
                });
            }
            // Preserve loop-body value for expression contexts that collect iteration results.
            loop_body.push(Stmt::Expr(Expr::Var(result_var)));
        }

        let post = if last_ph.is_empty() {
            Vec::new()
        } else {
            vec![Stmt::If {
                cond: Expr::Var(ran_var),
                then_branch: last_ph,
                else_branch: Vec::new(),
                binding_var: None,
            }]
        };

        (pre, loop_body, post)
    }

    pub(super) fn is_dostmt_vardecl(expr: &Expr) -> bool {
        matches!(expr, Expr::DoStmt(s) if matches!(s.as_ref(), Stmt::VarDecl { .. }))
    }

    /// Check if a method call is a known mutating method on an indexed target
    /// (e.g., `%hash<key>.push(4)` or `@array[0].push(5)`).
    pub(super) fn is_mutating_method_on_index(
        target: &Expr,
        method_name: &crate::symbol::Symbol,
    ) -> bool {
        let method = method_name.resolve();
        let is_mutating = matches!(
            method.as_str(),
            "push" | "pop" | "shift" | "unshift" | "append" | "prepend" | "splice"
        );
        if !is_mutating {
            return false;
        }
        if let Expr::Index {
            target: idx_target, ..
        } = target
        {
            Self::postfix_index_name(idx_target).is_some()
        } else {
            false
        }
    }

    /// Extract the variable name from a DoStmt(VarDecl { .. }) expression,
    /// used for `++state $` patterns.
    pub(super) fn extract_vardecl_name(expr: &Expr) -> Option<String> {
        if let Expr::DoStmt(stmt) = expr
            && let Stmt::VarDecl { name, .. } = stmt.as_ref()
        {
            Some(name.clone())
        } else {
            None
        }
    }

    pub(super) fn postfix_index_name(target: &Expr) -> Option<String> {
        match target {
            Expr::HashVar(name) => Some(format!("%{}", name)),
            Expr::ArrayVar(name) => Some(format!("@{}", name)),
            Expr::Var(name) => Some(name.clone()),
            Expr::AssignExpr { name, .. } => Some(name.clone()),
            Expr::DoStmt(stmt) => match stmt.as_ref() {
                Stmt::VarDecl { name, .. } | Stmt::Assign { name, .. } => Some(name.clone()),
                _ => None,
            },
            _ => None,
        }
    }

    pub(super) fn index_assign_target_name(target: &Expr) -> Option<String> {
        match target {
            Expr::HashVar(name) => Some(format!("%{}", name)),
            Expr::ArrayVar(name) => Some(format!("@{}", name)),
            Expr::Var(name) => Some(name.clone()),
            Expr::AssignExpr { name, .. } => Some(name.clone()),
            Expr::DoStmt(stmt) => match stmt.as_ref() {
                Stmt::VarDecl { name, .. } | Stmt::Assign { name, .. } => Some(name.clone()),
                _ => None,
            },
            // (temp %hash){key} = value → treat as %hash{key} = value
            // TODO: implement proper temp save/restore semantics
            Expr::Call { name, args } if name == "temp" => {
                args.first().and_then(Self::index_assign_target_name)
            }
            _ => None,
        }
    }

    pub(super) fn index_assign_target_requires_eval(target: &Expr) -> bool {
        matches!(target, Expr::AssignExpr { .. } | Expr::DoStmt(_))
    }

    /// Extract the variable name from a method call target (e.g., `$foo.bar` → "foo").
    pub(super) fn method_call_target_var_name(target: &Expr) -> Option<String> {
        match target {
            Expr::Var(name) => Some(name.clone()),
            _ => None,
        }
    }

    pub(super) fn index_assign_nested_target(target: &Expr) -> Option<(String, &Expr)> {
        if let Expr::Index {
            target: inner_target,
            index: inner_index,
        } = target
            && let Some(name) = Self::index_assign_target_name(inner_target)
        {
            return Some((name, inner_index));
        }
        None
    }

    /// Hoist sub declarations: emit RegisterSub for all SubDecl statements
    /// before executing the rest of the block, so that `&name` references
    /// are available before the sub declaration appears in source order.
    /// Note: we only emit RegisterSub here, not compile_sub_body, because
    /// the normal SubDecl handling will compile the body. Compiling here
    /// would cause the compiled_functions map (which is flat) to be overwritten
    /// by later hoists from other scopes.
    pub(super) fn hoist_sub_decls(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            if let Stmt::SubDecl { .. } = stmt {
                let idx = self.code.add_stmt(stmt.clone());
                self.code.emit(OpCode::RegisterSub(idx));
            }
        }
    }

    /// Compile a block inline (for blocks without placeholders).
    pub(super) fn compile_block_inline(&mut self, stmts: &[Stmt]) {
        let saved = self.push_dynamic_scope_lexical();
        if stmts.is_empty() {
            self.code.emit(OpCode::LoadNil);
            self.pop_dynamic_scope_lexical(saved);
            return;
        }
        // Hoist sub declarations
        self.hoist_sub_decls(stmts);
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
