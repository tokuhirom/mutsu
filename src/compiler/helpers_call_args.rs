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

    /// Methods that STORE their closure argument for later invocation on
    /// another thread rather than calling it immediately. A closure passed to
    /// `Promise.then` runs on the thread pool when the promise is kept, so it
    /// genuinely escapes the call frame — the locals it captures-and-mutates must
    /// be promoted to shared `ContainerRef` cells (the same escape rule that
    /// `start {...}` uses). Without this a lexical the parent thread reassigns
    /// after registering the continuation is invisible to the continuation body,
    /// which reads a stale clone-time snapshot.
    ///
    /// `tap`/`act` escape too: a supply tap callback is stored and later driven
    /// by whatever thread emits into the supply (e.g. the socket-listener worker
    /// — roast S32-io/socket-recv-vs-read.t test 11). Boxing their accumulators
    /// became safe once the Proc::Async tap path was unified to a single
    /// delivery (the await-time `replay_proc_taps`; the tap-time live-channel
    /// loop used to fire the same collected output a second time, which a
    /// by-value snapshot silently swallowed but a shared cell keeps —
    /// S17-procasync/basic.t test 37).
    pub(super) fn method_escapes_closure_args(name: &str) -> bool {
        matches!(name, "then" | "tap" | "act")
    }

    /// Compile a method call argument. Named args (AssignExpr) are
    /// compiled as Pair values so they survive VM execution.
    pub(super) fn compile_method_arg(&mut self, arg: &Expr) {
        self.compile_method_arg_with_escape(arg, false);
    }

    /// Like [`compile_method_arg`] but lets the caller force the closure
    /// argument into an escaping position (for supply-consuming methods; see
    /// [`method_escapes_closure_args`]).
    pub(super) fn compile_method_arg_with_escape(&mut self, arg: &Expr, escaping: bool) {
        // A method argument is normally passed to the callee, not stored in the
        // caller frame, so a closure argument is conservatively NON-escaping
        // (the #2746 guard). `tap`/`act` override this with `escaping = true`.
        self.with_escape(escaping, |s| {
            s.with_suppress_pair_capture(true, |s| {
                if let Expr::AssignExpr { name, expr, .. } = arg {
                    // `foo(arg = 1)` in method-call argument position is treated as a
                    // named argument only for sigilless identifiers. Sigiled targets
                    // (`$x = ...`, `@x = ...`, `%x = ...`) are real assignment exprs.
                    if name.starts_with('$')
                        || name.starts_with('@')
                        || name.starts_with('%')
                        || name.starts_with('&')
                    {
                        s.compile_expr(arg);
                        if Self::needs_decont(arg) {
                            s.code.emit(OpCode::Decont);
                        }
                    } else {
                        s.compile_expr(&Expr::Literal(Value::str(name.clone())));
                        s.compile_expr(expr);
                        s.code.emit(OpCode::MakePair);
                    }
                } else {
                    s.compile_expr(arg);
                    if Self::needs_decont(arg) {
                        s.code.emit(OpCode::Decont);
                    }
                }
            })
        });
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
    fn is_named_arg_expr(expr: &Expr) -> bool {
        match expr {
            Expr::Binary { op, .. } if *op == crate::token_kind::TokenKind::FatArrow => true,
            Expr::Literal(lit) if matches!(lit.view(), crate::value::ValueView::Pair(..)) => true,
            Expr::Unary { op, .. } if *op == crate::token_kind::TokenKind::Pipe => true,
            _ => false,
        }
    }

    pub(super) fn compile_call_arg(&mut self, arg: &Expr) {
        self.compile_call_arg_with_escape(arg, false);
    }

    /// Like `compile_call_arg` but lets the caller force the argument into an
    /// escaping position. Used for thread-spawning constructs (`start`) whose
    /// block argument genuinely outlives the call frame (it is stored in a
    /// Promise and run later on another thread), so the locals it captures and
    /// mutates must be promoted to shared `ContainerRef` cells (escape analysis).
    pub(super) fn compile_call_arg_with_escape(&mut self, arg: &Expr, escaping: bool) {
        // Read-and-clear immediately: this call is the *direct* bind-target
        // compile iff the caller just set the flag for us. Clearing it up
        // front (before any nested `compile_expr`/`compile_call_arg`
        // recursion below) means a genuine call nested inside a bind RHS
        // (`my $x := f(@a[$i])`) sees `false` for its own argument compile,
        // so `f`'s `is rw` writeback machinery is untouched. See the field
        // doc on `bind_target_direct`.
        let is_bind_target = self.bind_target_direct;
        self.bind_target_direct = false;
        // A multi-dimensional subscript (`@a[0;1;2]`, `%h{"a";"b"}`) passed as a
        // raw `\target` / `is rw` argument must alias the underlying nested
        // slot, so a later `target = v` inside the callee mutates the real
        // container and is visible immediately. Emit a `MultiDimIndexBindRef`
        // that descends to the leaf and promotes it to a shared `ContainerRef`
        // cell (a missing hash leaf gets a deferred `HashEntryRef`); the callee
        // binds through it. Slice dimensions that can't collapse to one cell
        // yield a list of leaf cells, or fall back to the plain read value.
        if let Expr::MultiDimIndex { target, dimensions } = arg {
            self.compile_expr(target);
            for dim in dimensions {
                self.compile_expr(dim);
            }
            self.code
                .emit(OpCode::MultiDimIndexBindRef(dimensions.len() as u32));
            return;
        }
        // A call argument's value is normally passed to the callee, not stored
        // in the caller frame, so a closure argument is conservatively
        // NON-escaping (the #2746 guard: `map {...}` / `lives-ok {...}` must not
        // box even when the whole call sits in an escaping position like
        // `my @r = map {...}`). `start` overrides this with `escaping = true`.
        self.with_escape(escaping, |c| {
            c.with_suppress_pair_capture(true, |c| c.compile_expr(arg))
        });
        if Self::needs_decont(arg) {
            self.code.emit(OpCode::Decont);
        }
        if !Self::is_named_arg_expr(arg) {
            self.code.emit(OpCode::ContainerizePair);
        }
        let source_name = match arg {
            Expr::Var(n) => Some(n.clone()),
            Expr::ArrayVar(n) => Some(format!("@{}", n)),
            Expr::HashVar(n) => Some(format!("%{}", n)),
            Expr::CodeVar(n) => Some(format!("&{}", n)),
            Expr::BareWord(n) => Some(n.clone()),
            // Anonymous scalar assignment (`$ = value`) produces a writable
            // container, so wrap it with VarRef so `is rw` dispatch can match.
            Expr::AssignExpr { name, .. } => Some(name.clone()),
            // An inline declaration used as an argument (`$y := my $x`,
            // `f(my $z)`) parses to `DoStmt(VarDecl { .. })`. Compiling it
            // declares the variable in the enclosing scope and leaves its value
            // on the stack; wrap that with a VarRef to the freshly-declared
            // variable so a `:=` bind (or an `is rw` parameter) can alias the
            // new container rather than snapshotting its value. The `VarDecl`
            // `name` already carries the sigil convention WrapVarRef expects
            // ("x" for `$x`, "@x" for `@x`, "%y" for `%y`).
            Expr::DoStmt(stmt) => match stmt.as_ref() {
                Stmt::VarDecl { name, .. } => Some(name.clone()),
                _ => None,
            },
            _ => None,
        };
        // For Index expressions, create temp variables for `is rw` writeback
        // and wrap with VarRef so `is rw` parameters can bind through.
        if matches!(arg, Expr::Index { .. }) && is_bind_target {
            // `:=` bind to an Index expression (`my $x := @a[$i]`): the Index
            // compile already promoted the element to a first-class
            // `ContainerRef` cell on the stack (IndexAutovivifyLazyTerminal /
            // array_slot_ref). Just wrap it with VarRef so SetLocal's
            // `extract_varref_binding` sees `is_bind = true`. Skip the is-rw
            // *call-argument* writeback temps entirely: there is no function
            // call to writeback after here, and those temps are
            // compile-time-fixed global names — reused verbatim on every
            // iteration of a loop wrapping this same bind statement, whose
            // "write through an existing ContainerRef" semantics would
            // corrupt the *previous* iteration's bound cell instead of
            // storing a fresh reference to this one.
            let tmp = format!("__mutsu_bind_index_ref_{}", self.code.constants.len());
            let name_idx = self.code.add_constant(Value::str(tmp));
            self.code.emit(OpCode::WrapVarRef(name_idx));
        } else if matches!(arg, Expr::Index { .. }) {
            let tmp = format!("__mutsu_index_rw_arg_{}", self.code.constants.len());
            let orig = format!("__mutsu_index_rw_orig_{}", self.code.constants.len());
            let tmp_idx = self.code.add_constant(Value::str(tmp.clone()));
            let orig_idx = self.code.add_constant(Value::str(orig.clone()));
            self.code.emit(OpCode::Dup);
            self.code.emit(OpCode::SetGlobal(tmp_idx));
            self.code.emit(OpCode::Dup);
            self.code.emit(OpCode::SetGlobal(orig_idx));
            self.pending_index_rw_writebacks
                .push((arg.clone(), tmp.clone(), orig.clone()));
            let name_idx = self.code.add_constant(Value::str(tmp));
            self.code.emit(OpCode::WrapVarRef(name_idx));
        } else if let Some(name) = source_name {
            let name_idx = self.code.add_constant(Value::str(name));
            self.code.emit(OpCode::WrapVarRef(name_idx));
        } else if is_bind_target && matches!(arg, Expr::MethodCall { .. }) {
            // `:=` bind to a method-call RHS (`my $ref := $obj.attr`): flag the
            // dispatch so a public attribute accessor returns the attribute
            // slot's `ContainerRef` cell instead of a value copy — the bound
            // variable then aliases the attribute container (writes through
            // either side are seen by both). A non-accessor method ignores the
            // flag and the bind degrades to today's bind-by-value.
            self.mark_trailing_method_call_as_accessor_ref();
        }
    }

    /// Insert a `MarkAccessorRefContext` immediately before the trailing
    /// `CallMethod`/`CallMethodMut` op (skipping the post-call `Decont` /
    /// `ContainerizePair` the arg compile may have appended), so that ONE
    /// dispatch sees the accessor-ref flag. Inserting (rather than emitting
    /// after the fact) is safe here: any jump patched to the call op's old
    /// index now lands on the marker and falls through to the same call.
    /// No-op when the compiled tail is not a method call.
    pub(super) fn mark_trailing_method_call_as_accessor_ref(&mut self) {
        let mut i = self.code.ops.len();
        while i > 0 {
            match &self.code.ops[i - 1] {
                OpCode::Decont | OpCode::ContainerizePair => i -= 1,
                OpCode::CallMethod { .. } | OpCode::CallMethodMut { .. } => {
                    self.code.ops.insert(i - 1, OpCode::MarkAccessorRefContext);
                    return;
                }
                _ => return,
            }
        }
    }

    /// Emit writeback code for Index expressions passed as function arguments.
    /// After a function call, if any `is rw` parameter modified the temp variable,
    /// we write the new value back to the original hash/array slot.
    /// Only writes back when the temp value differs from the original value
    /// (using `===` identity check).
    pub(super) fn emit_index_rw_writebacks(&mut self) {
        let writebacks = std::mem::take(&mut self.pending_index_rw_writebacks);
        if writebacks.is_empty() {
            return;
        }
        for (index_expr, tmp_name, orig_name) in writebacks {
            if let Expr::Index {
                target,
                index,
                is_positional,
            } = &index_expr
            {
                // Save the call result
                let result_tmp = format!("__mutsu_call_result_{}", self.code.constants.len());
                let result_idx = self.code.add_constant(Value::str(result_tmp));
                self.code.emit(OpCode::SetGlobal(result_idx));

                // Compare current temp value with original value.
                // If they're identical (===), skip writeback.
                let tmp_idx = self.code.add_constant(Value::str(tmp_name.clone()));
                let orig_idx = self.code.add_constant(Value::str(orig_name));
                self.code.emit(OpCode::GetGlobal(tmp_idx));
                self.code.emit(OpCode::GetGlobal(orig_idx));
                self.code.emit(OpCode::StrictEq);
                // If equal (True), skip writeback
                let skip_idx = self.code.emit(OpCode::JumpIfTrue(0));
                // Values differ: pop comparison result and do the writeback
                self.code.emit(OpCode::Pop); // pop False from StrictEq
                let writeback = Expr::IndexAssign {
                    target: target.clone(),
                    index: index.clone(),
                    value: Box::new(Expr::Var(tmp_name)),
                    is_positional: *is_positional,
                };
                self.compile_expr(&writeback);
                self.code.emit(OpCode::Pop); // discard assignment result
                let jump_to_restore = self.code.emit(OpCode::Jump(0));
                // Skip target: pop True from StrictEq
                self.code.patch_jump(skip_idx);
                self.code.emit(OpCode::Pop); // pop True from StrictEq
                // Restore point
                self.code.patch_jump(jump_to_restore);
                self.code.emit(OpCode::GetGlobal(result_idx));
            }
        }
    }

    pub(super) fn stmt_has_placeholder(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Expr(e) | Stmt::Return(e) | Stmt::Die(e) | Stmt::Fail(e) | Stmt::Take(e, _) => {
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
            Expr::Index { target, index, .. } | Expr::IndexAssign { target, index, .. } => {
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
