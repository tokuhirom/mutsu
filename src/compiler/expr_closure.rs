use super::*;

/// Find expression-position declarations inside a synthesized WhateverCode.
/// The generated callable is transparent for lexical scoping, while explicit
/// source closures/blocks remain boundaries of their own.
fn collect_whatever_expr_decls(body: &[Stmt], out: &mut std::collections::HashSet<String>) {
    fn expr(node: &Expr, out: &mut std::collections::HashSet<String>) {
        match node {
            Expr::DoStmt(stmt) => {
                if let Stmt::VarDecl { name, is_our, .. } = stmt.as_ref()
                    && !*is_our
                {
                    out.insert(name.clone());
                }
            }
            Expr::Unary { expr: inner, .. }
            | Expr::PostfixOp { expr: inner, .. }
            | Expr::Grouped(inner) => expr(inner, out),
            Expr::Binary { left, right, .. } => {
                expr(left, out);
                expr(right, out);
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                expr(cond, out);
                expr(then_expr, out);
                expr(else_expr, out);
            }
            _ => {}
        }
    }
    for stmt in body {
        if let Stmt::Expr(e) = stmt {
            expr(e, out);
        }
    }
}
use crate::symbol::Symbol;

impl Compiler {
    /// If `body` uses a placeholder variable directly (not captured by any inner
    /// signature-capable block), emit an `X::Placeholder::Block` die and return
    /// true. Used for class/role bodies, which do not take a signature.
    pub(super) fn emit_block_placeholder_die(&mut self, body: &[Stmt]) -> bool {
        // A method always carries an implicit `*%_` (leftover named args),
        // so `%_` is a valid lexical ANYWHERE in the method body -- including
        // directly inside a nested NoSignature block (`try {}`, `loop {}`,
        // ...), not just `do {}`. Mirrors the same exemption in
        // `compile_do_block_expr` (`src/compiler/helpers_do_expr.rs`); see
        // `t/placeholder-named-in-method-do.t` and the ADR-0048 Phase 2 fix
        // this comment documents (DBIish's `DBIish::CommonTesting.
        // connect-or-skip` calls `DBIish.connect($driver-name, |%_)` inside a
        // `try {}`, which regressed without this exemption). `@_` is NOT
        // exempted -- only a METHOD gets an implicit `*%_`, never `*@_`.
        if let Some(ph) = crate::ast::collect_unattached_placeholders(body)
            .into_iter()
            .find(|ph| !(self.lexically_in_method && ph == "%_"))
        {
            let err = crate::method_signature_shared::placeholder_scope_error("block", &ph);
            let idx = self.code.add_constant(err);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die);
            true
        } else {
            false
        }
    }

    /// Compile AnonSub expression.
    /// `is_block` = true for bare blocks `{ }`, false for `sub { }`.
    /// Bare blocks are NOT routine boundaries for `return`.
    pub(super) fn compile_expr_anon_sub(&mut self, body: &[Stmt], is_rw: bool, is_block: bool) {
        if is_rw {
            // A bare `is rw` block (a `<->`-style loop body) keeps its tail
            // as a value; only an anonymous `sub ... is rw` hands out its
            // tail's container (ADR-0059 Slice 2).
            let compiled = if is_block {
                self.compile_closure_body(&[], &[], body)
            } else {
                self.compile_routine_closure_body(&[], &[], body, true)
            };
            let esc = self.escaping_position;
            let cc_idx = self.add_closure_code_baked(compiled, esc);
            let idx = self.code.add_stmt(Stmt::SubDecl {
                name: Symbol::intern(""),
                name_expr: None,
                params: Vec::new(),
                param_defs: Vec::new(),
                return_type: None,
                associativity: None,
                precedence_trait: None,
                signature_alternates: Vec::new(),
                body: body.to_vec(),
                multi: false,
                is_rw: true,
                is_raw: false,
                is_export: false,
                export_tags: Vec::new(),
                is_test_assertion: false,
                supersede: false,
                custom_traits: Vec::new(),
            });
            self.code
                .emit(OpCode::MakeAnonSubParams(idx, Some(cc_idx), false));
        } else {
            // Conflict checking, like params, uses the *shallow* collector: a
            // placeholder belongs to its innermost enclosing block, so a
            // placeholder nested in an inner closure must not be attributed to
            // this block (which would falsely flag a redeclaration against this
            // block's `my $a`).
            let block_placeholders = crate::ast::collect_placeholders_shallow(body);
            // Immediate block calls (`{ ... }(...)`) are parsed as AnonSub.
            // Apply the same placeholder conflict checks as block/sub declarations.
            if let Some(err_val) = self.check_placeholder_conflicts(&block_placeholders, body, None)
            {
                let idx = self.code.add_constant(err_val);
                self.code.emit(OpCode::LoadConst(idx));
                self.code.emit(OpCode::Die);
                return;
            }
            let placeholders = crate::ast::collect_placeholders_shallow(body);
            let compiled = if is_block {
                self.compile_closure_body(&placeholders, &[], body)
            } else {
                self.compile_routine_closure_body(&placeholders, &[], body, false)
            };
            let esc = self.escaping_position;
            let cc_idx = self.add_closure_code_baked(compiled, esc);
            let idx = self.code.add_stmt(Stmt::Block(body.to_vec()));
            self.code
                .emit(OpCode::MakeAnonSub(idx, Some(cc_idx), is_block));
        }
    }

    /// Compile `anon sub NAME ... { ... }` (a `Stmt::SubDecl` marked
    /// `__anon_decl` by the parser): compile the routine body and emit
    /// `MakeAnonSubParams` with the original decl (which carries the name)
    /// so the built `SubData` keeps `.name` — without registering or
    /// installing `&NAME` in the enclosing scope.
    pub(super) fn compile_anon_named_sub_decl(&mut self, stmt: &Stmt) {
        let Stmt::SubDecl {
            params,
            param_defs,
            body,
            is_rw,
            is_raw,
            ..
        } = stmt
        else {
            return;
        };
        if let Some(err_val) = self.check_placeholder_conflicts(params, body, None) {
            let idx = self.code.add_constant(err_val);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die);
            return;
        }
        if let Some(err_val) = Self::check_native_readonly_param_assignment(param_defs, body) {
            let idx = self.code.add_constant(err_val);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die);
            return;
        }
        let compiled =
            self.compile_routine_closure_body(params, param_defs, body, *is_rw || *is_raw);
        let esc = self.escaping_position;
        let cc_idx = self.add_closure_code_baked(compiled, esc);
        let idx = self.code.add_stmt(stmt.clone());
        self.code
            .emit(OpCode::MakeAnonSubParams(idx, Some(cc_idx), false));
    }

    /// Compile AnonSubParams expression.
    pub(super) fn compile_expr_anon_sub_params(
        &mut self,
        params: &[String],
        param_defs: &[crate::ast::ParamDef],
        return_type: &Option<String>,
        body: &[Stmt],
        is_rw: bool,
        is_whatever_code: bool,
    ) {
        // Validate for placeholder conflicts
        if let Some(err_val) = self.check_placeholder_conflicts(params, body, None) {
            let idx = self.code.add_constant(err_val);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die);
            return;
        }
        // Placeholders cannot appear in blocks/subs with explicit signatures.
        // A WhateverCode is exempt: it was synthesized from `*` and owns only its
        // `*`-derived params, so a `$^name` in its body (e.g. the `$^namespace` in
        // `{ ... $^namespace ~ * => * }`) is a free variable belonging to the
        // enclosing explicit block, not an override of the WhateverCode's own
        // signature.
        if !is_whatever_code {
            let is_placeholder_param = |p: &str| {
                let name = p
                    .strip_prefix('&')
                    .or_else(|| p.strip_prefix('@'))
                    .or_else(|| p.strip_prefix('%'))
                    .unwrap_or(p);
                name.starts_with('^') || name.starts_with(':')
            };
            let has_explicit_sig =
                params.is_empty() || params.iter().all(|p| !is_placeholder_param(p));
            // Only placeholders belonging to THIS block conflict with its
            // signature. A nested block owns the placeholders written inside it
            // (`-> $b, $i { ({ $^a + $^b }, { $^a * $^b })[$i](1, 2) }` is legal),
            // so the scan must stop at every nested `{}` — the deep
            // `collect_placeholders` used to report the inner block's own `$^a`
            // as an override of the outer signature. This is the same collector
            // the `sub` declaration check uses (`placeholder_overrides_signature_error`),
            // so — as there — the implicit slurpies it reports (`@_` / `%_`) are
            // legal when explicitly declared as parameters (`-> @_ { … @_ }`).
            let declared: std::collections::HashSet<&str> =
                params.iter().map(String::as_str).collect();
            let body_placeholders: Vec<String> = crate::ast::collect_unattached_placeholders(body)
                .into_iter()
                .filter(|ph| !declared.contains(ph.as_str()))
                .collect();
            if has_explicit_sig && !body_placeholders.is_empty() {
                // `collect_unattached_placeholders` already returns display names
                // (`$^x`, `@_`).
                let display = body_placeholders[0].clone();
                let mut attrs = std::collections::HashMap::new();
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!(
                        "Placeholder variable '{}' cannot override existing signature",
                        display
                    )),
                );
                attrs.insert("placeholder".to_string(), Value::str(display));
                let err = Value::make_instance(
                    crate::symbol::Symbol::intern("X::Signature::Placeholder"),
                    attrs,
                );
                let idx = self.code.add_constant(err);
                self.code.emit(OpCode::LoadConst(idx));
                self.code.emit(OpCode::Die);
                return;
            }
        }
        // Compile-time check: assignment to native-typed read-only params
        if let Some(err_val) = Self::check_native_readonly_param_assignment(param_defs, body) {
            let idx = self.code.add_constant(err_val);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die);
            return;
        }
        // Check if this is a pointy block (-> { }) vs a named anonymous sub.
        // Pointy blocks inject a SetLine as the first body statement.
        let is_pointy = body
            .first()
            .is_some_and(|s| matches!(s, crate::ast::Stmt::SetLine(_)));
        // Block params have Mu (not Any) as their implicit nominal type;
        // mark them here so an unpassed untyped optional seeds Mu.
        let marked_defs: Vec<crate::ast::ParamDef>;
        let param_defs: &[crate::ast::ParamDef] = if is_pointy {
            marked_defs = param_defs
                .iter()
                .map(|pd| {
                    let mut pd = pd.clone();
                    pd.mark_block_param();
                    pd
                })
                .collect();
            &marked_defs
        } else {
            param_defs
        };
        // Pointy blocks are NOT routine boundaries for `return`.
        let mut promoted_decls = std::collections::HashSet::new();
        if is_whatever_code {
            collect_whatever_expr_decls(body, &mut promoted_decls);
        }
        let mut promoted_decls: Vec<String> = promoted_decls
            .into_iter()
            .filter(|name| {
                self.local_map.contains_key(name) || self.enclosing_local_names.contains(name)
            })
            .collect();
        promoted_decls.sort();
        for name in &promoted_decls {
            let slot = self.declare_local(name);
            self.record_block_decl(name);
            self.code.emit(OpCode::LoadNil);
            self.code.emit(OpCode::MarkVarDeclContext);
            self.code.emit(OpCode::SetLocal(slot));
        }
        let mut compiled = if is_pointy {
            self.compile_closure_body(params, param_defs, body)
        } else if is_whatever_code {
            self.compile_closure_body_with_promoted_decls(params, param_defs, body, &promoted_decls)
        } else {
            // An anonymous `sub (...) is rw { ... }` hands out its tail's
            // container (ADR-0059 Slice 2); a pointy block's `is_rw` is a
            // loop-parameter trait and does not reach here.
            self.compile_routine_closure_body(params, param_defs, body, is_rw)
        };
        if is_pointy {
            compiled.is_pointy_block = true;
        }
        let esc = self.escaping_position;
        let cc_idx = self.add_closure_code_baked(compiled, esc);
        let idx = self.code.add_stmt(Stmt::SubDecl {
            name: Symbol::intern(""),
            name_expr: None,
            params: params.to_vec(),
            param_defs: param_defs.to_vec(),
            return_type: return_type.clone(),
            associativity: None,
            precedence_trait: None,
            signature_alternates: Vec::new(),
            body: body.to_vec(),
            multi: false,
            is_rw,
            is_raw: false,
            is_export: false,
            export_tags: Vec::new(),
            is_test_assertion: false,
            supersede: false,
            custom_traits: Vec::new(),
        });
        self.code.emit(OpCode::MakeAnonSubParams(
            idx,
            Some(cc_idx),
            is_whatever_code,
        ));
    }

    /// Compile Lambda expression.
    pub(super) fn compile_expr_lambda(
        &mut self,
        param: &str,
        body: &[Stmt],
        is_whatever_code: bool,
        param_sigilless: bool,
    ) {
        let params: Vec<String> = if param.is_empty() {
            Vec::new()
        } else {
            vec![param.to_string()]
        };
        // A sigilless pointy parameter (`-> \x { }`) shadows a same-named term
        // constant (e.g. the imaginary unit `i`) inside the body. Mark it with
        // the same `__mutsu_sigilless_readonly::<name>` marker that `my \x = ...`
        // uses, so a bareword read resolves the parameter, not the constant.
        let sigilless_body: Vec<Stmt>;
        let body: &[Stmt] = if param_sigilless && !param.is_empty() {
            let mut v = Vec::with_capacity(body.len() + 1);
            v.push(Stmt::MarkSigillessReadonly(param.to_string()));
            v.extend_from_slice(body);
            sigilless_body = v;
            &sigilless_body
        } else {
            body
        };
        // A `$`-sigiled single pointy-block parameter (`-> $v { }`) reaches this
        // function with an empty `param_defs` (see the module doc on
        // `bind_function_args_values`'s legacy binding path), so it never passes
        // through the general "mark parameters readonly unless `is rw`/`is copy`/
        // `is raw`" logic that a multi-param pointy block (which DOES get a real
        // `ParamDef`, even with no traits) or a named sub parameter goes through.
        // That let `-> $v { $v = 1 }` silently accept an assignment Raku rejects
        // (`X::AdHoc`, "Cannot assign to a readonly variable or a value") --
        // `sub f($x) { $x = 1 }` and `-> $v, $w { $v = 1 }` were already correctly
        // rejected.
        //
        // This is marked at the CALL SITE (`call_compiled_closure_with_topic`,
        // gated on `cc.pointy_alias_param`), NOT by injecting a `MarkReadonly`
        // prologue statement into the compiled BODY. A prologue statement runs
        // unconditionally whenever this bytecode executes -- including several
        // "fast native loop" paths (`.map`/`.grep`/`.first`, in
        // `resolution_map_grep.rs`/`resolution_map_grep_rw.rs`) that bind a
        // block's params by a direct `env.insert` and run its body via
        // `run_reuse`, bypassing `push_call_frame`/`enter_readonly_frame`
        // entirely for performance. A mark made with no readonly frame open
        // skips the undo journal (see `mark_readonly_sym_with`) and leaks
        // PERMANENTLY into any later, unrelated same-named lexical in the
        // program -- this reached CI as a real regression (SHA3's `map -> $x
        // {...}` leaking into a later `for ... -> ($x, $y)` reusing the same
        // name) before being moved here. `call_compiled_closure_with_topic`
        // always calls `push_call_frame` before binding parameters, so marking
        // there is properly scoped and rolled back on return -- and it is
        // never reached by those fast-loop paths for this closure shape, so
        // marking there also does not (and must not) affect them: whether a
        // `.map`/`.grep` block topic is writable depends on whether the
        // SOURCE item is a container, which mutsu cannot yet decide on the
        // eager path (ADR-0040) -- that is a separate, deliberately
        // unaddressed gap, not something this fix may accidentally start
        // rejecting via a shared mechanism.
        let pointy_alias_param = !is_whatever_code && !param_sigilless && !param.is_empty();
        // A single-`*` WhateverCode whose body *mutates* the `_` placeholder
        // (`*++`, `*--`, `++*`, `* =:= $x`, `*.=foo`) binds `_` `is raw`, so the
        // mutation/identity reaches the caller's container (S02 "WhateverCode
        // parameters are rw"). Read-only bodies keep `_` by value (no param_defs),
        // which avoids a topic-`_` writeback leak in `for`/grep frames.
        let wc_raw = is_whatever_code && param == "_" && whatever_lambda_body_mutates_topic(body);
        let wc_param_defs: Vec<crate::ast::ParamDef> = if wc_raw {
            vec![crate::ast::ParamDef {
                name: "_".to_string(),
                default: None,
                multi_invocant: true,
                required: false,
                named: false,
                slurpy: false,
                sigilless: false,
                type_constraint: None,
                literal_value: None,
                sub_signature: None,
                where_constraint: None,
                traits: vec!["raw".to_string()],
                double_slurpy: false,
                onearg: false,
                optional_marker: false,
                outer_sub_signature: None,
                code_signature: None,
                is_invocant: false,
                shape_constraints: None,
                block_param: false,
            }]
        } else {
            Vec::new()
        };
        let mut promoted_decls = std::collections::HashSet::new();
        if is_whatever_code {
            collect_whatever_expr_decls(body, &mut promoted_decls);
        }
        let mut promoted_decls: Vec<String> = promoted_decls
            .into_iter()
            .filter(|name| {
                self.local_map.contains_key(name) || self.enclosing_local_names.contains(name)
            })
            .collect();
        promoted_decls.sort();
        for name in &promoted_decls {
            let slot = self.declare_local(name);
            self.record_block_decl(name);
            self.code.emit(OpCode::LoadNil);
            self.code.emit(OpCode::MarkVarDeclContext);
            self.code.emit(OpCode::SetLocal(slot));
        }
        let mut compiled = if is_whatever_code {
            self.compile_closure_body_with_promoted_decls(
                &params,
                &wc_param_defs,
                body,
                &promoted_decls,
            )
        } else {
            self.compile_closure_body(&params, &wc_param_defs, body)
        };
        // A pointy block (`-> $x {...}`) is a `Block`, not a `Sub`. A WhateverCode
        // (`*+1`, also an `Expr::Lambda`) is tagged separately via callable_type.
        if !is_whatever_code {
            compiled.is_pointy_block = true;
        }
        compiled.pointy_alias_param = pointy_alias_param;
        // `supply { … }` lowers to `Supply.on-demand(-> $__mutsu_supply_emitter_N
        // { … })`, so the generated emitter parameter identifies the supply body.
        // See `CompiledCode::is_supply_block_body`.
        compiled.is_supply_block_body = param.starts_with(crate::parser::SUPPLY_EMITTER_PREFIX);
        compiled.supply_emitter_sym = compiled
            .is_supply_block_body
            .then(|| crate::symbol::Symbol::intern(param));
        let esc = self.escaping_position;
        let cc_idx = self.add_closure_code_baked(compiled, esc);
        let idx = self.code.add_stmt(Stmt::SubDecl {
            name: Symbol::intern(""),
            name_expr: None,
            params: params.clone(),
            param_defs: wc_param_defs,
            return_type: None,
            associativity: None,
            precedence_trait: None,
            signature_alternates: Vec::new(),
            body: body.to_vec(),
            multi: false,
            is_rw: false,
            is_raw: wc_raw,
            is_export: false,
            export_tags: Vec::new(),
            is_test_assertion: false,
            supersede: false,
            custom_traits: Vec::new(),
        });
        self.code
            .emit(OpCode::MakeLambda(idx, Some(cc_idx), is_whatever_code));
    }

    /// Compile IndexAssign expression.
    /// `outer_positional` is the `is_positional` flag of the outermost
    /// subscript (i.e. `[...]` vs `{...}`/`<...>`), preserved from the
    /// `IndexAssign` AST node.
    /// Compile the RHS value of an `IndexAssign`. When the value is a `:=` bind
    /// marker (`__mutsu_bind_index_value(...)`) whose RHS is itself an indexed
    /// element (`$bar[..]<..> := $foo[1]<key>`), compile it with terminal
    /// promotion (`scalar_bind_autovivify` + `bind_terminal`) so the bound
    /// element shares a `ContainerRef` cell with the source. That makes
    /// element-to-element binds propagate writes bidirectionally (nested.t
    /// 32-37). For a plain (non-bind) value, this is just `compile_expr`.
    /// Slice 2b (`docs/scalar-array-sharing.md`): detect `@aoa[i] = @row` /
    /// `%h<k> = @row` where the RHS is a whole array/hash variable. Such a plain
    /// `=` shares the source container by reference (raku stores the same Array
    /// object in the element's scalar). Returns a `:=`-bind-wrapped value so the
    /// existing index-bind machinery installs a shared `ContainerRef` cell and
    /// promotes the source; the caller additionally emits `MarkElementShare` so
    /// the runtime records the element as a value share (replace-on-reassign),
    /// not a true bind. Only `@`/`%` element targets and direct container-var
    /// RHS are handled here; a scalar source (`@aoa[i] = $x`) stays a copy.
    fn element_share_bind_value(target_name: &str, index: &Expr, value: &Expr) -> Option<Expr> {
        if !target_name.starts_with('@') && !target_name.starts_with('%') {
            return None;
        }
        // Only a single scalar subscript shares by reference. A slice / range /
        // multi-index (`@b[1,2,3] = @x`, `@b[*-3,*-2] = @x`) is a distributing
        // slice assignment, not an element reference share, so it must keep its
        // normal value-assignment path (whitelist the known single-element index
        // shapes; anything else stays a plain copy).
        let single_subscript = match index {
            Expr::Literal(v) => matches!(
                v.view(),
                crate::value::ValueView::Int(_)
                    | crate::value::ValueView::Str(_)
                    | crate::value::ValueView::Num(_)
                    | crate::value::ValueView::Bool(_)
            ),
            Expr::Var(_) | Expr::Binary { .. } | Expr::Unary { .. } => true,
            _ => false,
        };
        if !single_subscript {
            return None;
        }
        let source = match value {
            Expr::ArrayVar(n) => format!("@{n}"),
            Expr::HashVar(n) => format!("%{n}"),
            _ => return None,
        };
        // A self-reference (`%h<k> = %h`, `@a[0] = @a`) must keep the existing
        // self-ref-marker path (infinite HoH / AoA). Promoting the source to a
        // shared cell here would replace the whole container with a
        // `ContainerRef`, breaking type checks (`isa-ok %h, Hash`) and the
        // cyclic-structure reads.
        if source == target_name {
            return None;
        }
        Some(Expr::Call {
            name: Symbol::intern("__mutsu_bind_index_value"),
            args: vec![
                value.clone(),
                Expr::ArrayLiteral(vec![Expr::Literal(Value::str(source))]),
            ],
        })
    }

    fn compile_bind_index_value(&mut self, value: &Expr) {
        let is_bind = matches!(
            value,
            Expr::Call { name, .. } if *name == "__mutsu_bind_index_value"
        );
        if is_bind {
            let saved_av = self.scalar_bind_autovivify;
            let saved_term = self.bind_terminal;
            self.scalar_bind_autovivify = true;
            self.bind_terminal = true;
            // Mark the RHS as a direct bind target so an Index source
            // (`%h{k} := %d<t>`) takes the per-site `__mutsu_bind_index_ref`
            // wrap in `compile_call_arg` instead of the `is rw` call-arg
            // writeback temps (`__mutsu_index_rw_arg_N` + SetGlobal). Those
            // temps are compile-time-fixed globals reused verbatim on every
            // loop iteration, and their "write through the existing
            // ContainerRef" semantics corrupt the PREVIOUS iteration's bound
            // cell (a `for ... { %s{$k} := %d<t> }` loop had the first key's
            // element track the latest source value). There is no function
            // call after a bind RHS, so the writeback machinery is never
            // needed here.
            self.bind_target_direct = true;
            self.compile_expr(value);
            self.scalar_bind_autovivify = saved_av;
            self.bind_terminal = saved_term;
        } else {
            self.compile_expr(value);
        }
    }

    pub(super) fn compile_expr_index_assign(
        &mut self,
        target: &Expr,
        index: &Expr,
        value: &Expr,
        outer_positional: bool,
    ) {
        // Binding (`:=`) to a WhateverCode subscript (`@a[*-1] := 42`) is illegal:
        // the index is a computed slice, not a fixed container slot, so rakudo
        // throws X::Bind::Slice ("Cannot bind to Array slice"). A slice bind
        // (`@a[0,1] := ...`) and a plain variable-index bind (`@a[$i] := ...`)
        // stay valid. Emit a runtime throw so `throws-like { ... }` catches it
        // (a parse-time error would abort the whole enclosing file).
        if matches!(
            value,
            Expr::Call { name, .. } if *name == "__mutsu_bind_index_value"
        ) && matches!(
            index,
            // ADR-0033 Phase 1: the index is still an un-expanded
            // `WhateverCurry` marker here (this runs before the compiler's
            // `Expr::WhateverCurry` arm would expand it) — check that
            // directly rather than the built closure the parser used to
            // produce eagerly. (The `Lambda` arm is kept as a
            // belt-and-suspenders match; only reachable pre-ADR-0033.)
            Expr::WhateverCurry(_)
                | Expr::Lambda {
                    is_whatever_code: true,
                    ..
                }
        ) {
            // A Whatever index is always positional, so the sliced container is
            // an Array. `type` and `message` mirror what rakudo's exception
            // carries (`X::Bind::Slice.new(type => Array).message` is
            // "Cannot bind to Array slice").
            self.compile_expr(&Expr::Call {
                name: Symbol::intern("die"),
                args: vec![Expr::MethodCall {
                    target: Box::new(Expr::BareWord("X::Bind::Slice".to_string())),
                    name: Symbol::intern("new"),
                    args: vec![
                        Expr::Binary {
                            left: Box::new(Expr::Literal(Value::str("type".to_string()))),
                            op: TokenKind::FatArrow,
                            right: Box::new(Expr::BareWord("Array".to_string())),
                        },
                        Expr::Binary {
                            left: Box::new(Expr::Literal(Value::str("message".to_string()))),
                            op: TokenKind::FatArrow,
                            right: Box::new(Expr::Literal(Value::str(
                                "Cannot bind to Array slice".to_string(),
                            ))),
                        },
                    ],
                    modifier: None,
                    quoted: false,
                }],
            });
            return;
        }
        if let Expr::BareWord(name) = target
            && name == "s"
            && let Some(pattern) = Self::topic_subst_pattern_from_index(index)
        {
            let rewritten = Expr::AssignExpr {
                name: "_".to_string(),
                expr: Box::new(Expr::MethodCall {
                    target: Box::new(Expr::Var("_".to_string())),
                    name: Symbol::intern("subst"),
                    args: vec![
                        Expr::Literal(Value::regex(pattern)),
                        Expr::AnonSub {
                            body: vec![Stmt::Expr(value.clone())],
                            is_rw: false,
                            is_block: true,
                        },
                    ],
                    modifier: None,
                    quoted: false,
                }),
                is_bind: false,
            };
            self.compile_expr(&rewritten);
            return;
        }
        if let Expr::PseudoStash(stash_name) = target
            && (stash_name == "MY::" || stash_name == "PROCESS::")
            && let Expr::Literal(lit) = index
            && let Some(key_name) = lit.as_str()
        {
            self.compile_expr(value);
            let stash_name_idx = self.code.add_constant(Value::str(stash_name.clone()));
            let key_name_idx = self.code.add_constant(Value::str(key_name.to_string()));
            self.code.emit(OpCode::IndexAssignPseudoStashNamed {
                stash_name_idx,
                key_name_idx,
            });
            return;
        }
        // `OUR::.<$name> {:=,=} value` is the stash spelling of `$OUR::name
        // {:=,=} value` (roast pseudo-6c: `OUR::.<$x32> := $x`). The subscript
        // key already carries the sigil, so reconstruct the sigiled pseudo-var
        // and route through the working sigiled-OUR assignment/bind path rather
        // than the generic index-assign (which writes a throwaway stash hash and
        // silently drops the store).
        if let Expr::PseudoStash(stash_name) = target
            && stash_name == "OUR::"
            && let Expr::Literal(lit) = index
            && let Some(key) = lit.as_str()
            && key.starts_with(['$', '@', '%', '&'])
        {
            let (is_bind, rhs) = match value {
                Expr::Call { name, args } if *name == "__mutsu_bind_index_value" => (
                    true,
                    args.first().cloned().unwrap_or(Expr::Literal(Value::NIL)),
                ),
                other => (false, other.clone()),
            };
            // `$OUR::name` compiles to AssignExpr name "OUR::name" (scalar sigil
            // dropped); `@`/`%`/`&` keep their sigil ahead of the qualifier.
            let compiled_name = match key.as_bytes()[0] {
                b'$' => format!("OUR::{}", &key[1..]),
                sig => format!("{}OUR::{}", sig as char, &key[1..]),
            };
            self.compile_expr(&Expr::AssignExpr {
                name: compiled_name,
                expr: Box::new(rhs),
                is_bind,
            });
            return;
        }
        // Runtime-key PROCESS:: assignment (`PROCESS::{$k} = v`), notably how a
        // `//=` / `||=` compound assignment desugars its subscript into a temp
        // variable. Without this it would fall through to the generic path, which
        // writes into a throwaway `build_pseudo_stash` hash and silently drops the
        // store. Route it to the same env dynamic-var write as the literal-key op.
        if let Expr::PseudoStash(stash_name) = target
            && stash_name == "PROCESS::"
        {
            self.compile_expr(value);
            self.compile_expr(index);
            let stash_name_idx = self.code.add_constant(Value::str(stash_name.clone()));
            self.code
                .emit(OpCode::IndexAssignPseudoStashKeyed { stash_name_idx });
            return;
        }
        if let Some(name) = Self::index_assign_target_name(target) {
            let target_slot = self.local_map.get(&name).copied();
            if Self::index_assign_target_requires_eval(target) {
                self.compile_expr(target);
                self.code.emit(OpCode::Pop);
            }
            let share_value = Self::element_share_bind_value(&name, index, value);
            match &share_value {
                Some(bind_value) => self.compile_bind_index_value(bind_value),
                None => self.compile_bind_index_value(value),
            }
            if share_value.is_some() {
                self.code.emit(OpCode::MarkElementShare);
            }
            self.compile_expr(index);
            let name_idx = self.code.add_constant(Value::str(name));
            self.code.emit(OpCode::IndexAssignExprNamed {
                name_idx,
                is_positional: outer_positional,
                target_slot,
            });
        } else if let Some((name, chain)) = Self::index_assign_deep_nested_target(target) {
            // Deep nested index assignment (3+ levels): @a[i][j][k]... = val
            // chain contains (index_expr, is_positional) from innermost to outermost
            // We also have the IndexAssign's own (index, outer_positional) as the final level.
            let depth = (chain.len() + 1) as u32; // +1 for the outermost from IndexAssign
            // Build positional flags array: innermost to outermost
            let mut flags: Vec<Value> = chain.iter().map(|(_, p)| Value::truth(*p)).collect();
            flags.push(Value::truth(outer_positional));
            let positional_flags_idx = self.code.add_constant(Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(flags)),
                crate::value::ArrayKind::Array,
            ));
            // Stack order: [value, idx_outermost, ..., idx_innermost]
            self.compile_bind_index_value(value);
            self.compile_expr(index); // outermost
            for (idx_expr, _) in chain.iter().rev() {
                self.compile_expr(idx_expr);
            }
            let name_idx = self.code.add_constant(Value::str(name));
            self.code.emit(OpCode::IndexAssignDeepNested {
                name_idx,
                depth,
                positional_flags_idx,
            });
        } else if let Some((name, inner_index, inner_positional)) =
            Self::index_assign_nested_target(target)
        {
            // `outer_positional` (the outermost subscript flag) is passed in
            // from the IndexAssign AST node. `inner_positional` is the inner
            // Index node's flag (the subscript closer to the variable).
            self.compile_bind_index_value(value);
            self.compile_expr(index);
            self.compile_expr(inner_index);
            let name_idx = self.code.add_constant(Value::str(name));
            self.code.emit(OpCode::IndexAssignExprNested {
                name_idx,
                outer_positional,
                inner_positional,
            });
        } else if let Expr::Index {
            target: method_call_target,
            index: inner_index,
            ..
        } = target
            && let Expr::MethodCall {
                target: method_target,
                name: method_name,
                args: method_args,
                ..
            } = method_call_target.as_ref()
            && method_args.is_empty()
            && let Some(var_name) = Self::method_call_target_var_name(method_target)
        {
            // Nested subscript on a method call (e.g., $o.a[42]<foo> = 3 or $o.h<key1><key2> = val).
            // This would autovivify an intermediate value in the typed container, which Raku disallows.
            // Emit a call to __mutsu_index_assign_method_lvalue_nested that checks type constraints.
            let rewritten = Expr::Call {
                name: Symbol::intern("__mutsu_index_assign_method_lvalue_nested"),
                args: vec![
                    (**method_target).clone(),
                    Expr::Literal(Value::str(method_name.resolve().to_string())),
                    (**inner_index).clone(),
                    index.clone(),
                    value.clone(),
                    Expr::Literal(Value::str(var_name)),
                ],
            };
            self.compile_expr(&rewritten);
        } else if let Expr::MethodCall {
            target: method_target,
            name: method_name,
            args: method_args,
            ..
        } = target
            && ((method_args.is_empty() && matches!(method_target.as_ref(), Expr::Var(_)))
                || (!method_args.is_empty() && matches!(method_target.as_ref(), Expr::BareWord(_))))
            && let Some(var_name) = Self::method_call_target_var_name(method_target)
        {
            let writeback_name = if var_name.is_empty() {
                method_args
                    .first()
                    .and_then(Self::index_assign_target_name)
                    .unwrap_or_default()
            } else {
                var_name
            };
            let mut args = vec![
                (**method_target).clone(),
                Expr::Literal(Value::str(method_name.resolve().to_string())),
            ];
            if !method_args.is_empty() {
                args.push(Expr::BracketArray(method_args.clone(), false));
            }
            args.extend([
                index.clone(),
                value.clone(),
                Expr::Literal(Value::str(writeback_name)),
            ]);
            let rewritten = Expr::Call {
                name: Symbol::intern("__mutsu_index_assign_method_lvalue"),
                args,
            };
            self.compile_expr(&rewritten);
        } else if let Some(arr_name) = Self::map_rw_identity_target_name(target) {
            // @arr.map(-> $v is rw {$v})[idx] = val  →  @arr[idx] = val
            // When map's closure has an `is rw` parameter and returns it unchanged,
            // the result is a list of containers bound to the original array elements.
            // Assignment through them writes back to the original array.
            let target_slot = self.local_map.get(&arr_name).copied();
            self.compile_expr(value);
            self.compile_expr(index);
            let name_idx = self.code.add_constant(Value::str(arr_name));
            self.code.emit(OpCode::IndexAssignExprNamed {
                name_idx,
                is_positional: outer_positional,
                target_slot,
            });
        } else if let Expr::ArrayLiteral(elements) = target {
            // List construction container assignment:
            // ($var1, literal, $var2, ...)[idx] = value
            // When the target is a literal list, resolve which element is
            // being assigned and write through to the original variable.
            if let Some(indices) = Self::extract_literal_int_indices(index) {
                // Multi-index slice or single index
                if indices.len() == 1 {
                    // Single index: ($foo, "x")[0] = 23
                    let i = indices[0] as usize;
                    if i < elements.len() {
                        if let Expr::Var(name) = &elements[i] {
                            self.compile_expr(value);
                            let name_idx = self.code.add_constant(Value::str(name.clone()));
                            self.code.emit(OpCode::AssignExpr(name_idx));
                        } else {
                            // Assigning to a literal element — emit code
                            // that throws X::Assignment::RO at runtime.
                            self.compile_expr(target);
                            self.compile_expr(index);
                            self.compile_expr(value);
                            self.code.emit(OpCode::IndexAssignGeneric {
                                is_positional: outer_positional,
                            });
                        }
                    } else {
                        self.compile_expr(target);
                        self.compile_expr(index);
                        self.compile_expr(value);
                        self.code.emit(OpCode::IndexAssignGeneric {
                            is_positional: outer_positional,
                        });
                    }
                } else {
                    // Multi-index slice: ($foo, 42, $bar, 19)[0, 2] = (23, 24)
                    // Compile value first, then distribute assignments
                    self.compile_list_slice_assign(elements, &indices, value);
                }
            } else {
                // Non-literal index — fall through to generic
                self.compile_expr(target);
                self.compile_expr(index);
                self.compile_expr(value);
                self.code.emit(OpCode::IndexAssignGeneric {
                    is_positional: outer_positional,
                });
            }
        } else {
            // Generic fallback: compile target, then index, then value
            // and emit IndexAssignGeneric to do runtime assignment.
            self.compile_expr(target);
            self.compile_expr(index);
            self.compile_expr(value);
            self.code.emit(OpCode::IndexAssignGeneric {
                is_positional: outer_positional,
            });
        }
    }

    /// Compile MultiDimIndexAssign expression.
    pub(super) fn compile_expr_multidim_index_assign(
        &mut self,
        target: &Expr,
        dimensions: &[Expr],
        value: &Expr,
        is_positional: bool,
    ) {
        if let Some(var_name) = Self::index_assign_target_name(target) {
            self.compile_expr(value);
            for dim in dimensions {
                self.compile_expr(dim);
            }
            let name_idx = self.code.add_constant(Value::str(var_name));
            self.code.emit(OpCode::MultiDimIndexAssign {
                name_idx,
                ndims: dimensions.len() as u32,
                is_positional,
            });
        } else if let Some((name, chain)) = Self::index_chain_target(target) {
            // `%o<inner>{1;2} = 5`: the target is a subscript chain rooted at a
            // named variable. `MultiDimIndexAssignGeneric` would pop the chain's
            // *value* and mutate that detached copy, so an autovivified level
            // (the common case -- the chain names a slot that does not exist
            // yet) was silently dropped. Carry the root name and the chain
            // instead, so the VM walks the real container.
            let flags: Vec<Value> = chain.iter().map(|(_, p)| Value::truth(*p)).collect();
            let prefix_flags_idx = self.code.add_constant(Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(flags)),
                crate::value::ArrayKind::Array,
            ));
            // Stack: [value, prefix_innermost, ..., prefix_outermost, dim0, ...]
            self.compile_expr(value);
            for (idx_expr, _) in &chain {
                self.compile_expr(idx_expr);
            }
            for dim in dimensions {
                self.compile_expr(dim);
            }
            let name_idx = self.code.add_constant(Value::str(name));
            self.code.emit(OpCode::MultiDimIndexAssignNested {
                name_idx,
                prefix_depth: chain.len() as u32,
                prefix_flags_idx,
                ndims: dimensions.len() as u32,
                is_positional,
            });
        } else {
            // Fallback: compile target, dims, value, use generic handler
            self.compile_expr(target);
            for dim in dimensions {
                self.compile_expr(dim);
            }
            self.compile_expr(value);
            self.code.emit(OpCode::MultiDimIndexAssignGeneric {
                ndims: dimensions.len() as u32,
                is_positional,
            });
        }
    }

    /// Extract literal integer indices from an index expression.
    /// Returns `Some(vec![i])` for `Literal(Int(i))`, or `Some(vec![i, j, ...])`
    /// for `ArrayLiteral([Literal(Int(i)), Literal(Int(j)), ...])`.
    fn extract_literal_int_indices(index: &Expr) -> Option<Vec<i64>> {
        match index {
            Expr::Literal(lit) => lit.as_int().map(|i| vec![i]),
            Expr::ArrayLiteral(items) => {
                let mut result = Vec::with_capacity(items.len());
                for item in items {
                    if let Expr::Literal(lit) = item
                        && let Some(i) = lit.as_int()
                    {
                        result.push(i);
                    } else {
                        return None;
                    }
                }
                Some(result)
            }
            _ => None,
        }
    }

    /// Compile a list-slice assignment: ($foo, 42, $bar, 19)[0, 2] = (23, 24)
    /// Each index maps to an element in the target ArrayLiteral.
    /// Variable elements get assigned; literal elements cause X::Assignment::RO.
    fn compile_list_slice_assign(&mut self, elements: &[Expr], indices: &[i64], value: &Expr) {
        // Check if all indexed elements are variables (for success path)
        // or if any is a literal (for error path — dies-ok test).
        let mut all_vars = true;
        let mut has_literal = false;
        for &i in indices {
            let i = i as usize;
            if i < elements.len() && !matches!(&elements[i], Expr::Var(_)) {
                all_vars = false;
                has_literal = true;
            }
        }

        if all_vars {
            // Extract value elements: compile value, then assign each element
            // to the corresponding variable.
            // First, compute the value into an array.
            self.compile_expr(value);
            // For each index, extract element and assign to var.
            for (pos, &i) in indices.iter().enumerate() {
                let i = i as usize;
                if i < elements.len()
                    && let Expr::Var(name) = &elements[i]
                {
                    // Dup the value array, index into it, assign to var
                    self.code.emit(OpCode::Dup);
                    let idx = self.code.add_constant(Value::int(pos as i64));
                    self.code.emit(OpCode::LoadConst(idx));
                    self.code.emit(OpCode::Index {
                        is_positional: true,
                    });
                    let name_idx = self.code.add_constant(Value::str(name.clone()));
                    self.code.emit(OpCode::AssignExpr(name_idx));
                    self.code.emit(OpCode::Pop);
                }
            }
            // The value array is still on the stack.
        } else if has_literal {
            // If any indexed element is a literal, the assignment should die
            // with X::Assignment::RO since literals are immutable containers.
            let ro_call = Expr::Call {
                name: Symbol::intern("__mutsu_assignment_ro"),
                args: Vec::new(),
            };
            self.compile_expr(&ro_call);
        }
    }
}

/// Whether a single-`*` WhateverCode body (the `*` already lowered to `Var("_")`)
/// *mutates* its placeholder or depends on its container identity, requiring the
/// `_` parameter to bind `is raw`: `*++`/`*--`/`++*`/`--*`, `* =:= $x`, `*.=foo`.
fn whatever_lambda_body_mutates_topic(body: &[Stmt]) -> bool {
    body.iter().any(|stmt| match stmt {
        Stmt::Expr(e) => expr_mutates_topic(e),
        _ => false,
    })
}

fn is_topic_var(e: &Expr) -> bool {
    matches!(e, Expr::Var(name) if name == "_")
}

fn expr_refs_topic(e: &Expr) -> bool {
    if is_topic_var(e) {
        return true;
    }
    match e {
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => expr_refs_topic(expr),
        Expr::Binary { left, right, .. } => expr_refs_topic(left) || expr_refs_topic(right),
        Expr::MethodCall { target, .. } => expr_refs_topic(target),
        _ => false,
    }
}

fn expr_mutates_topic(e: &Expr) -> bool {
    match e {
        // `*++` / `*--`
        Expr::PostfixOp {
            op: TokenKind::PlusPlus | TokenKind::MinusMinus,
            expr,
        } => expr_refs_topic(expr) || expr_mutates_topic(expr),
        // `++*` / `--*`
        Expr::Unary {
            op: TokenKind::PlusPlus | TokenKind::MinusMinus,
            expr,
        } => expr_refs_topic(expr) || expr_mutates_topic(expr),
        Expr::Unary { expr, .. } => expr_mutates_topic(expr),
        Expr::PostfixOp { expr, .. } => expr_mutates_topic(expr),
        // `* =:= $x` — container identity needs the same container.
        Expr::Binary {
            op: TokenKind::Ident(name),
            left,
            right,
        } if name == "=:=" => {
            expr_refs_topic(left)
                || expr_refs_topic(right)
                || expr_mutates_topic(left)
                || expr_mutates_topic(right)
        }
        Expr::Binary { left, right, .. } => expr_mutates_topic(left) || expr_mutates_topic(right),
        // `*.=foo` — mutating method-assign on the placeholder.
        Expr::MethodCall {
            target,
            modifier: Some('='),
            ..
        } if expr_refs_topic(target) => true,
        Expr::MethodCall { target, args, .. } => {
            expr_mutates_topic(target) || args.iter().any(expr_mutates_topic)
        }
        _ => false,
    }
}
