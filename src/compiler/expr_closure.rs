use super::*;
use crate::symbol::Symbol;
use std::sync::Arc;

impl Compiler {
    /// Build an `X::Placeholder::Mainline` (`kind == "mainline"`) or
    /// `X::Placeholder::Block` exception value for a placeholder variable used
    /// where no signature-capable block can capture it.
    pub(super) fn placeholder_scope_error(kind: &str, placeholder: &str) -> Value {
        let (type_name, message) = if kind == "mainline" {
            (
                "X::Placeholder::Mainline",
                format!(
                    "Cannot use placeholder parameter {} outside of a sub or block",
                    placeholder
                ),
            )
        } else {
            (
                "X::Placeholder::Block",
                format!(
                    "Placeholder variable '{}' may not be used here because the \
                     surrounding block does not take a signature.",
                    placeholder
                ),
            )
        };
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(message));
        attrs.insert(
            "placeholder".to_string(),
            Value::str(placeholder.to_string()),
        );
        Value::make_instance(Symbol::intern(type_name), attrs)
    }

    /// If `body` uses a placeholder variable directly (not captured by any inner
    /// signature-capable block), emit an `X::Placeholder::Block` die and return
    /// true. Used for class/role bodies, which do not take a signature.
    pub(super) fn emit_block_placeholder_die(&mut self, body: &[Stmt]) -> bool {
        if let Some(ph) = crate::ast::collect_unattached_placeholders(body)
            .into_iter()
            .next()
        {
            let err = Self::placeholder_scope_error("block", &ph);
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
            let compiled = if is_block {
                self.compile_closure_body(&[], &[], body)
            } else {
                self.compile_routine_closure_body(&[], &[], body)
            };
            let esc = self.escaping_position;
            let cc_idx = self.code.add_closure_code(compiled, esc);
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
                self.compile_routine_closure_body(&placeholders, &[], body)
            };
            let esc = self.escaping_position;
            let cc_idx = self.code.add_closure_code(compiled, esc);
            let idx = self.code.add_stmt(Stmt::Block(body.to_vec()));
            self.code
                .emit(OpCode::MakeAnonSub(idx, Some(cc_idx), is_block));
        }
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
        // Placeholders cannot appear in blocks/subs with explicit signatures
        {
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
            let body_placeholders = crate::ast::collect_placeholders(body);
            if has_explicit_sig && !body_placeholders.is_empty() {
                let ph_name = &body_placeholders[0];
                let display = if let Some(stripped) = ph_name.strip_prefix('^') {
                    format!("$^{}", stripped)
                } else {
                    format!("${}", ph_name)
                };
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
        // Pointy blocks are NOT routine boundaries for `return`.
        let mut compiled = if is_pointy {
            self.compile_closure_body(params, param_defs, body)
        } else {
            self.compile_routine_closure_body(params, param_defs, body)
        };
        if is_pointy {
            compiled.is_pointy_block = true;
        }
        let esc = self.escaping_position;
        let cc_idx = self.code.add_closure_code(compiled, esc);
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
    ) {
        let params: Vec<String> = if param.is_empty() {
            Vec::new()
        } else {
            vec![param.to_string()]
        };
        let mut compiled = self.compile_closure_body(&params, &[], body);
        // A pointy block (`-> $x {...}`) is a `Block`, not a `Sub`. A WhateverCode
        // (`*+1`, also an `Expr::Lambda`) is tagged separately via callable_type.
        if !is_whatever_code {
            compiled.is_pointy_block = true;
        }
        let esc = self.escaping_position;
        let cc_idx = self.code.add_closure_code(compiled, esc);
        let idx = self.code.add_stmt(Stmt::SubDecl {
            name: Symbol::intern(""),
            name_expr: None,
            params: params.clone(),
            param_defs: Vec::new(),
            return_type: None,
            associativity: None,
            precedence_trait: None,
            signature_alternates: Vec::new(),
            body: body.to_vec(),
            multi: false,
            is_rw: false,
            is_raw: false,
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
                v,
                Value::Int(_) | Value::Str(_) | Value::Num(_) | Value::Bool(_)
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
                        Expr::Literal(Value::Regex(Arc::new(pattern))),
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
            && let Expr::Literal(Value::Str(key_name)) = index
        {
            self.compile_expr(value);
            let stash_name_idx = self.code.add_constant(Value::str(stash_name.clone()));
            let key_name_idx = self
                .code
                .add_constant(Value::str(key_name.as_ref().clone()));
            self.code.emit(OpCode::IndexAssignPseudoStashNamed {
                stash_name_idx,
                key_name_idx,
            });
            return;
        }
        if let Some(name) = Self::index_assign_target_name(target) {
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
            });
        } else if let Some((name, chain)) = Self::index_assign_deep_nested_target(target) {
            // Deep nested index assignment (3+ levels): @a[i][j][k]... = val
            // chain contains (index_expr, is_positional) from innermost to outermost
            // We also have the IndexAssign's own (index, outer_positional) as the final level.
            let depth = (chain.len() + 1) as u32; // +1 for the outermost from IndexAssign
            // Build positional flags array: innermost to outermost
            let mut flags: Vec<Value> = chain.iter().map(|(_, p)| Value::Bool(*p)).collect();
            flags.push(Value::Bool(outer_positional));
            let positional_flags_idx = self.code.add_constant(Value::Array(
                Arc::new(crate::value::ArrayData::new(flags)),
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
            && method_args.is_empty()
            && let Some(var_name) = Self::method_call_target_var_name(method_target)
        {
            let rewritten = Expr::Call {
                name: Symbol::intern("__mutsu_index_assign_method_lvalue"),
                args: vec![
                    (**method_target).clone(),
                    Expr::Literal(Value::str(method_name.resolve().to_string())),
                    index.clone(),
                    value.clone(),
                    Expr::Literal(Value::str(var_name)),
                ],
            };
            self.compile_expr(&rewritten);
        } else if let Some(arr_name) = Self::map_rw_identity_target_name(target) {
            // @arr.map(-> $v is rw {$v})[idx] = val  →  @arr[idx] = val
            // When map's closure has an `is rw` parameter and returns it unchanged,
            // the result is a list of containers bound to the original array elements.
            // Assignment through them writes back to the original array.
            self.compile_expr(value);
            self.compile_expr(index);
            let name_idx = self.code.add_constant(Value::str(arr_name));
            self.code.emit(OpCode::IndexAssignExprNamed {
                name_idx,
                is_positional: outer_positional,
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
                            self.code.emit(OpCode::IndexAssignGeneric);
                        }
                    } else {
                        self.compile_expr(target);
                        self.compile_expr(index);
                        self.compile_expr(value);
                        self.code.emit(OpCode::IndexAssignGeneric);
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
                self.code.emit(OpCode::IndexAssignGeneric);
            }
        } else {
            // Generic fallback: compile target, then index, then value
            // and emit IndexAssignGeneric to do runtime assignment.
            self.compile_expr(target);
            self.compile_expr(index);
            self.compile_expr(value);
            self.code.emit(OpCode::IndexAssignGeneric);
        }
    }

    /// Compile MultiDimIndexAssign expression.
    pub(super) fn compile_expr_multidim_index_assign(
        &mut self,
        target: &Expr,
        dimensions: &[Expr],
        value: &Expr,
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
            });
        } else {
            // Fallback: compile target, dims, value, use generic handler
            self.compile_expr(target);
            for dim in dimensions {
                self.compile_expr(dim);
            }
            self.compile_expr(value);
            self.code
                .emit(OpCode::MultiDimIndexAssignGeneric(dimensions.len() as u32));
        }
    }

    /// Extract literal integer indices from an index expression.
    /// Returns `Some(vec![i])` for `Literal(Int(i))`, or `Some(vec![i, j, ...])`
    /// for `ArrayLiteral([Literal(Int(i)), Literal(Int(j)), ...])`.
    fn extract_literal_int_indices(index: &Expr) -> Option<Vec<i64>> {
        match index {
            Expr::Literal(Value::Int(i)) => Some(vec![*i]),
            Expr::ArrayLiteral(items) => {
                let mut result = Vec::with_capacity(items.len());
                for item in items {
                    if let Expr::Literal(Value::Int(i)) = item {
                        result.push(*i);
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
                    let idx = self.code.add_constant(Value::Int(pos as i64));
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
