//! One `for`-loop lowering, shared by both source positions.
//!
//! A `for` loop can appear as a statement (`for @xs { ... }`) or as an
//! expression that produces the list of its iteration values (`my @r = do for
//! @xs { ... }`, a routine's tail `for`, `(... for @xs)`). Those two positions
//! used to be compiled by two independent code paths — the `Stmt::For` arm of
//! [`Compiler::compile_stmt`] and `compile_do_for_expr` in `helpers_do_expr.rs`
//! — which each built their own [`crate::opcode::ForLoopSpec`] literal. With 30
//! fields to populate by hand, every new loop feature had to be applied twice
//! and the two copies drifted (the expression copy never emitted loop phasers,
//! never reversed a `.reverse` source's container tag, never autothreaded a
//! typed parameter, and never recorded the loop parameter as this code's own
//! declaration).
//!
//! This module is the single lowering. The only thing the two positions differ
//! in is [`ForParts::collect`]: whether each iteration's value is gathered and
//! left on the stack.

use super::*;

/// Everything a `for` construct's lowering needs, independent of the position
/// it was written in.
pub(super) struct ForParts<'a> {
    pub iterable: &'a Expr,
    pub param: &'a Option<String>,
    pub param_def: &'a Option<crate::ast::ParamDef>,
    pub params: &'a [String],
    pub params_def: &'a [crate::ast::ParamDef],
    pub body: &'a [Stmt],
    pub label: &'a Option<String>,
    pub mode: crate::ast::ForMode,
    pub rw_block: bool,
    pub explicit_zero_params: bool,
    pub is_statement_modifier: bool,
    pub uses_block_magic: bool,
    /// Expression position: collect every iteration's value and leave the
    /// resulting list on the stack. Statement position leaves nothing.
    pub collect: bool,
}

impl Compiler {
    /// The variable whose *container* a value-collecting `for` body hands back,
    /// when the body's tail statement is a bare read of one.
    ///
    /// A Raku block's value is not decontainerized, so a collecting `for` whose
    /// body ends in `$g` gathers the `Scalar` container `$g` denotes and every
    /// collected slot reads it at the point the list is consumed — after the
    /// loop, so after each iteration's `temp` restore. `do for 1..2 { temp $g =
    /// 9; $g }` is therefore `(1 1)` and not `(9 9)`, and `do for 1..2 { $g =
    /// $g + 1; $g }` is `(3 3)` and not `(2 3)`. Decontainerizing the tail
    /// (`$g + 0`) opts back out, because that expression is a value.
    ///
    /// Returning the name here makes the loop tag it with `TagContainerRef`,
    /// which is the same signal `compile_expr_assign` already emits for a tail
    /// *assignment* (`do for 1..3 { $s += $_ }` → `(6 6 6)`); the VM re-reads
    /// every tagged slot once the loop is over.
    ///
    /// Only a container that outlives the iteration qualifies:
    ///
    /// - the loop's own parameters and the topic are rebound per iteration, so
    ///   `do for 1..3 -> $i { $i }` must stay `(1 2 3)`;
    /// - so is a `my` declared anywhere in the body, hence
    ///   `do for 1..3 { my $x = $_ * 2; $x }` is `(2 4 6)`. A `state`
    ///   declaration is the exception — its storage is one cell for the whole
    ///   loop, and raku collects it as one (`(6 6 6)`, not `(1 3 6)`);
    /// - a twigil'd or punctuation name (`$*d`, `$!a`, `$^a`, `$/`) is left
    ///   alone: those do not resolve through the plain env lookup the VM's
    ///   re-read uses, and nothing here has measured them.
    fn collected_tail_container_name(
        body: &[Stmt],
        param: &Option<String>,
        params: &[String],
    ) -> Option<String> {
        let Some(Stmt::Expr(Expr::Var(name))) = body.last() else {
            return None;
        };
        if name == "_"
            || !name.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_')
            || param.as_deref() == Some(name.as_str())
            || params
                .iter()
                .any(|p| p.strip_prefix('\\').unwrap_or(p) == name)
        {
            return None;
        }
        let mut body_declared = std::collections::HashSet::new();
        crate::ast::collect_all_my_decl_names(body, &mut body_declared);
        let declared_as_state = body.iter().any(|s| {
            matches!(
                s,
                Stmt::VarDecl {
                    name: declared,
                    is_state: true,
                    ..
                } if declared == name
            )
        });
        if body_declared.contains(name.as_str()) && !declared_as_state {
            return None;
        }
        Some(name.clone())
    }

    /// Compile a `for` construct in either position.
    ///
    /// Statement callers must have run the statement-level source desugars
    /// (`for @a[*]`, element-source writeback) first; those rewrite the whole
    /// `Stmt::For` and re-enter `compile_stmt`, so they are not part of this
    /// lowering.
    pub(super) fn compile_for_construct(&mut self, parts: ForParts<'_>) {
        let ForParts {
            iterable,
            param,
            param_def,
            params,
            params_def,
            body,
            label,
            mode,
            rw_block,
            explicit_zero_params,
            is_statement_modifier,
            uses_block_magic,
            collect,
        } = parts;

        // `&?BLOCK` / `.leave` in the body need the loop's own block as a
        // callable value. Build it up-front and stash it in a fresh local;
        // `ForLoopSpec::block_callable_local` hands it to the VM.
        let block_callable_local = if uses_block_magic {
            let closure = if param.is_none() && params.is_empty() && !explicit_zero_params {
                Expr::AnonSub {
                    body: body.to_vec(),
                    is_rw: rw_block,
                    is_raw: false,
                    is_block: true,
                }
            } else {
                let (closure_params, closure_param_defs) = if params.is_empty() {
                    (
                        param.iter().cloned().collect(),
                        param_def.iter().cloned().collect(),
                    )
                } else {
                    (params.to_vec(), params_def.to_vec())
                };
                Expr::AnonSubParams {
                    params: closure_params,
                    param_defs: closure_param_defs,
                    return_type: None,
                    body: body.to_vec(),
                    is_rw: rw_block,
                    is_raw: false,
                    is_whatever_code: false,
                    is_sub: false,
                }
            };
            self.compile_expr(&closure);
            let local =
                self.alloc_fresh_local(&format!("__mutsu_for_block_callable_{}", self.tmp_counter));
            self.tmp_counter += 1;
            self.code.emit(OpCode::SetLocal(local));
            Some(local)
        } else {
            None
        };

        // `collect` is exactly "this caller wants the body's trailing value", which is
        // what decides whether the phaser lowering must preserve it across appended
        // NEXT/LEAVE bodies.
        let (pre_stmts, loop_body, post_stmts) =
            self.expand_loop_phasers(body, label.as_deref(), collect);
        for s in &pre_stmts {
            self.compile_stmt(s);
        }

        // A single named param (`-> $k`) is bound by the VM directly, by name,
        // without overriding `$_`.
        let param_idx = param
            .as_ref()
            .map(|p| self.code.add_constant(Value::str(p.clone())));
        let bind_stmts =
            Self::build_for_bind_stmts(param, param_def, param_idx, params, params_def, rw_block);

        // ADR-0061: a loop parameter spelled `$self` is a `ParamDef` named
        // `self` and binds the plain key, exactly like a routine's `$self`
        // parameter — so `$self` in the body must resolve to it, not to the
        // reserved lexical key. The loop body compiles inline in THIS compiler,
        // so the flag is scoped by hand (restored at the end) instead of being
        // seeded on a child compiler.
        let saved_self_is_signature_param = self.self_is_signature_param;
        if param.as_deref() == Some("self") || params.iter().any(|p| p == "self") {
            self.self_is_signature_param = true;
        }

        // A sigilless raw binding (`-> \v`) aliases the source element directly;
        // in Raku it is writable and modifications propagate back to the source
        // container (`for @a -> \v { v = 99 }` mutates @a, and `for %h.kv -> \k,
        // \v { v = 9 }` / `for %h.values -> \v` write back through the value
        // alias). Treat it like an rw param: don't mark it readonly, and write
        // modifications back.
        let has_sigilless = param_def.as_ref().is_some_and(|def| def.sigilless)
            || params_def.iter().any(|def| def.sigilless);
        // rw params come from `<->` or an `is rw` trait — for multi-param blocks
        // the per-param defs live in `params_def`, not `param_def`.
        let has_rw = rw_block
            || has_sigilless
            || param_def
                .as_ref()
                .is_some_and(|def| def.traits.iter().any(|t| t == "rw"))
            || params_def
                .iter()
                .any(|def| def.traits.iter().any(|t| t == "rw"));
        // `is copy` also makes the param writable (but without writeback).
        let has_copy = param_def
            .as_ref()
            .is_some_and(|def| def.traits.iter().any(|t| t == "copy"));

        // Statements that bind the loop parameters (`-> \a, @b, %c`) and mark
        // them read-only. They must run — and the params must be bound —
        // *before* any hoisted body `sub` closes over them, so a sub declared in
        // the body (e.g. a `GENERATE-USAGE` that references the loop's
        // `@expected`) captures this iteration's value, not the previous one.
        // Kept separate from `loop_body` so the hoist is emitted after them.
        let mut bind_prefix: Vec<Stmt> = Vec::new();
        if !bind_stmts.is_empty() {
            bind_prefix = bind_stmts;
            // The loop signature DECLARES these names; the binds are plain
            // assignments, so record them for `use strict` (see
            // `CompiledCode::param_bind_names`).
            for p in params {
                if !self.code.param_bind_names.contains(p) {
                    self.code.param_bind_names.push(p.clone());
                }
            }
            // After binding multi-param variables, mark them readonly (unless
            // the block uses `<->` or `is rw`). Skip @-sigil and %-sigil params:
            // they bind to a mutable Array/Hash container, so assignments must
            // be allowed. Also skip params whose OWN def is writable (`is copy`
            // / `is rw` / sigilless) — `-> $x is copy, $fn` must leave $x
            // assignable while $fn stays readonly.
            if !has_rw && !has_copy && !params.is_empty() {
                for (i, p) in params.iter().enumerate() {
                    let per_param_writable = params_def.get(i).is_some_and(|d| {
                        d.sigilless || d.traits.iter().any(|t| t == "rw" || t == "copy")
                    });
                    if !p.starts_with('@') && !p.starts_with('%') && !per_param_writable {
                        bind_prefix.push(Stmt::MarkReadonly(
                            p.clone(),
                            crate::ast::ReadonlyKind::Alias,
                        ));
                    }
                }
            }
        }

        let arity = Self::for_chunk_arity(params, params_def);
        let normalized_iterable = self.normalize_for_iterable(iterable);
        // A `for`-loop handles `is rw` write-back through its own
        // `TagContainerRef` mechanism, so the iterable's synthetic
        // single-element wrap (`for $a` -> `ArrayLiteral([$a])`) must NOT also
        // box `$a` into an aliasing `ContainerRef` cell -- doing so would write
        // that shared cell back into `$a` and create a self-referential cycle
        // (infinite loop on the next read).
        let saved_suppress = self.suppress_list_var_alias;
        self.suppress_list_var_alias = true;
        self.compile_expr(&normalized_iterable);
        self.suppress_list_var_alias = saved_suppress;
        if let Some(source_name) = Self::for_iterable_source_name(iterable) {
            let source_slot = self.local_map.get(source_name.as_str()).copied();
            let source_idx = self.code.add_constant(Value::str(source_name));
            if Self::for_iterable_is_reversed(iterable) {
                self.code
                    .emit(OpCode::TagContainerRefReversed(source_idx, source_slot));
            } else {
                self.code
                    .emit(OpCode::TagContainerRef(source_idx, source_slot));
            }
        }
        // If the for-loop parameter name already has a local slot (e.g. from a
        // prior `my $i` in an enclosing scope), tell the VM so it can keep the
        // local in sync with the env on each iteration and on redo.
        let param_local = param
            .as_ref()
            .and_then(|p| self.local_map.get(p.as_str()).copied());
        // The VM carries a loop parameter's type in `ForLoopSpec` for bind-time
        // checking, but the compiler's call-site provenance mask also needs to
        // know that a native loop parameter is native when it is passed to a
        // multi candidate inside the body. Keep this temporary compiler-side
        // entry scoped to the loop body so an untyped shadow cannot inherit the
        // enclosing parameter's native shape.
        let loop_param_types: Vec<(String, Option<String>)> = if let Some(def) = param_def
            .as_ref()
            .filter(|def| def.type_constraint.is_some())
        {
            let name = param.clone().unwrap_or_else(|| def.name.clone());
            let old = self.local_types.insert(
                name.clone(),
                def.type_constraint.clone().unwrap_or_default(),
            );
            vec![(name, old)]
        } else {
            params
                .iter()
                .zip(params_def.iter())
                .filter_map(|(name, def)| {
                    let type_constraint = def.type_constraint.as_ref()?;
                    let name = name.strip_prefix('\\').unwrap_or(name).to_string();
                    let old = self
                        .local_types
                        .insert(name.clone(), type_constraint.clone());
                    Some((name, old))
                })
                .collect()
        };
        // A single scalar for-loop param is this compiled code's OWN
        // declaration, not something it could ever need to capture from an
        // enclosing scope -- record it so `compute_free_vars` (opcode.rs)
        // excludes it from `free_var_syms`, mirroring the `my_declared_enum_sym`
        // precedent for a `my enum`'s bareword bindings. Without this, a pure
        // body read of the param name (the loop's own binding write happens
        // inside the ForLoop opcode exec, not a compiled name-write op the
        // free-var scan recognizes) is misclassified as free and rewritten to
        // `GetUpvalue`, which resolves against whatever same-named OUTER lexical
        // this closure happened to capture -- bypassing the loop's per-iteration
        // binding entirely. `@`/`%`-sigil and sigilless (`\v`) params are
        // excluded: they don't hit this GetUpvalue path the same way and giving
        // them blanket free-var immunity is unproven for this fix's scope.
        if let Some(p) = param.as_ref()
            && !p.starts_with(['@', '%', '\\'])
        {
            self.code
                .for_loop_param_syms
                .insert(crate::symbol::Symbol::intern(p));
        }
        // Only an implicit-topic loop rebinds `$_`; see `ForLoopSpec::topic_local`.
        let topic_local = param
            .is_none()
            .then(|| self.local_map.get("_").copied())
            .flatten();
        let kv_mode = has_rw && Self::for_iterable_is_kv(iterable);
        // Names of the loop's multi-params whose value is written back to the
        // source each iteration. Only a *genuinely rw* param writes back: a
        // `<->` block (all params rw), a sigilless raw binding (`\a`), or a
        // param with the `is rw` trait. A plain sigil'd param (`@b` in `-> \a,
        // @b`) is NOT rw just because a sibling forced the loop into rw mode —
        // writing it back would corrupt the source (`for @e -> \a, @b {}` must
        // leave @e untouched). Non-rw slots are kept as "" so the vector stays
        // positionally aligned with the chunk (the writeback skips empty names).
        // In `.kv` mode the key param is read (not written) by the writeback, so
        // it must keep its name — leave that path on the all-names form.
        let rw_param_names: Vec<String> = if has_rw && !params.is_empty() {
            params
                .iter()
                .enumerate()
                .map(|(i, p)| {
                    let stripped = p.strip_prefix('\\').unwrap_or(p).to_string();
                    // A slurpy binds a fresh list of the chunk's leftovers, not
                    // an element of it, so there is nothing to write back
                    // through: `for @a <-> $a, *@r { $a = ... }` must write back
                    // only `$a`.
                    if params_def.get(i).is_some_and(|d| d.is_variadic()) {
                        return String::new();
                    }
                    let per_param_rw = kv_mode
                        || rw_block
                        || params_def
                            .get(i)
                            .is_some_and(|d| d.sigilless || d.traits.iter().any(|t| t == "rw"))
                        // No per-param def (fallback): keep prior behavior and
                        // treat a `\`-prefixed name as rw.
                        || (params_def.get(i).is_none() && p.starts_with('\\'));
                    if per_param_rw {
                        stripped
                    } else {
                        String::new()
                    }
                })
                .collect()
        } else {
            Vec::new()
        };
        let source_var_names = Self::for_iterable_var_names(iterable);
        let source_var_locals = self.for_source_var_locals(&source_var_names);
        let source_container_local = Self::for_iterable_source_name(iterable)
            .and_then(|name| self.local_map.get(&name).copied());
        // The local slot each multi-param bind will land in, captured BEFORE
        // `bind_prefix` is compiled (see the field doc on
        // `ForLoopSpec::multi_param_locals`): `build_for_bind_stmts` binds via
        // `Stmt::Assign`, which never allocates a new slot — it resolves to
        // whatever `local_map` already maps the name to right now, or falls
        // through to a global write if there is none. Reading `local_map` at
        // this exact point mirrors that resolution exactly.
        let multi_param_locals: Vec<Option<u32>> = params
            .iter()
            .map(|p| {
                let bare = p.strip_prefix('\\').unwrap_or(p);
                self.local_map.get(bare).copied()
            })
            .collect();
        // When the block parameter has a type constraint other than Mu or
        // Junction, junction items are autothreaded (expanded into their
        // eigenstates).
        let autothread_junctions = match param_def.as_ref() {
            Some(def) => match def.type_constraint.as_deref() {
                None | Some("Mu") | Some("Junction") => false,
                Some(_) => true,
            },
            // No param_def means default (Mu) — no autothreading.
            None => false,
        };
        let loop_idx = self
            .code
            .emit(OpCode::ForLoop(Box::new(crate::opcode::ForLoopSpec {
                param_idx,
                param_local,
                topic_local,
                source_container_local,
                body_end: 0,
                block_callable_local,
                label: label.clone(),
                arity,
                collect,
                threaded: matches!(mode, crate::ast::ForMode::Race | crate::ast::ForMode::Hyper),
                // is_rw: param is writable (don't mark readonly)
                is_rw: has_rw || has_copy,
                // do_writeback: actually write back modifications to the source
                do_writeback: has_rw && !has_copy,
                rw_param_names,
                kv_mode,
                source_var_names,
                source_var_locals,
                autothread_junctions,
                zero_positional_params: Self::for_zero_positional_params(
                    explicit_zero_params,
                    param,
                    params,
                    params_def,
                ),
                multi_param_names: params
                    .iter()
                    .map(|p| p.strip_prefix('\\').unwrap_or(p).to_string())
                    .collect(),
                multi_param_locals,
                param_type_constraint: param_def.as_ref().and_then(|d| d.type_constraint.clone()),
                multi_param_type_constraints: (0..params.len())
                    .map(|i| params_def.get(i).and_then(|d| d.type_constraint.clone()))
                    .collect(),
                loop_var_wraps_element: Self::for_iterable_wraps_pair(iterable),
                values_mode: Self::for_iterable_is_values_alias(iterable),
                direct_smartmatch: Self::for_direct_smartmatch(iterable),
                single_array_source: Self::for_single_array_source(iterable),
                single_array_source_local: self
                    .for_single_array_source_local(&Self::for_single_array_source(iterable)),
                body_declares_routines: Self::stmts_declare_routines(&loop_body),
                source_items_are_bare: Self::for_iterable_yields_bare_items(iterable),
                param_sigilless: param_def.as_ref().is_some_and(|d| d.sigilless),
            })));
        // Register sigilless for-params (`-> \v`, `-> \k, \v`) as sigilless
        // locals while compiling the body so postfix/prefix `++`/`--` on the
        // bare word (`v--`, `++v`) resolve to an in-place PostDecrement/etc. on
        // the bound env var rather than the `__mutsu_incdec_nomatch` fallback.
        // They are NOT readonly (rw aliases), so we only add them to the set,
        // not mark them.
        let sigilless_param_names: Vec<String> = if has_sigilless {
            let mut names = Vec::new();
            let single_sigilless = param_def.as_ref().is_some_and(|def| def.sigilless);
            if let Some(p) = param.as_ref().filter(|_| single_sigilless) {
                names.push(p.strip_prefix('\\').unwrap_or(p).to_string());
            }
            for (p, def) in params.iter().zip(params_def.iter()) {
                if def.sigilless {
                    names.push(p.strip_prefix('\\').unwrap_or(p).to_string());
                }
            }
            names
        } else {
            Vec::new()
        };
        let newly_registered: Vec<String> = sigilless_param_names
            .iter()
            .filter(|n| self.sigilless_locals.insert((*n).clone()))
            .cloned()
            .collect();
        // Bind the loop parameters first, then hoist the body's routine
        // declarations so they capture the freshly-bound params. Hoisting makes
        // a `sub` declared later in the body visible from its beginning (Raku
        // scopes named subs to their whole lexical block); the paired registry
        // snapshot/restore in the VM (gated on `body_declares_routines`) keeps
        // them from leaking past the loop.
        for s in &bind_prefix {
            self.compile_stmt(s);
        }
        self.hoist_sub_decls(&loop_body, true);
        // A `for` body is its own Raku call frame; count it so a
        // `callframe`/`caller` inside sees the enclosing routine one level
        // further up (see `callframe_block_depth`).
        self.callframe_block_depth += 1;
        // Only the statement-MODIFIER form's sole block is the loop's own body
        // (cloned once per statement, so its `state` persists across
        // iterations). A sole block inside a prefix `for` body (`for ^3 { {
        // state ... } }`) is a NESTED bare block that re-clones per iteration,
        // so its per-execution ResetStateLocals must stay (raku prints 1 1 1
        // there — t/state-var-per-block-clone.t test 5).
        self.suppress_loop_block_state_reset =
            is_statement_modifier && Self::loop_body_is_sole_block(&loop_body);
        // ...and by the same token that sole block is the loop's own block,
        // supplied one element per iteration -- not a nested zero-argument one
        // (ADR-0048 D3/D6). Re-noted here because `expand_loop_phasers` rebuilt
        // the body list.
        if is_statement_modifier {
            self.note_construct_body_block_stmts(&loop_body);
        }
        if collect {
            // The `ForLoop` opcode brackets this body with
            // `push_loop_local_scope`/`pop_loop_local_scope` just like the
            // statement form, so a `my TYPE $x` here is env-restored on exit and
            // can use the env-only scoped constraint opcode.
            self.compile_scope_restored_body_value(&loop_body);
            // Emitted AFTER that call, so outside the body's own `let`/`temp`
            // save frame (#7677): `exec_let_block_op` jumps the ip past
            // everything inside the frame's range, and this tag has to run.
            // Being outside it is also what the container semantics want — the
            // tag records a name whose value is read once the whole loop is
            // over, by which point every iteration's `temp` has been restored.
            //
            // A bare variable read in tail position hands back that variable's
            // CONTAINER, exactly as a tail assignment does (see the
            // `TagContainerRef` emitted by `compile_expr_assign`). Tag it so the
            // `ForLoop` opcode collects the container rather than a snapshot of
            // what it held mid-iteration.
            if let Some(name) = Self::collected_tail_container_name(&loop_body, param, params) {
                let source_slot = self.local_map.get(name.as_str()).copied();
                let name_idx = self.code.add_constant(Value::str(name));
                self.code
                    .emit(OpCode::TagContainerRef(name_idx, source_slot));
            }
        } else {
            self.compile_scope_restored_loop_body(&loop_body);
        }
        for (name, old) in loop_param_types {
            if let Some(old) = old {
                self.local_types.insert(name, old);
            } else {
                self.local_types.remove(&name);
            }
        }
        self.callframe_block_depth -= 1;
        for n in &newly_registered {
            self.sigilless_locals.remove(n);
        }
        self.code.patch_loop_end(loop_idx);
        for s in &post_stmts {
            self.compile_stmt(s);
        }
        // Restore the single named loop param after the post (LAST) phasers ran.
        // The ForLoop opcode deferred this restore (pushing its saved binding)
        // so the phasers could still see the param at its final value. Emit
        // whenever a single named param exists, mirroring the VM's save
        // condition so push/pop stay balanced.
        if param.is_some() {
            self.code.emit(OpCode::RestoreForParam);
        }
        self.self_is_signature_param = saved_self_is_signature_param;
    }
}
