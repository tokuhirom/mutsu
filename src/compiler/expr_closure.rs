use super::expr_routine_literal::collect_whatever_expr_decls;
use super::*;

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
            self.code.emit(OpCode::Die { user_throw: false });
            true
        } else {
            false
        }
    }

    /// Compile AnonSub expression.
    /// `is_block` = true for bare blocks `{ }`, false for `sub { }`.
    /// Bare blocks are NOT routine boundaries for `return`.
    pub(super) fn compile_expr_anon_sub(&mut self, body: &[Stmt], is_rw: bool, is_block: bool) {
        // One-shot, consumed here so only THIS block (not one nested inside it)
        // picks up the enclosing `.map`/`.grep`-over-a-literal verdict the
        // caller set -- see `CompiledCode::immutable_topic`.
        let immutable_topic = std::mem::take(&mut self.pending_immutable_topic_block);
        let declarator_doc = self.pending_declarator_doc.take().map(std::sync::Arc::new);
        if is_rw {
            // A bare `is rw` block (a `<->`-style loop body) keeps its tail
            // as a value; only an anonymous `sub ... is rw` hands out its
            // tail's container (ADR-0059 Slice 2).
            let mut compiled = if is_block {
                self.compile_closure_body(&[], &[], body)
            } else {
                self.compile_routine_closure_body(&[], &[], body, true)
            };
            compiled.immutable_topic = immutable_topic;
            compiled.declarator_doc = declarator_doc;
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
                self.code.emit(OpCode::Die { user_throw: false });
                return;
            }
            let placeholders = crate::ast::collect_placeholders_shallow(body);
            let mut compiled = if is_block {
                self.compile_closure_body(&placeholders, &[], body)
            } else {
                self.compile_routine_closure_body(&placeholders, &[], body, false)
            };
            // Only a bare, parameterless block topicalizes the element; a
            // placeholder block (`{ $^a }`) binds its own parameter and keeps
            // the enclosing `$_`.
            compiled.immutable_topic = immutable_topic && placeholders.is_empty();
            compiled.declarator_doc = declarator_doc;
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
            self.code.emit(OpCode::Die { user_throw: false });
            return;
        }
        if let Some(err_val) = Self::check_native_readonly_param_assignment(param_defs, body) {
            let idx = self.code.add_constant(err_val);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die { user_throw: false });
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

    /// Compile Lambda expression.
    pub(super) fn compile_expr_lambda(
        &mut self,
        param: &str,
        body: &[Stmt],
        is_whatever_code: bool,
        param_sigilless: bool,
    ) {
        // A sigilless pointy parameter (`-> \x { }`) binds its term key
        // (ADR-12441).
        let params: Vec<String> = if param.is_empty() {
            Vec::new()
        } else if param_sigilless {
            vec![crate::runtime::term_names::term_key(param)]
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
        let wc_raw = is_whatever_code
            && param == "_"
            && crate::whatever_curry::body_mutates_placeholder(body, "_");
        let wc_param_defs: Vec<crate::ast::ParamDef> = if wc_raw {
            vec![crate::ast::ParamDef { trait_meta: Default::default(),
                type_capture: None,
                name: "_".to_string(),
                default: None,
                multi_invocant: true,
                required: false,
                named: false,
                named_alias: false,
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
                code: Default::default(),
                trait_args: Vec::new(),
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
        compiled.is_whatever_code = is_whatever_code;
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
    /// promotes the source; the caller sets `IndexAssignExprNamed::element_share` so
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
            // A range / sequence / repetition operator builds a LIST of
            // indices (`@a[2 ..^ 4] = @o`, `@a[^2] = @o`), so it is a slice
            // even though it is a single `Binary` node (#10370).
            Expr::Binary { op, .. } => {
                !matches!(
                    op,
                    TokenKind::DotDot
                        | TokenKind::DotDotCaret
                        | TokenKind::CaretDotDot
                        | TokenKind::CaretDotDotCaret
                        | TokenKind::DotDotDot
                        | TokenKind::DotDotDotCaret
                ) && !matches!(op, TokenKind::Ident(name) if name == "xx")
            }
            Expr::Var(_) | Expr::Unary { .. } => true,
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
            listop: false,
        })
    }

    /// A named package's stash (`EXPORT::DEFAULT::{$_} = ::($_)`, the
    /// re-export idiom; `Foo::<&bar> := &f`). A literal non-`&` key keeps the
    /// generic index-assign; a `&` key or a runtime key goes to the keyed
    /// stash op, which installs a routine as the package's symbol (and
    /// export) and defers any other key back to the generic path.
    ///
    /// Out of line so its locals stay out of `compile_expr_index_assign`'s
    /// frame, which the compiler re-enters recursively.
    #[inline(never)]
    fn try_compile_named_package_stash_assign(
        &mut self,
        target: &Expr,
        index: &Expr,
        value: &Expr,
    ) -> bool {
        let Expr::PseudoStash(stash_name) = target else {
            return false;
        };
        let named = stash_name.strip_suffix("::").is_some_and(|pkg| {
            !pkg.is_empty()
                && (pkg == "GLOBAL" || !pkg.split("::").any(crate::parser::is_pseudo_package))
        });
        if !named {
            return false;
        }
        // `Pkg::<$v> {=,:=} value` is the stash spelling of `$Pkg::v {=,:=}
        // value`: route it through the qualified-variable assignment, which
        // writes through the variable's container (and so checks its declared
        // constraint, ADR-0042) instead of replacing the stash entry. Only a
        // `$` key: an `@`/`%` stash entry is the Array/Hash itself, not a
        // Scalar, and rakudo refuses `Pkg::<@a> = ...` as an assignment to an
        // immutable value.
        if let Expr::Literal(lit) = index
            && let Some(key) = lit.as_str()
            && !key.starts_with('&')
        {
            // `Pkg::<@a> = ...` / `Pkg::<%h> = ...` (assignment, not bind).
            if key.starts_with(['@', '%'])
                && key.len() > 1
                && !matches!(value, Expr::Call { name, .. } if *name == "__mutsu_bind_index_value")
            {
                self.compile_expr(&Expr::Call {
                    name: Symbol::intern("die"),
                    args: vec![Expr::Literal(Value::str(
                        "Cannot assign to an immutable value".to_string(),
                    ))],
                    listop: false,
                });
                return true;
            }
            // `Pkg::<@a> := v` / `Pkg::<%h> := v`: the stash spelling of
            // `@Pkg::a := v`, which rebinds the package variable (#10546).
            if let Some(Expr::Call { name, args, .. }) = Some(value)
                && *name == "__mutsu_bind_index_value"
                && let Some(sigil) = key.chars().next().filter(|c| matches!(c, '@' | '%'))
                && key[1..].starts_with(|c: char| c.is_alphabetic() || c == '_')
            {
                let rhs = args.first().cloned().unwrap_or(Expr::Literal(Value::NIL));
                self.compile_expr(&Expr::AssignExpr {
                    name: format!("{sigil}{stash_name}{}", &key[1..]),
                    expr: Box::new(rhs),
                    is_bind: true,
                });
                return true;
            }
            let Some(bare) = key
                .strip_prefix('$')
                .filter(|bare| bare.starts_with(|c: char| c.is_alphabetic() || c == '_'))
            else {
                return false;
            };
            let (is_bind, rhs) = match value {
                Expr::Call { name, args, .. } if *name == "__mutsu_bind_index_value" => (
                    true,
                    args.first().cloned().unwrap_or(Expr::Literal(Value::NIL)),
                ),
                other => (false, other.clone()),
            };
            // `GLOBAL::<$x> = v` (plain assign): the statement form's store, so
            // a never-declared `$GLOBAL::x` lands where the GLOBAL stash reads
            // it back (the expression-form `AssignExpr` op does not register it).
            if !is_bind && stash_name == "GLOBAL::" {
                self.with_escape(true, |c| c.compile_expr(&rhs));
                let qualified = format!("GLOBAL::{bare}");
                self.emit_set_named_var(&qualified);
                let name_idx = self
                    .code
                    .add_constant(Value::str(self.qualify_variable_name(&qualified)));
                self.code.emit(OpCode::GetGlobal(name_idx));
                return true;
            }
            // `$Pkg::v` compiles to AssignExpr name "Pkg::v" (sigil dropped).
            self.compile_expr(&Expr::AssignExpr {
                name: format!("{stash_name}{bare}"),
                expr: Box::new(rhs),
                is_bind,
            });
            return true;
        }
        self.compile_bind_index_value(value);
        self.compile_expr(index);
        let stash_name_idx = self.code.add_constant(Value::str(stash_name.clone()));
        self.code
            .emit(OpCode::IndexAssignPseudoStashKeyed { stash_name_idx });
        true
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
            // wrap in `compile_call_arg`, which tags the promoted element cell
            // as a bind source.
            self.bind_target_direct = true;
            self.compile_expr(value);
            self.scalar_bind_autovivify = saved_av;
            self.bind_terminal = saved_term;
        } else {
            // A closure literal stored into a container element (`%h<k> = sub
            // {...}`) escapes the creating frame exactly like a closure literal
            // handed to a named call argument (see the identical fix at the
            // `CallArg::Named` site in `stmt.rs`, `todo/deep/closure-capture-
            // shadowed-by-colliding-callee-parameter.md`). Without this, the
            // closure compiles as non-escaping, so a captured-and-mutated free
            // variable (even one mutated only in dead code — the mutation
            // analysis cannot see it is unreachable) is never boxed into a
            // shared `ContainerRef` cell. Its capture then falls back to a
            // plain-value snapshot that a same-named parameter in whatever
            // frame later happens to call the closure can shadow (#8663).
            let escaping = Self::is_closure_literal_arg(value);
            self.with_escape(escaping, |s| s.compile_expr(value));
        }
    }

    /// Preserve the lvalue when an indexed assignment is itself the source of
    /// an indexed bind, as in `%h<a> := %h<b> = value`.  The parser represents
    /// the source assignment as an `IndexAssign` nested inside the bind marker,
    /// but compiling that assignment normally would leave only its value on
    /// the stack.  Evaluate the source location once, store its location in a
    /// raw call temporary, perform the assignment through that location, then
    /// bind the outer element to the temporary.
    fn compile_nested_index_bind_source(
        &mut self,
        target: &Expr,
        index: &Expr,
        value: &Expr,
        outer_positional: bool,
    ) -> bool {
        let Expr::Call { name, args, .. } = value else {
            return false;
        };
        if *name != "__mutsu_bind_index_value" {
            return false;
        }
        let Some(Expr::IndexAssign {
            target: source_target,
            index: source_index,
            value: source_value,
            is_positional: source_positional,
            ..
        }) = args.first()
        else {
            return false;
        };
        // A bind marker here is the ordinary representation of a chained
        // bind (`%a<x> := %a<y> := value`).  Its existing compiler path keeps
        // the chain's value semantics intact; only a real assignment value
        // needs the raw-location preservation below.
        if matches!(source_value.as_ref(), Expr::Call { name, .. } if *name == "__mutsu_bind_index_value")
        {
            return false;
        }

        let source = Expr::Index {
            target: source_target.clone(),
            index: source_index.clone(),
            is_positional: *source_positional,
            spelling: Default::default(),
        };
        let temp_name = format!(
            "__mutsu_bind_index_assign_src_{}",
            self.code.constants.len()
        );
        let temp_idx = self.code.add_constant(Value::str(temp_name.clone()));

        let saved_av = self.scalar_bind_autovivify;
        let saved_terminal = self.bind_terminal;
        let saved_nested_source = self.compile_nested_index_bind_source;
        self.scalar_bind_autovivify = true;
        self.bind_terminal = true;
        self.compile_nested_index_bind_source = true;
        self.compile_expr(&source);
        self.scalar_bind_autovivify = saved_av;
        self.bind_terminal = saved_terminal;
        self.compile_nested_index_bind_source = saved_nested_source;
        self.code.emit(OpCode::SetCallTemp(temp_idx));

        self.compile_bind_index_value(source_value);
        self.code.emit(OpCode::SetGlobal(temp_idx));

        let rebound_source = Expr::Call {
            name: Symbol::intern("__mutsu_bind_index_value"),
            args: vec![Expr::Var(temp_name)],
            listop: false,
        };
        self.compile_expr_index_assign(
            target,
            index,
            &rebound_source,
            outer_positional,
            crate::ast::IndexSpelling::Subscript,
        );
        true
    }

    /// For an lvalue subscript chain of depth >= 2 whose innermost target is an
    /// accessor-style method call (`$o.a[0]<x>`, `$o.h<a><b><c>`), evaluate that
    /// method call once into a compiler temp and return the same chain with the
    /// method call replaced by the temp's name.
    ///
    /// The temp is a plain env entry holding the accessor's return value, so it
    /// is *node-shared* with the attribute's container: the ordinary
    /// variable-rooted chain walk then autovivifies and stores in place, and the
    /// write reaches the attribute. Returns `None` for every other shape,
    /// including a single-level `$o.a[0] = v` (already handled, and it needs the
    /// accessor write-back path).
    ///
    /// The temp's name embeds the accessor spelling (`@.a`) so an element type
    /// error still names something the user wrote -- see
    /// `format_var_name_for_error`.
    fn bind_method_rooted_chain_root(&mut self, target: &Expr, binding: bool) -> Option<Expr> {
        // `target` is the chain BELOW the outermost subscript, so requiring one
        // `Expr::Index` here is what makes this depth >= 2.
        let Expr::Index { .. } = target else {
            return None;
        };
        // Walk down to the root, remembering the levels on the way.
        let mut levels: Vec<(&Expr, bool)> = Vec::new();
        let mut cur = target;
        while let Expr::Index {
            target: inner,
            index,
            is_positional,
            ..
        } = cur
        {
            levels.push((index.as_ref(), *is_positional));
            cur = inner.as_ref();
        }
        let sigil = if levels.last().map(|(_, p)| *p).unwrap_or(true) {
            '@'
        } else {
            '%'
        };
        // The temp's name after the prefix: `<sigil><method>` for an accessor
        // root, `=<spelling>` (shown verbatim in an error) for any other root.
        let spelling = match cur {
            Expr::MethodCall {
                name: method_name,
                args,
                modifier,
                ..
            } => {
                // Only a no-argument accessor-style call yields a container to
                // store through; a call with arguments (or an adverb) can
                // compute a fresh value, and writing into that must keep its
                // existing behaviour.
                if !args.is_empty() || modifier.is_some() {
                    return None;
                }
                format!("{sigil}{}", method_name.resolve())
            }
            // A parenthesized variable is the variable: subscript it directly,
            // so the by-name store (and its autovivification) applies (#10900).
            Expr::Grouped(inner) if Self::is_plain_named_container(inner.peel_parens()) => {
                return Some(Self::rebuild_index_chain(
                    inner.peel_parens().clone(),
                    levels,
                ));
            }
            // A root no by-name store reaches -- `@OUTER::a` past a shadow, a
            // call -- yields a reference-shared container all the same, so the
            // chain stores into it in place and autovivifies its inner levels
            // (#10900). A variable root keeps its by-name store, and a list
            // literal its own write-through.
            // A `:=` keeps its own path: it binds the element in the container
            // the generic store reaches, which a temp would only copy.
            root if !binding
                && !Self::is_plain_named_container(root.peel_parens())
                && !matches!(root.peel_parens(), Expr::ArrayLiteral(_))
                && self.index_assign_target_name(target).is_none() =>
            {
                // An error names the variable, as rakudo does, not its
                // `OUTER::` spelling.
                let bare = |n: &str| Self::parse_outer_prefix(n).map_or(n.to_string(), |(b, _)| b);
                match root.peel_parens() {
                    Expr::ArrayVar(n) => format!("=@{}", bare(n)),
                    Expr::HashVar(n) => format!("=%{}", bare(n)),
                    Expr::Var(n) => format!("=${}", bare(n)),
                    _ => format!("={sigil}"),
                }
            }
            _ => return None,
        };
        self.compile_expr(cur);
        let tmp = format!(
            "{}{}#{}",
            crate::runtime::utils::LVALUE_ROOT_TEMP_PREFIX,
            spelling,
            self.code.constants.len()
        );
        let tmp_idx = self.code.add_constant(Value::str(tmp.clone()));
        self.code.emit(OpCode::SetGlobal(tmp_idx));
        Some(Self::rebuild_index_chain(Expr::Var(tmp), levels))
    }

    /// Whether `root` is a variable an lvalue chain stores through by name: a
    /// `$`/`@`/`%` variable that is not an `OUTER::` spelling (which no
    /// by-name store reaches past a shadow).
    fn is_plain_named_container(root: &Expr) -> bool {
        matches!(root, Expr::Var(n) | Expr::ArrayVar(n) | Expr::HashVar(n)
            if !n.starts_with("OUTER::"))
    }

    /// Rebuild the subscript chain `levels` (outermost first, as collected by
    /// [`Self::bind_method_rooted_chain_root`]) on top of `root`.
    fn rebuild_index_chain(root: Expr, levels: Vec<(&Expr, bool)>) -> Expr {
        let mut rebuilt = root;
        for (index, is_positional) in levels.into_iter().rev() {
            rebuilt = Expr::Index {
                target: Box::new(rebuilt),
                index: Box::new(index.clone()),
                is_positional,
                spelling: Default::default(),
            };
        }
        rebuilt
    }

    pub(super) fn compile_expr_index_assign(
        &mut self,
        target: &Expr,
        index: &Expr,
        value: &Expr,
        outer_positional: bool,
        spelling: crate::ast::IndexSpelling,
    ) {
        // `(state buf32 $w .= new)[3] = 77` and `($a, 42, $b)[1, 3] = ...`
        // subscript what the parentheses hold, so the shape dispatch below looks
        // through the marker the parser records for every `(...)`.
        let original_target = target;
        let target = target.peel_parens();
        if self.compile_nested_index_bind_source(target, index, value, outer_positional) {
            return;
        }
        if matches!(value, Expr::Call { name, .. } if *name == "__mutsu_bind_index_value")
            && crate::parser::index_bind_target_is_immutable(original_target)
        {
            // An immutable subscript cannot receive a binding. Evaluate the
            // target, index, and RHS first, then throw at run time so earlier
            // statements and operand side effects happen in source order.
            self.compile_expr(target);
            self.code.emit(OpCode::Pop);
            self.compile_expr(index);
            self.code.emit(OpCode::Pop);
            if let Expr::Call { args, .. } = value
                && let Some(rhs) = args.first()
            {
                self.compile_expr(rhs);
                self.code.emit(OpCode::Pop);
            }
            let message = "Cannot use bind operator with this left-hand side";
            let exception = Value::make_instance(
                Symbol::intern("X::Bind"),
                std::collections::HashMap::from([(
                    "message".to_string(),
                    Value::str_from(message),
                )]),
            );
            let idx = self.code.add_constant(exception);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die { user_throw: false });
            return;
        }
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
                            form: Default::default(),
                        },
                        Expr::Binary {
                            left: Box::new(Expr::Literal(Value::str("message".to_string()))),
                            op: TokenKind::FatArrow,
                            right: Box::new(Expr::Literal(Value::str(
                                "Cannot bind to Array slice".to_string(),
                            ))),
                            form: Default::default(),
                        },
                    ],
                    modifier: None,
                    quoted: false,
                    sugar: false,
                }],
                listop: false,
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
                            is_raw: false,
                            is_block: true,
                            doc: Default::default(),
                        },
                    ],
                    modifier: None,
                    quoted: false,
                    sugar: false,
                }),
                is_bind: false,
            };
            self.compile_expr(&rewritten);
            return;
        }
        if let Expr::PseudoStash(stash_name) = target
            && matches!(
                stash_name.as_str(),
                "MY::" | "PROCESS::" | "CALLER::LEXICAL::"
            )
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
                Expr::Call { name, args, .. } if *name == "__mutsu_bind_index_value" => (
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
        // Runtime-key `OUR::` assignment (`OUR::{'&' ~ $tag} := sub {...}`, the
        // generated-export idiom -- see `publish_our_pseudo_stash_symbol`). The
        // literal-key branch above cannot take it because the subscript is only
        // known at runtime, and the generic index-assign it would otherwise
        // reach writes a throwaway stash hash and drops the store.
        if let Expr::PseudoStash(stash_name) = target
            && stash_name == "OUR::"
        {
            let rhs = match value {
                Expr::Call { name, args, .. } if *name == "__mutsu_bind_index_value" => {
                    args.first().cloned().unwrap_or(Expr::Literal(Value::NIL))
                }
                other => other.clone(),
            };
            self.compile_expr(&rhs);
            self.compile_expr(index);
            let stash_name_idx = self.code.add_constant(Value::str(stash_name.clone()));
            self.code
                .emit(OpCode::IndexAssignPseudoStashKeyed { stash_name_idx });
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
        if self.try_compile_named_package_stash_assign(target, index, value) {
            return;
        }
        // A subscript chain rooted at an accessor-style method call
        // (`$o.a[0]<x> = 5`, `$o.h<a><b> = 5`) is compiled by evaluating the
        // accessor ONCE into a compiler temp and then running the ordinary
        // variable-rooted chain against that temp.
        //
        // The accessor hands back the attribute's *shared* container (which is
        // why `$o.a.push(1)` reaches the attribute), so the chain walk
        // autovivifies into it in place and the write lands. Previously this
        // shape was rewritten into the `__mutsu_index_assign_method_lvalue_nested`
        // runtime builtin, which rebuilt the containers copy-on-write and never
        // installed a freshly autovivified level back into the attribute -- so
        // `$o.a[0]<x> = 5` left `[]` and the write was silently lost, at every
        // depth >= 2 and in both the `[...]` and `<...>` spellings. Same temp
        // trick as `compile_expr_method_on_nested_index`, which already relies
        // on the node sharing for `$obj.attr<a><b>.push`.
        //
        // Only depth >= 2 routes here: a single-level `$o.a[0] = v` is already
        // correct through `__mutsu_index_assign_method_lvalue`, which also
        // performs the accessor write-back a non-container attribute needs.
        let binding =
            matches!(value, Expr::Call { name, .. } if *name == "__mutsu_bind_index_value");
        if let Some(rewritten) = self.bind_method_rooted_chain_root(target, binding) {
            self.compile_expr_index_assign(&rewritten, index, value, outer_positional, spelling);
            return;
        }
        if spelling == crate::ast::IndexSpelling::SemicolonDimensions
            && matches!(value, Expr::Call { name, .. } if *name == "__mutsu_bind_index_value")
            && let Expr::ArrayLiteral(dimensions) = index
            && let Some(name) = self.index_assign_target_name(target)
        {
            self.compile_multidim_value_bind(name, dimensions, value, outer_positional);
            return;
        }
        if let Some(name) = self.index_assign_target_name(target) {
            let target_slot = self.local_map.get(&name).copied();
            if Self::index_assign_target_requires_eval(target) {
                self.compile_expr(target);
                self.code.emit(OpCode::Pop);
            }
            let share_value = Self::element_share_bind_value(&name, index, value);
            self.compile_expr(index);
            // `%h{$k} ~= rhs`: leave the element and the RHS unconcatenated so
            // the store can append in place (#9141).
            if share_value.is_none()
                && let Some((seeded, rhs)) =
                    Self::index_concat_append_parts(target, index, value, outer_positional)
            {
                self.compile_expr(seeded);
                self.compile_expr(rhs);
                let name_idx = self.code.add_constant(Value::str(name));
                self.code.emit(OpCode::IndexAssignExprNamed {
                    name_idx,
                    is_positional: outer_positional,
                    index_first: true,
                    target_slot,
                    concat_append: true,
                    element_share: false,
                });
                return;
            }
            match &share_value {
                Some(bind_value) => self.compile_bind_index_value(bind_value),
                None => self.compile_bind_index_value(value),
            }
            let name_idx = self.code.add_constant(Value::str(name));
            self.code.emit(OpCode::IndexAssignExprNamed {
                name_idx,
                is_positional: outer_positional,
                index_first: true,
                target_slot,
                concat_append: false,
                element_share: share_value.is_some(),
            });
        } else if let Some((name, chain)) = self
            .index_assign_deep_nested_target(target)
            .or_else(|| binding.then(|| self.index_chain_target(target)).flatten())
        {
            // All chained binds use the cell-preserving walk. Ordinary
            // assignments use it for three or more subscript levels.
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
            self.index_assign_nested_target(target)
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
        } else if let Expr::MethodCall {
            target: method_target,
            name: method_name,
            args: method_args,
            modifier: None,
            ..
        } = target
            && ((method_args.is_empty() && matches!(method_target.as_ref(), Expr::Var(_)))
                || (!method_args.is_empty() && matches!(method_target.as_ref(), Expr::BareWord(_))))
            && let Some(var_name) = Self::method_call_target_var_name(method_target)
        {
            let writeback_name = if var_name.is_empty() {
                method_args
                    .first()
                    .and_then(|t| self.index_assign_target_name(t))
                    .unwrap_or_default()
            } else {
                var_name
            };
            let mut args = vec![
                (**method_target).clone(),
                Expr::Literal(Value::str(method_name.resolve())),
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
                listop: false,
            };
            self.compile_expr(&rewritten);
        } else if let Some(arr_name) = self.map_rw_identity_target_name(target) {
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
                index_first: false,
                target_slot,
                concat_append: false,
                element_share: false,
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
                            self.code.emit(OpCode::AssignExpr(name_idx, false));
                        } else if let Expr::DoStmt(decl_stmt) = &elements[i]
                            && let Stmt::VarDecl { name, .. } = decl_stmt.as_ref()
                        {
                            // `(my $x = $a, 6)[0] = 10`: an inline declaration
                            // in a list literal denotes the freshly-declared
                            // variable's own container (#7556 section F), so
                            // the store writes through it exactly like the
                            // `Expr::Var` case above -- run the declaration
                            // (and its initializer) first for its side effect.
                            self.compile_stmt(decl_stmt);
                            self.compile_expr(value);
                            let name_idx = self.code.add_constant(Value::str(name.clone()));
                            self.code.emit(OpCode::AssignExpr(name_idx, false));
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
        if let Some(var_name) = self.index_assign_target_name(target) {
            if matches!(value, Expr::Call { name, .. } if *name == "__mutsu_bind_index_value") {
                self.compile_bind_index_value(value);
            } else {
                self.compile_expr(value);
            }
            for dim in dimensions {
                self.compile_expr(dim);
            }
            let name_idx = self.code.add_constant(Value::str(var_name));
            self.code.emit(OpCode::MultiDimIndexAssign {
                name_idx,
                ndims: dimensions.len() as u32,
                is_positional,
            });
        } else if let Some((name, chain)) = self.index_chain_target(target) {
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
                    self.code.emit(OpCode::AssignExpr(name_idx, false));
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
                listop: false,
            };
            self.compile_expr(&ro_call);
        }
    }
}
