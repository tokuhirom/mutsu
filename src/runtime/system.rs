use super::*;

#[path = "system_suggestions.rs"]
mod suggestions;

/// Core built-in type names used as candidates for type-name "Did you mean"
/// suggestions (in addition to user-registered classes).
pub(crate) const CORE_TYPE_NAMES: &[&str] = &[
    "Mu",
    "Any",
    "Cool",
    "Int",
    "Num",
    "Rat",
    "FatRat",
    "Complex",
    "Str",
    "Bool",
    "Array",
    "Hash",
    "List",
    "Map",
    "Set",
    "Bag",
    "Mix",
    "SetHash",
    "BagHash",
    "MixHash",
    "Range",
    "Pair",
    "Seq",
    "Slip",
    "Junction",
    "Regex",
    "Match",
    "Grammar",
    "Exception",
    "Failure",
    "Version",
    "Nil",
    "Block",
    "Code",
    "Routine",
    "Sub",
    "Method",
    "Whatever",
    "WhateverCode",
    "Callable",
    "Numeric",
    "Real",
    "Stringy",
    "Positional",
    "Associative",
    "Iterable",
    "Iterator",
    "Capture",
    "Signature",
    "Parameter",
    "Date",
    "DateTime",
    "Instant",
    "Duration",
    "Buf",
    "Blob",
    "Promise",
    "Supply",
    "Channel",
    "Thread",
    "Proc",
    "IO",
    "Scalar",
];

impl Interpreter {
    fn validate_eval_source(&self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        self.check_eval_mainline_placeholders(stmts)?;
        self.check_eval_class_redeclarations(stmts)?;
        self.check_eval_undeclared_trusts(stmts)?;
        self.check_eval_undeclared_type_args(stmts)?;
        self.check_eval_undeclared_vars(stmts)?;
        self.check_eval_undeclared_names(stmts)?;
        self.check_eval_undeclared_routines(stmts)?;
        Self::check_eval_routine_magicals(stmts)?;
        self.check_eval_post_declared_types(stmts)?;
        self.check_eval_begin_forward_calls(stmts)?;
        self.check_eval_param_type_constraints(stmts)?;
        self.check_type_capture_inheritance(stmts)
    }

    pub(super) fn parse_and_eval_with_operators(
        &mut self,
        src: &str,
        op_names: &[String],
        op_assoc: &HashMap<String, String>,
    ) -> Result<Value, RuntimeError> {
        let user_sub_names = self.collect_eval_user_sub_names();
        let user_type_names = self.collect_eval_user_type_names();
        let user_value_term_names = self.collect_eval_user_value_term_names();
        // Make module search paths visible to the parser so that `use Foo`
        // inside the EVAL'd code can resolve and register Foo's exported sub
        // names (needed for parenless calls like `use Foo; bar`).
        crate::parser::set_parser_lib_paths(self.parser_scan_lib_paths());
        crate::parser::set_parser_program_path(self.io.program_path.clone());
        let parse_result = crate::parser::parse_program_with_operators_and_user_subs(
            src,
            op_names,
            op_assoc,
            &user_sub_names,
            &user_type_names,
            &user_value_term_names,
            crate::rakuast::frontend::covers(crate::rakuast::frontend::Unit::Eval),
        );
        crate::parser::clear_parser_lib_paths();
        match parse_result {
            Ok((stmts, _)) => {
                // EVAL is its own compilation unit, so its `$=pod` must be
                // collected from the evaluated source before the mainline
                // runs.  This also adds declarator blocks from `#|` comments;
                // without it a module such as Pod::EOD sees only its ordinary
                // Pod blocks and loses the declaration it is meant to move.
                let docs = crate::parser::decl_doc::take_unit_docs();
                self.establish_pod_variables_from_stmts(src, &stmts, docs)?;
                // A placeholder parameter used directly in the mainline is
                // `X::Placeholder::Mainline`, and this has to run BEFORE the
                // undeclared check -- otherwise `@_` / `%_` are reported as
                // `X::Undeclared`. It used to live only in the retired native
                // `throws-like`, which is why `roast/S32-exceptions/misc2.t`
                // passed there and failed under the real `Test` module, whose
                // `throws-like` EVALs its string through this ordinary path.
                self.validate_eval_source(&stmts)?;
                // Diagnose the source unit before converting it. Invalid names
                // must keep the ordinary frontend's typed CHECK-time errors,
                // rather than becoming a RakuAST conversion refusal.
                let stmts = crate::rakuast::frontend::round_trip_eval_if_enabled(
                    stmts,
                    || self.eval_caller_type_names(),
                    || self.eval_caller_enum_value_names(),
                )?;
                // Lowering resolves spelled and deferred names that the source
                // checks cannot yet classify, including dynamic EXPORT terms.
                if crate::rakuast::frontend::covers(crate::rakuast::frontend::Unit::Eval) {
                    self.validate_eval_source(&stmts)?;
                }
                // When EVAL is called inside a class body, MethodDecl statements
                // should be added to the enclosing class rather than lowered to subs.
                let mut stmts = self.inject_eval_methods_into_class(stmts);
                // Reorder so the top-level BEGINs run first, in the EVAL unit's
                // prologue (ADR-0134): a read textually preceding a BEGIN sees
                // its side effects and a later initializer still clobbers it,
                // so `EVAL 'my $x = 0; BEGIN { $x = 1 }; $x'` is 0, as in raku.
                // CHECK runs at compile time and INIT once before the main body,
                // matching the top-level pipeline. The EVAL-specific variant also
                // lifts BEGIN from closure bodies in the EVAL'd code.
                crate::runtime::phasers::reorder_phasers_for_eval(&mut stmts);
                // A `my` declared inside EVAL is lexically scoped to that EVAL and
                // must NOT leak into the caller's pad (raku: `EVAL 'my $y'; $y` is
                // X::Undeclared). Snapshot the set of *plain user lexical* keys
                // present before running, so any new ones the EVAL introduces (no
                // matter where: a bare `my`, a loop/if/while-condition declaration,
                // a comma expression, …) can be removed afterwards. Pre-existing
                // keys are untouched, so assignments to outer variables persist,
                // matching raku. `&`-sub keys are handled by the callable-key
                // restore in `eval_eval_string`, so they are excluded here.
                //
                // `visible_keys_where`, not `keys()`: `Env::keys` exposes only the
                // innermost tier's overlay, but inside a closure or a routine the
                // frame env is a scoped child and the caller's lexicals live in a
                // PARENT tier. Scanning the overlay alone made every such lexical
                // look brand-new, so `EVAL '$a = 32'` — whose write lands in the
                // overlay — was classified as an EVAL-local `my` and deleted on the
                // way out. Worse, removing a name from a scoped env leaves a
                // TOMBSTONE, so the caller's binding was hidden outright: a second
                // `EVAL 'say $a'` in the same block then died with
                // "Variable '$a' is not declared". (Mirrors the identical `keys()`
                // -> `visible_keys_where` fix already made for the `&`-code-var
                // shadow snapshot in `eval_eval_string`.)
                let eval_pre_lexicals: HashSet<crate::symbol::Symbol> = self
                    .env
                    .visible_keys_where(|s| {
                        crate::env::is_plain_user_lexical(s) && !s.starts_with('&')
                    })
                    .iter()
                    .map(|s| crate::symbol::Symbol::intern(s))
                    .collect();
                // Removing only the *new* keys is not enough: a `my` whose name the
                // caller already uses SHADOWS it, and the shared env has one entry
                // per name, so the declaration overwrites the caller's value in
                // place. `my $a = 10; EVAL 'my $a = 999'; say $a` printed 999.
                // Collect the names this snippet declares and snapshot exactly
                // those, so they are restored afterwards while a plain assignment
                // (`EVAL '$a = 999'`, which must write through) is untouched.
                let eval_shadowed: Vec<(crate::symbol::Symbol, Option<Value>)> =
                    super::eval_decl_scans::eval_declared_lexical_keys(&stmts)
                        .into_iter()
                        .flat_map(|key| {
                            // A `my class`/`my role` is also bound in the
                            // type-only key space (`lexical_type_key`).
                            let type_key = crate::term_names::lexical_type_key(&key);
                            [key, type_key]
                        })
                        .map(|key| {
                            let sym = crate::symbol::Symbol::intern(&key);
                            let prev = self.env.get_sym(sym).cloned();
                            (sym, prev)
                        })
                        .collect();
                // Snapshot stubs already pending in the outer unit. Class/package
                // decls install at BEGIN time in raku, so an outer stub that is
                // defined later in the file is NOT undefined from the EVAL's view;
                // only stubs the EVAL itself introduces (and leaves unresolved)
                // should error here.
                let eval_pre_stubs: HashSet<String> = self
                    .registry()
                    .class_stubs
                    .iter()
                    .chain(self.registry().package_stubs.iter())
                    .cloned()
                    .collect();
                // Raku resolves `self!private()` at compile time, so a typo'd
                // private call in the EVAL'd string errors even if a preceding
                // `return` would short-circuit it at runtime.
                self.validate_private_calls_against_self(&stmts)?;
                // The lexical cleanup below has to run whatever the snippet did, so
                // carry the outcome instead of `?`-ing out of the middle: a
                // `my` in code that then *dies* (`throws-like 'my $x = 1; die …'`
                // is a common assertion shape) is still EVAL-scoped.
                //
                // A `my class`/`my package` the snippet declares must not stay
                // resolvable by bareword after the EVAL returns (raku: `EVAL 'my
                // package A { }'; A` is `X::Undeclared` even when nothing else in
                // the program ever mentions `A`) — the same push/pop pair a bare
                // `{ ... }` block already uses (`vm_misc_scope.rs`) to re-suppress
                // whatever it declared on the way out. Without this, a `my class A`
                // that had gone out of scope *earlier* in the program (and was
                // therefore suppressed) gets silently un-suppressed for good the
                // moment ANY later EVAL — even one that itself fails after
                // declaring `A` — redeclares the same bare name, since
                // `unsuppress_name` runs unconditionally when a class/package body
                // starts executing (`vm_typedecl_ops.rs`) and nothing here used to
                // undo it. Pop runs unconditionally (mirrors the block cleanup
                // being exception-safe), so a failing snippet is cleaned up too.
                self.push_lexical_class_scope();
                let mut free_var_writes = Vec::new();
                // `nqp::getcomp("Raku").eval` keeps what this unit declares for
                // the next line of a REPL session (runtime::repl_compiler); the
                // snapshot has to be taken before the cleanup below drops it.
                self.begin_unit_capture();
                let mut outcome = self.eval_unit_value(&stmts, &mut free_var_writes);
                self.end_unit_capture();
                // The free variables this snippet WROTE (`EVAL '$a = 32'`). They are
                // assignments to the caller's lexicals, not the snippet's own `my`,
                // so the leaked-lexical cleanup below must leave them alone.
                let snippet_free_var_writes: HashSet<crate::symbol::Symbol> = free_var_writes
                    .iter()
                    .filter(|n| crate::env::is_plain_user_lexical(n))
                    .map(|n| crate::symbol::Symbol::intern(n))
                    .collect();
                self.pop_lexical_class_scope();
                if let Ok(value) = &outcome
                    && self.eval_result_is_unresolved_bareword(&stmts, value)
                {
                    outcome = Err(RuntimeError::undeclared_symbols("Undeclared name"));
                }
                if outcome.is_ok()
                    && let Err(e) = self.check_unresolved_stubs_excluding(&eval_pre_stubs)
                {
                    outcome = Err(e);
                }
                // The EVAL is its own compilation unit, so a stub it introduced
                // and left unresolved dies WITH that unit — whatever the
                // outcome. The check above only runs when the snippet
                // succeeded, so an EVAL that died BEFORE reaching it (e.g.
                // `class A { ... }; class B does A { }`, which dies composing
                // against the stub) used to leave `A` sitting in the outer
                // program's registry; the top-level end-of-run check then
                // reported "The following packages were stubbed but not
                // defined: A" for a name the outer program never mentioned,
                // and the process exited non-zero after every test had passed.
                // raku prints nothing at all there (measured). Surfaced by
                // `roast/integration/error-reporting.t` and
                // `roast/S12-class/augment-supersede.t` under
                // `MUTSU_REAL_TEST=1`, where `throws-like` really EVALs the
                // code string it is given.
                //
                // Mark them reported rather than REMOVING them: `class_stubs`
                // is what every class-system check reads to answer "is this
                // name still an open stub", so deleting the entry would make a
                // *later* EVAL that re-stubs the same name see a fully-defined
                // class instead — `EVAL 'class A { ... }; class B is A {}'`
                // stopped raising `X::Inheritance::NotComposed` once a previous
                // EVAL had stubbed `A` (caught by `roast/S12-class/stubs.t`
                // test 7). `reported_stub_errors` exists for exactly this
                // distinction: the name stays a stub, only its *error* is
                // spent.
                let eval_introduced_stubs: Vec<String> = self
                    .registry()
                    .class_stubs
                    .iter()
                    .chain(self.registry().package_stubs.iter())
                    .filter(|n| !eval_pre_stubs.contains(*n))
                    .cloned()
                    .collect();
                for name in eval_introduced_stubs {
                    self.registry_mut().reported_stub_errors.insert(name);
                }
                // When the last statement is an assignment, the VM pops the
                // value from the stack, so eval_block_value returns Nil/Any.
                // In Raku, EVAL returns the value of the last expression,
                // which for assignments is the assigned value.
                if let Ok(value) = &mut outcome
                    && (value.is_nil()
                        || matches!(value.view(), ValueView::Package(name) if name == "Any"))
                    && let Some(Stmt::Assign { name, .. }) = stmts.last()
                    && let Some(assigned) = self.env.get(name).cloned()
                {
                    *value = assigned;
                }
                // Drop the EVAL's own `my` lexicals (plain user lexical keys that
                // did not exist before) so they don't leak into the caller's pad.
                //
                // A name the snippet ASSIGNED to is not one of its own lexicals,
                // even when the caller's env had no entry for it: a `my $a;` with no
                // initializer materializes no env key, so `EVAL '$a = 32'` created
                // the key and this cleanup used to delete the write — which is how
                // an `EVAL` that assigns an outer lexical silently lost the
                // assignment as soon as it ran inside a closure or a routine (the
                // mainline still worked only because the caller's slot was
                // reconciled from `env` before the removal). The snippet's own
                // compiler knows exactly which free variables it wrote; those are
                // exempt. A `my` the snippet really declares is a LOCAL of its code,
                // never a free variable, so it is still dropped here (and a `my`
                // that shadows a caller name is separately restored by
                // `eval_shadowed` below).
                let leaked: Vec<crate::symbol::Symbol> = self
                    .env
                    .keys()
                    .filter(|k| {
                        !eval_pre_lexicals.contains(k)
                            && !snippet_free_var_writes.contains(k)
                            && k.with_str(|s| {
                                crate::env::is_plain_user_lexical(s) && !s.starts_with('&')
                            })
                    })
                    .copied()
                    .collect();
                for key in leaked {
                    self.env.remove_sym(key);
                }
                // Restore the caller's value for each name the snippet re-declared,
                // so the EVAL's `my` stayed scoped to the EVAL.
                for (sym, prev) in eval_shadowed {
                    match prev {
                        Some(v) => {
                            self.env.insert_sym(sym, v);
                        }
                        None => {
                            self.env.remove_sym(sym);
                        }
                    }
                }
                outcome
            }
            Err(parse_err) => {
                // BEGIN blocks should execute even when a later parse error occurs.
                // Do a partial parse to find any BEGIN phasers and execute them
                // before returning the parse error.
                let (partial_stmts, _) =
                    crate::parser::parse_program_partial_with_operators(src, op_names, op_assoc);
                self.execute_begin_phasers(&partial_stmts);
                Err(parse_err)
            }
        }
    }

    /// When EVAL is called inside a class body, method declarations should be
    /// added to the enclosing class rather than lowered to subs. This method
    /// extracts MethodDecl statements and injects them into the current class,
    /// returning the remaining statements for normal evaluation.
    pub(super) fn inject_eval_methods_into_class(&mut self, stmts: Vec<Stmt>) -> Vec<Stmt> {
        // Only applies when current_package is a class being defined
        let class_name = self.current_package();
        if !self.registry().classes.contains_key(&class_name) {
            return stmts;
        }
        let mut remaining = Vec::new();
        for stmt in stmts {
            if let Stmt::MethodDecl {
                name: method_name,
                name_expr: _,
                param_defs,
                body: method_body,
                multi,
                is_rw,
                is_raw,
                is_private,
                is_my,
                return_type,
                is_default_candidate,
                deprecated_message,
                ..
            } = &stmt
            {
                let resolved_method_name = method_name.resolve();
                let effective_param_defs =
                    crate::method_signature_shared::effective_method_param_defs(param_defs, false);
                let effective_params: Vec<String> = effective_param_defs
                    .iter()
                    .map(|p| p.name.clone())
                    .collect();
                let def = MethodDef {
                    syms: Default::default(),
                    lexical_package: self.current_package_sym(),
                    params: effective_params,
                    param_defs: effective_param_defs,
                    body: std::sync::Arc::new(method_body.clone()),
                    is_rw: *is_rw,
                    is_raw: *is_raw,
                    is_private: *is_private,
                    is_multi: *multi,
                    is_my: *is_my,
                    role_origin: None,
                    original_role: None,
                    return_type: return_type.clone(),
                    compiled_code: None,
                    compiled_fns: None,
                    delegation: None,
                    is_default: *is_default_candidate,
                    deprecated_message: deprecated_message.clone(),
                    is_submethod: false,
                    is_hidden_from_backtrace: false,
                    captured_env: None,
                    source_file: self.current_source_file(),
                    role_param_bindings: None,
                    nested_capture_index: None,
                    captured_readonly: None,
                    routine_cell: Default::default(),
                };
                let owner = crate::symbol::Symbol::intern(&class_name);
                let method_sym = crate::symbol::Symbol::intern(&resolved_method_name);
                if *multi {
                    self.registry_mut().push_user_method(owner, method_sym, def);
                } else {
                    self.registry_mut()
                        .set_user_methods(owner, method_sym, vec![def]);
                }
            } else {
                remaining.push(stmt);
            }
        }
        remaining
    }

    /// Execute BEGIN phasers found in a list of statements (used for partial
    /// parse results where a later parse error prevents full evaluation).
    pub(super) fn execute_begin_phasers(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            if let Stmt::Phaser {
                kind: PhaserKind::Begin,
                body,
                ..
            } = stmt
            {
                let _ = self.eval_block_value(body);
            }
        }
    }

}
