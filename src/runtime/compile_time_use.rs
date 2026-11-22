//! The interpreter's side of the compile-time loader seam (ADR-17558): a `use`
//! loads its module when the parser reaches it, in the interpreter that will
//! run the unit, and the unit's run-time `use` then only imports.
//!
//! The load is the BEGIN-time preload the prologue performs for a nested `use`
//! (`OpCode::PreloadModule`, `Interpreter::preload_module`), moved to the
//! moment the parser finishes the statement: the module's packages, types and
//! routines are registered and its mainline runs, nothing is imported. The
//! `use` that later runs in position finds the module in `loaded_modules` and
//! takes the import-only path. A module's `sub EXPORT` hook (its own, or the
//! `&EXPORT` it imported, the Slangify pattern) is only recorded by the load:
//! the import runs it, once per `use`, as rakudo does.

use super::*;
use crate::compile_time_loader::{
    CompileTimeLoader, LoadOutcome, LoadedDeclarator, LoadedExport, LoadedFacts, LoaderGuard,
    UseRequest,
};

impl CompileTimeLoader for Interpreter {
    // Cost: O(p) for a module already loaded or not taken, p = search path
    // entries; the module's own load otherwise.
    fn load_use(&mut self, request: &UseRequest<'_>) -> LoadOutcome {
        // The slang activation interpreter (ADR-0026 §2.1) keeps loading at
        // run time: which `define_slang` it hands to the importer is decided by
        // the load order of the run-time `use`s (`slang_define_slang`). Slice 4
        // of ADR-17558 retires that interpreter.
        if std::thread::current().name()
            == Some(crate::runtime::slang_activation::ACTIVATION_THREAD_NAME)
        {
            return LoadOutcome::NotTaken;
        }
        // The same `use`s the prologue preloads: not a pragma, `Test`, a
        // `:from<...>` compunit, a `:if` load or one whose arguments feed a
        // `sub EXPORT`.
        let Some(module) = crate::compiler::begin_use::preloadable_use_shape(
            request.module,
            request.tags,
            request.has_condition,
            request.has_arg && request.literal_args.is_none(),
        ) else {
            self.reserve_module_end_slot(request);
            return LoadOutcome::NotTaken;
        };
        let (bare, _) = Self::split_dist_selectors(module);
        // A module loaded already (by an earlier `use`) does not load again,
        // but its `sub EXPORT` runs for this `use` too, now, as rakudo runs it
        // once per `use` statement at compile time.
        let already_loaded = self.module.loaded_modules.contains(bare);
        if already_loaded
            && (request.activates_slang || !self.module.module_export_defs.contains_key(bare))
        {
            return LoadOutcome::Loaded;
        }
        // The unit's parse-time `use lib` additions are BEGIN-time effects that
        // precede this `use`: the module resolves through them, prepended as a
        // `use lib` prepends. They are on the chain only for this load; the
        // BEGIN-time prologue registers them for good (and in `$*REPO`), at
        // their own positions.
        let known = self.parser_scan_lib_paths();
        let saved_lib_paths = std::sync::Arc::clone(&self.module.lib_paths);
        for path in request.lib_paths.iter().filter(|path| !known.contains(path)) {
            self.prepend_lib_path(Self::strip_file_repo_spec(path.clone()));
        }
        // The module's own compile is a parse of its own: the importer's parse
        // is suspended around it and resumes exactly where it was.
        let suspended = crate::parser::unit_state::suspend();
        // A module that imports through a `sub EXPORT` hook runs it now, once
        // for this `use`, as rakudo does at compile time; the map it returns
        // is kept for the `use`'s import in position, which installs it
        // instead of running the hook again (`rerun_module_export`).
        // A literal argument list goes to the hook as the run-time `use`
        // would hand it over, so the import finds the map under the same key.
        let saved_export_args = std::mem::replace(
            &mut self.module.pending_use_export_args,
            request.literal_args.clone(),
        );
        // A `use` in a module's own compile imports into the module's package,
        // as it does from the module's prologue (`import_target_package_for_load`
        // reads the `unit module` name there): the type aliases its load
        // records belong to the module, not to the unit that loads it.
        let in_module_compile = self
            .module
            .module_loading_unit_stack
            .last()
            .is_some_and(|&(_, depth)| depth == self.routine_stack_len());
        let replaced_import_target = match (&request.package, in_module_compile) {
            (Some(package), true) => Some(
                self.module
                    .import_target_package
                    .replace(package.clone()),
            ),
            _ => None,
        };
        let saved_hook_depth = self
            .module
            .parse_time_hook_depth
            .replace(self.module.module_load_stack.len() + 1);
        let saved_hook_site =
            std::mem::replace(&mut self.module.parse_time_hook_site, request.site);
        take_hook_reached_importer();
        let nested = request.nested;
        let load = move |interp: &mut Self| {
            if already_loaded {
                interp.run_export_hook_for_parse(bare)
            } else {
                interp.without_use_attach_depth(|interp| interp.preload_module(module))
            }
        };
        let preload = move |interp: &mut Self| {
            if nested {
                // A `use` inside a block: what the module installs while it
                // loads is lexical to the block (`OpCode::PreloadModule`'s scope).
                interp.push_preload_scope();
                let result = load(interp);
                interp.pop_import_scope();
                result
            } else {
                load(interp)
            }
        };
        // A slang module registers its slang for this parse while it loads
        // (its `EXPORT` hook calls `$*LANG.define_slang`), as on rakudo; the
        // parser reads what it registered (ADR-17558 slice 4).
        let result = if request.activates_slang {
            self.load_activating_slang(bare, request.literal_args.as_deref(), preload)
        } else {
            preload(self)
        };
        self.module.parse_time_hook_depth = saved_hook_depth;
        self.module.parse_time_hook_site = saved_hook_site;
        if let Some(saved) = replaced_import_target {
            self.module.import_target_package = saved;
        }
        self.module.pending_use_export_args = saved_export_args;
        drop(suspended);
        self.module.lib_paths = saved_lib_paths;
        match result {
            Ok(()) => LoadOutcome::Loaded,
            // A module the chain cannot find now may still be found when the
            // unit runs (a `use lib` the parser could not fold, a repository a
            // BEGIN registers): the in-position `use` loads it, or reports it.
            Err(err) if err.is_unsatisfied_dependency() => LoadOutcome::NotTaken,
            Err(err) => LoadOutcome::Failed(err),
        }
    }

    // Cost: O(s + v + k), s = exported subs of `module`, v = its exported
    // symbols, k = registered candidates of its exported operators.
    fn loaded_facts(&mut self, module: &str, import_tags: Option<&[String]>) -> LoadedFacts {
        let (bare, _) = Self::split_dist_selectors(module);
        let exports = self
            .module_exported_routine_names(bare, import_tags)
            .into_iter()
            .map(|name| {
                let op_prec = self.declared_op_prec_of_name(&name);
                LoadedExport { name, op_prec }
            })
            .collect();
        // The `EXPORTHOW` declarators the load recorded for the module, a
        // BEGIN-time merge included (#17557), each with what its HOW object
        // builds (slice 3).
        let declarators = self
            .module
            .exporthow_declares
            .get(bare)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .map(|(directive, keyword)| {
                let is_role = directive == "DECLARE"
                    && self
                        .declarator_how_type(&keyword)
                        .is_some_and(|how| self.how_builds_parametric_role(&how));
                LoadedDeclarator {
                    directive,
                    keyword,
                    is_role,
                }
            })
            .collect();
        LoadedFacts {
            exports,
            declarators,
        }
    }

    // Cost: O(a), a = slangs activated at parse time so far.
    fn slang_activation(
        &mut self,
        module: &str,
        args: Option<&[crate::slang_link::SlangUseArg]>,
    ) -> Option<crate::slang_link::SlangActivation> {
        self.parse_time_slang_activation(module, args)
    }

    // Cost: O(n) for the analysis, n = size of the phaser's body; the
    // phaser's own run otherwise.
    fn run_begin(&mut self, begin: &Stmt) -> crate::compile_time_loader::BeginOutcome {
        self.run_parse_time_begin(begin)
    }

    // Cost: O(1).
    fn reserve_decl_order(&mut self) -> Option<u64> {
        crate::runtime::compile_time_begin::is_program_parse()
            .then(crate::runtime::resolution::next_decl_order)
    }
}

impl Interpreter {
    /// Reserve the END install position of a main-unit `use` this loader
    /// leaves to run time (#18183): its ENDs then run where rakudo, which
    /// installs every `use` at compile time, runs them -- before those of the
    /// modules an earlier `use` loaded, after those of a later one's.
    ///
    /// Only the main unit's own `use`s reserve: a module's `use`s run during
    /// that module's load, which is already in order. A module loaded once
    /// keeps its first reservation.
    // Cost: O(|module|).
    fn reserve_module_end_slot(&mut self, request: &UseRequest<'_>) {
        if !self.control.module_load_order.is_empty()
            || request.tags.iter().any(|t| t == "from")
        {
            return;
        }
        let (bare, _) = Self::split_dist_selectors(request.module);
        if self.module.loaded_modules.contains(bare)
            || self.control.reserved_module_end_slots.contains_key(bare)
        {
            return;
        }
        let seq = self.control.end_phaser_seq;
        self.control.end_phaser_seq += 1;
        self.control
            .reserved_module_end_slots
            .insert(bare.to_string(), seq);
    }

    /// Run `parse`, the parse of a unit this interpreter is about to run, with
    /// this interpreter as its compile-time loader. A parse that failed because
    /// a `use` failed to load reports the load's error.
    // Cost: O(1) besides `parse`.
    pub(crate) fn parse_with_loader<T>(
        &mut self,
        parse: impl FnOnce() -> Result<T, RuntimeError>,
    ) -> Result<T, RuntimeError> {
        crate::runtime::compile_time_begin::with_parse_kind(false, || {
            self.parse_unit_with_loader(parse)
        })
    }

    /// [`Interpreter::parse_with_loader`] for the program's own unit, whose
    /// unit-level `BEGIN`s may also run at parse time (ADR-17558 slice 5).
    // Cost: O(1) besides `parse`.
    pub(crate) fn parse_program_with_loader<T>(
        &mut self,
        parse: impl FnOnce() -> Result<T, RuntimeError>,
    ) -> Result<T, RuntimeError> {
        crate::runtime::compile_time_begin::with_parse_kind(true, || {
            self.parse_unit_with_loader(parse)
        })
    }

    // Cost: O(1) besides `parse`.
    fn parse_unit_with_loader<T>(
        &mut self,
        parse: impl FnOnce() -> Result<T, RuntimeError>,
    ) -> Result<T, RuntimeError> {
        let loader: *mut dyn CompileTimeLoader = self as *mut Self;
        crate::compile_time_loader::take_failure();
        // SAFETY: `loader` comes from the live `&mut self`, which this function
        // holds for the guard's whole lifetime and does not use until the
        // guard is dropped: `parse` cannot capture `self` (it is mutably
        // borrowed by this call), so only `compile_time_loader::load_use`
        // re-borrows the interpreter while the guard lives.
        let guard = unsafe { LoaderGuard::push(loader) };
        let result = parse();
        drop(guard);
        let failure = crate::compile_time_loader::take_failure();
        match (result, failure) {
            (Err(_), Some(failure)) => Err(failure),
            (result, _) => result,
        }
    }
}

impl Interpreter {
    /// The `Routine.prec` hash declared on the operator routine `name`, found
    /// through the registry's candidates for that name.
    // Cost: O(k), k = registered candidates of `name`.
    pub(crate) fn declared_op_prec_of_name(&mut self, name: &str) -> Option<crate::op_prec::OpPrec> {
        crate::op_prec::split_op_name(name)?;
        let keys = self.fn_keys_for_base(name);
        keys.iter().find_map(|key| {
            self.routines()
                .functions
                .get(key)
                .and_then(|def| def.op_prec.clone())
        })
    }

    /// Every routine name `module` exports under a tag `import_tags` imports
    /// (`None`: every tag): its exported subs, plus the `&`-sigiled entries of
    /// its exported symbols (a closure bound into an export stash is recorded
    /// as an exported `&name` variable). A `use` with no tags imports
    /// `DEFAULT`, one with tags imports those; `MANDATORY` always comes, and
    /// `:ALL` takes everything. Sorted and deduplicated.
    // Cost: O(s + v), s = exported subs of `module`, v = its exported symbols.
    pub(crate) fn module_exported_routine_names(
        &self,
        module: &str,
        import_tags: Option<&[String]>,
    ) -> Vec<String> {
        let admits = |tags: &dyn Fn(&str) -> bool| match import_tags {
            None => true,
            Some(requested) if requested.iter().any(|tag| tag == "ALL") => true,
            Some([]) => tags("DEFAULT") || tags("MANDATORY"),
            Some(requested) => tags("MANDATORY") || requested.iter().any(|tag| tags(tag)),
        };
        let mut names: Vec<String> = Vec::new();
        if let Some(subs) = self.module.exported_subs.get(module) {
            names.extend(
                subs.iter()
                    .filter(|(_, tags)| admits(&|tag| tags.contains(tag)))
                    .map(|(name, _)| name.clone()),
            );
        }
        if let Some(vars) = self.module.exported_vars.get(module) {
            names.extend(
                vars.iter()
                    .filter(|(_, tags)| admits(&|tag| tags.contains(tag)))
                    .filter_map(|(name, _)| name.strip_prefix('&'))
                    .filter(|name| !name.is_empty())
                    .map(str::to_string),
            );
        }
        names.sort();
        names.dedup();
        names
    }
}

thread_local! {
    /// Set when a `sub EXPORT` hook running with no in-position `use` (a
    /// preload, a parse-time load) asked `$*R` to attach a phaser to its
    /// importing scope, which only the run at the `use`'s import performs
    /// (`runtime::attach_target`): the parse-time run cannot stand in for it
    /// (`record_parse_time_hook_result`).
    static HOOK_REACHED_IMPORTER: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
    /// Set while `Str.AST` parses: the tree is compiled again by the `EVAL`
    /// that runs it, whose own `use`s run their hooks then, as on rakudo, so
    /// a hook run of the `.AST` parse keeps nothing for an import.
    static PARSE_FOR_AST: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// Run `parse`, a `Str.AST` parse, with [`PARSE_FOR_AST`] set.
// Cost: O(1) besides `parse`.
/// The installation stamp the parse reserved for a candidate of the program's
/// top level, among its declaration's traits
/// (`compile_time_loader::PARSE_DECL_ORDER_TRAIT`, ADR-17558 slice 5b).
// Cost: O(t), t = the declaration's traits.
pub(crate) fn parse_decl_order(custom_traits: &[(String, Option<crate::opcode::DeclTraitArg>)]) -> Option<u64> {
    custom_traits.iter().find_map(|(t, arg)| {
        (t == crate::compile_time_loader::PARSE_DECL_ORDER_TRAIT)
            .then(|| arg.as_ref()?.literal()?.as_int())
            .flatten()
            .and_then(|order| u64::try_from(order).ok())
    })
}

pub(crate) fn parsing_for_ast<T>(parse: impl FnOnce() -> T) -> T {
    let saved = PARSE_FOR_AST.with(|flag| flag.replace(true));
    let result = parse();
    PARSE_FOR_AST.with(|flag| flag.set(saved));
    result
}

/// Whether the parse in progress is a `Str.AST` parse.
// Cost: O(1).
pub(crate) fn parse_is_for_ast() -> bool {
    PARSE_FOR_AST.with(std::cell::Cell::get)
}

/// Note that the running hook reached its importing unit (see
/// `HOOK_REACHED_IMPORTER`).
// Cost: O(1).
pub(crate) fn note_hook_reached_importer() {
    HOOK_REACHED_IMPORTER.with(|flag| flag.set(true));
}

/// Whether a hook reached its importing unit since the last call, clearing it.
// Cost: O(1).
pub(crate) fn take_hook_reached_importer() -> bool {
    HOOK_REACHED_IMPORTER.with(|flag| flag.replace(false))
}
