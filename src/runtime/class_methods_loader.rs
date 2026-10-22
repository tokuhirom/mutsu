//! Class methods of three runtime-internal classes, each the handler of a
//! class-method row (ADR-11276 §9.159): `Rakudo::Internals.REGISTER-DYNAMIC`,
//! `Deprecation.report` and `CompUnit::Loader`'s `load-source` and
//! `load-source-file`.

use super::*;
use crate::value::value_buf::with_buf_bytes;

impl Interpreter {
    /// `Rakudo::Internals.REGISTER-DYNAMIC: '$*name', { ... }` installs a
    /// default for a process dynamic variable by running the initializer
    /// block (which typically does `PROCESS::<$name> = ...`). Real Rakudo runs
    /// it lazily on first access; running it eagerly here is sufficient for the
    /// modules that use it (e.g. DBIish's `$*DBI-DEFS`).
    // Cost: the block's own.
    pub(crate) fn register_dynamic(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let (clean_args, _) = self.sanitize_call_args(args);
        let name = clean_args
            .iter()
            .find(|a| matches!(a.view(), ValueView::Str(_)))
            .map(|v| v.to_string_value());
        let block = clean_args
            .iter()
            .find(|a| {
                matches!(
                    a.view(),
                    ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
                )
            })
            .cloned();
        if let Some(block) = block {
            // Run the initializer block. Its `PROCESS::<$name> = ...` writes
            // into the block's own frame (lost on return), so also bind the
            // dynamic var in the caller's scope from the block's result — the
            // assignment's value — keyed by the supplied name. `$*name`
            // (env key `*name`) then resolves.
            let result = self.call_sub_value(block, Vec::new())?;
            if let Some(name) = name {
                let env_key = match name.strip_prefix("$*").or_else(|| name.strip_prefix('$')) {
                    Some(bare) => format!("*{bare}"),
                    None => format!("*{name}"),
                };
                if self.env().get(&env_key).is_none() {
                    self.env_mut().insert(env_key, result);
                }
            }
        }
        Ok(Value::NIL)
    
    }

    /// `Deprecation.report`: the deprecations seen so far, `Nil` when none.
    // Cost: O(r), r = chars of the report.
    pub(crate) fn deprecation_report() -> Value {
        match super::deprecation::take_report() {
            Some(report) => Value::str(report),
            None => Value::NIL,
        }
    }

    /// `CompUnit::Loader.load-source($buf)`: run the source in this
    /// interpreter and answer a `CompUnit::Handle`.
    // TODO: compile to bytecode -- this uses the interpreter's EVAL
    // Cost: the source's compile and run.
    pub(crate) fn compunit_load_source(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let code_str = match args.first() {
            Some(v) => match v.view() {
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve()) => {
                    with_buf_bytes(&attributes, |bytes| {
                        String::from_utf8_lossy(bytes).to_string()
                    })
                    .unwrap_or_default()
                }
                _ => v.to_string_value(),
            },
            None => String::new(),
        };
        let _ = self.eval_eval_string(&code_str);
        let mut unit_hash = ValueMap::default();
        unit_hash.insert(
            "$?PACKAGE".to_string(),
            Value::package(crate::symbol::Symbol::intern("GLOBAL")),
        );
        let mut handle_attrs = ValueMap::default();
        handle_attrs.insert("unit".to_string(), Value::hash(unit_hash));
        Ok(Value::make_instance(
            crate::symbol::Symbol::intern("CompUnit::Handle"),
            handle_attrs,
        ))
    }

    /// `CompUnit::Loader.load-source-file($path)`: load a source file in the
    /// current interpreter, preserving the caller's dynamic environment.
    /// Test::Script uses this to run a helper script while redirecting
    /// `$*OUT`/`$*ERR` and then reads the resulting globalish package.
    // Cost: the file's read, compile and run.
    pub(crate) fn compunit_load_source_file(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let path = args
            .first()
            .and_then(|value| match value.view() {
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name.resolve() == "IO::Path"
                    || class_name.as_str().starts_with("IO::Path::") =>
                {
                    let attributes = attributes.as_map();
                    let path = attributes
                        .get("path")
                        .map(Value::to_string_value)
                        .unwrap_or_default();
                    Some(self.resolve_io_path_buf(&attributes, &path))
                }
                _ => None,
            })
            .ok_or_else(|| {
                RuntimeError::new(
                    "CompUnit::Loader.load-source-file expects an IO::Path argument",
                )
            })?;
        let code = std::fs::read_to_string(&path).map_err(|err| {
            RuntimeError::new(format!(
                "Failed to load source file '{}': {}",
                path.display(),
                err
            ))
        })?;
        let saved_source_file =
            crate::parser::set_parser_source_file(Some(path.to_string_lossy().to_string()));
        let saved_package = self.current_package_sym();
        self.set_current_package_with_sym(
            "GLOBAL".to_string(),
            crate::symbol::Symbol::intern("GLOBAL"),
        );
        let result = (|| {
            let (stmts, _) = crate::parser::parse_program(&code).map_err(|err| {
                RuntimeError::new(format!(
                    "Parse error in source file {}: {}",
                    path.display(),
                    err.message
                ))
            })?;
            self.check_undeclared_routines_mainline(&stmts)?;
            let registry_snapshot = self.snapshot_routine_registry();
            self.preregister_top_level_subs(&stmts)?;
            let run_result = self.run_block(&stmts);
            if let Err(err) = run_result {
                self.restore_routine_registry_eval(registry_snapshot);
                return Err(err);
            }
            let dispatch_result = self.dispatch_main(&crate::opcode::CompiledFns::default());
            self.restore_routine_registry_eval(registry_snapshot);
            dispatch_result
        })();
        self.set_current_package_with_sym(saved_package.as_str().to_owned(), saved_package);
        crate::parser::set_parser_source_file(saved_source_file);
        result?;

        let mut unit_hash = ValueMap::default();
        unit_hash.insert(
            "$?PACKAGE".to_string(),
            Value::package(crate::symbol::Symbol::intern("GLOBAL")),
        );
        let mut handle_attrs = ValueMap::default();
        handle_attrs.insert("unit".to_string(), Value::hash(unit_hash));
        Ok(Value::make_instance(
            crate::symbol::Symbol::intern("CompUnit::Handle"),
            handle_attrs,
        ))
    }
}
