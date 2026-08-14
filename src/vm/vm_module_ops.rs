//! Module ops: `use`/`import`/`need`/`no` and `use lib`/var-export.
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn exec_use_module_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        tags_idx: Option<u32>,
        arg_count: u16,
    ) -> Result<(), RuntimeError> {
        let module = Self::const_str(code, name_idx);
        // Pop the `use`-argument values pushed by the compiler (in source
        // order) and stash them for the module's `sub EXPORT`. Cleared by
        // `use_module_with_tags` even when the load takes a native/early-return
        // path, so they can never leak into a later `use`.
        if arg_count > 0 {
            let n = arg_count as usize;
            let split = self.stack.len().saturating_sub(n);
            let args: Vec<Value> = self.stack.split_off(split);
            self.pending_use_export_args = Some(args);
        } else {
            self.pending_use_export_args = None;
        }
        let tags: Vec<String> = tags_idx
            .and_then(|idx| code.constants.get(idx as usize))
            .and_then(|v| match v.view() {
                ValueView::Array(items, ..) => Some(
                    items
                        .iter()
                        .map(|v| v.to_string_value())
                        .collect::<Vec<String>>(),
                ),
                _ => None,
            })
            .unwrap_or_default();
        // ADR-0019 Phase F box F5 cutover: a module load can install classes
        // and subs, but each such installation already invalidates dispatch
        // caches at its OWN registration site (`exec_register_class_op`
        // bumps `Registry::method_generation` unconditionally on every real
        // change; `exec_register_sub_op` calls
        // `invalidate_method_dispatch_caches()`, including its unconditional
        // `fn_resolve_gen` bump, on every actual install). This USED to be
        // reinforced by a second, redundant `invalidate_method_dispatch_caches()`
        // call right here, after the module finished loading. Verified via the
        // `MUTSU_VM_STATS`-gated shadow check below across the full `t/` suite
        // (4049 checks, 164 generation bumps) and the roast whitelist (2479
        // checks, 89 bumps): every bump traces to a module that genuinely
        // installs a class (`use`/`need`), and no bump was ever needed for a
        // module with nothing to install. See the box's progress notes in
        // `docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md`.
        let f5_gen_before = self.registry().method_generation;
        self.vm_use_module_with_tags(module, &tags)?;
        // Shadow-only: confirms the claim above holds; does not affect
        // dispatch (see `record_module_reg_gen_shadow_check`'s doc comment).
        {
            let f5_gen_after = self.registry().method_generation;
            crate::vm::vm_stats::record_module_reg_gen_shadow_check(
                f5_gen_after != f5_gen_before,
                || format!("use module={module}"),
            );
        }
        // A module load writes imported symbols into env by name; flag the env so
        // the next GetLocal barrier reconciles them into locals. (An eager
        // sync_locals_from_env here is unsafe: it can clobber a fresh in-place
        // cell mutation of a local that env does not yet reflect -- see the
        // cyclic-`:=`-bind regression in t/element-bind-cell.t. Only the
        // flag-deferred barrier pull, which runs once env is fresh, is correct.)
        Ok(())
    }

    pub(super) fn exec_import_module_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        tags_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let module = Self::const_str(code, name_idx);
        let tags = tags_idx
            .and_then(|idx| code.constants.get(idx as usize))
            .and_then(|v| match v.view() {
                ValueView::Array(items, ..) => Some(
                    items
                        .iter()
                        .map(|v| v.to_string_value())
                        .collect::<Vec<String>>(),
                ),
                _ => None,
            })
            .unwrap_or_default();
        // ADR-0019 Phase F box F5 cutover: see `exec_use_module_op`'s comment.
        let f5_gen_before = self.registry().method_generation;
        loan_env!(self, import_module(module, &tags))?;
        {
            let f5_gen_after = self.registry().method_generation;
            crate::vm::vm_stats::record_module_reg_gen_shadow_check(
                f5_gen_after != f5_gen_before,
                || format!("import module={module}"),
            );
        }
        // Slice F: write imported symbols through to the caller's local slots
        // (import_module recorded their names); keeps an imported `constant c`
        // coherent without the reverse pull. This op holds the outer `code`.
        self.apply_pending_rw_writeback(code);
        // A module load writes imported symbols into env by name; flag the env so
        // the next GetLocal barrier reconciles them into locals. (An eager
        // sync_locals_from_env here is unsafe: it can clobber a fresh in-place
        // cell mutation of a local that env does not yet reflect -- see the
        // cyclic-`:=`-bind regression in t/element-bind-cell.t. Only the
        // flag-deferred barrier pull, which runs once env is fresh, is correct.)
        Ok(())
    }

    pub(super) fn exec_no_module_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let module = Self::const_str(code, name_idx);
        // ADR-0019 Phase F box F5 cutover: see `exec_use_module_op`'s comment.
        let f5_gen_before = self.registry().method_generation;
        self.no_module(module)?;
        {
            let f5_gen_after = self.registry().method_generation;
            crate::vm::vm_stats::record_module_reg_gen_shadow_check(
                f5_gen_after != f5_gen_before,
                || format!("no module={module}"),
            );
        }
        // A module load writes imported symbols into env by name; flag the env so
        // the next GetLocal barrier reconciles them into locals. (An eager
        // sync_locals_from_env here is unsafe: it can clobber a fresh in-place
        // cell mutation of a local that env does not yet reflect -- see the
        // cyclic-`:=`-bind regression in t/element-bind-cell.t. Only the
        // flag-deferred barrier pull, which runs once env is fresh, is correct.)
        Ok(())
    }

    pub(super) fn exec_need_module_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let module = Self::const_str(code, name_idx);
        // ADR-0019 Phase F box F5 cutover: see `exec_use_module_op`'s comment.
        let f5_gen_before = self.registry().method_generation;
        self.need_module(module)?;
        {
            let f5_gen_after = self.registry().method_generation;
            crate::vm::vm_stats::record_module_reg_gen_shadow_check(
                f5_gen_after != f5_gen_before,
                || format!("need module={module}"),
            );
        }
        // A module load writes imported symbols into env by name; flag the env so
        // the next GetLocal barrier reconciles them into locals. (An eager
        // sync_locals_from_env here is unsafe: it can clobber a fresh in-place
        // cell mutation of a local that env does not yet reflect -- see the
        // cyclic-`:=`-bind regression in t/element-bind-cell.t. Only the
        // flag-deferred barrier pull, which runs once env is fresh, is correct.)
        Ok(())
    }

    pub(super) fn exec_use_lib_path_op(
        &mut self,
        _code: &CompiledCode,
    ) -> Result<(), RuntimeError> {
        let value = self.stack.pop().unwrap_or(Value::NIL);
        // `use lib` takes a *list* of repository specs (`use lib <a b>`,
        // `use lib "a", "b"`, `use lib @paths`); each element is its own spec,
        // so never stringify the list as a whole.
        let specs: Vec<Value> = match value.view() {
            ValueView::Array(items, ..) => items.iter().cloned().collect(),
            ValueView::Seq(items) | ValueView::Slip(items) => items.iter().cloned().collect(),
            _ => vec![value.clone()],
        };
        for spec in specs {
            self.add_one_lib_path(spec.to_string_value())?;
        }
        Ok(())
    }

    /// Register a single `use lib` repository spec.
    fn add_one_lib_path(&mut self, path: String) -> Result<(), RuntimeError> {
        if path.is_empty() {
            return Err(RuntimeError::new(
                "X::LibEmpty: Repository specification can not be an empty string",
            ));
        }
        // An `inst#PREFIX` spec selects a CompUnit::Repository::Installation as
        // the current `$*REPO`, chained in front of whatever was there before.
        if let Some(prefix) = path.strip_prefix("inst#") {
            let prev = self.env().get("*REPO").cloned().unwrap_or(Value::NIL);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("prefix".to_string(), Value::str(prefix.to_string()));
            attrs.insert("next-repo".to_string(), prev);
            let repo =
                Value::make_instance(Symbol::intern("CompUnit::Repository::Installation"), attrs);
            self.env_mut().insert("*REPO".to_string(), repo);
        }
        // Prepended, mirroring the `$*REPO` chaining just above: a `use lib` path
        // takes precedence over `-I`, `MUTSULIB` and the installed repositories.
        self.prepend_lib_path(path);
        Ok(())
    }

    pub(super) fn exec_register_var_export_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        tags_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        let tags = tags_idx
            .and_then(|idx| code.constants.get(idx as usize))
            .and_then(|v| match v.view() {
                ValueView::Array(items, ..) => Some(
                    items
                        .iter()
                        .map(|v| v.to_string_value())
                        .collect::<Vec<String>>(),
                ),
                _ => None,
            })
            .unwrap_or_else(|| vec!["DEFAULT".to_string()]);
        self.register_exported_var(self.current_package().to_string(), name, tags);
        Ok(())
    }
}
