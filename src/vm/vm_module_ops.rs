//! Module ops: `use`/`import`/`need`/`no` and `use lib`/var-export.
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(super) fn exec_use_module_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        tags_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let module = Self::const_str(code, name_idx);
        let tags: Vec<String> = tags_idx
            .and_then(|idx| code.constants.get(idx as usize))
            .and_then(|v| match v {
                Value::Array(items, ..) => Some(
                    items
                        .iter()
                        .map(|v| v.to_string_value())
                        .collect::<Vec<String>>(),
                ),
                _ => None,
            })
            .unwrap_or_default();
        self.vm_use_module_with_tags(module, &tags)?;
        self.fn_resolve_gen += 1;
        self.method_resolve_cache.clear();
        self.last_method_resolve = None;
        self.fast_method_cache.clear();
        self.multi_resolve_cache.clear();
        self.multi_type_cacheable.clear();
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
            .and_then(|v| match v {
                Value::Array(items, ..) => Some(
                    items
                        .iter()
                        .map(|v| v.to_string_value())
                        .collect::<Vec<String>>(),
                ),
                _ => None,
            })
            .unwrap_or_default();
        loan_env!(self, import_module(module, &tags))?;
        self.fn_resolve_gen += 1;
        self.method_resolve_cache.clear();
        self.last_method_resolve = None;
        self.fast_method_cache.clear();
        self.multi_resolve_cache.clear();
        self.multi_type_cacheable.clear();
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
        self.no_module(module)?;
        self.fn_resolve_gen += 1;
        self.method_resolve_cache.clear();
        self.last_method_resolve = None;
        self.fast_method_cache.clear();
        self.multi_resolve_cache.clear();
        self.multi_type_cacheable.clear();
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
        self.need_module(module)?;
        self.fn_resolve_gen += 1;
        self.method_resolve_cache.clear();
        self.last_method_resolve = None;
        self.fast_method_cache.clear();
        self.multi_resolve_cache.clear();
        self.multi_type_cacheable.clear();
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
        let value = self.stack.pop().unwrap_or(Value::Nil);
        let path = value.to_string_value();
        if path.is_empty() {
            return Err(RuntimeError::new(
                "X::LibEmpty: Repository specification can not be an empty string",
            ));
        }
        // An `inst#PREFIX` spec selects a CompUnit::Repository::Installation as
        // the current `$*REPO`, chained in front of whatever was there before.
        if let Some(prefix) = path.strip_prefix("inst#") {
            let prev = self.env().get("*REPO").cloned().unwrap_or(Value::Nil);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("prefix".to_string(), Value::str(prefix.to_string()));
            attrs.insert("next-repo".to_string(), prev);
            let repo =
                Value::make_instance(Symbol::intern("CompUnit::Repository::Installation"), attrs);
            self.env_mut().insert("*REPO".to_string(), repo);
        }
        self.add_lib_path(path);
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
            .and_then(|v| match v {
                Value::Array(items, ..) => Some(
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
