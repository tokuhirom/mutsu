//! Package type traits use the same compiled multi-dispatch as class traits.

use super::*;

impl Interpreter {
    // Cost: O(a + c), a = trait name bytes, c = trait_mod:<is> candidates; excludes the callback body.
    pub(super) fn exec_apply_package_trait_op(
        &mut self,
        code: &CompiledCode,
        package_idx: u32,
        trait_idx: u32,
        explicit_argument: bool,
    ) -> Result<(), RuntimeError> {
        let argument = self.stack.pop().unwrap_or(Value::TRUE);
        let package = Self::const_str(code, package_idx);
        let name = Self::const_str(code, trait_idx);
        let known_type = self.has_type(name)
            || self.has_class(name)
            || self.has_role(name)
            || Self::is_builtin_type(name)
            || crate::core_type_names::contains(name);
        let target = Value::package(code.const_sym(package_idx));
        let trait_argument = if known_type && !explicit_argument {
            Value::package(Symbol::intern(&self.canonical_class_parent_name(name)))
        } else {
            Value::pair(name.to_string(), argument)
        };
        if self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>") {
            match self.vm_call_function("trait_mod:<is>", vec![target, trait_argument]) {
                Ok(_) => return Ok(()),
                Err(error) if Self::is_trait_mod_no_candidate(&error) => {}
                Err(error) => return Err(error),
            }
        }
        let method = match name {
            "rw" => Some("set_rw"),
            "hidden" => Some("set_hidden"),
            _ if known_type => Some("add_parent"),
            _ => None,
        };
        if let Some(method) = method {
            let how = match self.registry().package_kinds.get(package) {
                Some(crate::ast::PackageKind::Module) => "Perl6::Metamodel::ModuleHOW",
                _ => "Perl6::Metamodel::PackageHOW",
            };
            return Err(RuntimeError::meta_method_not_found(method, how));
        }
        Err(self.unknown_parent_error(package, name))
    }
}
