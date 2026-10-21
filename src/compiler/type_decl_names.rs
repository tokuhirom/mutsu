//! Explicit declaration-name provenance shared by plans and their body chunks.

use super::Compiler;

impl Compiler {
    pub(super) fn decl_name_is_prequalified(&self) -> bool {
        self.in_unit_package
            && self.current_package != "GLOBAL"
            && !self.current_package.contains("::&")
    }

    pub(super) fn declared_type_name(&self, name: &str, prequalified: bool) -> String {
        if let Some(absolute) = name.strip_prefix("GLOBAL::") {
            absolute.to_string()
        } else if prequalified || self.runtime_current_package() == "GLOBAL" {
            name.to_string()
        } else {
            format!("{}::{name}", self.runtime_current_package())
        }
    }

    pub(super) fn declared_class_storage_name(
        &self,
        name: &str,
        prequalified: bool,
        lexical: bool,
        decl_id: u64,
    ) -> String {
        let qualified = self.declared_type_name(name, prequalified);
        if lexical && decl_id != 0 {
            format!("{qualified}\u{0}{decl_id}")
        } else {
            qualified
        }
    }
}
