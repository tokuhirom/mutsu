//! The exported types a module's scan records (`exported_types.rs`).

use super::{DeclScan, package_type_name};

impl DeclScan {
    /// Record the type `name`, declared at the current package path, as
    /// exported under `tags` (`DEFAULT` when there are none).
    pub(super) fn export_type(&mut self, name: &str, tags: &[String]) {
        if !self.spine {
            return;
        }
        let short = name.rsplit_once("::").map_or(name, |(_, short)| short);
        let tags = if tags.is_empty() {
            vec!["DEFAULT".to_string()]
        } else {
            tags.to_vec()
        };
        self.out
            .exported_types
            .push(super::super::exported_types::ExportedType {
                name: short.to_string(),
                qualified: package_type_name(&self.prefix, name),
                tags,
            });
    }
}
