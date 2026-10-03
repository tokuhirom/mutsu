//! Declaration-site identity for `my role` (ADR-0047 P1, #9894).
//!
//! A lexical role is stored in the registry under a mangled storage name,
//! `Name\u{0}<decl_id>`, exactly like a `my class` or a `my subset`. Two
//! same-named lexical roles in sibling scopes therefore never share a registry
//! entry, so a later declaration cannot replace an earlier one's methods for
//! type objects and mixins that escaped its block. The bare name is bound to
//! the storage name in the declaring scope's env and restored at scope exit.
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// The registry key a role declaration registers under, and whether it is
    /// a mangled lexical storage name.
    ///
    /// A later declaration of the same name in the SAME scope is a further
    /// candidate of the same role group (`my role R[$x] { }; my role R[$x, $y]
    /// { }`) or the completion of a stub (`my role R { ... }`), so it reuses
    /// that declaration's storage name instead of minting its own.
    ///
    /// A compound name the source wrote (`my role Entry::Handler`) is left
    /// unmangled: it is also reached by its package-qualified spelling
    /// (`TAP::Entry::Handler`), which a mangled key cannot answer. `name` may
    /// arrive pre-qualified by the compiler (`role R1` in `unit module M` is
    /// `M::R1`), so only `has_source_compound_name` or a `::` outside the
    /// current package's prefix counts as written.
    // Cost: O(s + n), s = declarations recorded in the innermost scope frame,
    // n = name length.
    pub(super) fn lexical_role_storage_name(
        &mut self,
        name: &str,
        qualified: &str,
        current_package: &str,
        is_my_scoped: bool,
        has_source_compound_name: bool,
        decl_id: u64,
    ) -> (String, bool) {
        if let Some(storage) = self.lexical_role_continuation(qualified) {
            return (storage, true);
        }
        let written_compound = has_source_compound_name
            || (crate::qualified::is_qualified(Symbol::intern(name))
                && !crate::qualified::is_inside_package(
                    Symbol::intern(name),
                    Symbol::intern(current_package),
                ));
        if !is_my_scoped || decl_id == 0 || written_compound {
            return (qualified.to_string(), false);
        }
        let storage = format!("{qualified}\u{0}{decl_id}");
        self.record_lexical_class_pending(qualified.to_string(), storage.clone());
        (storage, true)
    }

    /// Bind the source-facing names of a lexical role (`R`, and `Owner::R`
    /// inside a package) to its storage name for the rest of the declaring
    /// scope. `register_lexical_class` enrolls each binding in the block-exit
    /// restore, like a `my` variable.
    // Cost: O(n), n = name length.
    pub(super) fn bind_lexical_role_names(&mut self, storage: &str, qualified: &str, name: &str) {
        let short = crate::qualified::unqualified_part(Symbol::intern(qualified))
            .as_str()
            .to_string();
        let value = Value::package(Symbol::intern(storage));
        for bound in [qualified, name, short.as_str()] {
            // Hand an enclosing same-named binding back when a branch/loop body
            // that declared this role exits (#10594).
            self.save_lexical_type_binding_for_scope_exit(bound);
            self.env_mut().insert(bound.to_string(), value.clone());
        }
        self.register_lexical_class(short.clone());
        if qualified != short {
            self.register_lexical_class(qualified.to_string());
        }
        if name != short && name != qualified {
            self.register_lexical_class(name.to_string());
        }
        self.mark_my_scoped_package_item(storage.to_string());
    }
}
