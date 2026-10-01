//! Re-binding a package body's lexicals for the body's run-time part
//! (ADR-0134, #10332).
//!
//! The BEGIN prologue registers a class or package with the BEGIN-time part
//! of its body. Its `my` lexicals then live in the package's static store
//! (`package_lexicals`), where its methods and subs read them. The body's
//! run-time part runs later, at the declaration's source position, in an
//! `OpCode::PackageScope` that names those lexicals. They are bound from the
//! store for the duration of the body, so a same-named outer lexical is
//! shadowed as it is in the source, and written back to it afterwards.

use super::*;
use crate::compiler::CLASS_LEXICAL;

impl Interpreter {
    /// Bind each of `lexicals` (newline-joined `VarDecl` names) from
    /// `package`'s static store into the env. `$?CLASS` (named for a class
    /// body) is bound to the class itself.
    // Cost: O(k), k = lexicals named.
    pub(super) fn bind_package_body_lexicals(&mut self, package: &str, lexicals: &str) {
        let store = self.package_lexicals.get(package);
        let values: Vec<(&str, Value)> = lexicals
            .split('\n')
            .filter_map(|name| {
                if name == CLASS_LEXICAL {
                    return Some((name, Value::package(Symbol::intern(package))));
                }
                store?.get(name).map(|value| (name, value.clone()))
            })
            .collect();
        for (name, value) in values {
            self.env_mut().insert(name.to_string(), value);
        }
    }

    /// Write each of `lexicals` the store holds back to it, then put back
    /// whatever the enclosing scope bound under the name.
    // Cost: O(k), k = lexicals named.
    pub(super) fn unbind_package_body_lexicals(
        &mut self,
        package: &str,
        lexicals: &str,
        saved_env: &crate::env::Env,
    ) {
        for name in lexicals.split('\n') {
            // Only a name the store holds was bound from it; anything else
            // under the name belongs to the enclosing scope.
            if self
                .package_lexicals
                .get(package)
                .is_some_and(|store| store.contains_key(name))
                && let Some(value) = self.env().get(name).cloned()
            {
                self.package_lexicals_cow_mut()
                    .entry(package.to_string())
                    .or_default()
                    .insert(name.to_string(), value);
            }
            match saved_env.get(name) {
                Some(previous) => {
                    let previous = previous.clone();
                    self.env_mut().insert(name.to_string(), previous);
                }
                None => {
                    self.env_mut().remove(name);
                }
            }
        }
    }
}
