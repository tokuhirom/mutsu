//! The routines in a calling routine's lexical scope: what `CALLERS::<&name>`
//! resolves one name to, and what `CALLER::LEXICAL::` lists (#17733).
//!
//! A file-scope `sub` lives in the routine registry (or, when lexical, in its
//! unit's private table), not in the env a caller frame snapshots, so a
//! caller pseudo-stash built from that env alone leaves it out. The scope of a
//! calling routine is its package and enclosing packages (and those of the
//! role it was written in), `GLOBAL`, its unit's top-level imports, and its
//! unit's private routines.

use super::*;
use crate::value::ValueMap;

/// The lexical scope a calling routine runs in.
#[derive(Clone, Copy)]
struct CallerScope {
    package: Symbol,
    lexical_package: Option<Symbol>,
    unit: Symbol,
}

impl CallerScope {
    /// The packages whose routines the scope sees, innermost first.
    // Cost: O(p), p = enclosing packages.
    fn packages(self) -> impl Iterator<Item = Symbol> {
        // A role's method runs with the composing class as its package but
        // is lexically inside the role (and the file the role was written
        // in), so the role's own enclosing packages count too.
        crate::qualified::package_ancestors(self.package)
            .chain(
                self.lexical_package
                    .into_iter()
                    .flat_map(crate::qualified::package_ancestors),
            )
            .chain(std::iter::once(Symbol::intern("GLOBAL")))
    }
}

impl Interpreter {
    /// The routine `name` in the lexical scope of the calling routines, nearest
    /// caller first: a `sub` its unit or enclosing package declares, or one its
    /// unit's top-level `use`s imported. `outers_only` (`CALLERS::OUTERS::`)
    /// leaves out the program's mainline, whose declarations and imports are in
    /// its own pad and not in a scope outside it.
    // Cost: O(s + p), s = import sets of the caller's unit, p = enclosing
    // packages of the caller.
    pub(crate) fn caller_scope_routine(&self, name: &str, outers_only: bool) -> Option<Value> {
        if outers_only && self.routine_stack.len() <= 1 {
            return None;
        }
        self.routine_in_caller_scope(self.caller_scope(1), name)
    }

    /// The scope of the routine `depth` calls up from the running one. Only
    /// the calling routine's own scope counts: a routine two calls up is not
    /// the caller's lexical scope (a module routine that imported nothing does
    /// not see what the program that called it imported). With no calling
    /// routine the caller is the program's mainline.
    // Cost: O(1).
    fn caller_scope(&self, depth: usize) -> CallerScope {
        let callers = self.routine_stack.len().saturating_sub(depth);
        match self.routine_stack[..callers].last() {
            Some(frame) => CallerScope {
                package: frame.package,
                lexical_package: frame.lexical_package,
                unit: self.unit_of_source_sym(frame.def_file),
            },
            None => CallerScope {
                package: Symbol::intern("GLOBAL"),
                lexical_package: None,
                unit: crate::runtime::main_unit(),
            },
        }
    }

    /// The routine `name` as `scope` sees it.
    // Cost: O(s + p), s = import sets of the scope's unit, p = its packages.
    fn routine_in_caller_scope(&self, scope: CallerScope, name: &str) -> Option<Value> {
        let name_sym = Symbol::intern(name);
        scope
            .packages()
            .find_map(|pkg| {
                let def = self
                    .routines()
                    .function_def(crate::qualified::qualified(pkg, name_sym))?;
                (self.unit_of_declaring_file(def.source_file.as_deref()) == scope.unit)
                    .then(|| self.sub_value_from_function_def((*def).clone()))
            })
            .or_else(|| self.unit_import_routine(scope.unit, name))
            // A package-less `sub` of a parameterized role's body is moved
            // into its unit's private table (`seclude_role_lexical_routine`):
            // it is in the role file's lexical scope all the same.
            .or_else(|| {
                self.unit_private_routine_from(scope.unit, name_sym)
                    .map(|def| self.sub_value_from_function_def((*def).clone()))
            })
    }

    /// Add to `entries` (a `CALLER::LEXICAL::` stash `depth` calls up) every
    /// routine of the caller's scope it does not already list, under `&name`.
    /// The names come from the registry entries of the scope's packages, its
    /// unit's private routines and its unit's import sets; each is resolved as
    /// `CALLERS::<&name>` resolves it.
    // Cost: O(R + n · (s + p)), R = routine-registry entries, n = candidate
    // names, s = import sets of the caller's unit, p = its packages.
    pub(crate) fn add_caller_scope_routines(&self, entries: &mut ValueMap, depth: usize) {
        let scope = self.caller_scope(depth);
        let packages: Vec<Symbol> = scope.packages().collect();
        let mut names: Vec<String> = Vec::new();
        for key in self.routines().functions.keys() {
            let Some((package, tail)) = crate::qualified::split_qualified(*key) else {
                continue;
            };
            if !packages.contains(&package) {
                continue;
            }
            let tail = tail.as_str();
            let name = crate::runtime::dispatch_resolve::function_key_strip_arity_suffix(&tail);
            if !name.is_empty() {
                names.push(name.to_string());
            }
        }
        let mut unit = Some(scope.unit);
        // An EVAL nested in an EVAL is bounded in practice; the cap only stops
        // a cycle from hanging the VM (as `unit_private_routine_from`).
        for _ in 0..64 {
            let Some(sym) = unit else { break };
            if let Some(table) = self.module.world.unit_private_routines.read().get(&sym) {
                names.extend(table.keys().map(|name| name.resolve()));
            }
            unit = crate::runtime::eval_unit_parent(sym);
        }
        {
            let imports = self.module.lexical_imports.read();
            if let Some(set_ids) = imports.unit_sets.get(&scope.unit) {
                for id in set_ids {
                    if let Some(set) = imports.sets.get(id) {
                        names.extend(set.iter().map(|(name, _)| name.resolve()));
                    }
                }
            }
        }
        names.sort_unstable();
        names.dedup();
        for name in names {
            let key = format!("&{name}");
            if entries.contains_key(key.as_str()) {
                continue;
            }
            if let Some(value) = self.routine_in_caller_scope(scope, &name) {
                entries.insert(key, value);
            }
        }
    }
}
