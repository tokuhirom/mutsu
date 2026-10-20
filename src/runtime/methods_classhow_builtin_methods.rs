use super::*;
use crate::value::ValueView;

impl Interpreter {
    pub(super) fn dispatch_classhow_methods(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let invocant = &args[0];
        let class_name = self.mop_receiver_owner(invocant);

        // Parse named arguments
        let mut local = false;
        let mut all = false;
        let mut private = false;
        let mut tree = false;
        for arg in &args[1..] {
            if let ValueView::Pair(key, value) = arg.view() {
                match key.as_str() {
                    "local" => local = value.truthy(),
                    "all" => all = value.truthy(),
                    "private" => private = value.truthy(),
                    "tree" => tree = value.truthy(),
                    _ => {}
                }
            }
        }

        if tree {
            return self.classhow_methods_tree(&class_name, private);
        }

        if let Some(role) = self.mop_receiver_role(invocant, &class_name) {
            return Ok(Value::array(self.role_own_method_objects(&role, private)));
        }

        let mut result = Vec::new();

        // RakuAST model classes are native type objects rather than entries in
        // the user-class registry.  Expose the constructors/accessors that the
        // model layer really implements so `.^methods` is useful (and does not
        // fall through to an empty built-in method list).
        //
        // `Type/Metamodel/MethodContainer.rakudoc`: the default is the methods
        // of the class *and its parents*, stopping at Cool/Any/Mu; `:local` is
        // only what the class itself declares; `:all` adds the Any/Mu tail on
        // top of both. The model MRO supplies the first two, and `:all` falls
        // through to the ordinary built-in list below after seeding the model
        // names — otherwise `RakuAST::IntLiteral.^methods` was empty even
        // though `.^methods(:local)` and `.^can("value")` both found `value`.
        if local && let Some(names) = crate::rakuast::local_method_names(&class_name) {
            self.push_native_method_objects(&names, &class_name, &mut result);
            return Ok(Value::array(result));
        }
        if let Some(names) = crate::rakuast::inherited_method_names(&class_name) {
            self.push_native_method_objects(&names, &class_name, &mut result);
            if !all {
                return Ok(Value::array(result));
            }
        }

        // Extract mixin role names from the invocant for runtime role method collection
        let mixin_role_names: Vec<String> = if let ValueView::Mixin(_, mixins) = invocant.view() {
            mixins
                .keys()
                .filter_map(|key| key.strip_prefix("__mutsu_role__").map(String::from))
                .collect()
        } else {
            Vec::new()
        };

        if local {
            // Only methods defined directly on this class
            self.collect_class_methods(&class_name, private, &mut result);
            // Rakudo's compose protocol installs a class-local POPULATE
            // candidate even when the class declares no constructor hook.
            // Keep it in the local method view only once this class has
            // completed its native compose step.
            let has_local_populate = self
                .registry()
                .user_method_overloads(&class_name, "POPULATE")
                .is_some();
            if !has_local_populate
                && self.registry().composed_classes.contains(&class_name)
            {
                self.push_native_method_objects(&["POPULATE"], &class_name, &mut result);
            }
            // Also include methods from runtime-mixed-in roles
            for role_name in &mixin_role_names {
                self.collect_role_methods(role_name, private, &mut result);
            }
            // Built-in types have no registry entry; their declared own list
            // (leaf methods up to the coercion tail) approximates :local.
            if result.is_empty() && !self.registry().classes.contains_key(&class_name) {
                let names =
                    crate::builtins::builtin_type_methods::builtin_type_method_names(&class_name);
                self.push_native_method_objects(&names, &class_name, &mut result);
            }
        } else {
            // Runtime-mixed-in role methods sit ahead of the base class in the
            // MRO (raku puts the anonymous composite pun class first: `(5 but
            // R).^mro` is `((Int+{R}) (Int) (Cool) (Any) (Mu))`), so collect
            // them before the base `class_name`'s own MRO walk below --
            // mirroring the `:local` branch above, which already did this for
            // its own (narrower) enumeration but this default (non-`:local`)
            // branch never did. Confirmed missing against real `raku`:
            // `(5 but R).zork` was callable but absent from `(5 but
            // R).^methods` (no `:local`) before this fix; see
            // `t/classhow-methods-mixin-role.t`.
            for role_name in &mixin_role_names {
                self.collect_role_methods(role_name, private, &mut result);
            }

            // Walk MRO (already includes the class itself)
            let mro = self.class_mro(&class_name);

            // ADR-0019 Phase E box E7 step 6 (`.^methods`, `todo/deep/
            // adr0019-e5-e7-entry-routing.md` "E7 step 6"): shadow-check the
            // chain this walk actually enumerates (`class_mro(class_name)`,
            // the registry MRO primitive) against the E4 resolver's own
            // canonical chain for the same receiver (`dispatch_owner_chain`,
            // TypeId-based). `MUTSU_VM_STATS`-gated, zero behavior change:
            // `mro` alone still drives the enumeration below.
            if crate::vm::vm_stats::enabled() {
                let real_names: Vec<&str> = mro.iter().map(|s| s.as_str()).collect();
                let shadow_chain = self.dispatch_owner_chain(invocant);
                let shadow_names: Vec<&str> = shadow_chain.iter().map(|t| t.as_str()).collect();
                let matched = real_names == shadow_names;
                crate::vm::vm_stats::record_methods_shadow_check(matched, || {
                    format!("class={class_name} real={real_names:?} shadow={shadow_names:?}")
                });
            }

            for cn in mro.iter().map(|s| s.as_str()) {
                if !all && (cn == "Any" || cn == "Mu") {
                    continue;
                }
                self.collect_class_methods(cn, private, &mut result);
            }

            // For built-in types that don't have class defs, add known methods.
            // `:all` also exposes the universal Any/Mu methods for user-defined
            // classes. The MRO walk above visits those names, but they are
            // native catalog entries rather than registry ClassDefs, so the
            // normal user-class result is non-empty and the old condition
            // skipped them entirely.
            if all || result.is_empty() || !self.registry().classes.contains_key(&class_name) {
                self.collect_builtin_type_methods(&class_name, &mut result);
                if all {
                    self.collect_builtin_type_methods("Any", &mut result);
                    self.collect_builtin_type_methods("Mu", &mut result);
                }
            }
            // A class's own method table is more than the introspectable
            // subset: `Map` and `Pair` list almost nothing without it (#13494).
            for cn in mro.iter().map(|s| s.as_str()) {
                if !all && matches!(cn, "Any" | "Mu" | "Cool") {
                    continue;
                }
                let names = crate::builtins::method_introspection::declared_names_for_owner(cn);
                self.push_native_method_objects(&names, cn, &mut result);
            }
            let mro_names: Vec<String> = mro.iter().map(|s| s.to_string()).collect();
            self.collect_composed_role_methods(&mro_names, &mut result);
        }

        Ok(Value::array(result))
    }

    pub(super) fn collect_builtin_type_methods(&self, type_name: &str, result: &mut Vec<Value>) {
        // The registry owns the canonical type x method entries. Catalog
        // construction is static and introspection does not construct or probe
        // a parallel entry set on every query.
        let methods = self.registry().builtin_method_names(type_name);
        self.push_native_method_objects(&methods, type_name, result);
    }

    /// Rakudo flattens the methods of the roles a built-in class composes into
    /// its method table, so `.^methods` lists them with the role as `.package`
    /// (`List.^methods` has `Positional.of`, `Map` has `Associative.of`/`keyof`,
    /// `Hash` lists both its own and `Associative`'s). Each composed role counts
    /// once however many levels of the MRO compose it (#12990).
    // Cost: O(m * r * n), m = MRO levels, r = roles per level, n = catalog rows
    // scanned per role.
    fn collect_composed_role_methods(&self, mro: &[String], result: &mut Vec<Value>) {
        let mro_strs: Vec<&str> = mro.iter().map(String::as_str).collect();
        let mut seen: Vec<&str> = Vec::new();
        for (level, class) in mro_strs.iter().enumerate() {
            for role in crate::builtin_types::catalog::builtin_type_roles_strs(class) {
                let base = role.split_once('[').map_or(*role, |(base, _)| base);
                if seen.contains(&base)
                    || !crate::builtin_types::catalog::composes_role_at(&mro_strs, level, role)
                {
                    continue;
                }
                seen.push(base);
                for name in crate::builtins::method_introspection::introspectable_names_for_owner(base)
                {
                    result.push(self.make_native_method_object(name, base));
                }
            }
        }
    }

    /// Append a native Method object for each name not already present in
    /// `result` (dedup by the Method object's `name` attribute). `owner` is the
    /// catalog type these names were collected for -- ADR-0019 Phase F box F1's
    /// mechanism slice: the resulting `Method` object's `.package` defaults to
    /// it. This is not always Rakudo's true declaring type (e.g. `Str.uc`'s
    /// real `.package` is `Cool`, not `Str`) -- that per-method fidelity data
    /// is a later, separate slice (see `todo/deep/adr0019-f1-f2-introspection-canonical-source.md`).
    fn push_native_method_objects(&self, names: &[&str], owner: &str, result: &mut Vec<Value>) {
        for name in names {
            if !result.iter().any(|v| {
                if let ValueView::Instance { attributes, .. } = v.view() {
                    attributes
                        .as_map()
                        .get("name")
                        .map(|n| n.to_string_value())
                        .as_deref()
                        == Some(*name)
                } else {
                    false
                }
            }) {
                result.push(self.make_native_method_object(name, owner));
            }
        }
    }
}
