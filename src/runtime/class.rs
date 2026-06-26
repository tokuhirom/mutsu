use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn run_pending_instance_destroys(&mut self) -> Result<(), RuntimeError> {
        let pending = take_pending_instance_destroys();
        if pending.is_empty() {
            return Ok(());
        }
        // Set reentrancy guard to prevent infinite DESTROY recursion:
        // instances created during DESTROY execution should not queue new DESTROYs.
        crate::value::set_in_destroy_handler(true);
        let result = self.run_pending_instance_destroys_inner(&pending);
        crate::value::set_in_destroy_handler(false);
        result
    }

    /// Whether a type was declared under Raku 6.e+ semantics, keyed on the
    /// type's *declaration* revision (captured as type metadata). DESTROY/BUILD
    /// run long after parsing, when the globally-current language version may
    /// have been reset to the default, so reading that global would be wrong.
    pub(crate) fn type_decl_is_6e(&self, name: &str) -> bool {
        match self
            .type_metadata
            .get(name)
            .and_then(|meta| meta.get("language-revision"))
        {
            Some(Value::Str(rev)) => rev.as_str() >= "e",
            _ => crate::parser::current_language_version().starts_with("6.e"),
        }
    }

    fn run_pending_instance_destroys_inner(
        &mut self,
        pending: &[crate::value::PendingInstanceDestroy],
    ) -> Result<(), RuntimeError> {
        for item in pending {
            let instance_class = item.class_name.resolve();
            // 6.e role-submethod DESTROY dispatch is keyed on the instance's
            // class declaration revision, not the globally-current version.
            let is_6e = self.type_decl_is_6e(&instance_class);
            // Collect the MRO so we call DESTROY on each class in order (child → parent).
            let mro: Vec<String> = self
                .registry()
                .classes
                .get(&instance_class)
                .map(|cd| cd.mro.clone())
                .unwrap_or_default();
            // Track attributes across DESTROY calls so mutations are visible
            let mut current_attrs = item.attributes.clone();
            // Walk the MRO; submethods are per-class, not inherited.
            for mro_class in &mro {
                // Skip role entries in MRO
                if self.registry().roles.contains_key(mro_class)
                    && !self.registry().classes.contains_key(mro_class)
                {
                    continue;
                }
                // Clone DESTROY overloads out and drop the guard before re-entering
                // user code (run_instance_method_resolved).
                let destroy_overloads = match self.registry().classes.get(mro_class) {
                    Some(class_def) => class_def.methods.get("DESTROY").cloned(),
                    None => continue,
                };
                // Call class's own DESTROY submethod
                if let Some(overloads) = destroy_overloads
                    && let Some(method_def) = overloads.into_iter().find(|def| {
                        def.is_my && !def.is_private && self.method_args_match(&[], &def.param_defs)
                    })
                {
                    let invocant = Value::make_instance_without_destroy(
                        item.class_name,
                        current_attrs.clone(),
                    );
                    if let Ok((_v, updated)) = self.run_resolved_method_compiled_or_treewalk(
                        &instance_class,
                        mro_class,
                        "DESTROY",
                        method_def,
                        current_attrs.clone(),
                        Vec::new(),
                        Some(invocant),
                    ) {
                        current_attrs = updated;
                    }
                }
                // Under v6.e+, call DESTROY submethods from composed roles
                // (in reverse order: role submethods after the class's own DESTROY)
                if is_6e {
                    let role_order = self.ordered_role_submethods_for_class(mro_class, "DESTROY");
                    // DESTROY order is reverse of BUILD: role submethods after class
                    for (role_name, method_def) in role_order.into_iter().rev() {
                        let invocant = Value::make_instance_without_destroy(
                            item.class_name,
                            current_attrs.clone(),
                        );
                        if let Ok((_v, updated)) = self.run_resolved_method_compiled_or_treewalk(
                            &instance_class,
                            &role_name,
                            "DESTROY",
                            method_def,
                            current_attrs.clone(),
                            Vec::new(),
                            Some(invocant),
                        ) {
                            current_attrs = updated;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub(super) fn detect_unresolved_role_method_conflicts(
        &self,
        class_name: &str,
        class_def: &ClassDef,
    ) -> Result<(), RuntimeError> {
        for (method_name, defs) in &class_def.methods {
            // Submethods (like BUILD, TWEAK) from multiple roles do not conflict —
            // they are accumulated and all called during construction. Skip them.
            if defs.iter().all(|d| d.is_submethod) {
                continue;
            }
            // Check non-multi methods
            let non_multi: Vec<&MethodDef> = defs
                .iter()
                .filter(|d| !d.is_multi && !Self::is_stub_routine_body(&d.body))
                .collect();
            let class_defined_non_multi = non_multi.iter().any(|d| d.role_origin.is_none());
            if !class_defined_non_multi {
                let mut conflicting_roles = Vec::new();
                let mut seen_origins = Vec::new();
                for def in &non_multi {
                    let Some(role_name) = &def.role_origin else {
                        continue;
                    };
                    // Use original_role for diamond detection: if two methods
                    // trace back to the same original role, they are not in conflict.
                    let origin = def.original_role.as_ref().unwrap_or(role_name);
                    if seen_origins.contains(origin) {
                        continue;
                    }
                    seen_origins.push(origin.clone());
                    if !conflicting_roles.contains(role_name) {
                        conflicting_roles.push(role_name.clone());
                    }
                }
                if conflicting_roles.len() > 1 {
                    conflicting_roles.reverse();
                    return Err(RuntimeError::new(format!(
                        "X::Role::Composition::Conflict: Method '{}' must be resolved by class {} because it exists in multiple roles ({})",
                        method_name,
                        class_name,
                        conflicting_roles.join(", "),
                    )));
                }
            }

            // Check multi methods: detect per-signature conflicts from different roles
            let multi_defs: Vec<&MethodDef> = defs
                .iter()
                .filter(|d| d.is_multi && !Self::is_stub_routine_body(&d.body))
                .collect();
            if multi_defs.len() > 1 {
                // Group by signature, check if any signature has methods from multiple roles
                // without a class-provided resolution
                let mut checked: Vec<Vec<String>> = Vec::new();
                for (i, def_a) in multi_defs.iter().enumerate() {
                    let sig = Self::method_positional_signature(def_a);
                    if checked.contains(&sig) {
                        continue;
                    }
                    checked.push(sig);
                    // Find all methods with matching signature. Identify each
                    // candidate by its ORIGINAL defining role (diamond detection,
                    // mirroring the non-multi path above): a multi method composed
                    // transitively via several roles (`role R { multi method m }`,
                    // `role S does R`, `class C does S`) traces back to the same
                    // original role through every path and so is NOT a conflict.
                    // Using `role_origin` (the immediate composition source) instead
                    // would wrongly see it as `(S, R)` and demand resolution.
                    let mut roles_for_sig: Vec<String> = Vec::new();
                    let mut class_resolves = def_a.role_origin.is_none();
                    if let Some(r) = def_a.original_role.as_ref().or(def_a.role_origin.as_ref())
                        && !roles_for_sig.contains(r)
                    {
                        roles_for_sig.push(r.clone());
                    }
                    for def_b in multi_defs.iter().skip(i + 1) {
                        if Self::method_signatures_match(def_a, def_b) {
                            if def_b.role_origin.is_none() {
                                class_resolves = true;
                            }
                            if let Some(r) =
                                def_b.original_role.as_ref().or(def_b.role_origin.as_ref())
                                && !roles_for_sig.contains(r)
                            {
                                roles_for_sig.push(r.clone());
                            }
                        }
                    }
                    if roles_for_sig.len() > 1 && !class_resolves {
                        return Err(RuntimeError::new(format!(
                            "X::Role::Composition::Conflict: Method '{}' must be resolved by class {} because it exists in multiple roles ({})",
                            method_name,
                            class_name,
                            roles_for_sig.join(", "),
                        )));
                    }
                }
            }
        }

        Ok(())
    }

    /// Compute the C3 MRO for `class_name`. Delegates to the pure-registry
    /// [`Registry::compute_class_mro`] under a single read guard.
    pub(super) fn compute_class_mro(
        &mut self,
        class_name: &str,
        stack: &mut Vec<String>,
    ) -> Result<Vec<String>, RuntimeError> {
        self.registry().compute_class_mro(class_name, stack)
    }

    pub(super) fn class_has_method(&mut self, class_name: &str, method_name: &str) -> bool {
        self.registry_mut()
            .class_has_method(class_name, method_name)
    }

    /// Check whether the class (or its MRO ancestors) has a `new` method
    /// variant with a non-named positional parameter whose type matches the
    /// given value.  This is used by the coercion fallback: only try `new`
    /// when there is an explicit `new(TargetType:U: ValueType $x)` multi.
    pub(super) fn class_has_new_accepting_positional(
        &mut self,
        class_name: &str,
        value: &Value,
    ) -> bool {
        let mro = self.class_mro(class_name);
        for cn in &mro {
            let methods = match self.registry().classes.get(cn.as_str()) {
                Some(cd) => cd.methods.get("new").cloned(),
                None => None,
            };
            if let Some(overloads) = methods {
                for method in &overloads {
                    // Look for a positional (non-named, non-invocant) param
                    // that type-matches the value.
                    let has_matching_positional = method.param_defs.iter().any(|pd| {
                        !pd.named
                            && !pd.is_invocant
                            && pd
                                .type_constraint
                                .as_deref()
                                .is_some_and(|tc| self.type_matches_value(tc, value))
                    });
                    if has_matching_positional {
                        return true;
                    }
                }
            }
        }
        false
    }

    pub(crate) fn is_native_method(&mut self, class_name: &str, method_name: &str) -> bool {
        // IO::Pipe has native methods handled by native_io_pipe
        if class_name == "IO::Pipe"
            && matches!(
                method_name,
                "slurp"
                    | "Str"
                    | "gist"
                    | "encoding"
                    | "close"
                    | "split"
                    | "print"
                    | "say"
                    | "put"
                    | "flush"
                    | "write"
                    | "get"
                    | "lines"
                    | "eof"
                    | "proc"
                    | "IO"
                    | "path"
            )
        {
            return true;
        }
        // IO::Special has native methods handled by native_io_special
        if class_name == "IO::Special"
            && matches!(
                method_name,
                "Str"
                    | "gist"
                    | "what"
                    | "IO"
                    | "e"
                    | "d"
                    | "f"
                    | "l"
                    | "x"
                    | "s"
                    | "r"
                    | "w"
                    | "modified"
                    | "accessed"
                    | "changed"
                    | "mode"
                    | "raku"
                    | "perl"
                    | "WHICH"
                    | "new"
                    | "Bool"
                    | "defined"
            )
        {
            return true;
        }
        // IO::Handle has native methods handled by native_io_handle
        if class_name == "IO::Handle"
            && matches!(
                method_name,
                "DESTROY"
                    | "path"
                    | "IO"
                    | "Str"
                    | "gist"
                    | "open"
                    | "nl-out"
                    | "nl-in"
                    | "chomp"
                    | "print-nl"
                    | "close"
                    | "get"
                    | "getc"
                    | "readchars"
                    | "lines"
                    | "words"
                    | "read"
                    | "write"
                    | "print"
                    | "say"
                    | "put"
                    | "flush"
                    | "out-buffer"
                    | "seek"
                    | "tell"
                    | "eof"
                    | "encoding"
                    | "opened"
                    | "slurp"
                    | "Supply"
                    | "native-descriptor"
                    | "spurt"
                    | "t"
                    | "printf"
                    | "split"
                    | "comb"
            )
        {
            return true;
        }
        // IO::Path's comb reads file content and combs the result.
        if class_name == "IO::Path" && method_name == "comb" {
            return true;
        }
        // Thread native methods
        if class_name == "Thread"
            && matches!(
                method_name,
                "finish"
                    | "id"
                    | "name"
                    | "is-initial-thread"
                    | "app_lifetime"
                    | "Str"
                    | "gist"
                    | "WHAT"
            )
        {
            return true;
        }
        // VM native methods
        if class_name == "VM"
            && matches!(
                method_name,
                "name"
                    | "auth"
                    | "version"
                    | "precomp-ext"
                    | "precomp-target"
                    | "prefix"
                    | "desc"
                    | "signature"
                    | "config"
                    | "properties"
                    | "raku"
                    | "platform-library-name"
                    | "request-garbage-collection"
                    | "gist"
                    | "Str"
            )
        {
            return true;
        }
        let mro = self.class_mro(class_name);
        for cn in mro {
            if let Some(class_def) = self.registry().classes.get(&cn)
                && class_def.native_methods.contains(method_name)
            {
                return true;
            }
        }
        false
    }

    pub(crate) fn has_user_method(&mut self, class_name: &str, method_name: &str) -> bool {
        let mro = self.class_mro(class_name);
        for cn in mro {
            if let Some(class_def) = self.registry().classes.get(&cn)
                && let Some(defs) = class_def.methods.get(method_name)
            {
                return defs.iter().any(|d| !d.is_private);
            }
        }
        false
    }

    /// Check if a class has a public attribute accessor for the given name.
    pub(crate) fn has_public_accessor(&mut self, class_name: &str, method_name: &str) -> bool {
        let attrs = self.collect_class_attributes(class_name);
        attrs
            .iter()
            .any(|(attr_name, is_public, ..)| *is_public && attr_name == method_name)
    }

    /// Check if an attribute is buildable (can be set via .new).
    pub(crate) fn is_attribute_buildable(&self, class_name: &str, attr_name: &str) -> bool {
        if let Some(class_def) = self.registry().classes.get(class_name) {
            if let Some(&built) = class_def.attribute_built.get(attr_name) {
                return built;
            }
            for (name, is_public, ..) in &class_def.attributes {
                if name == attr_name {
                    return *is_public;
                }
            }
        }
        let mro = if let Some(cd) = self.registry().classes.get(class_name) {
            cd.mro.clone()
        } else {
            Vec::new()
        };
        for parent in &mro {
            if parent == class_name {
                continue;
            }
            if let Some(parent_def) = self.registry().classes.get(parent) {
                if let Some(&built) = parent_def.attribute_built.get(attr_name) {
                    return built;
                }
                for (name, is_public, ..) in &parent_def.attributes {
                    if name == attr_name {
                        return *is_public;
                    }
                }
            }
        }
        true
    }

    /// Look up a class-level attribute (declared with `our $.x` or `my $.x`).
    /// Searches the class and its MRO.
    pub(crate) fn get_class_level_attr(
        &mut self,
        class_name: &str,
        attr_name: &str,
    ) -> Option<Value> {
        // Check own class first
        if let Some(class_def) = self.registry().classes.get(class_name)
            && let Some(val) = class_def.class_level_attrs.get(attr_name)
        {
            return Some(val.clone());
        }
        // Walk MRO for inherited class-level attributes
        let mro = self.class_mro(class_name);
        for parent in &mro {
            if parent == class_name {
                continue;
            }
            if let Some(parent_def) = self.registry().classes.get(parent)
                && let Some(val) = parent_def.class_level_attrs.get(attr_name)
            {
                return Some(val.clone());
            }
        }
        None
    }

    /// Check if a class (or its MRO) has a class-level attribute.
    pub(crate) fn has_class_level_attr(&mut self, class_name: &str, attr_name: &str) -> bool {
        self.get_class_level_attr(class_name, attr_name).is_some()
    }

    /// Set a class-level attribute value. Searches the class and its MRO to find
    /// where the attribute is defined, then updates it.
    pub(crate) fn set_class_level_attr(
        &mut self,
        class_name: &str,
        attr_name: &str,
        value: Value,
    ) -> bool {
        // Check own class first
        if let Some(class_def) = self.registry_mut().classes.get_mut(class_name)
            && class_def.class_level_attrs.contains_key(attr_name)
        {
            class_def
                .class_level_attrs
                .insert(attr_name.to_string(), value);
            return true;
        }
        // Walk MRO
        let mro = self.class_mro(class_name);
        for parent in &mro {
            if parent == class_name {
                continue;
            }
            if let Some(parent_def) = self.registry_mut().classes.get_mut(parent)
                && parent_def.class_level_attrs.contains_key(attr_name)
            {
                parent_def
                    .class_level_attrs
                    .insert(attr_name.to_string(), value);
                return true;
            }
        }
        false
    }

    /// Collect wildcard-handles attribute var names from the class and its MRO.
    pub(super) fn collect_wildcard_handles(&mut self, class_name: &str) -> Vec<String> {
        let mro = self.class_mro(class_name);
        let mut result = Vec::new();
        for cn in &mro {
            if let Some(class_def) = self.registry().classes.get(cn) {
                result.extend(class_def.wildcard_handles.iter().cloned());
            }
        }
        result
    }

    /// Add `__mutsu_attr_alias::x` metadata for attributes declared with `has $x`
    /// (no twigil), so the method call dispatch can set up bidirectional aliases.
    pub(super) fn add_alias_attribute_metadata(
        &mut self,
        class_name: &str,
        attrs: &mut HashMap<String, Value>,
    ) {
        let mro = self.class_mro(class_name);
        for cn in &mro {
            if let Some(class_def) = self.registry().classes.get(cn) {
                for attr_name in &class_def.alias_attributes {
                    attrs.insert(
                        format!("__mutsu_attr_alias::{}", attr_name),
                        Value::str(attr_name.to_string()),
                    );
                }
            }
        }
    }

    pub(super) fn collect_class_attributes(&mut self, class_name: &str) -> Vec<ClassAttributeDef> {
        let mro = self.class_mro(class_name);
        let mut attrs: Vec<ClassAttributeDef> = Vec::new();
        for cn in mro.iter().rev() {
            if let Some(class_def) = self.registry().classes.get(cn) {
                for attr in &class_def.attributes {
                    if let Some(pos) = attrs.iter().position(|(n, ..)| n == &attr.0) {
                        attrs.remove(pos);
                    }
                    attrs.push(attr.clone());
                }
            }
        }
        attrs
    }

    /// Collect per-class attributes for all classes in the MRO.
    /// Returns `(declaring_class, ClassAttributeDef)` pairs.
    /// Unlike `collect_class_attributes`, this does NOT deduplicate by name —
    /// if Parent and Child both declare an attribute with the same name,
    /// both entries are returned. Used to initialize class-qualified attribute
    /// storage so that each class has its own private copy.
    pub(super) fn collect_per_class_attrs(
        &mut self,
        class_name: &str,
    ) -> Vec<(String, ClassAttributeDef)> {
        let mro = self.class_mro(class_name);
        let mut result: Vec<(String, ClassAttributeDef)> = Vec::new();
        // Track which attribute names appear in multiple classes (need qualified storage)
        let mut attr_counts: HashMap<String, usize> = HashMap::new();
        for cn in &mro {
            if let Some(class_def) = self.registry().classes.get(cn) {
                for attr in &class_def.attributes {
                    *attr_counts.entry(attr.0.clone()).or_insert(0) += 1;
                }
            }
        }
        // Only include attrs that appear in multiple classes (duplicated across hierarchy)
        for cn in &mro {
            if let Some(class_def) = self.registry().classes.get(cn) {
                for attr in &class_def.attributes {
                    if attr_counts.get(&attr.0).copied().unwrap_or(0) > 1 {
                        result.push((cn.clone(), attr.clone()));
                    }
                }
            }
        }
        result
    }

    /// Collect attributes from a role and all its composed parent roles.
    /// Used when the role has been punned (instantiated via mixin) and we need
    /// to check attribute metadata (e.g. `is rw`).
    pub(super) fn collect_role_attributes_for_class(
        &self,
        role_name: &str,
    ) -> Vec<ClassAttributeDef> {
        let mut attrs: Vec<ClassAttributeDef> = Vec::new();
        if let Some(role) = self.registry().roles.get(role_name) {
            attrs.extend(role.attributes.clone());
        }
        if let Some(parent_names) = self.registry().role_parents.get(role_name) {
            let mut role_stack: Vec<String> = parent_names.clone();
            let mut visited = vec![role_name.to_string()];
            while let Some(parent_role_name) = role_stack.pop() {
                if !visited.contains(&parent_role_name) {
                    visited.push(parent_role_name.clone());
                    if let Some(parent_role) = self.registry().roles.get(&parent_role_name) {
                        for attr in &parent_role.attributes {
                            if !attrs.iter().any(|a| a.0 == attr.0) {
                                attrs.push(attr.clone());
                            }
                        }
                    }
                    if let Some(grandparents) = self.registry().role_parents.get(&parent_role_name)
                    {
                        for gp in grandparents {
                            if !visited.contains(gp) {
                                role_stack.push(gp.clone());
                            }
                        }
                    }
                }
            }
        }
        attrs
    }

    /// Format the candidate signatures of a (multi) method across the
    /// receiver class's MRO, e.g. `(WorkingTie: Int $z, *%_)`. Used to build a
    /// Raku-style `X::Multi::NoMatch` message naming the invocant type and the
    /// available candidates.
    pub(crate) fn format_method_candidate_signatures(
        &self,
        receiver_class_name: &str,
        method_name: &str,
    ) -> Vec<String> {
        let mut sigs = Vec::new();
        for cn in self.mro_readonly(receiver_class_name) {
            let is_ancestor = cn.as_str() != receiver_class_name;
            // No user-code re-entry in this loop body (pure signature-string
            // building), so a let-bound guard is safe.
            let registry = self.registry();
            let Some(class_def) = registry.classes.get(cn.as_str()) else {
                continue;
            };
            let Some(overloads) = class_def.methods.get(method_name) else {
                continue;
            };
            for def in overloads {
                if def.is_private || (def.is_my && is_ancestor) {
                    continue;
                }
                let mut parts = Vec::new();
                for pd in &def.param_defs {
                    if pd.is_invocant {
                        continue;
                    }
                    // Skip the implicit `*%_` slurpy named param; we append it
                    // explicitly at the end of every candidate signature.
                    if pd.named
                        && (pd.slurpy || pd.double_slurpy)
                        && (pd.name == "%_" || pd.name == "_" || pd.name.is_empty())
                    {
                        continue;
                    }
                    let ty = pd.type_constraint.as_deref().unwrap_or("Any");
                    let sigil_prefix = if pd.slurpy || pd.double_slurpy {
                        "*"
                    } else {
                        ""
                    };
                    let var = if pd.name.is_empty() {
                        String::new()
                    } else if pd.name.starts_with('$')
                        || pd.name.starts_with('@')
                        || pd.name.starts_with('%')
                    {
                        format!(" {}{}", sigil_prefix, pd.name)
                    } else {
                        format!(" {}${}", sigil_prefix, pd.name)
                    };
                    parts.push(format!("{}{}", ty, var));
                }
                let mut sig = format!("({}: ", receiver_class_name);
                sig.push_str(&parts.join(", "));
                if !parts.is_empty() {
                    sig.push_str(", ");
                }
                sig.push_str("*%_)");
                sigs.push(sig);
            }
        }
        sigs
    }

    pub(crate) fn run_instance_method(
        &mut self,
        receiver_class_name: &str,
        attributes: HashMap<String, Value>,
        method_name: &str,
        args: Vec<Value>,
        invocant: Option<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        crate::vm::vm_stats::record_resolver_method_dispatch(method_name);
        let inv_value = if let Some(inv) = &invocant {
            inv.clone()
        } else if attributes.is_empty() {
            Value::Package(crate::symbol::Symbol::intern(receiver_class_name))
        } else {
            Value::make_instance(
                crate::symbol::Symbol::intern(receiver_class_name),
                attributes.clone(),
            )
        };
        let Some((owner_class, method_def)) = self.resolve_method_with_owner_invocant(
            receiver_class_name,
            method_name,
            &args,
            &inv_value,
        ) else {
            // Distinguish X::Multi::NoMatch (method exists but no candidate
            // matched) from X::Method::NotFound (method does not exist at all,
            // e.g. submethod on ancestor only).
            let has_visible_method = self.class_mro(receiver_class_name).iter().any(|cn| {
                self.registry()
                    .classes
                    .get(cn.as_str())
                    .and_then(|c| c.methods.get(method_name))
                    .is_some_and(|ovs| {
                        let is_ancestor = cn.as_str() != receiver_class_name;
                        ovs.iter()
                            .any(|d| !d.is_private && (!d.is_my || !is_ancestor))
                    })
            });
            if has_visible_method {
                let sigs =
                    self.format_method_candidate_signatures(receiver_class_name, method_name);
                return Err(
                    super::methods_signature::make_multi_no_match_error_detailed(
                        method_name,
                        receiver_class_name,
                        &sigs,
                    ),
                );
            }
            let type_name = receiver_class_name.to_string();
            return Err(super::methods_signature::make_method_not_found_error(
                method_name,
                &type_name,
                false,
            ));
        };
        // Ambiguous multi dispatch: two or more candidates were equally
        // specific. Raise X::Multi::Ambiguous rather than silently choosing.
        if self.dispatch_ambiguous {
            self.dispatch_ambiguous = false;
            let sigs = self.format_method_candidate_signatures(receiver_class_name, method_name);
            return Err(super::methods_signature::make_multi_ambiguous_error(
                method_name,
                receiver_class_name,
                &sigs,
            ));
        }
        // Helper to build remaining candidates, skipping the chosen one
        let build_remaining = |this: &mut Self,
                               method_def: &MethodDef|
         -> Vec<(String, MethodDef)> {
            let all = this.resolve_all_methods_with_owner(receiver_class_name, method_name, &args);
            let chosen_fp = crate::ast::function_body_fingerprint(
                &method_def.params,
                &method_def.param_defs,
                &method_def.body,
            );
            let mut remaining = Vec::new();
            let mut skipped = false;
            for (owner, def) in all {
                let fp =
                    crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body);
                if !skipped && fp == chosen_fp {
                    skipped = true;
                    continue;
                }
                if this.should_skip_defer_method_candidate(receiver_class_name, &owner) {
                    continue;
                }
                remaining.push((owner, def));
            }
            remaining
        };
        let make_invocant_for_dispatch =
            |invocant: &Option<Value>, attributes: &HashMap<String, Value>| -> Value {
                if let Some(inv) = invocant {
                    inv.clone()
                } else if attributes.is_empty() {
                    Value::Package(Symbol::intern(receiver_class_name))
                } else {
                    Value::make_instance(Symbol::intern(receiver_class_name), attributes.clone())
                }
            };
        // Check for method-level wrap chain on this candidate
        if !self.is_inside_wrap_dispatch()
            && let Some(cand_idx) =
                self.find_method_candidate_index(&owner_class, method_name, &method_def)
            && let Some(chain) = self
                .get_method_wrap_chain(&owner_class, method_name, cand_idx)
                .cloned()
        {
            let invocant_for_dispatch = make_invocant_for_dispatch(&invocant, &attributes);
            let remaining = build_remaining(self, &method_def);
            let pushed_dispatch = !remaining.is_empty();
            self.samewith_context_stack
                .push((method_name.to_string(), Some(invocant_for_dispatch.clone())));
            if pushed_dispatch {
                let rw_params = super::builtins_dispatch_next::rw_scalar_positional_params(
                    &method_def.param_defs,
                );
                self.method_dispatch_stack.push(MethodDispatchFrame {
                    receiver_class: receiver_class_name.to_string(),
                    invocant: invocant_for_dispatch,
                    args: args.clone(),
                    remaining,
                    rw_params,
                });
            }
            let mut orig_env = crate::env::Env::new();
            orig_env.insert(
                "__mutsu_method_wrap_original".to_string(),
                Value::Bool(true),
            );
            let original_sub = Value::make_sub(
                Symbol::intern(&owner_class),
                Symbol::intern(method_name),
                method_def.params.clone(),
                method_def.param_defs.clone(),
                (*method_def.body).clone(),
                method_def.is_rw,
                orig_env,
            );
            let outermost = chain.last().unwrap().1.clone();
            let mut wrap_remaining: Vec<Value> = Vec::new();
            for i in (0..chain.len() - 1).rev() {
                wrap_remaining.push(chain[i].1.clone());
            }
            wrap_remaining.push(original_sub);
            let mut call_args = vec![inv_value.clone()];
            call_args.extend(args);
            let frame = WrapDispatchFrame {
                sub_id: 0,
                remaining: wrap_remaining,
                args: call_args.clone(),
            };
            let wrapper_id = if let Value::Sub(ref wd) = outermost {
                Some(wd.id)
            } else {
                None
            };
            self.wrap_dispatch_stack.push(frame);
            let result = self.call_sub_value(outermost, call_args, false);
            self.wrap_dispatch_stack.pop();
            // Propagate closure variable mutations from the wrapper back to
            // the current env so captured variables are visible to the caller.
            if let Some(wid) = wrapper_id
                && let Some(persisted) = self.closure_env_overrides.get(&wid).cloned()
            {
                for (k, v) in persisted.iter() {
                    if self.env.contains_key_sym(*k) {
                        self.env.insert_sym(*k, v.clone());
                    }
                }
            }
            self.samewith_context_stack.pop();
            if pushed_dispatch {
                self.method_dispatch_stack.pop();
            }
            return result.map(|v| (v, attributes));
        }
        let invocant_for_dispatch = make_invocant_for_dispatch(&invocant, &attributes);
        let remaining = build_remaining(self, &method_def);
        let pushed_dispatch = !remaining.is_empty();
        self.samewith_context_stack
            .push((method_name.to_string(), Some(invocant_for_dispatch.clone())));
        if pushed_dispatch {
            let rw_params =
                super::builtins_dispatch_next::rw_scalar_positional_params(&method_def.param_defs);
            self.method_dispatch_stack.push(MethodDispatchFrame {
                receiver_class: receiver_class_name.to_string(),
                invocant: invocant_for_dispatch,
                args: args.clone(),
                remaining,
                rw_params,
            });
        }
        // Check for `is DEPRECATED` trait on the method
        if let Some(ref msg) = method_def.deprecated_message {
            let cl = self.test_pending_callsite_line;
            self.check_deprecation_for_method_with_line(method_name, &owner_class, msg, cl);
        }
        // §B: run the resolved (non-wrapped) candidate as compiled bytecode via the
        // VM-native `call_compiled_method` instead of the tree-walk
        // `run_instance_method_resolved` recompile-each-call path. The MRO frame is
        // already pushed above, so the candidate's own `nextsame`/`callsame`
        // continues this chain. This is the hot path for multi-method dispatch and
        // `samewith` re-dispatch reached through `call_method_with_values`
        // (roast S12-methods/defer-next.t: method `m` x10000). Delegation forwarders
        // (synthesized, `compiled_code = None`) and any other uncompiled method keep
        // the interpreter path.
        let result = self.run_resolved_method_compiled_or_treewalk(
            receiver_class_name,
            &owner_class,
            method_name,
            method_def,
            attributes,
            args,
            invocant,
        );
        self.samewith_context_stack.pop();
        if pushed_dispatch {
            self.method_dispatch_stack.pop();
        }
        result
    }

    /// §B: run an already-resolved method/submethod candidate as compiled bytecode
    /// (`call_compiled_method`). A candidate with no `compiled_code` is compiled
    /// on-demand in place first (`compile_method_def_in_place`), so the only thing that
    /// still reaches `run_instance_method_resolved` is a `handles`-delegation forwarder
    /// (#3658 — its former tree-walk method-execution arm has been deleted). Same
    /// `(result, attrs-to-commit)` contract at every resolved-candidate call site (the
    /// `run_instance_method` general dispatch and the construction/destruction
    /// BUILD/TWEAK/DESTROY runners).
    ///
    /// `pending_rw_writeback_sources` is merged (not restored) around the call so a
    /// sibling `submethod BUILD { $o++ }` write queued for an outer `.new` survives a
    /// nested `.new` (#3620) and the body's own captured-outer writes also propagate. A
    /// `submethod` that `fail`ed comes back as an unhandled Failure *value*; re-raise it
    /// as `Err(is_fail)` so the construction sites' fail-to-Failure handling is unchanged
    /// (a non-submethod keeps a Failure as a legitimate return). Attributes are committed
    /// from the live cell (unwrapping a `Value::Mixin` self to its inner instance) unless
    /// `:=`-adjusted.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn run_resolved_method_compiled_or_treewalk(
        &mut self,
        receiver_class_name: &str,
        owner_class: &str,
        method_name: &str,
        method_def: MethodDef,
        attributes: HashMap<String, Value>,
        args: Vec<Value>,
        invocant: Option<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        // Writeback-safety gate (§B, #3658 step 4 — free_var_writes filter REMOVED).
        // Any resolved candidate that has compiled bytecode and is not a delegation
        // forwarder now runs compiled, regardless of what free vars it writes:
        //   - captured-outer *lexical* writes (`method m { $outer++ }`) propagate via
        //     `call_compiled_method` queuing `pending_rw_writeback_sources` + the
        //     caller drain MERGING it (#3664);
        //   - dynamic (`$*x`) / captured-outer writes from a nested CLOSURE inside
        //     the body (the grammar reduce-time action `method delim { my $f = { $*L =
        //     '<' }; $f() }`) propagate now that the method fast-path `can_skip_merge`
        //     also gates on `has_calls` (#3670) — so the closure's env write is merged
        //     back instead of dropped;
        //   - attribute twigils (`.count`/`!x`/…) are not in `free_var_writes` at all
        //     (#3666).
        // A delegation forwarder is synthesized with `compiled_code = None`, so it
        // falls through to `forward_resolved_delegation` (the remaining reason it
        // still exists).
        // On-demand compile (§B, #3658): a resolved candidate reached before its
        // owner's registration compile pass — or one added at runtime (a role method
        // punned via `does`, a custom-HOW method) — can still have no `compiled_code`.
        // Compile this exact candidate's body IN PLACE (not via re-resolution, which
        // would mis-pick an override for a qualified `$obj.Class::meth` call) so it
        // runs compiled instead of tree-walked. Only a genuinely body-less method
        // (stub / delegation forwarder) then stays on the tree-walk path. The
        // compiled-execution Mixin/instance attribute writeback is handled by
        // `self_instance_attrs` (unwraps a `Value::Mixin` self to the inner cell) in
        // the attr ops and the `final_attrs` commit below.
        let mut method_def = method_def;
        if method_def.compiled_code.is_none() && method_def.delegation.is_none() {
            Self::compile_method_def_in_place(&mut method_def, owner_class);
        }
        let writeback_safe_compiled =
            method_def.compiled_code.is_some() && method_def.delegation.is_none();
        if !writeback_safe_compiled {
            return self.forward_resolved_delegation(
                receiver_class_name,
                owner_class,
                method_def,
                attributes,
                args,
                invocant,
            );
        }
        let cc = method_def.compiled_code.clone().unwrap();
        let inv_for_cell = invocant.clone();
        let empty_fns: HashMap<String, crate::opcode::CompiledFunction> = HashMap::new();
        let saved_pending = std::mem::take(&mut self.pending_rw_writeback_sources);
        let call_result = self.call_compiled_method(
            receiver_class_name,
            owner_class,
            method_name,
            &method_def,
            &cc,
            attributes,
            args,
            invocant,
            &empty_fns,
        );
        // MERGE the saved sibling writes (e.g. a sibling BUILD's captured-outer
        // write queued for the outer `.new` caller to drain, #3620) with the
        // body's OWN captured-outer writes that `call_compiled_method` recorded
        // on exit — rather than RESTORING `saved_pending`, which would discard
        // the body's writes (so a method's `$outer++` would be lost). With the
        // `free_var_writes.is_empty()` writeback-safety gate above on, the body
        // records nothing, so this is inert; it becomes correct once the gate is
        // relaxed for captured-outer-writing bodies.
        let mut merged = saved_pending;
        for src in std::mem::take(&mut self.pending_rw_writeback_sources) {
            if !merged.contains(&src) {
                merged.push(src);
            }
        }
        self.pending_rw_writeback_sources = merged;
        match call_result {
            Ok((v, updated, adjusted)) => {
                if method_def.is_submethod
                    && let Some(mut err) = self.failure_to_runtime_error_if_unhandled(&v)
                {
                    err.is_fail = true;
                    return Err(err);
                }
                // Read the committed attribute map from the live cell of `self`,
                // unwrapping a `Value::Mixin` invocant to its inner instance (so a
                // runtime-`does` mixin method's attribute mutations are captured, not
                // the stale pre-call `updated` map). `adjusted` keeps the `:=`-recovered
                // snapshot.
                let final_attrs = if adjusted {
                    updated
                } else if let Some(cell) = inv_for_cell.as_ref().and_then(Self::self_instance_attrs)
                {
                    cell.to_map()
                } else {
                    updated
                };
                Ok((v, final_attrs))
            }
            Err(e) => Err(e),
        }
    }

    /// Forward a `handles`-delegation method to its delegate. (§B #3658: the former
    /// tree-walk method-execution arm — the `run_block` of the user method body —
    /// has been deleted. Every non-delegation candidate is now compiled on-demand by
    /// its caller (`compile_method_def_in_place`) before reaching here, so this only
    /// handles delegation forwarders.)
    pub(super) fn forward_resolved_delegation(
        &mut self,
        receiver_class_name: &str,
        owner_class: &str,
        method_def: MethodDef,
        attributes: HashMap<String, Value>,
        args: Vec<Value>,
        invocant: Option<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        if let Some((ref attr_var_name, ref target_method)) = method_def.delegation {
            // Clear skip_pseudo_method_native: the outer call set it for the
            // delegator's own method name, but we're about to forward to a
            // different name on a (possibly different) target, so the flag
            // must not leak into the delegate dispatch.
            let saved_skip_pseudo = self.skip_pseudo_method_native.take();
            // Method-based delegation: attr_var_name starts with `&`, meaning
            // the delegate is obtained by invoking the named method on self.
            let delegate = if let Some(source_method) = attr_var_name.strip_prefix('&') {
                let invocant_val = if let Some(ref inv) = invocant {
                    inv.clone()
                } else if attributes.is_empty() {
                    Value::Package(Symbol::intern(receiver_class_name))
                } else {
                    Value::make_instance(Symbol::intern(receiver_class_name), attributes.clone())
                };
                self.call_method_with_values(invocant_val, source_method, Vec::new())?
            } else {
                let attr_key = attr_var_name
                    .trim_start_matches('.')
                    .trim_start_matches('!');
                attributes.get(attr_key).cloned().unwrap_or(Value::Nil)
            };
            let is_method_based = attr_var_name.starts_with('&');
            let attr_key = attr_var_name
                .trim_start_matches('&')
                .trim_start_matches('.')
                .trim_start_matches('!');
            if delegate == Value::Nil {
                return Err(RuntimeError::new(format!(
                    "No such method '{}' for invocant of type '{}'",
                    target_method, receiver_class_name
                )));
            }
            let delegate_id = match &delegate {
                Value::Instance { id, .. } => Some(*id),
                _ => None,
            };
            let delegate_class = match &delegate {
                Value::Instance { class_name, .. } => Some(*class_name),
                _ => None,
            };
            let result = self.call_method_with_values(delegate, target_method, args)?;
            // Restore the saved skip_pseudo flag so the outer caller is unaffected.
            self.skip_pseudo_method_native = saved_skip_pseudo;
            // For Instance delegates, check if the delegate was mutated and update
            // the frontend's attribute with the updated delegate.
            if !is_method_based && let (Some(did), Some(dcn)) = (delegate_id, delegate_class) {
                // Look for the updated delegate in env bindings
                let mut updated_delegate = None;
                for val in self.env.values() {
                    if let Value::Instance { class_name, id, .. } = val
                        && *class_name == dcn
                        && *id == did
                    {
                        updated_delegate = Some(val.clone());
                        break;
                    }
                }
                if let Some(updated) = updated_delegate {
                    let mut attrs = attributes;
                    attrs.insert(attr_key.to_string(), updated);
                    return Ok((result, attrs));
                }
            }
            return Ok((result, attributes));
        }
        // §B (#3658): the non-delegation tree-walk method-execution arm (the
        // `run_block` of the user method body) has been DELETED. Every caller now
        // compiles a non-delegation candidate on-demand
        // (`compile_method_def_in_place`) before reaching here, so this function is
        // only entered for a delegation forwarder (handled above). A non-delegation
        // method reaching this point would be an internal invariant violation.
        let _ = (&owner_class, &attributes, &args, &invocant);
        Err(RuntimeError::new(format!(
            "internal error: forward_resolved_delegation reached for a non-delegation method on '{}' (should have been compiled on-demand)",
            receiver_class_name
        )))
    }
}
