//! Class/role introspection helpers: native-method tables, user-method and
//! accessor presence checks, class-level attribute lookup/mutation, and
//! attribute collection across the MRO and composed roles. Lifecycle/MRO lives
//! in `class`; instance-method dispatch in `class_dispatch`.

use super::*;

impl Interpreter {
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
}
