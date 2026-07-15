//! Class/role introspection helpers: native-method tables, user-method and
//! accessor presence checks, class-level attribute lookup/mutation, and
//! attribute collection across the MRO and composed roles. Lifecycle/MRO lives
//! in `class`; instance-method dispatch in `class_dispatch`.

use super::*;

/// Winner of the per-MRO-level race between an explicit user method and a
/// public attribute accessor (see `resolve_user_method_or_accessor`).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum UserMethodOrAccessor {
    Method,
    Accessor,
}

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
        for cn in mro.iter() {
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
                    | "lock"
                    | "unlock"
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
                    | "raku"
                    | "perl"
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
                    | "osname"
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
        for cn in mro.iter() {
            if let Some(class_def) = self.registry().classes.get(cn.as_str())
                && class_def.native_methods.contains(method_name)
            {
                return true;
            }
        }
        false
    }

    pub(crate) fn has_user_method(&mut self, class_name: &str, method_name: &str) -> bool {
        let mro = self.class_mro(class_name);
        for cn in mro.iter() {
            if let Some(class_def) = self.registry().classes.get(cn.as_str())
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

    /// Decide, per MRO level, whether an explicit user method or a public
    /// attribute accessor handles `method_name` for `class_name`.
    ///
    /// In Raku the auto-generated accessor for `has $.x` is an ordinary method
    /// of its declaring class, so it participates in the MRO like any other
    /// method: a child's accessor shadows a parent's explicit method of the
    /// same name (Zef::Distribution's `has $.name` vs its parent
    /// DependencySpecification's `method name`). Within a single class level
    /// the priority is: explicit class-local method > public attribute
    /// accessor > role-composed method (class entities are prioritized over
    /// role entities — 6.c S14-roles/attributes.t "Class prioritization").
    pub(crate) fn resolve_user_method_or_accessor(
        &mut self,
        class_name: &str,
        method_name: &str,
    ) -> Option<UserMethodOrAccessor> {
        let mro = self.class_mro(class_name);
        for cn in mro.iter() {
            let is_ancestor = cn.as_str() != class_name;
            let (has_local_method, has_role_method, has_attr) = {
                let registry = self.registry();
                if let Some(class_def) = registry.classes.get(cn.as_str()) {
                    let (mut local, mut role) = (false, false);
                    if let Some(defs) = class_def.methods.get(method_name) {
                        for d in defs {
                            if d.is_private || (d.is_my && is_ancestor) {
                                continue;
                            }
                            if d.role_origin.is_none() {
                                local = true;
                            } else {
                                role = true;
                            }
                        }
                    }
                    let attr = class_def
                        .attributes
                        .iter()
                        .any(|(n, is_public, ..)| *is_public && n == method_name);
                    (local, role, attr)
                } else if let Some(role_def) = registry.roles.get(cn.as_str()) {
                    // A punned role used as a parent class: its own methods
                    // and attribute accessors sit at this MRO level.
                    let local = role_def
                        .methods
                        .get(method_name)
                        .is_some_and(|defs| defs.iter().any(|d| !d.is_private));
                    let attr = role_def
                        .attributes
                        .iter()
                        .any(|(n, is_public, ..)| *is_public && n == method_name);
                    (local, false, attr)
                } else {
                    (false, false, false)
                }
            };
            if has_local_method {
                return Some(UserMethodOrAccessor::Method);
            }
            if has_attr {
                return Some(UserMethodOrAccessor::Accessor);
            }
            if has_role_method {
                return Some(UserMethodOrAccessor::Method);
            }
        }
        None
    }

    /// Accessor-slot promotion gate: when `method_name` is a public `is rw`
    /// attribute accessor, return `Some(declared type constraint)` (`Some(None)`
    /// for an untyped rw attribute). `None` means the accessor is not rw — a
    /// want-ref read must NOT promote the slot (raku returns the decont'd value
    /// there, so `my $r := $obj.ro-attr; $r = v` stays an immutable-value error).
    pub(crate) fn rw_accessor_type_constraint(
        &mut self,
        class_name: &str,
        method_name: &str,
    ) -> Option<Option<String>> {
        let attrs = self.collect_class_attributes(class_name);
        let is_rw = attrs
            .iter()
            .any(|(attr_name, is_public, _default, is_rw, ..)| {
                *is_public && *is_rw && attr_name == method_name
            });
        if !is_rw {
            return None;
        }
        let mro = self.class_mro(class_name);
        for cn in mro.iter() {
            if let Some(tc) = self.get_attr_type_constraint(cn.as_str(), method_name) {
                return Some(Some(tc));
            }
        }
        Some(None)
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
            [].into()
        };
        for parent in mro.iter().map(|p| p.as_str()) {
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
        for parent in mro.iter().map(|s| s.as_str()) {
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
        for parent in mro.iter().map(|s| s.as_str()) {
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
        for cn in mro.iter() {
            if let Some(class_def) = self.registry().classes.get(cn.as_str()) {
                result.extend(class_def.wildcard_handles.iter().cloned());
            }
        }
        result
    }

    /// Add `__mutsu_attr_alias::x` metadata for attributes declared with `has $x`
    /// (no twigil), so the method call dispatch can set up bidirectional aliases.
    pub(super) fn add_alias_attribute_metadata(&mut self, class_name: &str, attrs: &mut AttrMap) {
        let mro = self.class_mro(class_name);
        for cn in mro.iter() {
            if let Some(class_def) = self.registry().classes.get(cn.as_str()) {
                for attr_name in &class_def.alias_attributes {
                    attrs.insert(
                        format!("__mutsu_attr_alias::{}", attr_name),
                        Value::str(attr_name.to_string()),
                    );
                }
            }
        }
    }

    /// Whether `class_name` (or any class in its MRO) declares an attribute
    /// with the bare name `bare` (`"x"` for `$!x`/`$.x`). Used by the VM's
    /// private-attribute read check: a read of an attribute that the invocant's
    /// class neither carries nor declares throws (P6opaque no-such-attribute).
    pub(crate) fn class_declares_attribute(&mut self, class_name: &str, bare: &str) -> bool {
        self.class_mro(class_name).iter().any(|cn| {
            self.registry()
                .classes
                .get(cn.as_str())
                .is_some_and(|cd| cd.attributes.iter().any(|(n, ..)| n == bare))
        })
    }

    pub(super) fn collect_class_attributes(&mut self, class_name: &str) -> Vec<ClassAttributeDef> {
        let mro = self.class_mro(class_name);
        let mut attrs: Vec<ClassAttributeDef> = Vec::new();
        for cn in mro.iter().rev() {
            if let Some(class_def) = self.registry().classes.get(cn.as_str()) {
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
        for cn in mro.iter() {
            if let Some(class_def) = self.registry().classes.get(cn.as_str()) {
                for attr in &class_def.attributes {
                    *attr_counts.entry(attr.0.clone()).or_insert(0) += 1;
                }
            }
        }
        // Only include attrs that appear in multiple classes (duplicated across hierarchy)
        for cn in mro.iter() {
            if let Some(class_def) = self.registry().classes.get(cn.as_str()) {
                for attr in &class_def.attributes {
                    if attr_counts.get(&attr.0).copied().unwrap_or(0) > 1 {
                        result.push((cn.resolve(), attr.clone()));
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
