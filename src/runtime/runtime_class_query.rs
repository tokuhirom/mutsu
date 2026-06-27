use super::*;

impl Interpreter {
    pub(crate) fn has_class(&self, name: &str) -> bool {
        self.registry().classes.contains_key(name)
    }

    /// True when `name` was declared via the `package`/`module` declarator (and
    /// is therefore not type-like enough to constrain a variable/parameter).
    /// Used to raise X::Syntax::Variable::BadType / X::Parameter::BadType instead
    /// of a generic "not declared" error. A `class`/`role`/`enum`/`subset` is a
    /// real type and is NOT recorded here (it goes through the class registry).
    pub(crate) fn is_declared_package(&self, name: &str) -> bool {
        self.chain_declared_packages.contains(name)
            || matches!(self.env.get(name), Some(Value::Package(_)))
    }

    /// Check if a class name refers to a user-defined class that inherits from
    /// a container type (Hash, Array, etc.). Used to skip element-level type
    /// checking for container subclasses.
    /// True when `name` is a user-declared class / package / module that is NOT
    /// parametric — i.e. parameterizing it with `[T]` (or `of T`) is an error
    /// (X::NotParametric). Roles, built-in container types (Array/Hash/Buf/Blob/
    /// ...), and subclasses of containers ARE parametric and return false.
    pub(crate) fn is_non_parametric_type(&self, name: &str) -> bool {
        // Built-in parametric container types accept `[T]`. Some (Buf, Blob, ...)
        // are registered as classes, so they must be allow-listed here.
        let parametric_builtin = matches!(
            name,
            "Array"
                | "Hash"
                | "Map"
                | "List"
                | "Slip"
                | "Seq"
                | "Range"
                | "Set"
                | "Bag"
                | "Mix"
                | "SetHash"
                | "BagHash"
                | "MixHash"
                | "Buf"
                | "Blob"
                | "buf8"
                | "buf16"
                | "buf32"
                | "buf64"
                | "blob8"
                | "blob16"
                | "blob32"
                | "blob64"
                | "Positional"
                | "Associative"
                | "Iterable"
        );
        !parametric_builtin
            && !self.is_role(name)
            && !self.is_container_subclass(name)
            && (self.has_class(name) || self.is_declared_package(name))
    }

    pub(crate) fn is_container_subclass(&self, name: &str) -> bool {
        const CONTAINER_TYPES: &[&str] = &[
            "Hash", "Array", "Map", "List", "Bag", "Set", "Mix", "BagHash", "SetHash", "MixHash",
            "Seq",
        ];
        // Clone out the parents under a single guard, then recurse without holding it.
        let parents = {
            let reg = self.registry();
            reg.classes
                .get(name)
                .or_else(|| {
                    reg.classes
                        .iter()
                        .find(|(k, _)| k.rsplit_once("::").is_some_and(|(_, short)| short == name))
                        .map(|(_, v)| v)
                })
                .map(|cd| cd.parents.clone())
        };
        if let Some(parents) = parents {
            for parent in &parents {
                if CONTAINER_TYPES.contains(&parent.as_str()) {
                    return true;
                }
                if self.is_container_subclass(parent) {
                    return true;
                }
            }
        }
        false
    }

    /// Check if a class inherits from an immutable Setty type (Set, Bag, Mix).
    /// Whether a user-declared class `name` inherits (transitively) from the base
    /// `Exception` type (or a built-in `X::`/`CX::` exception). Walks the registry
    /// parent chain with a shared borrow.
    pub(crate) fn class_inherits_from_exception(&self, name: &str) -> bool {
        let parents = {
            let reg = self.registry();
            reg.classes
                .get(name)
                .or_else(|| {
                    reg.classes
                        .iter()
                        .find(|(k, _)| k.rsplit_once("::").is_some_and(|(_, short)| short == name))
                        .map(|(_, v)| v)
                })
                .map(|cd| cd.parents.clone())
        };
        if let Some(parents) = parents {
            for parent in &parents {
                if parent == "Exception"
                    || parent.starts_with("X::")
                    || parent.starts_with("CX::")
                    || self.class_inherits_from_exception(parent)
                {
                    return true;
                }
            }
        }
        false
    }

    pub(crate) fn class_inherits_from_immutable_setty(&self, name: &str) -> bool {
        const IMMUTABLE_SETTY: &[&str] = &["Set", "Bag", "Mix"];
        if IMMUTABLE_SETTY.contains(&name) {
            return true;
        }
        // Clone out the parents under a single guard, then recurse without holding it.
        let parents = {
            let reg = self.registry();
            reg.classes
                .get(name)
                .or_else(|| {
                    reg.classes
                        .iter()
                        .find(|(k, _)| k.rsplit_once("::").is_some_and(|(_, short)| short == name))
                        .map(|(_, v)| v)
                })
                .map(|cd| cd.parents.clone())
        };
        if let Some(parents) = parents {
            for parent in &parents {
                if IMMUTABLE_SETTY.contains(&parent.as_str()) {
                    return true;
                }
                if self.class_inherits_from_immutable_setty(parent) {
                    return true;
                }
            }
        }
        false
    }

    /// Check if a class has scoped subs declared in its body.
    pub(crate) fn has_class_scoped_subs(&self, class_name: &str) -> bool {
        // Single guard for both reads (no user-code re-entry here, so let-binding
        // is safe and avoids a same-thread recursive read lock).
        let registry = self.registry();
        registry
            .class_subs
            .get(class_name)
            .is_some_and(|subs| !subs.is_empty())
            || registry.class_subs.keys().any(|k| {
                k.ends_with(&format!("::{}", class_name))
                    || class_name.ends_with(&format!("::{}", k))
            })
    }

    /// Read access to the shared declaration [`Registry`]. Returns a temporary
    /// guard — NEVER `let`-bind it across a call that re-enters user-code
    /// execution (`eval_block_value`/`run_block_raw`/`call_function`): `RwLock`
    /// is not reentrant and would deadlock. Use as `self.registry().subsets...`.
    ///
    /// In debug builds the returned guard detects a re-entrant lock attempt on
    /// this thread and panics with a located message instead of deadlocking (see
    /// [`RegistryReadGuard`](crate::runtime::registry::RegistryReadGuard)).
    #[inline]
    pub(crate) fn registry(&self) -> crate::runtime::registry::RegistryReadGuard<'_> {
        crate::runtime::registry::RegistryReadGuard::new(&self.registry, "registry")
    }

    /// Write access to the shared declaration [`Registry`]. Same guard discipline
    /// as [`Self::registry`].
    #[inline]
    pub(crate) fn registry_mut(&self) -> crate::runtime::registry::RegistryWriteGuard<'_> {
        crate::runtime::registry::RegistryWriteGuard::new(&self.registry, "registry")
    }

    /// Read access to the shared [`IoHandleTable`](io_handles::IoHandleTable).
    /// Same guard discipline as [`Self::registry`]: never hold the returned guard
    /// across a call that re-enters another handle operation (`RwLock` is not
    /// reentrant). Use as `self.io_handles().map.get(&id)`.
    #[inline]
    pub(crate) fn io_handles(&self) -> io_handles::IoHandlesReadGuard<'_> {
        io_handles::IoHandlesReadGuard::new(&self.io_handles, "io_handles")
    }

    /// Read access to the shared [`OutputSink`]. Same guard discipline as
    /// [`Self::io_handles`]: never hold the returned guard across a call that
    /// re-enters another output operation.
    #[inline]
    pub(crate) fn output_sink(&self) -> output_sink::OutputSinkReadGuard<'_> {
        output_sink::OutputSinkReadGuard::new(&self.output_sink, "output_sink")
    }

    /// Write access to the shared [`OutputSink`]. Same guard discipline as
    /// [`Self::output_sink`].
    #[inline]
    pub(crate) fn output_sink_mut(&self) -> output_sink::OutputSinkWriteGuard<'_> {
        output_sink::OutputSinkWriteGuard::new(&self.output_sink, "output_sink")
    }

    /// Write access to the shared [`IoHandleTable`](io_handles::IoHandleTable).
    /// Same guard discipline as [`Self::io_handles`].
    #[inline]
    pub(crate) fn io_handles_mut(&self) -> io_handles::IoHandlesWriteGuard<'_> {
        io_handles::IoHandlesWriteGuard::new(&self.io_handles, "io_handles")
    }

    /// Whether a TAP subtest is currently in progress. The VM queries this (the
    /// TAP state machine stays interpreter-owned) to pass `subtest_active` into
    /// the output sink's emit decision for VM-native Stdout/Stderr output.
    pub(crate) fn subtest_active(&self) -> bool {
        self.tap.subtest_depth() != 0
    }

    /// Allocate a fresh handle id, store `state` under it, and return the id.
    /// Build `state` fully (including anything that needs `&self`, e.g.
    /// `default_line_separators()`) *before* calling this, since the short write
    /// guard taken here must not be held across other `self` operations.
    pub(super) fn insert_handle_state(&mut self, state: IoHandleState) -> usize {
        let mut table = self.io_handles_mut();
        let id = table.next_id;
        table.next_id += 1;
        table.map.insert(id, state);
        id
    }

    /// Normalize a scoped `state`-variable storage key into a form stable across
    /// mutsu's two compilations of the same routine (the registered body `&f` and
    /// the on-the-fly multi-candidate body `&f/0` reach `state $n` under different
    /// `current_package` suffixes and opcode positions). Used as the cross-thread
    /// shared-cell key so `start f()` and a direct `f()` agree (Track C). Strips a
    /// trailing `@<digits>` (opcode position) and any `/<digits>` candidate suffix.
    pub(crate) fn normalize_state_key(key: &str) -> String {
        let trimmed = match key.rfind('@') {
            Some(pos)
                if pos + 1 < key.len() && key[pos + 1..].bytes().all(|b| b.is_ascii_digit()) =>
            {
                &key[..pos]
            }
            _ => key,
        };
        let mut out = String::with_capacity(trimmed.len());
        let mut chars = trimmed.chars().peekable();
        while let Some(c) = chars.next() {
            if c == '/' && chars.peek().is_some_and(|n| n.is_ascii_digit()) {
                while chars.peek().is_some_and(|n| n.is_ascii_digit()) {
                    chars.next();
                }
            } else {
                out.push(c);
            }
        }
        out
    }
}
