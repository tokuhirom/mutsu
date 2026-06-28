use super::*;

impl Interpreter {
    pub(crate) fn builtin_encodings() -> Vec<EncodingEntry> {
        vec![
            EncodingEntry {
                name: "utf-8".to_string(),
                alternative_names: vec!["utf8".to_string()],
                user_type: None,
            },
            EncodingEntry {
                name: "utf8-c8".to_string(),
                alternative_names: vec!["utf-8-c8".to_string()],
                user_type: None,
            },
            EncodingEntry {
                name: "ascii".to_string(),
                alternative_names: Vec::new(),
                user_type: None,
            },
            EncodingEntry {
                name: "iso-8859-1".to_string(),
                alternative_names: vec!["latin-1".to_string()],
                user_type: None,
            },
            EncodingEntry {
                name: "utf-16".to_string(),
                alternative_names: vec!["utf16".to_string()],
                user_type: None,
            },
            EncodingEntry {
                name: "utf-16le".to_string(),
                alternative_names: vec![
                    "utf16le".to_string(),
                    "utf16-le".to_string(),
                    "utf-16-le".to_string(),
                ],
                user_type: None,
            },
            EncodingEntry {
                name: "utf-16be".to_string(),
                alternative_names: vec![
                    "utf16be".to_string(),
                    "utf16-be".to_string(),
                    "utf-16-be".to_string(),
                ],
                user_type: None,
            },
            EncodingEntry {
                name: "windows-932".to_string(),
                alternative_names: vec!["windows932".to_string()],
                user_type: None,
            },
            EncodingEntry {
                name: "windows-1251".to_string(),
                alternative_names: vec!["windows1251".to_string()],
                user_type: None,
            },
            EncodingEntry {
                name: "windows-1252".to_string(),
                alternative_names: vec!["windows1252".to_string()],
                user_type: None,
            },
        ]
    }

    /// Find an encoding by name (case-insensitive). Returns the entry index if found.
    pub(crate) fn find_encoding(&self, name: &str) -> Option<&EncodingEntry> {
        let name_fc = name.to_lowercase();
        self.encoding_registry.iter().find(|e| {
            e.name.to_lowercase() == name_fc
                || e.alternative_names
                    .iter()
                    .any(|alt| alt.to_lowercase() == name_fc)
        })
    }

    /// Register a user-defined encoding. Returns Ok(()) on success,
    /// or the conflicting name on failure.
    pub(crate) fn register_encoding(&mut self, entry: EncodingEntry) -> Result<(), String> {
        // Check for conflicts
        let name_fc = entry.name.to_lowercase();
        for existing in &self.encoding_registry {
            if existing.name.to_lowercase() == name_fc {
                return Err(entry.name.clone());
            }
            if existing
                .alternative_names
                .iter()
                .any(|alt| alt.to_lowercase() == name_fc)
            {
                return Err(entry.name.clone());
            }
        }
        for alt in &entry.alternative_names {
            let alt_fc = alt.to_lowercase();
            for existing in &self.encoding_registry {
                if existing.name.to_lowercase() == alt_fc {
                    return Err(alt.clone());
                }
                if existing
                    .alternative_names
                    .iter()
                    .any(|a| a.to_lowercase() == alt_fc)
                {
                    return Err(alt.clone());
                }
            }
        }
        self.encoding_registry.push(entry);
        Ok(())
    }

    pub(crate) fn suppress_name(&mut self, name: &str) {
        self.suppressed_names.insert(name.to_string());
    }

    pub(crate) fn unsuppress_name(&mut self, name: &str) {
        self.suppressed_names.remove(name);
    }

    /// Push a new lexical class scope frame.
    pub(crate) fn push_lexical_class_scope(&mut self) {
        self.lexical_class_scopes.push(Vec::new());
    }

    /// Pop a lexical class scope frame, suppressing all class names registered in it.
    pub(crate) fn pop_lexical_class_scope(&mut self) {
        if let Some(names) = self.lexical_class_scopes.pop() {
            for name in names {
                self.suppressed_names.insert(name);
            }
        }
    }

    /// Register a class name as lexically scoped in the current block.
    pub(crate) fn register_lexical_class(&mut self, name: String) {
        if let Some(scope) = self.lexical_class_scopes.last_mut() {
            scope.push(name);
        }
    }

    /// A `package`/`module` declaration with a bare `name` shadows any same-named
    /// `class`/`role`/`enum` whose lexical scope has already exited (i.e. the name
    /// is in `suppressed_names`). Without this, a stale out-of-scope `class A` left
    /// in the registry keeps `has_type("A")` true, so a later `my package A {}; my A $x`
    /// resolves `A` to the dead class and never reports X::Syntax::Variable::BadType.
    /// Remove the stale type and un-suppress the name so the package becomes the
    /// active `A`. Only out-of-scope (suppressed) types are removed — an in-scope
    /// same-named class is a genuine redeclaration handled elsewhere.
    pub(crate) fn shadow_suppressed_type_with_package(&mut self, name: &str) {
        if name.contains("::") || !self.suppressed_names.contains(name) {
            return;
        }
        self.registry_mut().classes.remove(name);
        self.registry_mut().roles.remove(name);
        self.registry_mut().enum_types.remove(name);
        self.registry_mut().subsets.remove(name);
        self.suppressed_names.remove(name);
    }

    /// Mark a fully-qualified name as `my`-scoped within its parent package.
    /// Items in this set are excluded from the parent package's stash.
    pub(crate) fn mark_my_scoped_package_item(&mut self, fq_name: String) {
        self.my_scoped_package_items.insert(fq_name);
    }

    /// Check if a fully-qualified name is `my`-scoped within its parent package.
    pub(crate) fn is_my_scoped_package_item(&self, fq_name: &str) -> bool {
        self.my_scoped_package_items.contains(fq_name)
    }

    pub(crate) fn is_name_suppressed(&self, name: &str) -> bool {
        self.suppressed_names.contains(name)
    }

    /// Check if a bare enum variant name is poisoned (declared by multiple enums).
    pub(crate) fn is_poisoned_enum_alias(&self, name: &str) -> Option<&str> {
        self.poisoned_enum_aliases.get(name).map(|s| s.as_str())
    }

    /// Record a bare enum name in the current scope for poisoning detection.
    /// Only marks as poisoned if the name was already declared in the *same*
    /// scope level by a different enum.
    pub(crate) fn register_enum_bare_name(&mut self, name: &str, enum_type: &str) {
        // Check only the current scope for the same name from a different enum
        if let Some(current_scope) = self.enum_scope_names.last()
            && current_scope.iter().any(|n| n == name)
            && let Some(Value::Enum {
                enum_type: prev_type,
                ..
            }) = self.env.get(name)
            && prev_type.resolve() != enum_type
        {
            self.poisoned_enum_aliases
                .insert(name.to_string(), enum_type.to_string());
        }
        if let Some(scope) = self.enum_scope_names.last_mut() {
            scope.push(name.to_string());
        }
    }

    /// Push a new enum scope frame (called on block enter).
    pub(crate) fn push_enum_scope(&mut self) {
        self.enum_scope_names.push(Vec::new());
    }

    /// Pop an enum scope frame, removing poisoned aliases for names
    /// that were introduced in the exiting scope.
    pub(crate) fn pop_enum_scope(&mut self) {
        if let Some(names) = self.enum_scope_names.pop() {
            for name in names {
                self.poisoned_enum_aliases.remove(&name);
            }
        }
    }

    /// Resolve a suppressed nested class short name to its qualified form.
    /// For example, if `Frog` is suppressed and we are inside class `Forest`,
    /// this returns `Some("Forest::Frog")` if `Forest::Frog` is a known type.
    pub(crate) fn resolve_suppressed_type(&self, name: &str) -> Option<String> {
        if !self.suppressed_names.contains(name) {
            return None;
        }
        // Check current package
        let current_pkg = &self.current_package();
        if current_pkg != "GLOBAL" {
            let qualified = format!("{}::{}", current_pkg, name);
            if self.has_type(&qualified) {
                return Some(qualified);
            }
        }
        // Check method class stack
        for class_name in self.method_class_stack.iter().rev() {
            let qualified = format!("{}::{}", class_name, name);
            if self.has_type(&qualified) {
                return Some(qualified);
            }
        }
        // The class currently being constructed. A typed attribute's default
        // type object (`has Inner $.x` defaults to `BareWord("Inner")`) is
        // evaluated during construction with no method-class / package context,
        // yet the nested name must resolve within its owning class. This is set
        // only for the duration of attribute-default evaluation, so it does not
        // leak the suppressed name into outer lexical scopes (where raku keeps it
        // undeclared).
        if let Some(class_name) = &self.constructing_class {
            let qualified = format!("{}::{}", class_name, name);
            if self.has_type(&qualified) {
                return Some(qualified);
            }
        }
        None
    }

    pub fn set_pid(&mut self, pid: i64) {
        self.env.insert("*PID".to_string(), Value::Int(pid));
    }

    pub fn set_program_path(&mut self, path: &str) {
        self.program_path = Some(path.to_string());
        let io_path = self.make_io_path_instance(path);
        self.env.insert("*PROGRAM".to_string(), io_path);
        self.env
            .insert("*PROGRAM-NAME".to_string(), Value::str(path.to_string()));
    }

    pub fn set_args(&mut self, args: Vec<Value>) {
        self.env
            .insert("@*ARGS".to_string(), Value::real_array(args));
    }

    pub fn add_lib_path(&mut self, path: String) {
        if !path.is_empty() {
            self.lib_paths.push(path);
        }
    }
}
