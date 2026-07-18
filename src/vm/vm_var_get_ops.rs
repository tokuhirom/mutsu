use super::*;

impl Interpreter {
    pub(super) fn exec_get_bare_word_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        // A bare `_` term is never a declared name in raku (the topic is `$_`);
        // rakudo reports it as X::Undeclared::Symbols at compile time.
        if name == "_" {
            return Err(RuntimeError::undeclared_symbols(
                "Undeclared name:\n    _ used at line 1",
            ));
        }
        // A bareword with a type smiley whose base is a bound generic type
        // parameter (`T:D` inside a role method where `T` -> `Int`) resolves to
        // the parameterized type with the smiley applied (`Int:D`). Plain
        // built-in types like `Int:D` are NOT env-bound, so they fall through to
        // the normal resolution below and are unaffected.
        if let (base, Some(smiley)) = crate::runtime::types::strip_type_smiley(name)
            && !base.is_empty()
            && let Some(v) = self.env().get(base)
            && let ValueView::Package(pkg) = v.view()
        {
            let resolved = format!("{}{}", pkg.resolve(), smiley);
            self.stack.push(Value::package(Symbol::intern(&resolved)));
            return Ok(());
        }
        let val = if name == "Bool::True" {
            Value::TRUE
        } else if name == "Bool::False" {
            Value::FALSE
        } else if name == "Order::Less" {
            Value::enum_parts(
                Symbol::intern("Order"),
                Symbol::intern("Less"),
                EnumValue::Int(-1),
                0,
            )
        } else if name == "Order::Same" {
            Value::enum_parts(
                Symbol::intern("Order"),
                Symbol::intern("Same"),
                EnumValue::Int(0),
                1,
            )
        } else if name == "Order::More" {
            Value::enum_parts(
                Symbol::intern("Order"),
                Symbol::intern("More"),
                EnumValue::Int(1),
                2,
            )
        } else if (name.starts_with("infix:<")
            || name.starts_with("prefix:<")
            || name.starts_with("postfix:<"))
            && name.ends_with('>')
        {
            Value::routine_parts(Symbol::intern("GLOBAL"), Symbol::intern(name), false)
        } else if self.is_name_suppressed(name) {
            // If we are inside the parent class of the suppressed nested class,
            // resolve the short name to the qualified name (e.g. Frog -> Forest::Frog).
            if let Some(qualified) = self.resolve_suppressed_type(name) {
                Value::package(Symbol::intern(&qualified))
            } else {
                return Err(RuntimeError::new(format!(
                    "X::Undeclared::Symbols: Undeclared name:\n    {} used at line 1",
                    name,
                )));
            }
        } else if let Some((pkg, sym)) = name.rsplit_once("::")
            && let Some(stripped_sym) = sym.strip_prefix('&')
        {
            let qualified_name = format!("{pkg}::{stripped_sym}");
            if self.has_function(&qualified_name) || self.has_multi_function(&qualified_name) {
                Value::routine_parts(Symbol::intern(pkg), Symbol::intern(&qualified_name), false)
            } else {
                Value::str(name.to_string())
            }
        } else if name == "i" {
            // Raku term constant `i` (imaginary unit) — must be resolved before
            // the env lookup because `$i` is stored under key "i" (no sigil) and
            // would shadow the term.  Sigilless `my \i` goes through GetLocal.
            Value::complex(0.0, 1.0)
        } else if name == "NaN" {
            Value::num(f64::NAN)
        } else if name == "Inf" {
            Value::num(f64::INFINITY)
        } else if name == "Empty" && !self.has_type(name) {
            // A user-declared type named `Empty` shadows the empty-Slip term
            // (it is resolved to a Package by the `has_type` branch below).
            // `Inf`/`NaN` are numeric literals and are not shadowable.
            Value::slip_arc(std::sync::Arc::new(vec![]))
        } else if name == "GLOBALish" {
            // GLOBALish is the per-compunit alias for the GLOBAL package.
            Value::package(Symbol::intern("GLOBAL"))
        } else if Self::is_pseudo_package_bare(name) {
            // Pseudo-package names (MY, CORE, OUTER, CALLER, etc.) resolve to
            // Package values so that .WHO/.WHAT etc. work correctly.
            Value::package(Symbol::intern(name))
        } else if (self.has_type(name) || Self::is_builtin_type(name))
            && matches!(self.env().get(name).map(Value::view), Some(ValueView::Nil))
        {
            // A bareword that names a type resolves to the type object. A same-named
            // `$`-sigiled scalar (`my $foo` where a class `foo` exists) is stored
            // sigil-stripped under the same env key; while it is still unassigned
            // (Nil) it must NOT shadow the type — `$foo` and `foo` are distinct
            // symbols in Raku. This notably affects `my $foo = foo.new`, whose RHS
            // resolves `foo` before the `$foo` slot is assigned.
            Value::package(Symbol::intern(Self::resolve_type_alias(name)))
        } else if let Some(v) = self.env().get(name) {
            if matches!(v.view(), ValueView::Enum { .. } | ValueView::Nil)
                || matches!(v.view(), ValueView::Package(pkg) if pkg.resolve() != name)
            {
                // Check for poisoned enum aliases
                if matches!(v.view(), ValueView::Enum { .. })
                    && !name.contains("::")
                    && let Some(pkg_name) = self.is_poisoned_enum_alias(name)
                {
                    let pkg_name = pkg_name.to_string();
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert(
                        "message".to_string(),
                        Value::str(format!(
                            "Cannot directly use poisoned alias '{name}' because it \
                             was declared by several enums. Please access it via \
                             explicit package name like: '{pkg_name}::{name}'"
                        )),
                    );
                    attrs.insert("alias".to_string(), Value::str(name.to_string()));
                    attrs.insert("package-name".to_string(), Value::str(pkg_name.clone()));
                    attrs.insert("package-type".to_string(), Value::str("enum".to_string()));
                    let ex = Value::make_instance(Symbol::intern("X::PoisonedAlias"), attrs);
                    let mut err = RuntimeError::new(format!(
                        "Cannot directly use poisoned alias '{name}' because it was \
                         declared by several enums. Please access it via explicit \
                         package name like: '{pkg_name}::{name}'"
                    ));
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
                v.clone()
            } else if self.has_type(name) || Self::is_builtin_type(name) {
                Value::package(Symbol::intern(Self::resolve_type_alias(name)))
            } else if name.contains("::")
                && !name.starts_with('$')
                && !name.starts_with('@')
                && !name.starts_with('%')
                && matches!(
                    v.view(),
                    ValueView::Routine { .. } | ValueView::Sub(_) | ValueView::WeakSub(_)
                )
            {
                // CARRIER: pseudo-package names (SETTING::, OUTER::, CALLER::) need
                // the interpreter's reflective scope resolution. See ledger §C.
                // For regular qualified names, try compiled dispatch first.
                if self.is_interpreter_handled_function(name) {
                    self.vm_call_function(name, Vec::new())?
                } else {
                    self.call_function_compiled_first(name, Vec::new(), compiled_fns)?
                }
            } else if !name.starts_with('$') && !name.starts_with('@') && !name.starts_with('%') {
                // A sigilless bind (`given ... -> \ex`, `ex := $_`) stores a
                // shared ContainerRef cell in env; the bareword read must
                // deliver the cell's value, not the wrapper — method dispatch
                // on a raw ContainerRef misresolves (e.g. `ex.raku` on a bound
                // instance rendered the type object).
                v.deref_container()
            } else {
                Value::str(name.to_string())
            }
        } else if let Some(enum_val) = self.resolve_qualified_enum_alias(name) {
            // A qualified name `Alias::variant` where `Alias` is a constant (or
            // `my`/`our` symbol) bound to an enum type object. Raku resolves the
            // prefix through the alias and then looks up the enum variant, e.g.
            // `my constant G = F::B; G::c === F::B::c`.
            enum_val
        } else if name.contains("::")
            && let Some(def) = loan_env!(self, resolve_function_with_types(name, &[]))
        {
            if let Some(cf) = self.find_compiled_function(compiled_fns, name, &[]) {
                let pkg = def.package.resolve();
                // Slice 6.3 step 2: call_compiled_function_named signals env_dirty
                // precisely via its return merge; no blanket mark needed.
                self.call_compiled_function_named(cf, Vec::new(), compiled_fns, &pkg, name)?
            } else {
                self.compile_and_call_function_def(&def, Vec::new(), compiled_fns)?
            }
        } else if self.has_type(name)
            || Self::is_builtin_type(name)
            || Self::is_type_with_smiley(name, self)
        {
            Value::package(Symbol::intern(Self::resolve_type_alias(name)))
        } else if self.wrap_sub_id_for_name(name).is_some()
            && let Some(sub_val) = self.get_wrapped_sub(name)
        {
            // A wrapped sub used as a bareword term must dispatch through its
            // wrap chain. The `&name` env entry is a fresh Sub whose id differs
            // from the wrap-chain key, so it would bypass the wrappers — check
            // the wrap chain BEFORE the plain `&name` callable below.
            self.vm_call_on_value(sub_val, Vec::new(), Some(compiled_fns))?
        } else if let Some(callable) = self.env().get(&format!("&{name}")).cloned()
            && matches!(
                callable.view(),
                ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
            )
        {
            self.vm_call_on_value(callable, Vec::new(), Some(compiled_fns))?
        } else if Interpreter::is_test_function_name(name)
            && self.test_mode_active()
            // Only try test function dispatch for hyphenated names (e.g.
            // `make-temp-dir`, `done-testing`). Single-word names like
            // `run`, `is`, `ok` are either builtins or need args, so they
            // should go through the normal function resolution path.
            && name.contains('-')
        {
            // CARRIER: Test-framework function dispatch (test-mode state). See ledger §C.
            self.vm_call_function(name, Vec::new())?
        } else if self.has_function(name)
            || Interpreter::is_implicit_zero_arg_builtin(name)
            || self.has_multi_function(name)
        {
            if let Some(cf) = self.find_compiled_function(compiled_fns, name, &[]) {
                let pkg = self.current_package().to_string();
                // Slice 6.3 step 2: precise env_dirty from the named-call merge.
                self.call_compiled_function_named(cf, Vec::new(), compiled_fns, &pkg, name)?
            } else if let Some(native_result) =
                self.try_native_function(crate::symbol::Symbol::intern(name), &[])
            {
                native_result?
            } else {
                // Route the cold 0-arg term fallback through the Interpreter's unified
                // compiled-first function dispatch (ledger §2): this adds OTF
                // compilation of simple user subs; the interpreter remains only as
                // the terminal fallback inside call_function_compiled_first.
                self.call_function_compiled_first(name, Vec::new(), compiled_fns)?
            }
        } else if name == "callsame"
            || name == "nextsame"
            || name == "callwith"
            || name == "nextwith"
            || name == "nextcallee"
            || name == "lastcall"
        {
            // CARRIER: call-chain introspection (interpreter MOP dispatch stack). See ledger §C.
            self.vm_call_function(name, Vec::new())?
        } else if name.starts_with("Metamodel::") {
            // Meta-object protocol type objects
            Value::package(Symbol::intern(name))
        } else if name.contains("::") {
            // Check if this is an access to a non-existent enum variant
            if let Some((pkg, sym)) = name.rsplit_once("::")
                && self.has_enum_type(pkg)
                && !self.has_enum_variant(pkg, sym)
            {
                return Err(RuntimeError::new(format!(
                    "Could not find symbol '&{}' in '{}'",
                    sym, pkg,
                )));
            }
            // Try resolving as a package-qualified function call (e.g.
            // `Module::func` used as a term without parens).
            if let Some((_pkg, short)) = name.rsplit_once("::")
                && self.has_function(&format!("GLOBAL::{}", short))
            {
                // Route the package-qualified term fork through the Interpreter's unified
                // compiled-first function dispatch (ledger §2): interpreter remains
                // only as the terminal fallback.
                self.call_function_compiled_first(name, Vec::new(), compiled_fns)?
            } else if let Some((pkg_prefix, last_seg)) = name.rsplit_once("::")
                && !last_seg.is_empty()
                && last_seg.starts_with(|c: char| c.is_ascii_lowercase())
                && !self.has_type(name)
                && !Self::is_builtin_type(name)
                && !self.has_function(name)
                && !self.has_multi_function(name)
                && !self.has_function(last_seg)
                && !self.has_multi_function(last_seg)
            {
                // The last segment starts with lowercase, indicating a qualified
                // function/sub call (e.g. `Our::Package::pkg`). If the prefix
                // package and function are not known, throw an error like raku does.
                return Err(RuntimeError::new(format!(
                    "Could not find symbol '&{}' in '{}'",
                    last_seg, pkg_prefix,
                )));
            } else {
                // Strip pseudo-package prefixes (OUR::, GLOBAL::, MY::, etc.)
                // and resolve to the bare type name if it exists.
                let bare = Interpreter::strip_pseudo_packages(name);
                if bare != name
                    && (self.has_type(bare)
                        || Self::is_builtin_type(bare)
                        || Self::is_type_with_smiley(bare, self))
                {
                    Value::package(Symbol::intern(Self::resolve_type_alias(bare)))
                } else {
                    Value::package(Symbol::intern(name))
                }
            }
        } else if name.chars().count() == 1 {
            // Single unicode character — check for vulgar fractions etc.
            let ch = name.chars().next().unwrap();
            if let Some((n, d)) = crate::builtins::unicode::unicode_rat_value(ch) {
                Value::rat_raw(n, d)
            } else if let Some(val) = crate::builtins::unicode::unicode_numeric_int_value(ch) {
                Value::int(val)
            } else if let Some(our_val) = self.get_our_var(name).cloned() {
                // Single-character `our`-scoped constant declared in an inner
                // block (see the multi-char case below for rationale).
                our_val
            } else {
                Value::str(name.to_string())
            }
        } else if name == "self" {
            // `self` used outside of a method context
            let mut err = RuntimeError::new("'self' used where no object is available".to_string());
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "message".to_string(),
                Value::str("'self' used where no object is available".to_string()),
            );
            err.exception = Some(Box::new(Value::make_instance(
                Symbol::intern("X::Syntax::Self::WithoutObject"),
                attrs,
            )));
            return Err(err);
        } else if let Some(our_val) = self.get_our_var(name).cloned() {
            // `our`-scoped constants (and `our` variables) declared in an inner
            // block survive block-scope restoration in the package store
            // (`our_vars`) even though the lexical env entry was discarded.
            // Resolve the bare word to that persisted package value rather than
            // treating it as an undeclared bareword string.
            our_val
        } else {
            Value::str(name.to_string())
        };
        self.stack.push(val);
        Ok(())
    }

    /// Resolve a qualified name `Prefix::variant` where `Prefix` is a symbol
    /// (typically a `constant`/`my`/`our` declaration) bound to an enum type
    /// object, returning the corresponding enum variant value.
    ///
    /// For example, after `enum F::B <c d e>; my constant G = F::B;`, the name
    /// `G::c` resolves by mapping the prefix `G` to its package `F::B` and then
    /// looking up the already-registered variant under `F::B::c`.
    pub(super) fn resolve_qualified_enum_alias(&self, name: &str) -> Option<Value> {
        if name.starts_with(['$', '@', '%', '&']) {
            return None;
        }
        let (prefix, variant) = name.rsplit_once("::")?;
        // The prefix must resolve through the lexical env to an enum type object.
        let Some(ValueView::Package(pkg)) = self.env().get(prefix).map(Value::view) else {
            return None;
        };
        let pkg = pkg.resolve();
        // Avoid infinite identity: the prefix must alias a *different* package
        // (otherwise `F::B::c` would already have been found by direct lookup).
        if pkg == prefix {
            return None;
        }
        if !self.has_enum_variant(&pkg, variant) {
            return None;
        }
        // The enum registration stores variants under `{enum_type}::{variant}`.
        self.env().get(&format!("{pkg}::{variant}")).cloned()
    }

    /// Get a variable from an outer lexical scope.
    /// `depth` is the number of OUTER:: prefixes (1 = immediate outer, 2 = two levels up).
    /// Uses the `outer_scope_locals` stack which is populated by BlockScope operations.
    pub(super) fn get_outer_var(
        &self,
        code: &CompiledCode,
        name: &str,
        depth: usize,
        slot: Option<u32>,
    ) -> Value {
        if depth == 0 {
            return Value::NIL;
        }
        // §1.3 S14 (shadow slots only): the compiler baked the emit-point slot
        // of the binding visible `depth` scopes out (resolve_outer_var_slot).
        // With shadow slots each binding owns its slot, so the LIVE local value
        // IS the outer binding — no snapshot indexing, and no position search
        // (which picks the outermost same-named slot, wrong for depth < max).
        // Gated: the default build never bakes a meaningful slot for shadows
        // (declaration records are all `None`), and stays on the paths below.
        if crate::compiler::shadow_slots_active()
            && let Some(b) = slot
            && let Some(val) = self.locals.get(b as usize)
        {
            return val.clone();
        }
        let stack_len = self.outer_scope_locals.len();
        // Inline nested-block path: `outer_scope_locals` (runtime-saved slots)
        // reflects the lexical structure of nested bare blocks executed in place,
        // so it distinguishes multiple same-named `my $a` bindings by depth.
        //
        // Whether the target scope actually DECLARES `name` is not decided here:
        // `OUTER::` names exactly one scope (packages.rakudoc), and that is a
        // lexical, compile-time question, so `Compiler::emit_outer_var_access`
        // settles it and emits a Nil constant instead of this opcode when the
        // target scope does not declare the name. Re-deciding it here is not
        // possible anyway: this stack is only reset by `run()`, not by the fast
        // call paths, so inside a stored closure it still holds the CALLER's
        // blocks -- dynamic, not lexical. By the time a `GetOuterVar` executes,
        // the name is either declared in the target scope or the target lies
        // outside this frame (a separate compilation), which is what the captured
        // env below resolves.
        if stack_len > 0 && depth <= stack_len {
            let idx = stack_len - depth;
            let saved = &self.outer_scope_locals[idx];
            // Find the local slot for this variable name.
            for (slot, local_name) in code.locals.iter().enumerate() {
                if local_name == name && slot < saved.len() {
                    return saved[slot].clone();
                }
            }
        }
        // Stored-closure path: when a closure is invoked later (after its defining
        // block has exited), `outer_scope_locals` no longer holds that block's
        // scope — the closure's *lexical* outer scope was captured into its `env`
        // at creation time (the flattened enclosing scope). OUTER:: is a lexical
        // construct, so resolve the name against the captured env rather than the
        // dynamic runtime stack. The flat capture holds every enclosing lexical,
        // so a depth>1 access reaches transitive outers too.
        //
        // Prefer the reserved `__mutsu_outer::<name>` snapshot: the running frame
        // may have overwritten the plain name (e.g. a bare `sub {...}` establishes
        // a fresh topic `$_ = Any`), but the snapshot preserves the captured
        // enclosing value that `$OUTER::` must see.
        if let Some(val) = self.env().get(&format!("__mutsu_outer::{name}")) {
            return val.clone();
        }
        if let Some(val) = self.env().get(name) {
            return val.clone();
        }
        Value::NIL
    }
}
