//! Symbolic stash member lookup and package/indirect-type-name resolution.
use super::*;
use crate::value::ValueMap;
use crate::value::ValueView;
use crate::value::types::is_stash_class_name;

impl Interpreter {
    pub(super) fn stash_symbol_key_from_env_tail(rest: &str) -> String {
        if rest.starts_with('$')
            || rest.starts_with('@')
            || rest.starts_with('%')
            || rest.starts_with('&')
        {
            return rest.to_string();
        }
        if rest.contains("::") || rest.chars().next().is_some_and(|c| c.is_ascii_uppercase()) {
            return rest.to_string();
        }
        format!("${rest}")
    }

    /// Whether `name` is a package that actually holds something — i.e. some
    /// symbol is stored under `name::`. This is what makes an *implicitly*
    /// created package findable: `my $foo::bar = 1` declares no package
    /// anywhere, it just stores the env key `foo::bar`, and `foo` is a package
    /// precisely because that key exists.
    pub(crate) fn package_namespace_exists(&self, name: &str) -> bool {
        if name.is_empty() {
            return false;
        }
        let prefix = format!("{name}::");
        self.env.keys().any(|k| k.resolve().starts_with(&prefix))
            || self
                .registry()
                .classes
                .keys()
                .any(|k| k.starts_with(&prefix))
            || self
                .registry()
                .functions
                .keys()
                .any(|k| k.resolve().starts_with(&prefix))
    }

    /// Whether an env-key tail component carries a variable sigil, in which
    /// case it names a variable rather than a package component.
    pub(super) fn env_tail_has_sigil(component: &str) -> bool {
        component.starts_with('$')
            || component.starts_with('@')
            || component.starts_with('%')
            || component.starts_with('&')
    }

    /// Whether a single-segment (no `::`), package-qualification-stripped env
    /// key names a genuine member of the GLOBAL/root package stash, as
    /// opposed to a dynamic variable (`*CWD`), compile-time magical
    /// (`?FILE`), POD marker (`=pod`), internal bookkeeping key, or a `my`
    /// lexical that merely happens to live in the same flat env store.
    /// Sigiled array/hash/sub/scalar keys (`@arr`, `%h`, `&f`, `$x`) and
    /// uppercase bare names (types, constants, enum values -- always visible
    /// from the enclosing package in Raku) are kept; a lowercase bare name is
    /// a `my` lexical (genuine `our` scalars are covered separately, via the
    /// dedicated `our_vars` loop in `package_stash_value`).
    pub(super) fn is_global_root_symbol(rest: &str) -> bool {
        if rest.starts_with("__mutsu_") {
            return false;
        }
        // A dynamic var / compile-time magical is mirrored into the env
        // store both bare (`*CWD`) and pre-sigiled (`$*CWD`) -- see
        // `io_env.rs`'s `$*ARGFILES` / `*ARGFILES` pair -- so the twigil
        // check must look past a single leading sigil either way.
        let after_sigil = rest.strip_prefix(['$', '@', '%', '&']).unwrap_or(rest);
        if after_sigil.starts_with('*')
            || after_sigil.starts_with('?')
            || after_sigil.starts_with('!')
            || after_sigil.starts_with('=')
        {
            return false;
        }
        if rest.starts_with('@')
            || rest.starts_with('%')
            || rest.starts_with('&')
            || rest.starts_with('$')
        {
            return true;
        }
        // An uppercase bare name is a type, constant, or enum-member --
        // legitimately in the env store directly (not via the classes
        // registry, so `user_declared_classes` below cannot vet it). But a
        // *well-known built-in* type name (e.g. runtime init's internal
        // `env.insert("Any", ...)` sentinel) is not a user symbol just
        // because it happens to be mirrored into env; only a name the
        // interpreter does not already recognize as a core type is a
        // genuine root-package member.
        rest.chars().next().is_some_and(|c| c.is_ascii_uppercase())
            && !crate::runtime::utils::is_known_type_constraint(rest)
    }

    /// The stash key for a sigil-leading env / `our` spelling of a member of
    /// `package` -- `&Foo::bar` (how `Foo::<&bar> := ...`, `BIND-KEY` and
    /// `our &Foo::bar = ...` store a routine) is `Foo::`'s `&bar`.
    /// [`Self::stash_member_tail`] only matches the package-leading form, so
    /// such a member was callable but missing from the stash.
    pub(super) fn sigil_leading_stash_member(key: &str, package: &str) -> Option<String> {
        let sigil = key
            .chars()
            .next()
            .filter(|c| matches!(c, '&' | '@' | '%'))?;
        let rest = &key[1..];
        let bare = Self::stash_member_tail(rest, package)?;
        // GLOBAL's "tail" is the whole key (it has no prefix to strip); a root
        // routine is not spelled this way, so only a real strip counts.
        if bare.is_empty()
            || bare.len() == rest.len()
            || crate::qualified::is_qualified(Symbol::intern(bare))
        {
            return None;
        }
        Some(format!("{sigil}{bare}"))
    }

    pub(super) fn stash_member_tail<'a>(key: &'a str, package: &str) -> Option<&'a str> {
        let package = package.trim_end_matches("::");
        if package == "GLOBAL" {
            return Some(key);
        }
        let direct = format!("{package}::");
        if let Some(rest) = key.strip_prefix(&direct) {
            return Some(rest);
        }
        let needle = format!("::{package}::");
        if let Some(idx) = key.rfind(&needle) {
            let start = idx + needle.len();
            return Some(&key[start..]);
        }
        None
    }

    /// Attribute a pseudo-stash carries to remember the package of the frame it
    /// was taken from. It is an *attribute*, not a member of the stash's
    /// `symbols` hash, so it stays invisible to `.keys`/`.gist`; only
    /// `EVAL $code, context => $stash` reads it back (`eval_context_package`).
    pub(crate) const STASH_ORIGIN_PACKAGE_ATTR: &str = "__mutsu_origin_package";

    /// Attribute a pseudo-stash carries to remember the *control-flow*
    /// identity (`package::name`) of the routine that dynamically encloses the
    /// frame the stash was taken from — ADR-0037 §2.2. Same invisibility
    /// convention as `STASH_ORIGIN_PACKAGE_ATTR`: an attribute, not a
    /// `symbols` member, so `.keys`/`.gist` never see it. Absent (not
    /// inserted at all) when the captured frame is a mainline, which
    /// `eval_context_routine` treats identically to "key not found".
    pub(crate) const STASH_ORIGIN_ROUTINE_ATTR: &str = "__mutsu_origin_routine";

    /// Attribute a pseudo-stash carries to remember the *compilation unit* of
    /// the frame it was taken from (#7837). Same invisibility convention as
    /// `STASH_ORIGIN_PACKAGE_ATTR`. `EVAL ..., context => $stash` reads it back
    /// (`eval_context_unit`) and parents the EVAL unit on it, so the snippet
    /// inherits the *caller's* `use` grants instead of those of the module that
    /// called `EVAL` — which is what makes the vendored `Test.rakumod`'s string
    /// `throws-like` see the symbols the test file imported.
    pub(crate) const STASH_ORIGIN_UNIT_ATTR: &str = "__mutsu_origin_unit";

    /// Attribute carried by a `CALLER::...::` stash to identify the live caller
    /// frame whose lexical pad it reflects.  The visible `symbols` hash is a
    /// snapshot; mutating operations such as `BIND-KEY` need this depth to reach
    /// the frame's authoritative saved environment instead.
    pub(crate) const STASH_CALLER_DEPTH_ATTR: &str = "__mutsu_caller_depth";

    /// Stamp `origin` onto a pseudo-stash value. `CALLER::` names the frame that
    /// was current *where the stash was taken*, which is not recoverable later:
    /// `Test.rakumod` writes `my $ctx = CALLER::` in `throws-like` and uses it
    /// several frames deeper, inside a `subtest { ... }` block, so reading the
    /// routine stack at EVAL time would pick a different frame.
    pub(crate) fn stamp_stash_origin_package(stash: &Value, origin: &str) {
        if let ValueView::Instance { attributes, .. } = stash.view() {
            attributes.insert(
                Self::STASH_ORIGIN_PACKAGE_ATTR.to_string(),
                Value::str(origin.to_string()),
            );
        }
    }

    /// Stamp the frame's control-flow identity (see
    /// `caller_frame_enclosing_routine`) onto a pseudo-stash value, beside the
    /// package `stamp_stash_origin_package` already records. `None` (a
    /// mainline frame) stamps nothing, matching how `eval_context_routine`
    /// reads a missing attribute back as "no enclosing routine".
    pub(crate) fn stamp_stash_origin_routine(stash: &Value, origin: Option<&str>) {
        let Some(origin) = origin else {
            return;
        };
        if let ValueView::Instance { attributes, .. } = stash.view() {
            attributes.insert(
                Self::STASH_ORIGIN_ROUTINE_ATTR.to_string(),
                Value::str(origin.to_string()),
            );
        }
    }

    /// Stamp the compunit of the frame the stash was taken from
    /// (`caller_frame_unit`), beside the package and routine identities above.
    pub(crate) fn stamp_stash_origin_unit(stash: &Value, unit: Symbol) {
        if let ValueView::Instance { attributes, .. } = stash.view() {
            attributes.insert(
                Self::STASH_ORIGIN_UNIT_ATTR.to_string(),
                Value::str(unit.resolve()),
            );
        }
    }

    /// The compilation unit an `EVAL ..., context => $ctx` should compile
    /// inside — the one `$ctx` was captured from (#7837) — or `None` when the
    /// context value says nothing about one (it is not a stamped pseudo-stash,
    /// e.g. `context => SomePackage`, which names a package but no frame and
    /// so leaves the ambient unit in place).
    pub(crate) fn eval_context_unit(ctx: &Value) -> Option<Symbol> {
        match ctx.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if is_stash_class_name(class_name.as_str()) => attributes
                .as_map()
                .get(Self::STASH_ORIGIN_UNIT_ATTR)
                .map(|v| Symbol::intern(&v.to_string_value())),
            _ => None,
        }
    }

    /// The package an `EVAL ..., context => $ctx` should compile in, or `None`
    /// when the context value says nothing about a package.
    pub(crate) fn eval_context_package(ctx: &Value) -> Option<String> {
        match ctx.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if is_stash_class_name(class_name.as_str()) => {
                let map = attributes.as_map();
                if let Some(origin) = map.get(Self::STASH_ORIGIN_PACKAGE_ATTR) {
                    return Some(origin.to_string_value());
                }
                // A stash for a real package (`Foo::`) compiles in that package.
                let name = map.get("name")?.to_string_value();
                let name = Self::normalize_stash_package(&name);
                (!Self::is_pseudo_package_name(&name)).then_some(name)
            }
            ValueView::Package(sym) => {
                let name = sym.resolve();
                (!Self::is_pseudo_package_name(&name)).then_some(name)
            }
            _ => None,
        }
    }

    /// The `package::name` of the routine an `EVAL ..., context => $ctx`'s
    /// `return` should be classified against (ADR-0037 §2.3), or `None` when
    /// the context says nothing about one — either it is not a stamped
    /// pseudo-stash at all, or `CALLER::` was captured from a mainline (no
    /// enclosing routine; see `stamp_stash_origin_routine`).
    pub(crate) fn eval_context_routine(ctx: &Value) -> Option<String> {
        match ctx.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if is_stash_class_name(class_name.as_str()) => {
                let map = attributes.as_map();
                map.get(Self::STASH_ORIGIN_ROUTINE_ATTR)
                    .map(|v| v.to_string_value())
            }
            _ => None,
        }
    }

    pub(crate) fn make_stash_instance(package: &str, symbols: ValueMap) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::str(package.to_string()));
        attrs.insert("symbols".to_string(), Value::hash(symbols));
        Value::make_instance(
            Symbol::intern(Self::stash_class_for_package(package)),
            attrs,
        )
    }

    /// The class a stash for `package` carries: `Stash` for a real package
    /// symbol table, `PseudoStash` for a pseudo-package view of a lexical pad.
    ///
    /// Raku keeps the two apart — `PseudoStash.^mro` is
    /// `(PseudoStash Map Cool Any Mu)`, so it is a *sibling* of `Stash`, not a
    /// subclass — and the distinction is what lets a future `BIND-KEY` bind
    /// into a pad rather than into a package. Measured against rakudo:
    /// `MY OUTER OUTERS LEXICAL DYNAMIC CALLER CALLERS CORE SETTING UNIT
    /// CLIENT` answer `PseudoStash`; `OUR`, `GLOBAL` and `PROCESS` are genuine
    /// package symbol tables and stay `Stash`.
    ///
    /// A repeated spelling (`CALLER::CALLER::`) is still a pseudo-stash, which
    /// is why every component has to be one; a qualified name whose head only
    /// looks pseudo (`CORE::Foo`) is a package.
    pub(crate) fn stash_class_for_package(package: &str) -> &'static str {
        let normalized = Self::normalize_stash_package(package);
        let is_pseudo = !normalized.is_empty()
            && normalized.split("::").all(|part| {
                Self::is_pseudo_package_name(part) && !matches!(part, "OUR" | "GLOBAL")
            });
        if is_pseudo { "PseudoStash" } else { "Stash" }
    }

    /// Parse a stash made exclusively from repeated `CALLER` components.
    /// `CALLER::` is depth one and `CALLER::CALLER::` is depth two.
    pub(crate) fn caller_stash_depth(name: &str) -> Option<usize> {
        let trimmed = name.trim_end_matches("::");
        let parts: Vec<&str> = trimmed.split("::").collect();
        (!parts.is_empty() && parts.iter().all(|part| *part == "CALLER")).then_some(parts.len())
    }

    /// Build a `Stash` view that retains the address of one caller frame for
    /// container operations without exposing the hidden pad through iteration.
    pub(crate) fn caller_stash_value(&self, name: &str, depth: usize) -> Value {
        // CALLER stash enumeration is intentionally empty in mutsu.  Existing
        // EVAL-context behavior depends on that reflection surface; the hidden
        // depth below is enough for addressed container operations.
        let stash = Self::make_stash_instance(name, ValueMap::default());
        if let ValueView::Instance { attributes, .. } = stash.view() {
            attributes.insert(
                Self::STASH_CALLER_DEPTH_ATTR.to_string(),
                Value::int(depth as i64),
            );
        }
        stash
    }

    /// Bind a key in a `Stash` to a new container.  Caller stashes replace the
    /// binding in the addressed lexical pad; package stashes install the symbol
    /// under its fully-qualified environment/`our` name.
    pub(crate) fn bind_stash_key(
        &mut self,
        code: &CompiledCode,
        stash: &Value,
        raw_key: &str,
        value: Value,
        source_name: Option<&str>,
    ) -> Result<Value, RuntimeError> {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = stash.view()
        else {
            return Err(RuntimeError::new("BIND-KEY requires a Stash invocant"));
        };
        if !is_stash_class_name(class_name.as_str()) {
            return Err(RuntimeError::new("BIND-KEY requires a Stash invocant"));
        }

        let attrs = attributes.as_map();
        let caller_depth = attrs
            .get(Self::STASH_CALLER_DEPTH_ATTR)
            .and_then(Value::as_int)
            .and_then(|depth| usize::try_from(depth).ok());
        let package = attrs
            .get("name")
            .map(Value::to_string_value)
            .unwrap_or_default();
        // `attributes.insert` below takes the write side of this same cell.
        // Release the metadata snapshot's read guard before any user-visible
        // binding work, both to avoid deferring the symbol-table update and to
        // keep arbitrary Proxy callbacks out of an outstanding attribute read.
        drop(attrs);
        // A variable argument contributes its container identity, not merely
        // its current value.  Proxy is already a container in its own right;
        // ordinary values are promoted to the shared cell used by `:=`.
        let binding = if let Some(source_name) = source_name {
            if matches!(value.view(), ValueView::Proxy { .. }) {
                value.clone()
            } else if let Some(existing) = self.env().get(source_name).cloned()
                && existing.is_container_ref()
            {
                existing
            } else {
                let cell = value.clone().into_container_ref();
                self.set_env_with_main_alias(source_name, cell.clone());
                self.update_local_if_exists(code, source_name, &cell);
                cell
            }
        } else {
            value.clone()
        };

        if let Some(depth) = caller_depth {
            // `caller_env_stack` deliberately omits light/inlined call paths,
            // while repeated CALLER components count semantic routine frames.
            // `routine_stack` retains those frames for backtraces, so validate
            // against it and let the runtime-name carrier below cross whatever
            // physical VM frames happen to exist.
            if depth == 0 || depth > self.routine_stack.len() {
                return Err(RuntimeError::new(
                    "Cannot bind through CALLER stash: frame is gone",
                ));
            }
            let name = raw_key.strip_prefix('$').unwrap_or(raw_key).to_string();
            // The key is known only at runtime, so carry both its value and its
            // pending slot refresh across every intervening frame exactly as
            // `$::($name) = value` does. Keeping it in the current env supplies
            // `propagate_pending_caller_writes` at each return boundary.
            self.env_mut().insert(name.clone(), binding.clone());
            self.record_runtime_name_write(&name);
        } else {
            let package = Self::normalize_stash_package(&package);
            let (sigil, bare) = match raw_key.chars().next() {
                Some(sigil @ ('$' | '@' | '%' | '&')) => (Some(sigil), &raw_key[1..]),
                _ => (None, raw_key),
            };
            let qualified = Self::qualify_stash_name(&package, bare);
            let env_name = match sigil {
                Some('$') | None => qualified,
                Some(sigil) => format!("{sigil}{qualified}"),
            };
            self.env_mut().insert(env_name.clone(), binding.clone());
            self.set_our_var(env_name, binding.clone());
        }

        // Keep the already-materialized stash coherent for an immediate read.
        let mut symbols = attributes
            .as_map()
            .get("symbols")
            .and_then(|symbols| match symbols.view() {
                ValueView::Hash(map) => Some((**map).clone()),
                _ => None,
            })
            .unwrap_or_default();
        symbols.insert(raw_key.to_string(), binding);
        attributes.insert("symbols".to_string(), Value::hash(symbols));
        Ok(value)
    }

    pub(super) fn package_export_tag_parts(package: &str) -> Option<(&str, &str)> {
        let (module, rest) = package.split_once("::EXPORT::")?;
        if module.is_empty() || rest.is_empty() || rest.contains("::") {
            return None;
        }
        Some((module, rest))
    }

    pub(super) fn package_export_module(package: &str) -> Option<&str> {
        package.strip_suffix("::EXPORT")
    }

    pub(super) fn qualify_stash_name(package: &str, symbol: &str) -> String {
        let package = package.trim_end_matches("::");
        if package.is_empty() || package == "GLOBAL" {
            symbol.to_string()
        } else {
            format!("{package}::{symbol}")
        }
    }

    pub(crate) fn normalize_stash_package(package: &str) -> String {
        let trimmed = package.trim_end_matches("::");
        if let Some(inner) = trimmed
            .strip_prefix("GLOBAL[")
            .and_then(|s| s.strip_suffix(']'))
        {
            inner.to_string()
        } else {
            trimmed.to_string()
        }
    }

    fn has_package_members(&self, package: &str) -> bool {
        let prefix = format!("{package}::");
        self.env.keys().any(|k| k.starts_with(&prefix))
            || self
                .registry()
                .functions
                .keys()
                .any(|k| k.resolve().starts_with(&prefix))
            || self
                .registry()
                .classes
                .keys()
                .any(|k| k.starts_with(&prefix))
            || self.exported_subs.contains_key(package)
            || self.exported_vars.contains_key(package)
    }

    pub(crate) fn stash_lookup_symbol(stash: &Value, key: &str) -> Option<Value> {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = stash.view()
        else {
            return None;
        };
        if !is_stash_class_name(class_name.as_str()) {
            return None;
        }
        let map = attributes.as_map();
        let ValueView::Hash(symbols) = map.get("symbols")?.view() else {
            return None;
        };
        if let Some(value) = symbols.get(key) {
            return Some(value.clone());
        }
        if !key.starts_with('$')
            && !key.starts_with('@')
            && !key.starts_with('%')
            && !key.starts_with('&')
        {
            let scalar = format!("${key}");
            if let Some(value) = symbols.get(&scalar) {
                return Some(value.clone());
            }
        }
        None
    }

    pub(crate) fn no_such_symbol_failure(name: &str) -> Value {
        let mut ex_attrs = HashMap::new();
        ex_attrs.insert(
            "message".to_string(),
            Value::str(format!("No such symbol '{name}'")),
        );
        // A failed symbolic lookup (`::('NoSuchName')`, `::('')`) is
        // X::NoSuchSymbol in Raku, not a bare X::AdHoc. `symbol` carries the name.
        ex_attrs.insert("symbol".to_string(), Value::str(name.to_string()));
        let exception = Value::make_instance(Symbol::intern("X::NoSuchSymbol"), ex_attrs);
        let mut failure_attrs = HashMap::new();
        failure_attrs.insert("exception".to_string(), exception);
        failure_attrs.insert("handled".to_string(), Value::FALSE);
        Value::make_instance(Symbol::intern("Failure"), failure_attrs)
    }

    pub(crate) fn resolve_indirect_type_name(&self, name: &str) -> Value {
        if name.is_empty() {
            return Self::no_such_symbol_failure(name);
        }
        // Symbols loaded via `$*REPO.need(...)` stay invisible to `::('Name')`
        // until they are merged into GLOBAL with `merge-symbols`.
        if self.cur_repo.pending_global_symbols.contains(name) {
            return Self::no_such_symbol_failure(name);
        }
        // A lexical type remains registered for escaped values, but its
        // source-facing qualified name is not a package symbol visible from an
        // unrelated compunit (#8120). Check this before the generic compound
        // type/package fallback below, which otherwise manufactures a Package
        // value for any known-looking qualified name.
        if self.is_my_scoped_type_name(name) && !self.my_scoped_type_visible_here(name) {
            return Self::no_such_symbol_failure(name);
        }
        // Pseudo-package names like MY, CORE, OUTER, CALLER, etc. should
        // resolve to Package values so that .WHO can produce the stash.
        if Self::is_pseudo_package_name(name) {
            return Value::package(Symbol::intern(name));
        }
        if let Some(code_name) = name.strip_prefix('&') {
            let val = self.resolve_code_var(code_name);
            // When the code variable is not found via ::('&name'), return a
            // Failure (like Raku's X::NoSuchSymbol) so that attempting to use
            // the result throws an exception.
            if val.is_nil() {
                return Self::no_such_symbol_failure(name);
            }
            return val;
        }
        // Scalars are stored without the `$` sigil in the env; strip it for lookup.
        if let Some(bare) = name.strip_prefix('$')
            && let Some(value) = self.env.get(bare)
            && !value.is_nil()
        {
            return value.clone();
        }
        // `::('s')` names the TERM `s`, not the scalar `$s`. An enum key is stored
        // in its own key namespace (#7914), so probe it before the plain `env` key
        // — which, being sigil-less, belongs to `$s`.
        if let Some(value) = self.enum_bare_value(name)
            && !value.is_nil()
        {
            return value.clone();
        }
        if let Some(value) = self.env.get(name)
            && !value.is_nil()
            // Skip `my`-scoped package items for indirect type lookup (::())
            // since they should not be visible outside their declaring scope.
            && (!self.is_my_scoped_package_item(name)
                && (!self.is_my_scoped_type_name(name) || self.my_scoped_type_visible_here(name)))
        {
            return value.clone();
        }
        // Fallback: check persistent `our`-scoped variables (constants, `our` decls)
        // which may have been removed from the lexical env by block-scope restoration.
        if let Some(bare) = name.strip_prefix('$')
            && let Some(value) = self.our_vars.get(bare)
            && !value.is_nil()
        {
            return value.clone();
        }
        if let Some(value) = self.our_vars.get(name)
            && !value.is_nil()
        {
            return value.clone();
        }
        // Look up well-known numerical constants
        match name {
            "e" | "\u{1D452}" => return Value::num(std::f64::consts::E),
            "pi" => return Value::num(std::f64::consts::PI),
            "tau" | "\u{03C4}" => return Value::num(std::f64::consts::TAU),
            _ => {}
        }
        if !self.method_class_stack.is_empty() && self.loaded_modules.contains(name) {
            return Value::package(Symbol::intern(name));
        }

        // Check if the name is already a known registered type before
        // splitting on "::". A registered class/role/enum lives in the
        // REGISTRY for the whole process, regardless of which call frame
        // declared it (#8683): `RegisterClass`/`RegisterRole`/
        // `register_enum_decl` only install a BAREWORD binding into the
        // currently executing lexical env tier, which is exactly what a sub
        // call frame discards on return -- even when the declaration was not
        // `my`-scoped and so should remain a visible package member
        // (`sub make-it { class Foo {...} }; make-it(); ::('Foo')` must find
        // `Foo` after `make-it()` returns). A `my`-scoped declaration is not
        // wrongly picked up here: a namespaced one is excluded by the
        // `is_my_scoped_type_name` guard above, and a bare one registers
        // under a call-frame-mangled storage key, so `has_class`/`is_role`
        // miss it by its unmangled source-facing name. `is_known_compound_type`
        // stays gated on "::" since it only ever recognizes compound names
        // (e.g. "IO::Path"). A type loaded by a nested `require` is excluded
        // too: `require` installs into the CURRENT LEXICAL SCOPE rather than
        // the enclosing package, so it must rely purely on the ordinary
        // frame-scoped `env` check above for its (correctly frame-lifetime-
        // bound) visibility -- see `require_loaded_type_names`'s doc comment
        // and `roast/S11-modules/require.t`'s `GlobalOuter.load` case.
        if !self.is_require_loaded_type_name(name)
            && ((name.contains("::") && crate::runtime::utils::is_known_compound_type(name))
                || self.has_class(name)
                || self.is_role(name)
                || (self.registry().enum_types.contains_key(name)
                    && !self.is_my_scoped_package_item(name)))
        {
            return Value::package(Symbol::intern(name));
        }

        let mut parts = name.split("::").filter(|part| !part.is_empty());
        let Some(first) = parts.next() else {
            return Self::no_such_symbol_failure(name);
        };

        let mut current = if let Some(value) = self.env.get(first)
            && !value.is_nil()
        {
            value.clone()
        } else if crate::runtime::utils::is_known_type_constraint(first)
            || (name.contains("::")
                && (self.has_package_members(first)
                    || self.has_class(first)
                    || self.is_role(first)))
        {
            Value::package(Symbol::intern(first))
        } else {
            return Self::no_such_symbol_failure(name);
        };

        for part in parts {
            current = match current.view() {
                ValueView::Package(package) => {
                    let stash = self.package_stash_value(&package.resolve());
                    if let Some(value) = Self::stash_lookup_symbol(&stash, part) {
                        value
                    } else {
                        return Self::no_such_symbol_failure(name);
                    }
                }
                ValueView::Instance { class_name, .. }
                    if is_stash_class_name(class_name.as_str()) =>
                {
                    if let Some(value) = Self::stash_lookup_symbol(&current, part) {
                        value
                    } else {
                        return Self::no_such_symbol_failure(name);
                    }
                }
                _ => return Self::no_such_symbol_failure(name),
            };
        }

        current
    }
}
