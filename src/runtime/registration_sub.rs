use super::*;
use crate::symbol::Symbol;
use std::cell::RefCell;
use std::collections::HashSet;

/// Outcome of a `register_sub_decl` call, distinguishing a genuine
/// (re-)installation from an idempotent no-op re-registration of an already
/// present, structurally identical declaration. The VM uses this to decide
/// whether the resolution caches must be invalidated: an `Unchanged` outcome
/// means the registry is exactly as it was, so the caches stay valid.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SubRegisterOutcome {
    /// The declaration was (re-)derived and installed; resolution state changed.
    Installed,
    /// An identical declaration was already installed under this key; nothing
    /// was derived or installed beyond refreshing the routine's callable id.
    Unchanged,
}

thread_local! {
    /// Stack (for nested EVALs) of the `&name` code-variable keys that already
    /// existed in the enclosing scope when an `EVAL` began. A sub declared inside
    /// the EVAL may shadow one of these outer names (e.g.
    /// `my &f := EVAL 'sub f {...}'`, or a loop re-binding) without it counting as
    /// a redeclaration — but a name declared *inside* the same EVAL
    /// (`EVAL 'my &x; sub x {}'`) is NOT in this set and still conflicts.
    static EVAL_OUTER_AMP_NAMES: RefCell<Vec<HashSet<String>>> = const { RefCell::new(Vec::new()) };
}

/// Push the set of `&name` keys currently in `env` as the outer-amp snapshot for
/// an EVAL that is about to run.
pub(crate) fn push_eval_outer_amp_names(amp_names: impl Iterator<Item = String>) {
    let set: HashSet<String> = amp_names.collect();
    EVAL_OUTER_AMP_NAMES.with(|stack| stack.borrow_mut().push(set));
}

/// Pop the outer-amp snapshot when an EVAL finishes.
pub(crate) fn pop_eval_outer_amp_names() {
    EVAL_OUTER_AMP_NAMES.with(|stack| {
        stack.borrow_mut().pop();
    });
}

/// Whether `code_var_key` (`&name`) existed before the innermost active EVAL.
fn is_outer_amp_name(code_var_key: &str) -> bool {
    EVAL_OUTER_AMP_NAMES.with(|stack| {
        stack
            .borrow()
            .last()
            .is_some_and(|set| set.contains(code_var_key))
    })
}

/// Format a function body for comparison, stripping SetLine annotations.
/// Used to allow identical sub redeclarations that differ only in source line.
fn body_debug_without_setline(body: &[Stmt]) -> String {
    let filtered: Vec<_> = body
        .iter()
        .filter(|s| !matches!(s, Stmt::SetLine(_)))
        .collect();
    format!("{:?}", filtered)
}

/// Increment a string by one character (Raku's string increment for enums).
/// "x" -> "y", "z" -> "aa", "Z" -> "AA", etc.
fn string_increment(s: &str) -> String {
    if s.is_empty() {
        return String::new();
    }
    let mut chars: Vec<char> = s.chars().collect();
    if let Some(last) = chars.last_mut() {
        *last = char::from_u32(*last as u32 + 1).unwrap_or(*last);
    }
    chars.into_iter().collect()
}

impl Interpreter {
    /// Insert a multi-overload `def` into the function registry under `base_key`,
    /// falling back to the first free `base_key__m{N}` suffix when `base_key` is
    /// already taken. Uses a SINGLE write guard for all probes — the previous
    /// `match self.registry_mut().functions.entry(..) { Occupied => self.registry_mut()... }`
    /// shape acquired a second write lock inside the arm and deadlocked (the
    /// borrow checker cannot see it because each `registry_mut()` is a fresh guard).
    fn insert_multi_overload(&mut self, base_key: &str, def: FunctionDef) {
        let def = std::sync::Arc::new(def);
        let mut registry = self.registry_mut();
        let funcs = &mut registry.functions;
        if let std::collections::hash_map::Entry::Vacant(entry) =
            funcs.entry(Symbol::intern(base_key))
        {
            entry.insert(def);
            return;
        }
        let mut idx = 1usize;
        loop {
            let key = format!("{}__m{}", base_key, idx);
            if let std::collections::hash_map::Entry::Vacant(entry) =
                funcs.entry(Symbol::intern(&key))
            {
                entry.insert(def);
                return;
            }
            idx += 1;
        }
    }

    fn default_check_constraint_base(constraint: &str) -> &str {
        let mut end = constraint.len();
        for (idx, ch) in constraint.char_indices() {
            if ch == '[' || ch == '(' || ch == ':' {
                end = idx;
                break;
            }
        }
        &constraint[..end]
    }

    fn static_default_value_for_typecheck(&mut self, expr: &Expr) -> Option<Value> {
        match expr {
            Expr::Literal(v) => Some(v.clone()),
            Expr::AnonSub { .. } | Expr::AnonSubParams { .. } => {
                self.eval_block_value(&[Stmt::Expr(expr.clone())]).ok()
            }
            _ => None,
        }
    }

    /// Reject parameter type constraints that name an unknown type. To avoid
    /// false positives we only flag a *simple, undecorated* uppercase type name
    /// (no `[]`/`()`/smiley/`::`/`<`), which is the typo case rakudo reports as
    /// X::Parameter::InvalidType. Decorated forms (parametric, coercion,
    /// smileys, qualified, captures) are left alone. `declared_types` holds the
    /// type names declared anywhere in the current compilation unit (collected
    /// before execution, since subs are pre-registered before their declaring
    /// type executes), so backward/forward references within the unit are valid.
    pub(crate) fn validate_param_type_constraints(
        &self,
        param_defs: &[ParamDef],
        declared_types: &std::collections::HashSet<String>,
        declared_packages: &std::collections::HashSet<String>,
        declared_classes: &std::collections::HashSet<String>,
    ) -> Result<(), RuntimeError> {
        // Type-capture names declared in this signature (e.g. `::T`) are valid
        // type names for the rest of the signature.
        let captures: std::collections::HashSet<&str> = param_defs
            .iter()
            .filter_map(|pd| pd.type_constraint.as_deref())
            .filter_map(|tc| tc.strip_prefix("::"))
            .collect();
        for pd in param_defs {
            let Some(tc) = pd.type_constraint.as_deref() else {
                continue;
            };
            // A native `array[T]` may only be parameterized with a *native*
            // element type (`int`/`uint`/`num`/`str`, …). A boxed type such as
            // `array[Int]` cannot back a native array, so rakudo fails the
            // parameterization at compile time (X::Comp::BeginTime). The boxed
            // `Array[Int]` (capital A) is fine and not matched here.
            if let Some(inner) = tc.strip_prefix("array[").and_then(|r| r.strip_suffix(']')) {
                let inner = inner.trim();
                if !inner.is_empty()
                    && !crate::runtime::native_types::is_native_array_element_type(inner)
                {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert(
                        "message".to_string(),
                        Value::str(format!(
                            "An exception occurred while parameterizing array\nException details: Can only parameterize array with a native type, not {}",
                            inner
                        )),
                    );
                    return Err(RuntimeError::typed("X::Comp::BeginTime", attrs));
                }
            }
            // A parameterized form `Base[...]` (also how `Base of T` is stored,
            // including the type-only `(Base of T)` param named `__type_only__`)
            // on a non-parametric type (a plain class or a package/module) is
            // X::NotParametric. Built-in containers and roles are parametric and
            // not in these statically-collected sets. (Runtime `is_non_parametric_type`
            // can't be used here: this pre-pass runs before the type is registered.)
            // This is checked before the synthetic-param skip below because a
            // `(Base of T)` term param IS named `__type_only__`.
            if let Some(base) = tc.split_once('[').map(|(b, _)| b.trim())
                && !base.is_empty()
                && (declared_packages.contains(base) || declared_classes.contains(base))
            {
                let mut attrs = std::collections::HashMap::new();
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!("{} cannot be parameterized", base)),
                );
                attrs.insert(
                    "type".to_string(),
                    Value::Package(crate::symbol::Symbol::intern(base)),
                );
                return Err(RuntimeError::typed("X::NotParametric", attrs));
            }
            // A genuine literal-value param (`(42)`, `("foo")`) smartmatches a
            // value, so its "constraint" is not a type name and must not be
            // validated. A bare *type-only* param `(TypeName)` IS validated
            // below (an undeclared one is X::Parameter::InvalidType).
            if pd.name.starts_with("__") && pd.name != "__type_only__" {
                continue;
            }
            // Only a bare identifier (letters/digits/_/-) starting uppercase is
            // considered; anything decorated is skipped as potentially valid.
            if tc.is_empty()
                || !tc.starts_with(|c: char| c.is_ascii_uppercase())
                || !tc
                    .chars()
                    .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-')
            {
                continue;
            }
            // Bare value-params are not type names and must not be rejected:
            // uppercase value-terms (`Inf`, `NaN`, `True`, `False`) and built-in
            // enum values (`LittleEndian`, `Less`, ... — kept in the global base).
            if captures.contains(tc)
                || declared_types.contains(tc)
                || self.is_resolvable_type(tc)
                || self.has_type(tc)
                || matches!(tc, "Inf" | "NaN" | "True" | "False" | "Empty")
                || crate::env::global_base_contains(tc)
            {
                continue;
            }
            // A `package`/`module` named `tc` exists but is not type-like enough
            // to qualify a parameter: throw X::Parameter::BadType, not the generic
            // invalid-typename error. Packages are gathered statically because this
            // pre-pass runs before the package is registered at runtime.
            if declared_packages.contains(tc) || self.is_declared_package(tc) {
                let msg = format!(
                    "Package '{}' is insufficiently type-like to qualify a parameter.  Did you mean 'class'?",
                    tc
                );
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("type".to_string(), Value::str(tc.to_string()));
                attrs.insert("message".to_string(), Value::str(msg));
                return Err(RuntimeError::typed("X::Parameter::BadType", attrs));
            }
            let mut suggestions = self.suggest_type_names(tc);
            // Also suggest type/enum-value names declared in this same
            // compilation unit; they are not yet registered at this pre-pass
            // stage, so `suggest_type_names` (which reads the runtime registry)
            // cannot see them.
            let declared_vec: Vec<String> = declared_types.iter().cloned().collect();
            for s in Self::suggest_from_candidates(tc, &declared_vec) {
                if !suggestions.contains(&s) {
                    suggestions.push(s);
                }
            }
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("typename".to_string(), Value::str(tc.to_string()));
            attrs.insert(
                "suggestions".to_string(),
                Value::array(suggestions.iter().cloned().map(Value::str).collect()),
            );
            let mut message = format!("Invalid typename '{}' in parameter declaration.", tc);
            if suggestions.len() == 1 {
                message.push_str(&format!(" Did you mean '{}'?", suggestions[0]));
            } else if suggestions.len() > 1 {
                let quoted: Vec<String> = suggestions.iter().map(|s| format!("'{}'", s)).collect();
                message.push_str(&format!(
                    " Did you mean any of these: {}?",
                    quoted.join(", ")
                ));
            }
            attrs.insert("message".to_string(), Value::str(message));
            return Err(RuntimeError::typed("X::Parameter::InvalidType", attrs));
        }
        Ok(())
    }

    /// Reject a sub return type (`--> NoSuchType` / `returns NoSuchType`) that
    /// names a type unknown to this compilation unit -> X::Undeclared (with
    /// `what` => "Type", `symbol` => the bad name). Mirrors the parameter-type
    /// check but uses the X::Undeclared hierarchy, matching rakudo
    /// ("Type 'NoSuchType' is not declared").
    pub(crate) fn validate_return_type_constraint(
        &self,
        return_type: Option<&str>,
        param_defs: &[ParamDef],
        declared_types: &std::collections::HashSet<String>,
        via_trait: bool,
    ) -> Result<(), RuntimeError> {
        let Some(rt) = return_type else {
            return Ok(());
        };
        // Type-capture names declared in this signature (e.g. `::T`) are valid
        // return type names too (`sub f(::T $x --> T) {...}`).
        let captures: std::collections::HashSet<&str> = param_defs
            .iter()
            .filter_map(|pd| pd.type_constraint.as_deref())
            .filter_map(|tc| tc.strip_prefix("::"))
            .collect();
        // Only a bare identifier starting uppercase is considered; anything
        // decorated (`Positional[Int]`, `Int:D`, lowercase native, …) is skipped.
        if rt.is_empty()
            || !rt.starts_with(|c: char| c.is_ascii_uppercase())
            || !rt
                .chars()
                .all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '-')
        {
            return Ok(());
        }
        // Well-known core constant terms are valid as literal return types
        // (`--> Empty`, `--> True`); they are not type names so
        // `is_resolvable_type` does not cover them.
        if matches!(rt, "Empty" | "True" | "False" | "NaN" | "Inf") {
            return Ok(());
        }
        if captures.contains(rt)
            || declared_types.contains(rt)
            || self.is_resolvable_type(rt)
            || self.has_type(rt)
        {
            return Ok(());
        }
        let suggestions = self.suggest_type_names(rt);
        let mut message = format!("Type '{}' is not declared", rt);
        if suggestions.len() == 1 {
            message.push_str(&format!(". Did you mean '{}'?", suggestions[0]));
        } else if suggestions.len() > 1 {
            let quoted: Vec<String> = suggestions.iter().map(|s| format!("'{}'", s)).collect();
            message.push_str(&format!(
                ". Did you mean any of these: {}?",
                quoted.join(", ")
            ));
        }
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(message));
        attrs.insert(
            "suggestions".to_string(),
            Value::array(suggestions.iter().cloned().map(Value::str).collect()),
        );
        // A `returns`/`of` trait naming an undeclared type is X::InvalidType
        // (with `typename`); a `-->` signature arrow is X::Undeclared (with
        // `what` => "Type", `symbol`).
        if via_trait {
            attrs.insert("typename".to_string(), Value::str(rt.to_string()));
            Err(RuntimeError::typed("X::InvalidType", attrs))
        } else {
            attrs.insert("what".to_string(), Value::str("Type".to_string()));
            attrs.insert("symbol".to_string(), Value::str(rt.to_string()));
            Err(RuntimeError::typed("X::Undeclared", attrs))
        }
    }

    fn validate_static_default_typechecks(
        &mut self,
        param_defs: &[ParamDef],
    ) -> Result<(), RuntimeError> {
        for pd in param_defs {
            let Some(constraint) = &pd.type_constraint else {
                continue;
            };
            let base_constraint = Self::default_check_constraint_base(constraint);
            if !self.has_type(base_constraint) {
                continue;
            }
            let Some(default_expr) = &pd.default else {
                continue;
            };
            let Some(default_value) = self.static_default_value_for_typecheck(default_expr) else {
                continue;
            };
            if !self.type_matches_value(constraint, &default_value) {
                return Err(RuntimeError::new(format!(
                    "X::Parameter::Default::TypeCheck: Type check failed for default value of parameter '{}'; expected {}, got {}",
                    pd.name,
                    constraint,
                    super::value_type_name(&default_value)
                )));
            }
        }
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn register_sub_decl(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[ParamDef],
        return_type: Option<&String>,
        associativity: Option<&String>,
        body: &[Stmt],
        multi: bool,
        is_rw: bool,
        is_raw: bool,
        is_test_assertion: bool,
        supersede: bool,
        custom_traits: &[(String, Option<crate::ast::Expr>)],
    ) -> Result<SubRegisterOutcome, RuntimeError> {
        self.register_sub_decl_fp(
            name,
            params,
            param_defs,
            return_type,
            associativity,
            body,
            multi,
            is_rw,
            is_raw,
            is_test_assertion,
            supersede,
            custom_traits,
            None,
        )
    }

    /// Whether registering a sub named `name` would raise `X::Redeclaration`
    /// because a conflicting `&name` (e.g. an earlier `my &name`) is already
    /// bound in the lexical env. Mirrors the env-`&name` check in
    /// `register_sub_decl_fp`; the idempotent fast path consults it so it never
    /// short-circuits past a redeclaration error (the registry can hold an
    /// identical entry from a hoisted pass while a `my &name` declared afterward
    /// must still make the textual `sub name` a redeclaration).
    fn sub_decl_would_redeclare(&self, name: &str, is_lexical_hoist: bool) -> bool {
        let code_var_key = format!("&{}", name);
        let Some(existing) = self.env.get(&code_var_key) else {
            return false;
        };
        if matches!(existing, Value::Mixin(..)) {
            return false;
        }
        let allow_lexical_shadow = (self.block_scope_depth > 0 || is_lexical_hoist)
            && !matches!(self.env.get("__mutsu_in_eval"), Some(Value::Bool(true)))
            && !matches!(
                self.env.get("__mutsu_eval_wrapped_decls"),
                Some(Value::Bool(true))
            );
        if allow_lexical_shadow {
            return false;
        }
        let is_in_eval = matches!(self.env.get("__mutsu_in_eval"), Some(Value::Bool(true)));
        let shadows_outer_eval_name = is_in_eval && is_outer_amp_name(&code_var_key);
        !shadows_outer_eval_name
    }

    /// `register_sub_decl` with an optional compile-time declaration fingerprint
    /// (`site_fingerprint`) enabling the idempotent-reregistration fast path. The
    /// `register_sub_decl` wrapper passes `None` for call sites (EVAL, the
    /// interpreter run path) where the fast path does not apply.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn register_sub_decl_fp(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[ParamDef],
        return_type: Option<&String>,
        associativity: Option<&String>,
        body: &[Stmt],
        multi: bool,
        is_rw: bool,
        is_raw: bool,
        is_test_assertion: bool,
        supersede: bool,
        custom_traits: &[(String, Option<crate::ast::Expr>)],
        site_fingerprint: Option<u64>,
    ) -> Result<SubRegisterOutcome, RuntimeError> {
        let is_method_value_decl = custom_traits
            .iter()
            .any(|(t, _)| t == "__mutsu_method_decl");
        let allow_redeclare = supersede || is_method_value_decl;
        let is_our_scoped = custom_traits.iter().any(|(t, _)| t == "__our_scoped");
        let is_lexical_hoist = custom_traits.iter().any(|(t, _)| t == "__lexical_hoist");
        // Idempotent re-registration fast path. A `RegisterSub` re-executes every
        // time its enclosing frame runs (e.g. a `my sub` inside a hot routine),
        // but the declaration it installs is constant. When a structurally
        // identical single (non-multi) sub with no user-visible traits is already
        // installed under this key, there is nothing to re-derive: refresh the
        // routine's callable id and return `Unchanged` so the caller leaves the
        // resolution caches intact. The `contains_key` guard keeps this correct
        // across block-scope restoration (a `my sub` removed when its scope exits
        // misses here and takes the full path).
        if let Some(site_fp) = site_fingerprint
            && !multi
            && !is_method_value_decl
            && !custom_traits.iter().any(|(t, _)| !t.starts_with("__"))
            && !self.sub_decl_would_redeclare(name, is_lexical_hoist)
        {
            let fq = format!("{}::{}", self.current_package(), name);
            let fq_sym = Symbol::intern(&fq);
            if self.registered_fn_fingerprints.get(&fq_sym) == Some(&site_fp)
                && self.registry().functions.contains_key(&fq_sym)
            {
                let callable_key =
                    format!("__mutsu_callable_id::{}::{}", self.current_package(), name);
                self.env.insert(
                    callable_key,
                    Value::Int(crate::value::next_instance_id() as i64),
                );
                return Ok(SubRegisterOutcome::Unchanged);
            }
        }
        // Derive-once re-install fast path. A `my sub` is removed from the registry
        // when its routine returns (lexical-scope snapshot/restore) and re-installed
        // on the next call. The registry no longer has it (so the idempotent path
        // above missed), but we derived an identical `FunctionDef` on an earlier
        // call — reuse that cached `Arc` instead of re-running the full AST→def
        // derivation. Restricted to simple single subs (no multi/our/traits/assoc)
        // so the streamlined install below covers every side effect the full path
        // would apply for this shape.
        if let Some(site_fp) = site_fingerprint
            && !multi
            && !is_method_value_decl
            && !is_our_scoped
            && associativity.is_none()
            && !custom_traits.iter().any(|(t, _)| !t.starts_with("__"))
            && !self.sub_decl_would_redeclare(name, is_lexical_hoist)
        {
            let pkg = self.current_package().to_string();
            let fq = format!("{}::{}", pkg, name);
            let fq_sym = Symbol::intern(&fq);
            if !self.registry().functions.contains_key(&fq_sym)
                && let Some(cached) = self
                    .prepared_fn_defs
                    .get(&fq_sym)
                    .filter(|(fp, _)| *fp == site_fp)
                    .map(|(_, arc)| arc.clone())
            {
                self.registry_mut().functions.insert(fq_sym, cached);
                self.registered_fn_fingerprints.insert(fq_sym, site_fp);
                if pkg != "GLOBAL" {
                    self.mark_my_scoped_package_item(fq);
                }
                let callable_key = format!("__mutsu_callable_id::{}::{}", pkg, name);
                self.env.insert(
                    callable_key,
                    Value::Int(crate::value::next_instance_id() as i64),
                );
                return Ok(SubRegisterOutcome::Installed);
            }
        }
        Self::validate_callable_param_return_redeclaration(param_defs)?;
        if let Some(spec) = return_type
            && self.is_definite_return_spec(spec)
            && Self::body_contains_non_nil_return(body)
        {
            return Err(Self::malformed_return_value_compile_error(spec));
        }
        // Auto-detect @_ / %_ usage for subs without explicit signatures
        let (effective_param_defs, empty_sig) = if param_defs.is_empty() && params.is_empty() {
            let (use_positional, use_named) = Self::auto_signature_uses(body);
            let mut defs = Vec::new();
            if use_positional {
                defs.push(ParamDef {
                    name: "@_".to_string(),
                    default: None,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    slurpy: true,
                    double_slurpy: false,
                    onearg: false,
                    sigilless: false,
                    type_constraint: None,
                    literal_value: None,
                    sub_signature: None,
                    where_constraint: None,
                    traits: Vec::new(),
                    optional_marker: false,
                    outer_sub_signature: None,
                    code_signature: None,
                    is_invocant: false,
                    shape_constraints: None,
                });
            }
            if use_named {
                defs.push(ParamDef {
                    name: "%_".to_string(),
                    default: None,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    slurpy: true,
                    double_slurpy: false,
                    onearg: false,
                    sigilless: false,
                    type_constraint: None,
                    literal_value: None,
                    sub_signature: None,
                    where_constraint: None,
                    traits: Vec::new(),
                    optional_marker: false,
                    outer_sub_signature: None,
                    code_signature: None,
                    is_invocant: false,
                    shape_constraints: None,
                });
            }
            // If neither @_ nor %_ is used, this is a true empty signature
            let is_empty = defs.is_empty();
            (defs, is_empty)
        } else {
            (param_defs.to_vec(), false)
        };
        self.validate_static_default_typechecks(&effective_param_defs)?;
        let deprecated_message = custom_traits.iter().find_map(|(t, _)| {
            if t == "DEPRECATED" {
                Some(String::new())
            } else {
                t.strip_prefix("DEPRECATED:").map(|msg| msg.to_string())
            }
        });
        if multi {
            let single_key = format!("{}::{}", self.current_package(), name);
            if is_our_scoped && !self.registry().proto_subs.contains(&single_key) {
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("scope".to_string(), Value::str("our".to_string()));
                attrs.insert(
                    "message".to_string(),
                    Value::str(
                        "Cannot declare individual multi candidates in 'our' scope".to_string(),
                    ),
                );
                return Err(RuntimeError::typed("X::Declaration::Scope::Multi", attrs));
            }
        }
        let new_def = FunctionDef {
            package: Symbol::intern(&self.current_package()),
            name: Symbol::intern(name),
            params: params.to_vec(),
            param_defs: effective_param_defs,
            body: body.to_vec(),
            is_test_assertion,
            is_rw,
            is_raw,
            is_method: false,
            empty_sig,
            return_type: return_type.cloned(),
            is_default: custom_traits.iter().any(|(t, _)| t == "default"),
            deprecated_message,
        };
        let single_key = format!("{}::{}", self.current_package(), name);
        let multi_prefix = format!("{}::{}/", self.current_package(), name);
        let single_key_sym = Symbol::intern(&single_key);
        let has_single = self.registry().functions.contains_key(&single_key_sym);
        let has_multi = self
            .registry()
            .functions
            .keys()
            .any(|k| k.resolve().starts_with(&multi_prefix));
        let has_proto = self.registry().proto_subs.contains(&single_key);
        let allow_lexical_shadow = (self.block_scope_depth > 0 || is_lexical_hoist)
            && !matches!(self.env.get("__mutsu_in_eval"), Some(Value::Bool(true)))
            && !matches!(
                self.env.get("__mutsu_eval_wrapped_decls"),
                Some(Value::Bool(true))
            );
        let code_var_key = format!("&{}", name);
        // A sub declared inside `EVAL` is lexically scoped to that EVAL and its
        // registry is restored afterwards, so it may shadow an `&name` that
        // already existed in the enclosing scope (e.g. a loop's `my &name := EVAL
        // 'sub name {...}'`, where a previous iteration's binding or a forward
        // `my &name` placeholder still lingers in env). But a name declared
        // *inside* the same EVAL (`EVAL 'my &x; sub x {}'`) still conflicts.
        let is_in_eval = matches!(self.env.get("__mutsu_in_eval"), Some(Value::Bool(true)));
        let shadows_outer_eval_name = is_in_eval && is_outer_amp_name(&code_var_key);
        if let Some(existing) = self.env.get(&code_var_key) {
            // Mixin values in &name come from trait_mod and should not block registration.
            if !matches!(existing, Value::Mixin(..))
                && !shadows_outer_eval_name
                && !allow_lexical_shadow
                && !is_method_value_decl
            {
                return Err(RuntimeError::redeclaration_routine(name));
            }
        }
        let has_user_custom_traits = custom_traits.iter().any(|(t, _)| !t.starts_with("__"));
        // Clone the existing def out so the guard drops before mutating self.env
        // (registration cold path, so the FunctionDef clone is cheap enough).
        let existing = self.registry().functions.get(&single_key_sym).cloned();
        if let Some(existing) = existing {
            let same = existing.package == new_def.package
                && existing.name == new_def.name
                && existing.params == new_def.params
                && existing.return_type == new_def.return_type
                && format!("{:?}", existing.param_defs) == format!("{:?}", new_def.param_defs)
                && body_debug_without_setline(&existing.body)
                    == body_debug_without_setline(&new_def.body);
            if same && !has_user_custom_traits {
                let callable_key =
                    format!("__mutsu_callable_id::{}::{}", self.current_package(), name);
                self.env.insert(
                    callable_key,
                    Value::Int(crate::value::next_instance_id() as i64),
                );
                return Ok(SubRegisterOutcome::Unchanged);
            }
            // When re-registering a hoisted sub with custom traits, skip redeclaration
            // checks but don't return early — fall through to apply trait_mod:<is>.
            if same && has_user_custom_traits {
                // Fall through to trait dispatch below
            } else {
                let same_signature = existing.package == new_def.package
                    && existing.name == new_def.name
                    && existing.params == new_def.params
                    && format!("{:?}", existing.param_defs) == format!("{:?}", new_def.param_defs);
                if body.is_empty() && same_signature {
                    let callable_key =
                        format!("__mutsu_callable_id::{}::{}", self.current_package(), name);
                    self.env.insert(
                        callable_key,
                        Value::Int(crate::value::next_instance_id() as i64),
                    );
                    return Ok(SubRegisterOutcome::Unchanged);
                }
            }
        }
        let existing_is_stub = self
            .registry()
            .functions
            .get(&single_key_sym)
            .is_some_and(|existing| Self::is_stub_routine_body(&existing.body));
        if multi {
            if has_single
                && !has_proto
                && !allow_redeclare
                && !allow_lexical_shadow
                && !has_user_custom_traits
            {
                return Err(RuntimeError::redeclaration_routine(name));
            }
        } else if !allow_redeclare && !allow_lexical_shadow && !has_user_custom_traits {
            if has_multi && !has_proto {
                return Err(RuntimeError::redeclaration_routine(name));
            }
            if has_single && !existing_is_stub {
                return Err(RuntimeError::redeclaration_routine(name));
            }
        }
        let def = new_def;
        if !multi && allow_lexical_shadow && !is_our_scoped {
            let lexical_single = format!("{}::{}", self.current_package(), name);
            let lexical_multi_prefix = format!("{}::{}/", self.current_package(), name);
            self.registry_mut().functions.retain(|key, _| {
                let resolved = key.resolve();
                resolved != lexical_single && !resolved.starts_with(&lexical_multi_prefix)
            });
        }
        if let Some(assoc) = associativity {
            self.operator_assoc.insert(name.to_string(), assoc.clone());
            self.operator_assoc.insert(
                format!("{}::{}", self.current_package(), name),
                assoc.clone(),
            );
        }
        if multi {
            let arity = if def.param_defs.is_empty() && !params.is_empty() {
                // Auto-params ($^a, $^b): param_defs is empty but params
                // contains the placeholder variable names.
                params.len()
            } else {
                def.param_defs
                    .iter()
                    .filter(|p| {
                        !p.named
                            && (!p.slurpy || p.name == "_capture")
                            && !p.is_capture_subsignature()
                    })
                    .count()
            };
            let type_sig: Vec<&str> = def
                .param_defs
                .iter()
                .filter(|p| {
                    !p.named && (!p.slurpy || p.name == "_capture") && !p.is_capture_subsignature()
                })
                .map(|p| p.type_constraint.as_deref().unwrap_or("Any"))
                .collect();
            let has_types = type_sig.iter().any(|t| *t != "Any");
            if has_types {
                let typed_fq = format!(
                    "{}::{}/{}:{}",
                    self.current_package(),
                    name,
                    arity,
                    type_sig.join(",")
                );
                self.insert_multi_overload(&typed_fq, def.clone());
            }
            let fq = format!("{}::{}/{}", self.current_package(), name, arity);
            if !has_types || name == "trait_mod:<is>" {
                self.insert_multi_overload(&fq, def);
            } else {
                self.registry_mut()
                    .functions
                    .entry(Symbol::intern(&fq))
                    .or_insert(std::sync::Arc::new(def));
            }
        } else {
            let pkg = self.current_package().to_string();
            let fq = format!("{}::{}", pkg, name);
            let fq_sym = Symbol::intern(&fq);
            let arc = std::sync::Arc::new(def);
            // Record this declaration's fingerprint so a later re-execution of the
            // same `RegisterSub` site is recognized as an idempotent no-op. Reuse
            // the compile-time `site_fingerprint` rather than recomputing it here:
            // the recomputation would `Debug`-format the body on every install,
            // which (for a `my sub` whose lexical scope is snapshot/restored each
            // enclosing call) is exactly the per-call cost this is meant to avoid.
            if let Some(fp) = site_fingerprint {
                self.registered_fn_fingerprints.insert(fq_sym, fp);
                // Cache the derived definition so a later re-install of this exact
                // simple single sub reuses the `Arc` instead of re-deriving it (the
                // derive-once fast path above). Only cache the shapes that fast path
                // accepts, so a cache hit always lands on the streamlined install.
                if !is_our_scoped
                    && associativity.is_none()
                    && !custom_traits.iter().any(|(t, _)| !t.starts_with("__"))
                {
                    self.prepared_fn_defs.insert(fq_sym, (fp, arc.clone()));
                }
            } else {
                self.registered_fn_fingerprints.remove(&fq_sym);
            }
            self.registry_mut().functions.insert(fq_sym, arc);
        }
        // If this is an our-scoped sub, also store it in the persistent our_scoped_functions
        // so it survives block scope restoration.
        if is_our_scoped {
            let fq = format!("{}::{}", self.current_package(), name);
            // Share the `Arc` already held in `functions` (read->write on the
            // same lock would deadlock, so clone the handle out first).
            let f = self.registry().functions.get(&Symbol::intern(&fq)).cloned();
            if let Some(f) = f {
                self.registry_mut()
                    .our_scoped_functions
                    .insert(Symbol::intern(&fq), f);
            }
        }
        // If this is NOT our-scoped and we're inside a non-GLOBAL package,
        // mark it as my-scoped so it doesn't appear in the package stash.
        if !is_our_scoped && self.current_package() != "GLOBAL" {
            let fq = format!("{}::{}", self.current_package(), name);
            self.mark_my_scoped_package_item(fq);
        }
        let callable_key = format!("__mutsu_callable_id::{}::{}", self.current_package(), name);
        self.env.insert(
            callable_key,
            Value::Int(crate::value::next_instance_id() as i64),
        );
        if is_method_value_decl {
            let sub_val = Value::make_sub(
                Symbol::intern(&self.current_package()),
                Symbol::intern(name),
                params.to_vec(),
                param_defs.to_vec(),
                body.to_vec(),
                is_rw,
                self.env.clone(),
            );
            self.env.insert(format!("&{}", name), sub_val);
            self.env
                .insert(format!("__mutsu_method_value::{}", name), Value::Bool(true));
        }
        // Apply custom trait_mod:<is> for each non-builtin trait
        let has_trait_mod =
            self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>");
        {
            for (trait_name, trait_arg) in custom_traits.iter().filter(|(t, _)| {
                !t.starts_with("__") && t != "default" && !t.starts_with("DEPRECATED")
            }) {
                if !has_trait_mod {
                    // In EVAL context, report the error. Outside EVAL
                    // (e.g. module loading), silently skip unknown traits
                    // because the handler may not be visible yet.
                    let in_eval = self.env.get("__mutsu_in_eval").is_some_and(|v| v.truthy());
                    if in_eval {
                        return Err(RuntimeError::new(format!(
                            "Can't use unknown trait 'is' -> '{}' in sub declaration.",
                            trait_name
                        )));
                    }
                    continue;
                }
                let sub_val = Value::make_sub(
                    Symbol::intern(&self.current_package()),
                    Symbol::intern(name),
                    params.to_vec(),
                    param_defs.to_vec(),
                    body.to_vec(),
                    is_rw,
                    self.env.clone(),
                );
                // Evaluate the trait argument expression if present
                let trait_arg_val = if let Some(arg_expr) = trait_arg {
                    Some(self.eval_block_value(&[crate::ast::Stmt::Expr(arg_expr.clone())])?)
                } else {
                    None
                };
                // Try positional dispatch first: if the trait name is a known type/role,
                // pass the type object as a positional argument.
                let type_obj = self.resolve_type_object(trait_name);
                let mut args = vec![sub_val];
                // Set up writeback so `$r does Role` inside trait_mod propagates
                // the Mixin back to `&name` in the outer scope.
                self.trait_mod_writeback_key = Some(format!("&{}", name));
                let call_result = if let Some(type_val) = type_obj {
                    args.push(type_val);
                    if let Some(arg_val) = trait_arg_val {
                        args.push(arg_val);
                    }
                    self.call_function("trait_mod:<is>", args)
                } else {
                    let named_val = if let Some(arg_val) = trait_arg_val {
                        Value::Pair(trait_name.clone(), Box::new(arg_val))
                    } else {
                        Value::Pair(trait_name.clone(), Box::new(Value::Bool(true)))
                    };
                    args.push(named_val);
                    self.call_function("trait_mod:<is>", args)
                };
                self.trait_mod_writeback_key = None;
                // Check if the trait_mod returned a Mixin or if the
                // trait_mod modified the routine parameter via `$r does Role`.
                // The writeback mechanism captures the Mixin from DoesVar
                // inside the trait_mod and propagates it back to &name.
                let code_var_key = format!("&{}", name);
                if let Ok(ref result) = call_result
                    && matches!(result, Value::Mixin(..))
                {
                    self.env.insert(code_var_key, result.clone());
                } else if let Some(mixin_val) = self.trait_mod_writeback_value.take() {
                    self.env.insert(code_var_key, mixin_val);
                }
            }
        }
        Ok(SubRegisterOutcome::Installed)
    }

    /// Resolve a name to a type object (Package value) if the name refers to a known class or role.
    pub(crate) fn resolve_type_object(&self, name: &str) -> Option<Value> {
        let fq_name = format!("{}::{}", self.current_package(), name);
        if self.registry().classes.contains_key(name)
            || self.registry().classes.contains_key(fq_name.as_str())
            || self.registry().roles.contains_key(name)
            || self.registry().roles.contains_key(fq_name.as_str())
        {
            Some(Value::Package(Symbol::intern(name)))
        } else if let Some(val) = self.env.get(name) {
            // Also check env for type objects (e.g. lexical roles/classes)
            if matches!(val, Value::Package(_)) {
                Some(val.clone())
            } else {
                None
            }
        } else {
            None
        }
    }

    pub(crate) fn register_token_decl(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[ParamDef],
        body: &[Stmt],
        multi: bool,
    ) {
        let def = FunctionDef {
            package: Symbol::intern(&self.current_package()),
            name: Symbol::intern(name),
            params: params.to_vec(),
            param_defs: param_defs.to_vec(),
            body: body.to_vec(),
            is_test_assertion: false,
            is_rw: false,
            is_raw: false,
            is_method: false,
            empty_sig: false,
            return_type: None,
            is_default: false,
            deprecated_message: None,
        };
        self.insert_token_def(name, def, multi);
    }

    pub(crate) fn register_proto_decl(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[ParamDef],
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let key = format!("{}::{}", self.current_package(), name);
        if self
            .registry()
            .functions
            .contains_key(&Symbol::intern(&key))
        {
            return Err(RuntimeError::redeclaration_routine(name));
        }
        if self.registry().proto_subs.contains(&key) {
            return Err(RuntimeError::redeclaration_routine(name));
        }
        let prefix = format!("{key}/");
        self.registry_mut().functions.retain(|existing, _| {
            let resolved = existing.resolve();
            resolved != key && !resolved.starts_with(&prefix)
        });
        self.registry_mut().proto_subs.insert(key);
        let fq = format!("{}::{}", self.current_package(), name);
        self.registry_mut().proto_functions.insert(
            Symbol::intern(&fq),
            std::sync::Arc::new(FunctionDef {
                package: Symbol::intern(&self.current_package()),
                name: Symbol::intern(name),
                params: params.to_vec(),
                param_defs: param_defs.to_vec(),
                body: body.to_vec(),
                is_test_assertion: false,
                is_rw: false,
                is_raw: false,
                is_method: false,
                empty_sig: false,
                return_type: None,
                is_default: false,
                deprecated_message: None,
            }),
        );
        Ok(())
    }

    /// Register a sub under GLOBAL:: (used for `is export` trait).
    #[allow(dead_code)]
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn register_sub_decl_as_global(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[ParamDef],
        return_type: Option<&String>,
        associativity: Option<&String>,
        body: &[Stmt],
        multi: bool,
        is_rw: bool,
        is_raw: bool,
        is_test_assertion: bool,
        supersede: bool,
    ) -> Result<(), RuntimeError> {
        Self::validate_callable_param_return_redeclaration(param_defs)?;
        if let Some(spec) = return_type
            && self.is_definite_return_spec(spec)
            && Self::body_contains_non_nil_return(body)
        {
            return Err(Self::malformed_return_value_compile_error(spec));
        }
        // Auto-detect @_ / %_ usage for subs without explicit signatures
        let (effective_param_defs, empty_sig) = if param_defs.is_empty() && params.is_empty() {
            let (use_positional, use_named) = Self::auto_signature_uses(body);
            let mut defs = Vec::new();
            if use_positional {
                defs.push(ParamDef {
                    name: "@_".to_string(),
                    default: None,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    slurpy: true,
                    double_slurpy: false,
                    onearg: false,
                    sigilless: false,
                    type_constraint: None,
                    literal_value: None,
                    sub_signature: None,
                    where_constraint: None,
                    traits: Vec::new(),
                    optional_marker: false,
                    outer_sub_signature: None,
                    code_signature: None,
                    is_invocant: false,
                    shape_constraints: None,
                });
            }
            if use_named {
                defs.push(ParamDef {
                    name: "%_".to_string(),
                    default: None,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    slurpy: true,
                    double_slurpy: false,
                    onearg: false,
                    sigilless: false,
                    type_constraint: None,
                    literal_value: None,
                    sub_signature: None,
                    where_constraint: None,
                    traits: Vec::new(),
                    optional_marker: false,
                    outer_sub_signature: None,
                    code_signature: None,
                    is_invocant: false,
                    shape_constraints: None,
                });
            }
            let is_empty = defs.is_empty();
            (defs, is_empty)
        } else {
            (param_defs.to_vec(), false)
        };
        let def = FunctionDef {
            package: Symbol::intern("GLOBAL"),
            name: Symbol::intern(name),
            params: params.to_vec(),
            param_defs: effective_param_defs,
            body: body.to_vec(),
            is_test_assertion,
            is_rw,
            is_raw,
            is_method: false,
            empty_sig,
            return_type: return_type.cloned(),
            is_default: false,
            deprecated_message: None,
        };
        let single_key = format!("GLOBAL::{}", name);
        let single_key_sym = Symbol::intern(&single_key);
        let multi_prefix = format!("GLOBAL::{}/", name);
        let has_single = self.registry().functions.contains_key(&single_key_sym);
        let has_multi = self
            .registry()
            .functions
            .keys()
            .any(|k| k.resolve().starts_with(&multi_prefix));
        let has_proto = self.registry().proto_subs.contains(&single_key);
        if let Some(assoc) = associativity {
            self.operator_assoc.insert(name.to_string(), assoc.clone());
            self.operator_assoc
                .insert(format!("GLOBAL::{}", name), assoc.clone());
        }
        if let Some(existing) = self.registry().functions.get(&single_key_sym) {
            let same = existing.package == def.package
                && existing.name == def.name
                && existing.params == def.params
                && format!("{:?}", existing.param_defs) == format!("{:?}", def.param_defs)
                && body_debug_without_setline(&existing.body)
                    == body_debug_without_setline(&def.body);
            if same {
                return Ok(());
            }
            let same_signature = existing.package == def.package
                && existing.name == def.name
                && existing.params == def.params
                && format!("{:?}", existing.param_defs) == format!("{:?}", def.param_defs);
            if body.is_empty() && same_signature {
                return Ok(());
            }
        }
        let existing_is_stub = self
            .registry()
            .functions
            .get(&single_key_sym)
            .is_some_and(|existing| Self::is_stub_routine_body(&existing.body));
        if multi {
            if has_single && !has_proto && !supersede {
                return Err(RuntimeError::redeclaration_routine(name));
            }
            let arity = param_defs
                .iter()
                .filter(|p| {
                    !p.named && (!p.slurpy || p.name == "_capture") && !p.is_capture_subsignature()
                })
                .count();
            let type_sig: Vec<&str> = param_defs
                .iter()
                .filter(|p| {
                    !p.named && (!p.slurpy || p.name == "_capture") && !p.is_capture_subsignature()
                })
                .map(|p| p.type_constraint.as_deref().unwrap_or("Any"))
                .collect();
            let has_types = type_sig.iter().any(|t| *t != "Any");
            if has_types {
                let typed_fq = format!("GLOBAL::{}/{}:{}", name, arity, type_sig.join(","));
                self.registry_mut()
                    .functions
                    .insert(Symbol::intern(&typed_fq), std::sync::Arc::new(def.clone()));
            }
            let fq = format!("GLOBAL::{}/{}", name, arity);
            if !has_types {
                self.insert_multi_overload(&fq, def);
            } else {
                self.registry_mut()
                    .functions
                    .entry(Symbol::intern(&fq))
                    .or_insert(std::sync::Arc::new(def));
            }
        } else {
            if has_multi && !has_proto && !supersede {
                return Err(RuntimeError::redeclaration_routine(name));
            }
            if has_single && !supersede && !existing_is_stub {
                return Err(RuntimeError::redeclaration_routine(name));
            }
            let fq = format!("GLOBAL::{}", name);
            self.registry_mut()
                .functions
                .insert(Symbol::intern(&fq), std::sync::Arc::new(def));
        }
        let callable_key = format!("__mutsu_callable_id::GLOBAL::{}", name);
        self.env.insert(
            callable_key,
            Value::Int(crate::value::next_instance_id() as i64),
        );
        Ok(())
    }

    /// Register a proto sub under GLOBAL:: (used for `is export` trait).
    pub(crate) fn register_proto_decl_as_global(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[ParamDef],
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let key = format!("GLOBAL::{}", name);
        if self
            .registry()
            .functions
            .contains_key(&Symbol::intern(&key))
        {
            return Err(RuntimeError::redeclaration_routine(name));
        }
        if self.registry().proto_subs.contains(&key) {
            // A proto with this name is already visible in GLOBAL. This happens
            // when `is export` on a GLOBAL proto hits both local/global paths,
            // or when an outer (mainline) `proto sub` of the same name already
            // exists before a module declares its own exported proto. In both
            // cases the existing proto subsumes the exported one; the actual
            // candidates are merged later at `import` time. Re-registering would
            // be a spurious redeclaration error, so skip it.
            return Ok(());
        }
        self.registry_mut().proto_subs.insert(key.clone());
        self.registry_mut().proto_functions.insert(
            Symbol::intern(&key),
            std::sync::Arc::new(FunctionDef {
                package: Symbol::intern("GLOBAL"),
                name: Symbol::intern(name),
                params: params.to_vec(),
                param_defs: param_defs.to_vec(),
                body: body.to_vec(),
                is_test_assertion: false,
                is_rw: false,
                is_raw: false,
                is_method: false,
                empty_sig: false,
                return_type: None,
                is_default: false,
                deprecated_message: None,
            }),
        );
        Ok(())
    }

    pub(crate) fn register_proto_token_decl(&mut self, name: &str) {
        let key = format!("{}::{}", self.current_package(), name);
        self.registry_mut().proto_tokens.insert(key);
    }

    pub(crate) fn register_enum_decl(
        &mut self,
        name: &str,
        variants: &[(String, Option<Expr>)],
        is_export: bool,
        base_type: Option<&str>,
    ) -> Result<Value, RuntimeError> {
        // Handle dynamic enum body: `enum Stuff (@variable)`
        let expanded;
        let variants: &[(String, Option<Expr>)] =
            if variants.len() == 1 && variants[0].0 == "__DYNAMIC__" && variants[0].1.is_some() {
                let expr = variants[0].1.as_ref().unwrap();
                let v = self.eval_block_value(&[Stmt::Expr(expr.clone())])?;
                let raw_items: Vec<Value> = match &v {
                    Value::Array(items, _) => items.as_ref().clone().items,
                    Value::Seq(items) | Value::Slip(items) => items.as_ref().clone(),
                    Value::Hash(map) => map
                        .iter()
                        .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                        .collect(),
                    _ => vec![v.clone()],
                };
                // Flatten any Slips in the list
                let items: Vec<Value> = raw_items
                    .into_iter()
                    .flat_map(|item| match item {
                        Value::Slip(inner) => inner.as_ref().clone(),
                        other => vec![other],
                    })
                    .collect();
                expanded = items
                    .iter()
                    .map(|item| match item {
                        Value::Pair(k, v) => (k.clone(), Some(Expr::Literal(v.as_ref().clone()))),
                        Value::ValuePair(k, v) => {
                            (k.to_str_context(), Some(Expr::Literal(v.as_ref().clone())))
                        }
                        _ => (item.to_str_context(), None::<Expr>),
                    })
                    .collect::<Vec<_>>();
                &expanded[..]
            } else {
                variants
            };
        let mut enum_variants = Vec::new();
        let mut next_int_value: i64 = 0;
        let mut next_str_value: Option<String> = None;
        for (key, value_expr) in variants {
            let val = if let Some(expr) = value_expr {
                let v = self.eval_block_value(&[Stmt::Expr(expr.clone())])?;
                match v {
                    Value::Int(i) => {
                        next_str_value = None;
                        EnumValue::Int(i)
                    }
                    Value::Bool(b) => {
                        next_str_value = None;
                        EnumValue::Int(if b { 1 } else { 0 })
                    }
                    Value::Str(s) => {
                        let ev = EnumValue::Str(s.to_string());
                        // Set up string increment for next auto-value
                        next_str_value = Some(string_increment(&s));
                        ev
                    }
                    other => {
                        next_str_value = None;
                        EnumValue::Generic(Box::new(other))
                    }
                }
            } else if let Some(ref s) = next_str_value {
                let ev = EnumValue::Str(s.clone());
                next_str_value = Some(string_increment(s));
                ev
            } else {
                EnumValue::Int(next_int_value)
            };
            match &val {
                EnumValue::Int(i) => {
                    // Wrap instead of panicking when an enum's running counter
                    // passes i64::MAX (e.g. `enum E (A => 2**63-1, ...)`). The
                    // auto-increment past MAX is degenerate, but it must not abort
                    // the interpreter.
                    next_int_value = i.wrapping_add(1);
                }
                EnumValue::Str(_) | EnumValue::Generic(_) => {}
            }
            enum_variants.push((key.clone(), val));
        }
        // Validate that all enum values are of the same type (no mixed Int/Str)
        if enum_variants.len() > 1 {
            let has_int = enum_variants
                .iter()
                .any(|(_, v)| matches!(v, EnumValue::Int(_)));
            let has_str = enum_variants
                .iter()
                .any(|(_, v)| matches!(v, EnumValue::Str(_)));
            if has_int && has_str {
                return Err(RuntimeError::new(
                    "Incompatible MROs in P6opaque rebless for types Str",
                ));
            }
        }
        // Validate base type constraint if specified
        if let Some(bt) = base_type {
            for (key, val) in &enum_variants {
                let type_ok = match (bt, val) {
                    ("Int", EnumValue::Int(_)) => true,
                    ("Str", EnumValue::Str(_)) => true,
                    ("Array", EnumValue::Generic(v)) => matches!(v.as_ref(), Value::Array(..)),
                    ("Cool", _) => true, // Cool covers both Int and Str
                    _ => false,
                };
                if !type_ok {
                    return Err(RuntimeError::new(format!(
                        "Type check failed in assignment to enum value '{}'; expected {} but got {}",
                        key,
                        bt,
                        match val {
                            EnumValue::Int(_) => "Int",
                            EnumValue::Str(_) => "Str",
                            EnumValue::Generic(_) => "other",
                        }
                    )));
                }
            }
        }

        let is_anonymous = name.is_empty();
        let enum_type_name = if is_anonymous { "__ANON_ENUM__" } else { name };
        self.registry_mut()
            .enum_types
            .insert(enum_type_name.to_string(), enum_variants.clone());
        if !is_anonymous {
            self.env
                .insert(name.to_string(), Value::Package(Symbol::intern(name)));
            // Also register with fully-qualified package name
            if self.current_package() != "GLOBAL" {
                self.env.insert(
                    format!("{}::{}", self.current_package(), name),
                    Value::Package(Symbol::intern(name)),
                );
            }
        }
        for (index, (key, val)) in enum_variants.iter().enumerate() {
            let enum_val = Value::Enum {
                enum_type: Symbol::intern(enum_type_name),
                key: Symbol::intern(key),
                value: val.clone(),
                index,
            };
            if !is_anonymous {
                self.env
                    .insert(format!("{}::{}", name, key), enum_val.clone());
                // Also register with fully-qualified package name
                if self.current_package() != "GLOBAL" {
                    self.env.insert(
                        format!("{}::{}::{}", self.current_package(), name, key),
                        enum_val.clone(),
                    );
                }
            }
            // Also register bare variant with package prefix for import lookup
            if self.current_package() != "GLOBAL" {
                self.env.insert(
                    format!("{}::{}", self.current_package(), key),
                    enum_val.clone(),
                );
            }
            // Track for poisoned alias detection before inserting
            if !is_anonymous {
                self.register_enum_bare_name(key, enum_type_name);
            }
            self.env.insert(key.clone(), enum_val);
        }
        // Register exports if `is export`
        if is_export && !is_anonymous {
            let pkg = self.current_package();
            for (key, _) in &enum_variants {
                self.register_exported_var(pkg.clone(), key.clone(), vec!["DEFAULT".to_string()]);
            }
        }

        // For anonymous enums, return a Map (Hash) of key => value pairs
        if is_anonymous {
            let mut map = HashMap::new();
            for (key, val) in &enum_variants {
                map.insert(key.clone(), val.to_value());
            }
            Ok(Value::hash(map))
        } else {
            Ok(Value::Nil)
        }
    }
}
