use super::*;
use crate::ast::{HandleSpec, ParamDef};
use crate::symbol::Symbol;

pub(super) type ResolvedRoleCandidate = (RoleDef, Vec<String>, Vec<Value>);

/// Whether a built-in type is a concrete (non-composable) class that cannot be
/// `does`-composed by a class — `class B does Int {}` is
/// X::Composition::NotComposable. This is a denylist of the concrete value/object
/// types; everything else built-in that is `does`-able is a role
/// (Real/Numeric/Positional/Baggy/Setty/Stringy/...) and composes fine. A
/// denylist is used (rather than an allowlist of composable roles) so new or
/// uncommon built-in roles like Baggy/Setty/Mixy are not wrongly rejected.
pub(super) fn is_non_composable_builtin(name: &str) -> bool {
    matches!(
        name,
        "Int" | "UInt" | "Str" | "Num" | "Rat" | "FatRat" | "Complex" | "Cool" | "Any" | "Mu"
    )
}

pub(super) fn type_value_name(value: &Value) -> String {
    match value.view() {
        ValueView::Package(name) => name.resolve(),
        ValueView::ParametricRole {
            base_name,
            type_args,
        } => format!(
            "{}[{}]",
            base_name.resolve(),
            type_args
                .iter()
                .map(type_value_name)
                .collect::<Vec<_>>()
                .join(",")
        ),
        _ => value
            .to_string_value()
            .trim_start_matches('(')
            .trim_end_matches(')')
            .to_string(),
    }
}

pub(super) fn builtin_role_def() -> RoleDef {
    RoleDef {
        attributes: Vec::new(),
        methods: HashMap::new(),
        is_stub_role: false,
        is_hidden: false,
        is_rw: false,
        captured_env: None,
        wildcard_handles: Vec::new(),
        role_id: 0,
        attribute_conflicts: Vec::new(),
        own_attribute_names: HashSet::new(),
        deferred_body: Vec::new(),
        deferred_custom_traits: Vec::new(),
    }
}

/// Intermediate representation for resolved handle specs.
pub(crate) enum ResolvedHandle {
    /// Forward `exposed` method to `target` method on the object in `attr_var`.
    Method {
        exposed: String,
        target: String,
        attr_var: String,
    },
    /// Regex-based delegation: forward methods matching `pattern`.
    Regex { attr_var: String, pattern: String },
    /// Wildcard delegation: forward all unknown methods.
    WildcardHandle(String),
}

/// Apply resolved handles to methods map and wildcard handles.
pub(super) fn apply_resolved_handles(
    handles: &[ResolvedHandle],
    methods: &mut HashMap<String, Vec<MethodDef>>,
    wildcard_handles: &mut Vec<String>,
) {
    for handle in handles {
        match handle {
            ResolvedHandle::Method {
                exposed,
                target,
                attr_var,
            } => {
                methods
                    .entry(exposed.clone())
                    .or_default()
                    .push(make_delegation_method(attr_var, target));
            }
            ResolvedHandle::Regex { attr_var, pattern } => {
                wildcard_handles.push(format!("{}:regex:{}", attr_var, pattern));
            }
            ResolvedHandle::WildcardHandle(attr_var) => {
                wildcard_handles.push(attr_var.clone());
            }
        }
    }
}

/// Create the slurpy `*@_` parameter used by delegation forwarding methods.
/// This ensures the method matches any number of positional/named arguments.
fn delegation_slurpy_param() -> ParamDef {
    ParamDef {
        name: "@_".to_string(),
        default: None,
        multi_invocant: false,
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
        block_param: false,
    }
}

/// Create a delegation MethodDef that forwards method calls to `target_method`
/// on the object in `attr_var_name`.
pub(super) fn make_delegation_method(attr_var_name: &str, target_method: &str) -> MethodDef {
    MethodDef {
        lexical_package: "GLOBAL".to_string(),
        params: vec!["@_".to_string(), "%_".to_string()],
        param_defs: vec![delegation_slurpy_param(), delegation_double_slurpy_param()],
        body: std::sync::Arc::new(Vec::new()),
        is_rw: false,
        is_private: false,
        is_multi: false,
        is_my: false,
        role_origin: None,
        original_role: None,
        return_type: None,
        compiled_code: None,
        compiled_fns: None,
        delegation: Some((attr_var_name.to_string(), target_method.to_string())),
        is_default: false,
        deprecated_message: None,
        is_submethod: false,
        captured_env: None,
        source_file: None,
        role_param_bindings: None,
    }
}

/// Create the double-slurpy `**@_` parameter for named arg forwarding.
fn delegation_double_slurpy_param() -> ParamDef {
    ParamDef {
        name: "%_".to_string(),
        default: None,
        multi_invocant: false,
        required: false,
        named: false,
        slurpy: true,
        double_slurpy: true,
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
        block_param: false,
    }
}

pub(crate) struct ClassDeclModifiers<'a> {
    pub(crate) class_is_rw: bool,
    pub(crate) is_hidden: bool,
    pub(crate) is_lexical: bool,
    pub(crate) hidden_parents: &'a [String],
    pub(crate) does_parents: &'a [String],
    /// Language version of the class being declared (e.g. "6.c", "6.d", "6.e").
    /// Used to determine whether submethods from composed roles should be included.
    pub(crate) language_version: &'a str,
    /// Whether the declared body is a yada stub (`...`, `!!!`, or `???`).
    /// Precomputed by the compiler at plan lowering (ADR-0019 D1), so
    /// registration never re-walks the body to judge this.
    pub(crate) is_stub: bool,
    /// `trusts SomeClass` declarations at the top level of the body,
    /// precomputed by the compiler at plan lowering (ADR-0019 D1) instead of
    /// `publish_class_shell` scanning the body for `Stmt::TrustsDecl` at
    /// registration time.
    pub(crate) trusts: &'a [Symbol],
    /// Attribute names the class declares directly in its own body,
    /// precomputed by the compiler at plan lowering (ADR-0019 D2a) instead
    /// of `run_class_body` re-scanning the body for `Stmt::HasDecl` at
    /// registration time.
    pub(crate) own_attribute_names: &'a [Symbol],
    /// Precompiled typed descriptor for each of the class's own attributes
    /// (ADR-0019 D2b remainder), keyed by attribute name, threaded down to
    /// `class_body_has_decl` via `ClassBodyCx`. Empty for registration paths
    /// with no compiled plan available (role bodies, `augment class`) —
    /// those keep building a descriptor from the raw AST statement.
    pub(crate) attr_decls: &'a [(Symbol, crate::opcode::CompiledAttrDecl)],
    /// Precompiled runtime-resolved-name chunk for each top-level `method`/
    /// `submethod` declaration in the body (ADR-0019 D3-1), read by position
    /// in `class_body_method_decl` via `ClassBodyCx`. Empty for registration
    /// paths with no compiled plan available (`augment class`, and the
    /// role-pun/mixin synthesis paths that call this with an empty body).
    pub(crate) method_name_chunks: &'a [Option<crate::opcode::CompiledDeclExpr>],
    /// Precompiled typed mirror of each top-level `method`/`submethod`
    /// declaration in the body (ADR-0019 D3-7), position-aligned with
    /// `method_name_chunks`, threaded down to `class_body_method_decl` via
    /// `ClassBodyCx`. Empty for registration paths with no compiled plan
    /// available (the role-pun/mixin synthesis paths that call this with an
    /// empty body).
    pub(crate) method_decls: &'a [crate::opcode::CompiledMethodDecl],
    /// Names the class body `my`/`state`-declares at its own top level
    /// (ADR-0019 D6-1), precomputed by the compiler at plan lowering instead
    /// of `persist_class_body_statics` re-walking the raw body at
    /// registration time. Empty for registration paths with no compiled plan
    /// available (role-pun/mixin synthesis, `augment class`).
    pub(crate) declared_static_names: &'a [Symbol],
    /// Precompiled declaration-trait-arg chunks for each `is`/`does`/`hides`
    /// parent's bracket arguments (ADR-0019 D4-2/D4-3), position-aligned
    /// with the `parents` argument to `register_class_decl` (looked up by
    /// the plan's original parent string before any lexical/sibling
    /// remapping, then carried through the same filter that can drop a
    /// parent — see the call site). `None` at a position means either the
    /// parent had no bracket arguments or its bracket content did not parse
    /// as a clean expression list (D4-1) — `compose_class_parent_roles`
    /// falls back to re-parsing the concatenated parent string in both
    /// cases. Empty for registration paths with no compiled plan available
    /// (role-pun/mixin synthesis, `augment class`).
    pub(crate) parent_pre_args: &'a [Option<&'a [crate::opcode::DeclTraitArg]>],
    /// The ambient program-wide compiled-function pool (ADR-0019 D3-8b),
    /// threaded down to `class_body_method_decl` via `ClassBodyCx` so it can
    /// look up each `method_decls[i].compiled_routine_key` and install the
    /// main-pass-compiled bytecode (ADR-0019 D3-8a,
    /// `Compiler::compile_method_body`) directly, instead of leaving
    /// `MethodDef::compiled_code` `None` for the registration-time
    /// throwaway-compile fallback (`compile_method_def_in_place_with_dist`)
    /// to fill in later. Call sites with no compiled plan available
    /// (role-pun/mixin synthesis, `augment class`) pass an empty table —
    /// harmless, since their `method_decls` is empty too, so the lookup is
    /// never reached.
    pub(crate) compiled_fns: &'a crate::opcode::CompiledFns,
    /// Precomputed, position-aligned, typed mirror of the body statement
    /// walk (ADR-0019 D6-3a-c), threaded down to `run_class_body` so its
    /// small statement arms can run a precompiled chunk instead of
    /// on-the-fly compiling the raw statement (D6-3d/e). Empty for
    /// registration paths with no compiled plan available (role-pun/mixin
    /// synthesis, `augment class`) — those keep the on-the-fly
    /// `run_block_raw` path unconditionally, same as an empty body.
    pub(crate) body_plan: &'a [crate::opcode::ClassBodyOp],
    /// True for a `__hoisted` forward-reference shell's throwaway
    /// registration (`hoist_type_decl_shells`): the shell composes roles
    /// too (so a forward reference sees role-provided methods), but its
    /// registration is superseded at runtime by the real, source-position
    /// declaration re-registering later. A role's deferred body must
    /// therefore run again at that real registration even though the shell
    /// already ran it once for the same (class, role) pair — the
    /// class/role composition memo (`Registry::composed_role_bodies`) is
    /// exempted for a hoisted-shell pass so it does not "use up" the one
    /// real run the shell's own (usually-discarded) execution shouldn't
    /// count as. `t/run-nested-role-body.t`'s `$side = @outer.elems * 100`
    /// caught a regression here: memoising the shell's run left the real
    /// declaration's run skipped, so `$side` never got set.
    pub(crate) is_hoisted_shell: bool,
}

pub(super) fn parse_role_type_args(input: &str) -> Vec<String> {
    split_balanced_comma_list(input)
}

pub(super) fn looks_like_type_arg_expr(input: &str) -> bool {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return false;
    }
    trimmed.chars().all(|ch| {
        ch.is_ascii_alphanumeric()
            || matches!(
                ch,
                ':' | '?' | '_' | '[' | ']' | '(' | ')' | ',' | ' ' | '\t'
            )
    })
}

pub(super) fn should_treat_role_arg_as_type_expr(input: &str) -> bool {
    let trimmed = input.trim();
    // Colonpair syntax like `:a(1)` or `:foo(42)` is a named argument, not a type.
    if trimmed.starts_with(':')
        && trimmed
            .chars()
            .nth(1)
            .is_some_and(|c| c.is_ascii_lowercase())
    {
        return false;
    }
    looks_like_type_arg_expr(trimmed)
        && (trimmed.contains(':') || trimmed.contains('(') || trimmed.contains("::"))
}

/// Substitute type parameters in a method definition.
/// E.g., if type_subs = [("T", "Str:D(Numeric)")], then any param with
/// type_constraint "T" becomes "Str:D(Numeric)".
pub(super) fn substitute_type_params_in_method(
    method: &MethodDef,
    type_subs: &[(String, String)],
) -> MethodDef {
    fn replace_type_name(type_name: &str, type_subs: &[(String, String)]) -> String {
        for (param_name, replacement) in type_subs {
            if type_name == param_name {
                return replacement.clone();
            }
        }
        type_name.to_string()
    }

    fn substitute_param_def(pd: &ParamDef, type_subs: &[(String, String)]) -> ParamDef {
        let mut new_pd = pd.clone();
        if let Some(tc) = &new_pd.type_constraint {
            new_pd.type_constraint = Some(replace_type_name(tc, type_subs));
        }
        if let Some(sub) = &new_pd.sub_signature {
            new_pd.sub_signature = Some(
                sub.iter()
                    .map(|p| substitute_param_def(p, type_subs))
                    .collect(),
            );
        }
        if let Some(outer) = &new_pd.outer_sub_signature {
            new_pd.outer_sub_signature = Some(
                outer
                    .iter()
                    .map(|p| substitute_param_def(p, type_subs))
                    .collect(),
            );
        }
        if let Some((sig_params, sig_ret)) = &new_pd.code_signature {
            let next_params = sig_params
                .iter()
                .map(|p| substitute_param_def(p, type_subs))
                .collect();
            let next_ret = sig_ret.as_ref().map(|r| replace_type_name(r, type_subs));
            new_pd.code_signature = Some((next_params, next_ret));
        }
        new_pd
    }

    let new_param_defs = method
        .param_defs
        .iter()
        .map(|pd| substitute_param_def(pd, type_subs))
        .collect();
    MethodDef {
        lexical_package: method.lexical_package.clone(),
        params: method.params.clone(),
        param_defs: new_param_defs,
        body: method.body.clone(),
        is_rw: method.is_rw,
        is_private: method.is_private,
        is_multi: method.is_multi,
        is_my: method.is_my,
        role_origin: method.role_origin.clone(),
        original_role: method.original_role.clone(),
        return_type: method.return_type.clone(),
        compiled_code: method.compiled_code.clone(),
        compiled_fns: method.compiled_fns.clone(),
        delegation: method.delegation.clone(),
        is_default: method.is_default,
        deprecated_message: method.deprecated_message.clone(),
        is_submethod: method.is_submethod,
        // Carried over like every other field: a role declared inside a routine
        // records its methods' lexical captures on its own `MethodDef`s, and a
        // PARAMETERIZED role reaches its composing class through here rather
        // than through the plain `md.clone()` path. Dropping it made
        // `sub f { my $v = 8; role P[::T] { method go { $v } };
        // class H does P[Int] {}; H.new }` read `Nil`.
        captured_env: method.captured_env.clone(),
        source_file: method.source_file.clone(),
        role_param_bindings: method.role_param_bindings.clone(),
    }
}

/// Context threaded through the `$!attr` declaration validators so that an
/// undeclared private attribute can be reported as a fully-populated
/// `X::Attribute::Undeclared` (with `package-name`/`package-kind`).
pub(crate) struct AttrValidationCtx<'a> {
    pub(crate) attrs: &'a HashSet<String>,
    pub(crate) pkg_name: &'a str,
    pub(crate) pkg_kind: &'a str,
}

/// Extract the language revision letter from a version string like "6.c", "6.d", "6.e".
pub(super) fn language_revision_letter(version: &str) -> String {
    // Version is like "6.c", "6.d", "6.e" — extract the letter after the dot
    if let Some(letter) = version.strip_prefix("6.") {
        letter.chars().next().unwrap_or('c').to_string()
    } else {
        "c".to_string()
    }
}

impl Interpreter {
    /// Apply `handles` specifications to a class definition.
    /// For type-based handles, collects method names from the referenced type
    /// first, then applies them without holding borrows on self.
    pub(crate) fn apply_handle_specs(
        &mut self,
        class_name: &str,
        specs: &[HandleSpec],
        attr_var_name: &str,
        class_def: &mut ClassDef,
    ) {
        let resolved = self.resolve_handle_specs_to_names(specs, attr_var_name);
        // ADR-0019 F4c-9b: unlike `apply_handle_specs_to_role` below, the
        // class path writes methods straight to the registry -- there is no
        // `ClassDef::methods` for `apply_resolved_handles`' shared
        // implementation to target anymore (`RoleDef::methods` stays out of
        // scope for the registry index, see the F4c design note section
        // (1), so the role path keeps using that shared helper unchanged).
        let owner = Symbol::intern(class_name);
        let mut registry = self.registry_mut();
        for handle in &resolved {
            match handle {
                ResolvedHandle::Method {
                    exposed,
                    target,
                    attr_var,
                } => {
                    registry.push_user_method(
                        owner,
                        Symbol::intern(exposed),
                        make_delegation_method(attr_var, target),
                    );
                }
                ResolvedHandle::Regex { attr_var, pattern } => {
                    class_def
                        .wildcard_handles
                        .push(format!("{}:regex:{}", attr_var, pattern));
                }
                ResolvedHandle::WildcardHandle(attr_var) => {
                    class_def.wildcard_handles.push(attr_var.clone());
                }
            }
        }
    }

    /// Apply `handles` specifications to a role definition.
    pub(crate) fn apply_handle_specs_to_role(
        &self,
        specs: &[HandleSpec],
        attr_var_name: &str,
        role_def: &mut RoleDef,
    ) {
        let resolved = self.resolve_handle_specs_to_names(specs, attr_var_name);
        apply_resolved_handles(
            &resolved,
            &mut role_def.methods,
            &mut role_def.wildcard_handles,
        );
    }

    /// Resolve handle specs to concrete (exposed_name, target_method, attr_var_name) tuples
    /// or wildcard/regex entries. This step only reads from self (immutable borrow).
    pub(crate) fn resolve_handle_specs_to_names(
        &self,
        specs: &[HandleSpec],
        attr_var_name: &str,
    ) -> Vec<ResolvedHandle> {
        let mut result = Vec::new();
        for spec in specs {
            match spec {
                HandleSpec::Name(name) => {
                    // Check if the name refers to a known class or role (type delegation)
                    let type_methods = self.collect_type_method_names(name);
                    if !type_methods.is_empty() {
                        for method_name in type_methods {
                            result.push(ResolvedHandle::Method {
                                exposed: method_name.clone(),
                                target: method_name,
                                attr_var: attr_var_name.to_string(),
                            });
                        }
                    } else {
                        result.push(ResolvedHandle::Method {
                            exposed: name.clone(),
                            target: name.clone(),
                            attr_var: attr_var_name.to_string(),
                        });
                    }
                }
                HandleSpec::Rename { exposed, target } => {
                    result.push(ResolvedHandle::Method {
                        exposed: exposed.clone(),
                        target: target.clone(),
                        attr_var: attr_var_name.to_string(),
                    });
                }
                HandleSpec::Type(type_name) => {
                    for method_name in self.collect_type_method_names(type_name) {
                        result.push(ResolvedHandle::Method {
                            exposed: method_name.clone(),
                            target: method_name,
                            attr_var: attr_var_name.to_string(),
                        });
                    }
                }
                HandleSpec::Regex(pattern) => {
                    result.push(ResolvedHandle::Regex {
                        attr_var: attr_var_name.to_string(),
                        pattern: pattern.clone(),
                    });
                }
                HandleSpec::Wildcard => {
                    result.push(ResolvedHandle::WildcardHandle(attr_var_name.to_string()));
                }
            }
        }
        result
    }

    /// Collect method names from a class or role by name.
    fn collect_type_method_names(&self, type_name: &str) -> Vec<String> {
        let mut names = Vec::new();
        let registry = self.registry();
        if registry.classes.contains_key(type_name) {
            // ADR-0019 F4c-1: enumerate via the canonical reverse index
            // instead of `class_def.methods.keys()` (zero-mismatch
            // shadow-checked across the full local `t/` suite before this
            // cutover). The existence check stays on `classes` -- an empty
            // `owner_method_names(type_name)` cannot distinguish "no class of
            // this name" from "a class with zero declared methods", and only
            // the former should fall through to the role branch below.
            names.extend(
                registry
                    .owner_method_names(type_name)
                    .iter()
                    .map(Symbol::resolve),
            );
        } else if let Some(role_def) = registry.roles.get(type_name) {
            names.extend(role_def.methods.keys().cloned());
            // Also include methods from composed roles
            if let Some(composed) = self.registry().class_composed_roles.get(type_name) {
                for role_name in composed {
                    if let Some(rd) = self.registry().roles.get(role_name) {
                        for key in rd.methods.keys() {
                            if !names.contains(key) {
                                names.push(key.clone());
                            }
                        }
                    }
                }
            }
        }
        names
    }

    /// Qualify a bare inheritance-parent name with the current package when a
    /// same-named sibling class/role is declared there. Inside
    /// `module M { class X is Exception {}; class X::Decode is X {} }`, the parent
    /// `X` collides with the built-in `X::` exception namespace; without this the
    /// MRO links `X::Decode` to the built-in namespace (an unknown parent, so it
    /// falls back to `Any`) instead of the module-local `M::X`.
    ///
    /// Only rewrites a name (no type args) when `{current_package}::{name}` names
    /// a registered class/role. A module-local class lexically shadows any
    /// same-named outer or built-in type, so the package-qualified sibling is
    /// preferred even when a bare built-in of that name also exists (e.g. `X`,
    /// which is registered as the built-in `X::` exception namespace class);
    /// genuine built-in and cross-package parents, which have no
    /// current-package-qualified sibling, are left untouched.
    ///
    /// A *nested* parent name is qualified the same way — the third link of
    /// `class X {}; class X::Decode is X {}; class X::Decode::Length is X::Decode {}`
    /// inside a module has to reach `M::X::Decode`, not a bare `X::Decode` that
    /// nothing declared.
    pub(crate) fn qualify_sibling_parent_name(&self, parent: &str) -> String {
        if parent.contains('[') {
            return parent.to_string();
        }
        // The `Grammar` metatype, when it appears as an inheritance parent, is the
        // implicit default parent the parser auto-adds to every grammar. In Raku a
        // grammar with no explicit `is` clause always inherits the *core* `Grammar`
        // Cursor, even inside a `module M` that declares its own `grammar Grammar`
        // (which qualifies to `M::Grammar`). Without this guard a sibling grammar
        // like `grammar Schema::JSON` in the same module would qualify its default
        // `Grammar` parent to `M::Grammar` and wrongly inherit that user grammar's
        // tokens/actions/`parse` override (the YAMLish `Schema::*` reduce bug).
        // Direct references (`Grammar.parse`) still resolve to the module-local
        // grammar via bare-word resolution, so the module-local shadow is intact.
        if parent == "Grammar" {
            return parent.to_string();
        }
        let pkg = self.current_package();
        if pkg.is_empty() || pkg == "GLOBAL" {
            return parent.to_string();
        }
        let qualified = format!("{}::{}", pkg, parent);
        let registered = {
            let reg = self.registry();
            reg.classes.contains_key(&qualified) || reg.roles.contains_key(&qualified)
        };
        if registered {
            qualified
        } else {
            parent.to_string()
        }
    }

    pub(crate) fn resolve_declared_type_name(&self, name: &str) -> String {
        let (base, suffix) = if let Some(bracket) = name.find('[') {
            (&name[..bracket], &name[bracket..])
        } else {
            (name, "")
        };
        let lookup = base.strip_prefix("::").unwrap_or(base);
        // Well-known builtin parent types should not be resolved to a
        // package-scoped variant (e.g. "Grammar" → "HTTP::Parser::Grammar").
        if !lookup.contains("::")
            && matches!(
                lookup,
                "Any"
                    | "Cool"
                    | "Mu"
                    | "Grammar"
                    | "Match"
                    | "Int"
                    | "Str"
                    | "Num"
                    | "Rat"
                    | "Bool"
                    | "IO"
                    | "Exception"
                    | "Stash"
            )
        {
            return format!("{}{}", lookup, suffix);
        }
        if let ValueView::Package(pkg) = self.resolve_indirect_type_name(lookup).view() {
            return format!("{}{}", pkg.resolve(), suffix);
        }
        if let Some(ValueView::Package(pkg)) = self.env.get(lookup).map(Value::view) {
            return format!("{}{}", pkg.resolve(), suffix);
        }
        // Fallback: when a compile-time pre-qualified name like `M::C1` cannot
        // be resolved (e.g. because `C1` lives outside module `M`), try the
        // bare suffix (`C1`).  This handles cross-package parents in classes
        // declared inside a `unit module`/`unit class` body.
        if lookup.contains("::") {
            let bare = lookup.rsplit_once("::").map(|(_, b)| b).unwrap_or(lookup);
            // Single guard for all four lookups (avoids stacking read guards).
            let needs_bare = {
                let registry = self.registry();
                !registry.classes.contains_key(lookup)
                    && !registry.roles.contains_key(lookup)
                    && (registry.classes.contains_key(bare) || registry.roles.contains_key(bare))
            };
            if needs_bare {
                return format!("{}{}", bare, suffix);
            }
            if let ValueView::Package(pkg) = self.resolve_indirect_type_name(bare).view() {
                let resolved = pkg.resolve();
                if self.registry().classes.contains_key(resolved.as_str())
                    || self.registry().roles.contains_key(resolved.as_str())
                {
                    return format!("{}{}", resolved, suffix);
                }
            }
        }
        name.to_string()
    }
}
