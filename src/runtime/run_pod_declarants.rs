//! The declarator half of `$=pod`: each `#|` / `#=` block's concrete
//! `WHEREFORE`, built from the unit's parsed declarations.

use super::*;
use crate::ast::Stmt;
use crate::value::ValueMap;

impl Interpreter {
    /// Install the unit's declarator documentation `docs` and append the
    /// `Pod::Block::Declarator` half of `$=pod`, giving every entry the
    /// concrete routine, method, attribute or parameter object its
    /// declaration describes instead of a `Sub`/`Method`/`Attribute` type
    /// placeholder. Each such object is also recorded in `why_object_cache`,
    /// so `$=pod[$i].WHEREFORE.WHY` resolves back to that exact declarator
    /// block -- which is what `Pod::To::Man`'s `declarator2man` path tests
    /// before it renders a method, attribute or subroutine.
    pub(crate) fn add_pod_declarator_entries_from_stmts(
        &mut self,
        stmts: &[Stmt],
        docs: Vec<DocComment>,
    ) {
        self.install_doc_comments(docs);
        let mut declarants = ValueMap::default();
        // Only a declarator block reads a declarant: a unit without one needs
        // none, and building them clones every routine body.
        if !self.declarator_docs.doc_comment_list.is_empty() {
            let mut scan = PodDeclarants {
                package: "GLOBAL".to_string(),
                out: &mut declarants,
                multi_counters: HashMap::new(),
            };
            crate::ast_visit::walk_stmts(&mut scan, stmts);
        }
        self.add_declarator_pod_entries(&declarants);
    }

    /// Carry a declaration's `--> T` / `returns T` constraint onto the
    /// declarant value `collect_pod_declarants` synthesizes, under the same
    /// `__mutsu_return_type` key a registered routine uses -- that key is what
    /// `callable_return_type` (and therefore `.returns` / `.of`) reads, and
    /// `Pod::To::Text`'s `signature2text` renders the `--> Bool` line from it.
    fn record_declarant_return_type(env: &mut crate::env::Env, return_type: Option<&str>) {
        if let Some(rt) = return_type {
            env.insert("__mutsu_return_type".to_string(), Value::str_from(rt));
        }
    }

    /// File a declarant under its plain key, and -- for a `multi` candidate --
    /// under the `<key>/multi.<n>` form the parser's doc table
    /// (`parser::decl_doc`) uses to keep one candidate's `#|` comment from
    /// overwriting its siblings'. The counter map spans the whole compilation
    /// unit, exactly as the parser's does, so the two agree on which
    /// candidate is which.
    fn insert_pod_declarant(
        out: &mut ValueMap,
        multi_counters: &mut HashMap<String, usize>,
        key: String,
        is_multi: bool,
        value: Value,
    ) {
        if is_multi {
            let counter = multi_counters.entry(key.clone()).or_insert(0);
            let multi_key = format!("{key}/multi.{counter}");
            *counter += 1;
            out.insert(multi_key, value.clone());
        }
        out.insert(key, value);
    }

    /// File one concrete `Parameter` per declared parameter, under the
    /// `<routine key>::<param name>` key the parser's doc table scopes a `#=`
    /// parameter comment by. Without these, a documented parameter's `$=pod`
    /// entry falls back to the bare `Parameter` type object and its `.WHY` is
    /// `Nil`.
    fn collect_pod_param_declarants(owner_key: &str, param_defs: &[ParamDef], out: &mut ValueMap) {
        for def in param_defs {
            if def.name.is_empty() || def.name == "self" || def.name == "%_" {
                continue;
            }
            let sig_param = crate::value::signature::param_def_to_sig_param(def);
            // The parser's doc table keys a parameter by its SIGILED name
            // (`$a`, or a bare `$`/`@`/`%` for an anonymous one), while
            // `ParamDef::name` drops the `$`. Rebuild the
            // source spelling from the `SigParam` so the two keys meet, and
            // join it to the owner through `qualified()` -- the memoizing
            // constructor `src/qualified.rs` exists to keep `Pkg::thing` from
            // being hand-rolled with `format!` (issue #8899).
            let sigiled = format!("{}{}", sig_param.sigil, sig_param.name);
            let key = crate::qualified::qualified(
                crate::symbol::Symbol::intern(owner_key),
                crate::symbol::Symbol::intern(&sigiled),
            );
            out.insert(
                key.as_str().to_string(),
                crate::value::signature::make_parameter_value_for_owner(
                    &sig_param, owner_key, None,
                ),
            );
        }
    }

    /// File the declarant `stmt` itself declares, if it is a documented kind
    /// of declaration (a sub, a method, an attribute) made in `package`.
    /// [`PodDeclarants`] calls it for every statement of the unit.
    fn record_pod_declarant(
        stmt: &Stmt,
        package: &str,
        out: &mut ValueMap,
        multi_counters: &mut HashMap<String, usize>,
    ) {
        match stmt {
            Stmt::SubDecl {
                name,
                params,
                param_defs,
                body,
                is_rw,
                return_type,
                multi,
                ..
            } => {
                let mut sub_env = crate::env::Env::new();
                Self::record_declarant_return_type(&mut sub_env, return_type.as_deref());
                let key = format!("&{}", name.resolve());
                Self::collect_pod_param_declarants(&key, param_defs, out);
                Self::insert_pod_declarant(
                    out,
                    multi_counters,
                    key,
                    *multi,
                    Value::make_sub(
                        crate::symbol::Symbol::intern(package),
                        *name,
                        params.clone(),
                        param_defs.clone(),
                        body.clone(),
                        *is_rw,
                        sub_env,
                    ),
                );
            }
            Stmt::MethodDecl {
                name,
                params,
                param_defs,
                body,
                is_rw,
                is_submethod,
                return_type,
                multi,
                ..
            } => {
                let mut method_env = crate::env::Env::new();
                method_env.insert(
                    "__mutsu_callable_type".to_string(),
                    Value::str_from(if *is_submethod { "Submethod" } else { "Method" }),
                );
                Self::record_declarant_return_type(&mut method_env, return_type.as_deref());
                let mut method_params = vec!["self".to_string()];
                method_params.extend(params.iter().filter(|p| p.as_str() != "self").cloned());
                if !method_params.iter().any(|p| p == "%_") {
                    method_params.push("%_".to_string());
                }
                let mut method_param_defs = vec![crate::ast::ParamDef {
                    type_capture: None,
                    name: "self".to_string(),
                    default: None,
                    multi_invocant: true,
                    required: false,
                    named: false,
                    named_alias: false,
                    slurpy: false,
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
                    is_invocant: true,
                    shape_constraints: None,
                    block_param: false,
                    code: Default::default(),
                    trait_args: Vec::new(),
                }];
                method_param_defs.extend(
                    param_defs
                        .iter()
                        .filter(|p| p.name.as_str() != "self")
                        .cloned(),
                );
                if !method_param_defs.iter().any(|p| p.name == "%_") {
                    method_param_defs.push(crate::ast::ParamDef {
                        type_capture: None,
                        name: "%_".to_string(),
                        default: None,
                        multi_invocant: true,
                        required: false,
                        named: true,
                        named_alias: false,
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
                        code: Default::default(),
                        trait_args: Vec::new(),
                    });
                }
                let key = format!("{package}::{}", name.resolve());
                Self::collect_pod_param_declarants(&key, param_defs, out);
                Self::insert_pod_declarant(
                    out,
                    multi_counters,
                    key,
                    *multi,
                    Value::make_sub(
                        crate::symbol::Symbol::intern(package),
                        *name,
                        method_params,
                        method_param_defs,
                        body.clone(),
                        *is_rw,
                        method_env,
                    ),
                );
            }
            Stmt::HasDecl {
                name,
                sigil,
                type_constraint,
                ..
            } => {
                let bare = name.resolve();
                let full_name = format!("{sigil}!{bare}");
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("name".to_string(), Value::str(full_name.clone()));
                attrs.insert("__mutsu_attr_name".to_string(), Value::str(bare));
                attrs.insert(
                    "__mutsu_attr_owner".to_string(),
                    Value::str(package.to_string()),
                );
                attrs.insert(
                    "package".to_string(),
                    Value::package(crate::symbol::Symbol::intern(package)),
                );
                attrs.insert(
                    "type".to_string(),
                    Value::package(crate::symbol::Symbol::intern(
                        type_constraint.as_deref().unwrap_or("Mu"),
                    )),
                );
                // The same identity `.^attributes` gives this attribute
                // (#10004), so `$=pod[$i].WHEREFORE === Foo.^attributes[0]`.
                let identity = super::attribute_identity::AttributeIdentity {
                    owner: crate::symbol::Symbol::intern(package),
                    sigil: *sigil,
                    name: *name,
                };
                out.insert(
                    format!("{package}::{full_name}"),
                    super::attribute_identity::attribute_meta_object(identity, attrs),
                );
            }
            _ => {}
        }
    }
}

/// Collects the declarant of every documented-kind declaration in a unit
/// (ADR-0137 visitor): nested ones too -- a `sub` declared in a block or in
/// another routine carries a `#|` block just as well, and the parser's doc
/// table numbers `multi` candidates across the whole unit, nested ones
/// included, so skipping them would shift every later candidate's key.
struct PodDeclarants<'a> {
    /// The package the current statement declares into.
    package: String,
    out: &'a mut ValueMap,
    multi_counters: HashMap<String, usize>,
}

impl<'ast> crate::ast_visit::Visit<'ast> for PodDeclarants<'_> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        Interpreter::record_pod_declarant(stmt, &self.package, self.out, &mut self.multi_counters);
        match stmt {
            Stmt::ClassDecl { name, .. }
            | Stmt::RoleDecl { name, .. }
            | Stmt::Package { name, .. } => {
                let declared = name.resolve();
                let nested = if declared.contains("::") || self.package == "GLOBAL" {
                    declared
                } else {
                    crate::qualified::qualified(crate::symbol::Symbol::intern(&self.package), *name)
                        .as_str()
                        .to_string()
                };
                let outer = std::mem::replace(&mut self.package, nested);
                crate::ast_visit::walk_stmt(self, stmt);
                self.package = outer;
            }
            _ => crate::ast_visit::walk_stmt(self, stmt),
        }
    }
}
