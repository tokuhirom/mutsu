//! In-place role mixin: Raku's `does` retags *the object*, not one variable.
//!
//! Rakudo implements `$obj does R` by creating the type `C+{R}` — a class that
//! inherits from `C` and composes `R` — and reblessing the object into it. Every
//! reference to that object then sees the mixin, which is exactly what
//! distinguishes `does` (mutating) from `but` (copying).
//!
//! mutsu's older mechanism wrapped the value in a `ValueRepr::Mixin`, which
//! cannot be shared by the object's other aliases. This module implements the
//! Rakudo mechanism for `Instance` values, whose type tag lives in the shared
//! `InstanceAttrs` node and so *can* be mutated in place. Non-instance values
//! (an `Int`, a `Str`) have no such node and keep the wrapper.

use super::*;
use crate::runtime::registration_class::ClassDeclModifiers;
use crate::symbol::Symbol;
use crate::value::ValueView;

impl Interpreter {
    /// Ensure the mixin type `base+{roles}` is registered and return its name.
    /// `base` may itself already be a mixin type, in which case the new roles are
    /// appended to the ones it already composes (`C+{A}` + `B` -> `C+{A,B}`).
    pub(crate) fn ensure_mixin_class(
        &mut self,
        base_class: &str,
        new_roles: &[String],
    ) -> Result<String, RuntimeError> {
        // The roles of *this* `does` are composed together into one new type, but
        // that type INHERITS from `base_class`. So a second `does` on an already
        // mixed-in object stacks a type on the previous one (`C+{A}+{B}`, exactly
        // how Rakudo names it) instead of re-composing everything side by side:
        // successive `does` calls are legal even when the roles declare the same
        // method (the later one wins), whereas one composition of both would be
        // X::Role::Composition::Conflict.
        let already = self
            .registry()
            .class_composed_roles
            .get(base_class)
            .cloned();
        let fresh: Vec<String> = new_roles
            .iter()
            .filter(|role| !already.as_ref().is_some_and(|c| c.contains(role)))
            .cloned()
            .collect();
        if fresh.is_empty() {
            return Ok(base_class.to_string());
        }
        let name = format!("{}+{{{}}}", base_class, fresh.join(","));
        if self.registry().classes.contains_key(&name) {
            return Ok(name);
        }
        // `register_class_decl` walks a single `parents` list and uses
        // `does_parents` only to tell which entries are composed roles rather
        // than inheritance parents, so the roles appear in both.
        let mut parents = vec![base_class.to_string()];
        parents.extend(fresh.iter().cloned());
        let language_version = crate::parser::current_language_version();
        let modifiers = ClassDeclModifiers {
            class_is_rw: false,
            is_hidden: false,
            is_lexical: false,
            hidden_parents: &[],
            does_parents: &fresh,
            language_version: &language_version,
        };
        self.register_class_decl(&name, &parents, modifiers, &[])?;
        self.compose_mixin_role_submethods(&name, &fresh);
        // A mixin type is synthesized, not written by the user: it must inherit
        // the base's accessor authority rather than claim its own. Marked
        // user-declared (which `register_class_decl` does for everything), a
        // `Parameter+{Query}` would answer `.named` with X::Method::NotFound,
        // because a built-in base contributes no *declared* attribute list.
        if !self.user_declared_classes.contains(base_class) {
            self.user_declared_classes.remove(&name);
        }
        Ok(name)
    }

    /// `$obj does R` where `$obj` is a real object: rebless it into `C+{R}` in
    /// place. Returns `None` when the value is not an instance (an `Int`, a
    /// `Str`, an already-wrapped `Mixin`), leaving those to the wrapper path.
    pub(crate) fn does_rebless_instance(
        &mut self,
        left: &Value,
        roles: &[(String, Vec<Value>)],
    ) -> Result<Option<Value>, RuntimeError> {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = left.view()
        else {
            return Ok(None);
        };
        // The internal descriptor objects keep the wrapper path: their consumers
        // (`record_attr_container_mixin`, `set_var_meta_value`, the `.HOW`
        // persistence in `eval_does_values`) read the composed result's mixin map
        // rather than the object itself.
        const WRAPPER_ONLY_MARKERS: [&str; 3] = [
            "__mutsu_attr_container_owner",
            "__mutsu_how_target",
            "__mutsu_var_target",
        ];
        {
            let map = attributes.as_map();
            if WRAPPER_ONLY_MARKERS
                .iter()
                .any(|marker| map.contains_key(*marker))
            {
                return Ok(None);
            }
        }
        let base = class_name.resolve();
        // The mixin type inherits from the object's class, so the class has to be
        // something a declaration may name as a parent. A built-in type with no
        // registry entry (`Attribute`) is not, and keeps the wrapper path.
        if !self.registry().classes.contains_key(base.as_str())
            && !crate::runtime::registration_class_decl::BUILTIN_PARENT_TYPES
                .contains(&base.as_str())
        {
            return Ok(None);
        }
        // The roles have to exist as *registered* roles for the class
        // declaration to compose them; a builtin-role name (`Positional`) has no
        // RoleDef, so those keep the wrapper path.
        if roles
            .iter()
            .any(|(name, _)| !self.registry().roles.contains_key(name))
        {
            return Ok(None);
        }
        // A parameterised mixin (`$o does R[Int]`, `$o does R(5)`) still needs
        // the wrapper's per-object type-argument and attribute-seed bookkeeping.
        if roles.iter().any(|(_, args)| !args.is_empty()) {
            return Ok(None);
        }
        let role_names: Vec<String> = roles.iter().map(|(name, _)| name.clone()).collect();
        for name in &role_names {
            self.run_mixin_role_body(name)?;
        }
        let mixin_class = self.ensure_mixin_class(&base, &role_names)?;
        attributes.rebless(Symbol::intern(&mixin_class));
        // Seed the composed roles' own attributes. The object already exists, so
        // no constructor runs for them: give each declared attribute its default
        // (or the sigil-appropriate empty container) unless the object carries it
        // already.
        self.seed_mixin_role_attributes(&attributes, &role_names)?;
        let reblessed = Value::instance_sharing_cell(
            &attributes,
            Symbol::intern(&mixin_class),
            attributes.instance_id(),
        );
        // 6.e runs a composed role's BUILD/TWEAK on the object it was mixed into.
        for name in &role_names {
            self.run_mixin_role_build(&reblessed, &attributes, name)?;
        }
        Ok(Some(reblessed))
    }

    /// Copy the composed roles' submethods onto the mixin type.
    ///
    /// `register_class_decl` composes a role's submethods only under the 6.c
    /// class-declaration rule, but a *runtime* mixin brings them along whatever
    /// the language revision — `$fh does File::Temp::AutoUnlink` has to make the
    /// role's `submethod DESTROY` callable on `$fh`. The mixin type is the
    /// object's own class, so a submethod declared on it is found by ordinary
    /// resolution (submethods are only excluded when inherited).
    ///
    /// `BUILD`/`TWEAK` are left out: they run once, on the object being mixed
    /// into, via `run_mixin_role_build`.
    fn compose_mixin_role_submethods(&mut self, class_name: &str, role_names: &[String]) {
        let mut composed: Vec<(String, Vec<MethodDef>)> = Vec::new();
        for role_name in role_names {
            let Some(role) = self.registry().roles.get(role_name).cloned() else {
                continue;
            };
            for (method_name, defs) in &role.methods {
                if method_name == "BUILD" || method_name == "TWEAK" {
                    continue;
                }
                let submethods: Vec<MethodDef> = defs
                    .iter()
                    .filter(|d| d.is_my)
                    .map(|d| {
                        let mut d = d.clone();
                        if d.original_role.is_none() {
                            d.original_role = d.role_origin.clone();
                        }
                        d.role_origin = Some(role_name.clone());
                        d
                    })
                    .collect();
                if !submethods.is_empty() {
                    composed.push((method_name.clone(), submethods));
                }
            }
        }
        if composed.is_empty() {
            return;
        }
        self.clear_private_zeroarg_method_cache();
        let mut registry = self.registry_mut();
        let Some(class_def) = registry.classes.get_mut(class_name) else {
            return;
        };
        for (method_name, defs) in composed {
            class_def
                .methods
                .entry(method_name)
                .or_default()
                .extend(defs);
        }
    }

    /// Run a role's non-declaration body once, the same way the wrapper path
    /// does — a guard in the body (`role R[::T] { die unless ... }`) must still
    /// fire when the role is mixed in.
    fn run_mixin_role_body(&mut self, role_name: &str) -> Result<(), RuntimeError> {
        if !self
            .registry_mut()
            .composed_role_bodies
            .insert(format!("mixin:{role_name}"))
        {
            return Ok(());
        }
        let stmts = self
            .registry()
            .roles
            .get(role_name)
            .map(|r| r.deferred_body_stmts.clone())
            .unwrap_or_default();
        self.run_role_body_for_composition(role_name, role_name, &stmts)?;
        self.run_composed_role_ancestor_bodies(role_name, role_name)
    }

    /// Give each newly composed role attribute its declared default on an
    /// already-constructed object.
    fn seed_mixin_role_attributes(
        &mut self,
        attributes: &crate::gc::Gc<crate::value::InstanceAttrs>,
        role_names: &[String],
    ) -> Result<(), RuntimeError> {
        for role_name in role_names {
            let Some(role) = self.registry().roles.get(role_name).cloned() else {
                continue;
            };
            let saved_env = role.captured_env.as_ref().map(|captured| {
                let saved = self.env.clone();
                for (k, v) in captured {
                    if !self.env.contains_key(k) {
                        self.env.insert(k.clone(), v.clone());
                    }
                }
                saved
            });
            for (attr_name, _is_public, default_expr, _, _, sigil, _) in &role.attributes {
                if attributes.contains_key(attr_name.as_str()) {
                    continue;
                }
                let value = match default_expr {
                    Some(expr) => self.eval_block_value(&[Stmt::Expr(expr.clone())])?,
                    None => match sigil {
                        '@' => Value::real_array(Vec::new()),
                        '%' => Value::hash_with_data(Value::hash_arc(HashMap::new())),
                        _ => Value::NIL,
                    },
                };
                attributes.insert(attr_name.as_str(), value);
            }
            if let Some(saved) = saved_env {
                self.env = saved;
            }
        }
        Ok(())
    }

    /// Run a freshly composed role's `BUILD` / `TWEAK` submethods on the object.
    /// A submethod is not inherited, so it cannot be reached by an ordinary
    /// method call on the mixin type — run the role's own definition directly,
    /// with the object as the invocant.
    fn run_mixin_role_build(
        &mut self,
        target: &Value,
        attributes: &crate::gc::Gc<crate::value::InstanceAttrs>,
        role_name: &str,
    ) -> Result<(), RuntimeError> {
        let Some(role) = self.registry().roles.get(role_name).cloned() else {
            return Ok(());
        };
        for submethod in ["BUILD", "TWEAK"] {
            let Some(def) = role
                .methods
                .get(submethod)
                .and_then(|defs| defs.iter().find(|d| d.is_my))
                .cloned()
            else {
                continue;
            };
            let (_, updated) = self.run_resolved_method_compiled_or_treewalk(
                role_name,
                role_name,
                submethod,
                def,
                attributes.to_map(),
                Vec::new(),
                Some(target.clone()),
            )?;
            attributes.commit_attrs(updated);
        }
        Ok(())
    }
}
