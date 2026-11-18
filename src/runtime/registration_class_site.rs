//! Re-entry of an unchanged lexical class declaration (#16978).
//!
//! A `my class` written inside a routine body executes its `RegisterClass` op
//! on every entry of that routine. In Raku the declaration is a compile-time
//! one: the type object is the same on every call (`sub f { my class C {}; C }
//! f() === f()` is `True`) and only the class body's statements and the
//! methods' closure over the current frame are per-entry. mutsu used to rebuild
//! the whole class every time — compose its roles, re-insert every method row,
//! recompile the method bodies — and, worse, every rebuild bumps
//! `Registry::method_generation`, which throws away every method-resolution,
//! multi-dispatch and regex call cache in the program. A grammar action method
//! as small as `method build { my class builder is AST { ... } }`
//! (CSS::Module) then turned each action call into a full re-warm of those
//! caches: 3.5 s of a 39-line stylesheet parse that rakudo does in 0.5 s.
//!
//! The registration of a declaration whose body is purely declarative is a
//! function of its plan and its resolved parents only. Such a declaration is
//! stamped with that identity when it registers in full, and a later execution
//! that finds the same stamp on the class keeps the registered class, redoing
//! only the frame-dependent part (`capture_declared_method_envs`). Anything
//! that registers the same storage name in full again clears the stamp first
//! (`register_class_decl`), so a stamp can never describe a class it did not
//! produce.

use super::Interpreter;
use crate::runtime::RuntimeError;
use crate::opcode::{ClassBodyOp, CompiledClassDeclPlan, DeclTraitArg};

/// What a full registration of a static lexical class declaration depended on.
/// Two executions with equal stamps register identical classes.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct ClassSiteStamp {
    /// The declaration site (`ClassDecl::decl_id`), unique per parsed site.
    pub(crate) decl_id: u64,
    /// The parents as `exec_register_class_op` resolved them for this entry:
    /// lexical parents resolve to their mangled storage names, so a parent
    /// that is itself a different declaration this time changes the stamp.
    pub(crate) parents: Vec<String>,
    pub(crate) hidden_parents: Vec<String>,
    /// The resolved `does` roles, under the same rule as `parents`.
    pub(crate) roles: Vec<String>,
}

/// Whether a declaration-time expression reads a lexical of the declaring
/// frame. Such an expression has to be evaluated, or captured, per entry.
fn reads_user_lexical(arg: Option<&DeclTraitArg>) -> bool {
    match arg {
        Some(DeclTraitArg::Compiled(chunk)) => chunk
            .code
            .free_var_syms
            .iter()
            .any(|sym| sym.with_str(crate::env::is_plain_user_lexical)),
        _ => false,
    }
}

impl CompiledClassDeclPlan {
    /// Whether registering this declaration depends on nothing but the plan
    /// and its resolved parents: a lexical or anonymous, non-stub class with a fixed name,
    /// no custom traits or metaclass, no parent or role arguments evaluated
    /// per entry, and a body of nothing but `has` and `method` declarations
    /// whose declaration-time expressions read no lexical of the declaring
    /// frame (line markers aside). A body statement (`my $x = ...`, a nested `sub` or type, a
    /// phaser, a `token`) runs per entry, so a declaration with one is never
    /// reused.
    // Cost: O(a + m + o), a = attributes, m = methods, o = body statements.
    pub(crate) fn reentry_is_static(&self) -> bool {
        self.reentry_header_is_static()
            && self.body_bind_source_slots.is_empty()
            && self.declared_static_names.is_empty()
            && self.body_plan.iter().all(|op| {
                matches!(
                    op,
                    ClassBodyOp::Attr { .. }
                        | ClassBodyOp::Method
                        | ClassBodyOp::Does { args: None, .. }
                        | ClassBodyOp::Other {
                            raw: crate::ast::Stmt::SetLine(..),
                            ..
                        }
                )
            })
    }

    /// Whether a re-entry keeps the registered type and only runs the body's
    /// per-entry part (#18070): the declaration is static but for body
    /// statements that run per entry -- `my` variables and their
    /// initializers, nested `sub`s, plain (non-block) statements,
    /// class-scoped LEAVE phasers. Rakudo creates the type once per declaration and runs those
    /// per entry (`sub f($i) { my class C { my $z = $i } }`: `f(1) === f(2)`,
    /// and the body runs on both calls). A statement that can change the
    /// class itself (`use`, a nested type, a BEGIN/CHECK phaser, an `EVAL`
    /// that may declare attributes) keeps the declaration on the full path.
    // Cost: O(a + m + o), as `reentry_is_static`.
    pub(crate) fn reentry_keeps_type(&self) -> bool {
        use crate::ast::Stmt;
        self.reentry_header_is_static()
            && self.body_plan.iter().all(|op| match op {
                ClassBodyOp::Attr { .. }
                | ClassBodyOp::Method
                | ClassBodyOp::Does { args: None, .. }
                | ClassBodyOp::LeavePhaser { .. } => true,
                ClassBodyOp::ClassSub {
                    is_swallowable,
                    is_compile_time_phaser,
                    ..
                } => !is_swallowable && !is_compile_time_phaser,
                ClassBodyOp::Other {
                    raw,
                    is_swallowable,
                    is_compile_time_phaser,
                    ..
                } => {
                    // Simple statements only: a nested block (or a `do`
                    // expression) may declare a method that closes over
                    // the block's own lexicals, which a full registration
                    // re-captures per entry.
                    !is_swallowable
                        && !is_compile_time_phaser
                        && match raw {
                            Stmt::Expr(expr) => !matches!(expr, crate::ast::Expr::DoBlock { .. }),
                            _ => matches!(
                                raw,
                                Stmt::SetLine(..)
                                    | Stmt::VarDecl { .. }
                                    | Stmt::Assign { .. }
                                    | Stmt::Say(..)
                                    | Stmt::Put(..)
                                    | Stmt::Print(..)
                                    | Stmt::Note(..)
                                    | Stmt::Call { .. }
                            ),
                        }
                }
                _ => false,
            })
    }

    /// The part of [`Self::reentry_is_static`] that does not depend on the
    /// body statements: a lexical or anonymous, non-stub class with a fixed
    /// name, no custom traits or metaclass, no parent or role arguments
    /// evaluated per entry, and attribute declaration-time expressions that
    /// read no lexical of the declaring frame.
    // Cost: O(a + m), a = attributes, m = methods.
    fn reentry_header_is_static(&self) -> bool {
        // An anonymous `class :: { ... }` is one type per site too (rakudo:
        // `sub f { class :: {} }; f() === f()`). Rebuilding it on every entry
        // also rewrote a world table other threads were dispatching through
        // (ADR-17216): a concurrent caller found its method table empty.
        (self.is_lexical
            || self
                .name
                .with_str(crate::anon_names::is_anon_class_name))
            && self.decl_id != 0
            && !self.is_stub
            && self.name_chunk.is_none()
            && self.custom_traits.is_empty()
            && self.declarator.is_none()
            && self.body_parents.is_empty()
            && self.repr.is_none()
            && !self.is_native
            && self.export_tags.is_none()
            && self.parent_arg_chunks.is_empty()
            && self.attr_decls.iter().all(|(_, decl)| {
                !reads_user_lexical(decl.default.as_ref())
                    && !reads_user_lexical(decl.where_constraint.as_ref())
                    && !reads_user_lexical(decl.is_default.as_ref())
            })
            && self
                .method_name_chunks
                .iter()
                .all(|chunk| chunk.is_none())
    }
}

impl Interpreter {
    /// The stamp a full registration of `plan` with these resolved parents
    /// records, or `None` when the declaration is not static.
    // Cost: O(a + m + o + p), the plan's `reentry_is_static` plus p = parents
    // and roles.
    pub(crate) fn class_site_stamp(
        &self,
        plan: &CompiledClassDeclPlan,
        parents: &[String],
        hidden_parents: &[String],
    ) -> Option<ClassSiteStamp> {
        if !plan.reentry_keeps_type() {
            return None;
        }
        // A role is composed into the class when it registers. A lexical role
        // is itself re-declared on every entry (roles are never kept), and a
        // punned role parent (`is SomeRole`) installs pun state the role's own
        // re-declaration resets, so a class built from either is rebuilt with
        // them.
        let registry = self.registry();
        let names_role = |name: &str| {
            let base = name.split_once('[').map_or(name, |(base, _)| base);
            registry.roles.contains_key(base)
        };
        if parents.iter().chain(hidden_parents).any(|p| names_role(p)) {
            return None;
        }
        drop(registry);
        let roles: Vec<String> = plan
            .does_parents
            .iter()
            .map(|role| self.lexical_env_remap_name(role))
            .collect();
        if roles.iter().any(|role| role.contains('\u{0}')) {
            return None;
        }
        Some(ClassSiteStamp {
            decl_id: plan.decl_id,
            parents: parents.to_vec(),
            hidden_parents: hidden_parents.to_vec(),
            roles,
        })
    }

    /// Whether `storage` is still the class a full registration stamped with
    /// `stamp`: the stamp is unchanged, and the class is registered and not
    /// reduced to a stub since.
    // Cost: O(p), p = parents and roles in the stamp (one map probe each).
    pub(crate) fn class_site_is_registered(&self, storage: &str, stamp: &ClassSiteStamp) -> bool {
        let registry = self.registry();
        registry.class_site_stamps.get(storage) == Some(stamp)
            && registry.classes.contains_key(storage)
            && !registry.class_stubs.contains(storage)
    }

    /// Record that `storage` was registered in full from a static declaration.
    // Cost: O(1) amortized.
    pub(crate) fn stamp_class_site(&mut self, storage: &str, stamp: ClassSiteStamp) {
        self.registry_mut()
            .class_site_stamps
            .insert(storage.to_string(), stamp);
    }

    /// Forget the stamp of `storage`: it is being registered again in full.
    // Cost: O(1).
    pub(crate) fn clear_class_site_stamp(&mut self, storage: &str) {
        if self.registry().class_site_stamps.contains_key(storage) {
            self.registry_mut().class_site_stamps.remove(storage);
        }
    }

    /// Re-enter the declaration of the registered class `name` whose site
    /// keeps its type (`CompiledClassDeclPlan::reentry_keeps_type`): run the
    /// body's per-entry statements in the class scope, as a full
    /// registration runs them, without touching the class (#18070).
    // Cost: the body's per-entry statements, plus one class-table read.
    pub(crate) fn reenter_class_decl(
        &mut self,
        name: &str,
        modifiers: super::ClassDeclModifiers<'_>,
    ) -> Result<(), RuntimeError> {
        let Some(class_def) = self.registry().classes.get(name).cloned() else {
            return Ok(());
        };
        let class_lang_rev =
            super::registration_class::language_revision_letter(modifiers.language_version);
        self.run_class_body(
            name,
            class_def.into_value(),
            modifiers.is_hidden,
            modifiers.class_is_rw,
            &class_lang_rev,
            modifiers.own_attribute_names,
            modifiers.attr_decls,
            modifiers.attr_type_aliases,
            modifiers.method_name_chunks,
            modifiers.method_decls,
            modifiers.declared_static_names,
            modifiers.nested_decl_names,
            modifiers.compiled_fns,
            modifiers.body_plan,
            modifiers.is_hoisted_shell,
            true,
        )?;
        Ok(())
    }
}
