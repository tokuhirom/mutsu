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
    /// and its resolved parents: a lexical, non-stub class with a fixed name,
    /// no custom traits or metaclass, no parent or role arguments evaluated
    /// per entry, and a body of nothing but `has` and `method` declarations
    /// whose declaration-time expressions read no lexical of the declaring
    /// frame (line markers aside). A body statement (`my $x = ...`, a nested `sub` or type, a
    /// phaser, a `token`) runs per entry, so a declaration with one is never
    /// reused.
    // Cost: O(a + m + o), a = attributes, m = methods, o = body statements.
    pub(crate) fn reentry_is_static(&self) -> bool {
        self.is_lexical
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
        plan.reentry_is_static().then(|| ClassSiteStamp {
            decl_id: plan.decl_id,
            parents: parents.to_vec(),
            hidden_parents: hidden_parents.to_vec(),
            roles: plan
                .does_parents
                .iter()
                .map(|role| self.lexical_env_remap_name(role))
                .collect(),
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
}
