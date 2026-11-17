//! The per-method frame plan of a compiled method call (ADR-15737 D1).
//!
//! `call_compiled_method_fast_in_unit` seeds every local slot of the method's
//! frame before the body runs. It used to decide, per slot and per call, what
//! the slot holds by matching the local's *name*: `self`, `?CLASS`, `?ROLE`,
//! `!`, a parameter (a linear search of the bound parameters by name), `$_`, a
//! private `$!a` (re-interning the attribute name and building its
//! owner-qualified key), a public `$.a`, an aggregate `@!a`/`%!a` (formatting
//! its sigil key into a fresh `String`), or an outer lexical. None of those
//! decisions depends on the call: they are a function of the method's
//! compiled code, its parameter list and the class that owns it.
//!
//! [`MethodFramePlan`] makes each decision once and stores it as a
//! [`LocalInit`] per slot, with every attribute key already interned, so the
//! per-call loop is a slot-indexed walk with no string matching, no
//! `Symbol::intern` and no allocation.
//!
//! # Where it lives, and why it cannot go stale
//!
//! The plan is memoized on the [`MethodDef`](crate::runtime::MethodDef) itself
//! (`MethodDefSyms::frame_plan`, the same lazily-filled cache that holds its
//! interned parameter names), because everything it reads is immutable once
//! the def exists: `params`, `param_defs`, `compiled_code` and
//! `original_role`/`role_origin` are never reassigned on a def -- a change
//! builds a new def (and a role composition that clones a def and re-stamps it
//! clones the empty-or-filled cache alongside, see below). Two inputs are not
//! part of the def, so the plan records both and is only used when both match:
//!
//! * the **owner class** whose qualified attribute keys it interned (an
//!   inherited method is always run with its declaring owner, but nothing in
//!   the type system makes that a law, so a mismatch builds a throw-away
//!   plan instead of trusting the cached one);
//! * the **compiled code** it was built from (compared by address with the
//!   `cc` the call runs, which is `method_def.compiled_code` on every caller
//!   today).
//!
//! A clone of a def shares its filled `OnceLock` contents. That is sound for
//! the same reason: the clone's `compiled_code` and owner are checked on use,
//! and its parameter names fall under `method_def_syms`'s existing contract (a
//! def whose `params`/`param_defs` change gets a fresh `MethodDefSyms`). The
//! role-origin fields, which role composition does re-stamp on clones, are
//! deliberately *not* part of the plan.
//!
//! The plan is immutable after construction, so sharing it across Raku
//! threads (the def lives in the shared registry) needs no lock.

use crate::opcode::CompiledCode;
use crate::symbol::Symbol;

/// What one local slot of a method frame is seeded with.
#[derive(Debug, Clone, Copy)]
pub(crate) enum LocalInit {
    /// `self` (and the anonymous state slot): the invocant.
    Invocant,
    /// `?CLASS`.
    Class,
    /// `?ROLE`: the role context, or `Nil` outside one.
    Role,
    /// `$!`: `Nil`.
    Nil,
    /// The bound value of parameter `idx` (the first parameter whose binding
    /// name is this local's name).
    Param(u32),
    /// `$_`: `Any`.
    Topic,
    /// A private attribute `$!a`: its owner-qualified key, then its bare key.
    AttrPrivate { qualified: Symbol, bare: Symbol },
    /// A public attribute `$.a`.
    AttrPublic { bare: Symbol },
    /// An aggregate attribute `@!a`/`%!a`/`@.a`/`%.a`: its `<sigil>bare` key
    /// (where a private declaration colliding with a public one of another
    /// sigil lives), then its bare key.
    AttrAggregate { sigil_key: Symbol, bare: Symbol },
    /// Anything else: the outer environment's value of the slot's symbol, or
    /// `Nil`.
    Outer,
}

/// The call-independent part of a compiled method call's frame set-up.
#[derive(Debug, Clone)]
pub(crate) struct MethodFramePlan {
    /// The owner class the attribute keys were qualified with.
    owner: Symbol,
    /// Address of the `CompiledCode` the slots were planned from.
    code_addr: usize,
    /// One entry per local slot of that code.
    pub(crate) locals: Box<[LocalInit]>,
}

impl MethodFramePlan {
    /// Whether this plan was built for `owner` and `cc`.
    // Cost: O(1).
    #[inline]
    pub(crate) fn fits(&self, owner: Symbol, cc: &CompiledCode) -> bool {
        self.owner == owner && self.code_addr == cc as *const CompiledCode as usize
    }

    /// Plan the frame of a method run as `cc` with `owner` as its class.
    ///
    /// `binding_names` are the bound parameters' names in parameter order --
    /// exactly the names the fast binder pushes, so `LocalInit::Param(i)`
    /// indexes the binder's output when it bound one value per parameter.
    // Cost: O(l * p), l = local slots, p = parameters; run once per def.
    pub(crate) fn build(owner: Symbol, cc: &CompiledCode, binding_names: &[&str]) -> Self {
        let locals = cc
            .locals
            .iter()
            .map(|local_name| Self::plan_local(owner, local_name, binding_names))
            .collect();
        Self {
            owner,
            code_addr: cc as *const CompiledCode as usize,
            locals,
        }
    }

    /// The decision the per-call loop used to make by name, in the same order.
    // Cost: O(p + n), p = parameters, n = the local name's length.
    fn plan_local(owner: Symbol, name: &str, binding_names: &[&str]) -> LocalInit {
        match name {
            "self" | "__ANON_STATE#" => LocalInit::Invocant,
            "?CLASS" => LocalInit::Class,
            "?ROLE" => LocalInit::Role,
            "!" => LocalInit::Nil,
            name => {
                if let Some(idx) = binding_names.iter().position(|n| *n == name) {
                    LocalInit::Param(idx as u32)
                } else if name == "_" {
                    LocalInit::Topic
                } else if let Some(attr_name) = name.strip_prefix('!') {
                    let bare = Symbol::intern(attr_name);
                    LocalInit::AttrPrivate {
                        qualified: crate::qualified::qualified_attr_key(owner, bare),
                        bare,
                    }
                } else if let Some(attr_name) = name.strip_prefix('.') {
                    LocalInit::AttrPublic {
                        bare: Symbol::intern(attr_name),
                    }
                } else if ((name.starts_with("@!") || name.starts_with("@."))
                    || (name.starts_with("%!") || name.starts_with("%.")))
                    && name.len() > 2
                {
                    LocalInit::AttrAggregate {
                        sigil_key: Symbol::intern(&format!("{}{}", &name[..1], &name[2..])),
                        bare: Symbol::intern(&name[2..]),
                    }
                } else {
                    LocalInit::Outer
                }
            }
        }
    }
}

impl crate::runtime::MethodDef {
    /// The names the fast binder binds this def's parameters under, in
    /// parameter order: the storage name, or `self`'s lexical for a parameter
    /// that declares it (the `binding_name` of the binder's loop).
    // Cost: O(p), p = parameters.
    pub(crate) fn fast_binding_names(&self) -> Vec<&str> {
        self.param_storage_names()
            .iter()
            .enumerate()
            .map(|(idx, name)| {
                if self
                    .param_defs
                    .get(idx)
                    .is_some_and(|pd| pd.declares_self_lexical())
                {
                    crate::env::LEX_SELF
                } else {
                    name.as_str()
                }
            })
            .collect()
    }

    /// The frame plan for running this def as `cc` with `owner` as its class:
    /// the memoized one when it was built for the same pair, else a fresh one
    /// (see the module docs for why a mismatch is not cached).
    // Cost: O(1) after the first call; O(l * p) on a build.
    pub(crate) fn frame_plan(
        &self,
        owner: Symbol,
        cc: &CompiledCode,
    ) -> std::borrow::Cow<'_, MethodFramePlan> {
        let build = || MethodFramePlan::build(owner, cc, &self.fast_binding_names());
        let cached = self.syms.frame_plan.get_or_init(build);
        if cached.fits(owner, cc) {
            return std::borrow::Cow::Borrowed(cached);
        }
        std::borrow::Cow::Owned(build())
    }
}
