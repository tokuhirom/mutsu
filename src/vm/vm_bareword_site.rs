//! `OpCode::GetBareWord`, with the per-site type-object memo (ADR-0121 D3).
//!
//! A bareword term is resolved through `push_bare_word_value`'s whole chain
//! on every execution. When that chain answers the type object of the name's
//! own spelling (`P` → `P`), the answer depends only on what is declared, so
//! the chunk remembers it for one registry write generation
//! ([`BarewordSiteCaches`](crate::value::BarewordSiteCaches)) and a later
//! execution under the same generation pushes it without resolving.
//!
//! Three more things feed the chain ahead of its type branch, and the memo is
//! sound against each:
//!
//! - **A same-named `env` binding.** A type capture (`sub f(::T $x) { T }`),
//!   a role's type parameter, or a sigil-stripped `my $P` holding a package
//!   are all read from `env` under the bare name, and rebinding them writes
//!   no registry. So a site is only remembered, and a memo only used, while
//!   `env` binds nothing under the name, or binds that very type object (as
//!   a class declaration does for its own name). That probe is the one
//!   re-check a hit pays.
//! - **An enum member with the same spelling.** Declaring an enum registers
//!   its type, which bumps the generation.
//! - **Spellings the chain rewrites from bindings.** A parameterised name
//!   (`Box[T]`) or a smiley on a bound type parameter (`T:D`) is substituted
//!   from `env` under a *different* key than the name, so such names are
//!   never remembered.
//!
//! The generation is per interpreter, and every interpreter's counter starts
//! in a range of its own (`Interpreter::fresh_registry_write_gen`), so a memo
//! a thread wrote into a shared chunk is never read as current by another
//! interpreter whose registry snapshot differs.

use super::*;

impl Interpreter {
    // Cost: O(1) on a memo hit (one lock, one `env` probe); a miss costs one
    // bareword resolution, O(p*|name|), p = packages on the bare-name search path.
    pub(super) fn exec_get_bare_word_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let generation = self.registry_write_generation();
        let sites = code.constants.len();
        let idx = name_idx as usize;
        if let Some(sym) = code.bareword_sites.cached(sites, idx, generation)
            && self.env_leaves_type_name(sym)
        {
            self.stack.push(Value::package(sym));
            return Ok(());
        }
        let name = Self::const_str(code, name_idx);
        self.push_bare_word_value(name, compiled_fns)?;
        if let Some(ValueView::Package(sym)) = self.stack.last().map(Value::view)
            && sym == name
            && bareword_memoizable(name)
            && self.env_leaves_type_name(sym)
        {
            code.bareword_sites.remember(sites, idx, generation, sym);
        }
        Ok(())
    }

    /// Whether `env` leaves the bareword `name` to name its own type object:
    /// nothing is bound under the name, or what is bound is that type object
    /// (a class declaration binds its own name so; so does a type capture
    /// bound to the same-named class). Any other binding may be what the
    /// resolution chain answers, and it changes without a registry write.
    // Cost: O(1), one `env` probe.
    pub(crate) fn env_leaves_type_name(&self, name: Symbol) -> bool {
        match self.env().get_sym(name) {
            None => true,
            Some(v) => matches!(v.view(), ValueView::Package(p) if p == name),
        }
    }
}

/// Whether a bareword spelled `name` may be remembered once it resolved to
/// the type object of that same spelling: not when the chain could have
/// rewritten it from a binding stored under another key (see the module
/// comment).
// Cost: O(|name|).
fn bareword_memoizable(name: &str) -> bool {
    !name.contains('[') && crate::runtime::types::strip_type_smiley(name).1.is_none()
}
