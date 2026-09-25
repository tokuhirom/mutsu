//! A bareword term in a TRIR routine that names a type object
//! (`nqp::getattr($o, IB, '$!a')`, `nqp::create(IB)`).
//!
//! rakudo resolves that name at compile time. mutsu resolves a bareword at
//! run time, through the whole term-resolution chain, on every execution —
//! ~4,700 instructions, more than half of what such a site cost. A name that
//! resolved to a type object keeps resolving to it until a declaration
//! changes, so the site remembers the answer for one registry write
//! generation.
//!
//! Which answers may be remembered depends on whether the value is used:
//!
//! - The class operand of an attribute op is only owed for its *effect*
//!   (resolving at all, failing when it names nothing): the attribute ops
//!   ignore its value, because an instance has one attribute store, not one
//!   per class. Any type object it resolved to is remembered.
//! - Everywhere else the value is used, so only a type object named by the
//!   bareword's own spelling is remembered. That is the answer of the
//!   type-name branch of the resolution chain, which reads nothing but the
//!   registry. A name that answered through a binding (a sigilless `\T`
//!   bound to `Int`, an imported routine, a nested class's qualified name) is
//!   re-resolved on every execution, as before.

use crate::symbol::Symbol;

/// One class-operand site: the name, and the type object it last resolved to
/// with the registry write generation it was resolved under.
#[derive(Debug)]
pub(crate) struct ClassOperandSite {
    pub(crate) name: Symbol,
    /// Only the operand's effect is owed (an attribute op's class operand),
    /// so any type object it resolved to may be remembered.
    pub(crate) effect_only: bool,
    cache: std::sync::Mutex<Option<(u64, Symbol)>>,
}

impl ClassOperandSite {
    /// The class operand of an attribute op, whose value is ignored.
    pub(crate) fn new(name: Symbol) -> Self {
        Self::with(name, true)
    }

    /// A bareword term whose value is used.
    pub(crate) fn term(name: Symbol) -> Self {
        Self::with(name, false)
    }

    fn with(name: Symbol, effect_only: bool) -> Self {
        Self {
            name,
            effect_only,
            cache: std::sync::Mutex::new(None),
        }
    }

    /// Whether a resolution that answered type object `sym` may be
    /// remembered.
    // Cost: O(1).
    fn rememberable(&self, sym: Symbol) -> bool {
        self.effect_only || sym == self.name
    }

    /// The type object this site resolved to under generation `generation`.
    // Cost: O(1).
    pub(crate) fn cached(&self, generation: u64) -> Option<Symbol> {
        match *self.cache.lock().ok()? {
            Some((g, sym)) if g == generation => Some(sym),
            _ => None,
        }
    }

    /// Remember that the name resolved to type object `sym` under
    /// `generation` (read before the resolution ran, so a resolution that
    /// itself declared something is not remembered past it).
    // Cost: O(1).
    pub(crate) fn remember(&self, generation: u64, sym: Symbol) {
        if let Ok(mut c) = self.cache.lock() {
            *c = Some((generation, sym));
        }
    }
}

impl Clone for ClassOperandSite {
    /// A copied site starts cold.
    fn clone(&self) -> Self {
        Self::with(self.name, self.effect_only)
    }
}

impl crate::runtime::Interpreter {
    /// Execute a [`ClassOperandSite`]: the bareword's value, from the site's
    /// memo when the registry has not been written since it was resolved.
    // Cost: O(1) on a hit; a miss costs one bareword resolution.
    pub(super) fn trir_class_operand(
        &mut self,
        site: &ClassOperandSite,
        compiled_fns: &crate::opcode::CompiledFns,
    ) -> Result<crate::value::Value, crate::value::RuntimeError> {
        use crate::value::{Value, ValueView};
        let generation = self.registry_write_generation();
        if let Some(sym) = site.cached(generation) {
            return Ok(Value::package(sym));
        }
        self.push_bare_word_value(site.name.as_str(), compiled_fns)?;
        let v = self.stack.pop().unwrap_or(Value::NIL);
        if let ValueView::Package(sym) = v.view()
            && site.rememberable(sym)
        {
            site.remember(generation, sym);
        }
        Ok(v)
    }
}
