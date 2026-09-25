//! The class operand of an `nqp::getattr` / `nqp::bindattr` site, when it is
//! a bareword (`nqp::getattr($o, IB, '$!a')`).
//!
//! rakudo resolves that name at compile time. mutsu resolves a bareword at
//! run time, through the whole term-resolution chain, on every execution —
//! more than half of what such a site cost. The attribute ops ignore the
//! class operand's value (an instance has one attribute store, not one per
//! class), so what the operand still owes is its *effect*: resolving it, and
//! failing when it names nothing. A name that resolved to a type object keeps
//! resolving to it until a declaration changes, so the site remembers the
//! answer for one registry write generation.

use crate::symbol::Symbol;

/// One class-operand site: the name, and the type object it last resolved to
/// with the registry write generation it was resolved under.
#[derive(Debug)]
pub(crate) struct ClassOperandSite {
    pub(crate) name: Symbol,
    cache: std::sync::Mutex<Option<(u64, Symbol)>>,
}

impl ClassOperandSite {
    pub(crate) fn new(name: Symbol) -> Self {
        Self {
            name,
            cache: std::sync::Mutex::new(None),
        }
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
        Self::new(self.name)
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
        if let ValueView::Package(sym) = v.view() {
            site.remember(generation, sym);
        }
        Ok(v)
    }
}
