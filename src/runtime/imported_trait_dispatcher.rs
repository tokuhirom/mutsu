//! Routine traits dispatched through an imported `&trait_mod:<is>` (#11530).
//!
//! In Raku a trait applies through the `&trait_mod:<is>` lexically visible at
//! the declaration. A custom `sub EXPORT` can supply that symbol itself, as
//! upstream NativeCall does:
//!
//! ```raku
//! sub EXPORT(|) {
//!     my $native_trait := multi trait_mod:<is>(Routine $r, :$native!) { ... };
//!     Map.new('&trait_mod:<is>' => $native_trait.dispatcher);
//! }
//! ```
//!
//! That `multi` is lexical to `EXPORT`, so its registry rows do not reach the
//! importer. What does reach it is the dispatcher value, which carries its
//! candidates (`sub_value_from_multi_candidates`). mutsu applies a trait by
//! calling `trait_mod:<is>` by name, so these helpers make that call try the
//! imported dispatcher first, then fall back to the registry when none of the
//! dispatcher's candidates accepts the trait. The fallback keeps the
//! candidates the importer reaches some other way (its own `multi`s, an
//! `is export` import) in play.

use super::*;
use crate::runtime::dispatch_key;
use crate::symbol::Symbol;

impl Interpreter {
    /// The `&name` dispatcher a custom `sub EXPORT` installed into the
    /// running scope, when it is one that carries candidates.
    // Cost: O(1) expected.
    pub(crate) fn imported_trait_dispatcher(&self, name: &str) -> Option<Value> {
        let name_sym = Symbol::lookup(name)?;
        if !self.module.export_amp_override_names.contains(&name_sym) {
            return None;
        }
        let value = dispatch_key::with_amp_name(name, |amp| self.env.get(amp).cloned())?;
        match value.view() {
            ValueView::Sub(data)
                if data
                    .env
                    .contains_key_sym(crate::symbol::well_known::multi_dispatch_candidates()) =>
            {
                Some(value.clone())
            }
            _ => None,
        }
    }

    /// Whether any `name` (`trait_mod:<is>`) handler is reachable from the
    /// running scope: a registered proto or multi, or an imported dispatcher.
    // Cost: as `has_multi_candidates`, plus O(1).
    pub(crate) fn has_trait_mod_handler(&mut self, name: &str) -> bool {
        self.has_proto(name)
            || self.has_multi_candidates(name)
            || self.imported_trait_dispatcher(name).is_some()
    }

    /// Apply the trait handler `name` to `args` through the imported
    /// dispatcher. `None` when there is none, or when none of its candidates
    /// accepts the call: the caller then dispatches by name, as before.
    // Cost: one candidate selection over the imported dispatcher's candidates.
    pub(crate) fn try_imported_trait_mod(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let dispatcher = self.imported_trait_dispatcher(name)?;
        match self.call_sub_value(dispatcher, args.to_vec(), false) {
            Err(e) if Self::is_trait_mod_no_candidate(&e) => None,
            other => Some(other),
        }
    }

    /// [`Self::try_imported_trait_mod`], else the by-name dispatch.
    // Cost: as `try_imported_trait_mod`, plus the by-name dispatch.
    pub(crate) fn call_trait_mod(
        &mut self,
        name: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match self.try_imported_trait_mod(name, &args) {
            Some(result) => result,
            None => self.call_function(name, args),
        }
    }
}
