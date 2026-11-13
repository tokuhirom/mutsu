//! Memo for a method call on a parameterized role type object
//! (`R[Int].method`, `$M.some(...)` with `$M = R.^parameterize(T)`).
//!
//! The general dispatch answers such a call by resolving which role candidate
//! the arguments select (`resolve_parametric_role_runtime`: a clone of every
//! candidate's `RoleDef`, and a trial bind of each candidate's type
//! parameters against a cloned env), then by finding or composing the pun
//! class of that parameterization and re-dispatching to it. Rakudo
//! parameterizes once and keeps the pun on the parameterization's HOW, so
//! the call costs it nothing extra; mutsu re-ran the whole selection on every
//! call (~40k instructions) -- and Monad calls `.some`/`.right`/`.left` and
//! `.maybe-type` on a parameterized role several times per operation
//! (#17331, #17408).
//!
//! The memo records, per `(role, type arguments, method)`, the pun class the
//! call was re-dispatched to. It is written only when the dispatch decided
//! "re-dispatch to the pun", read only from the same point of the same
//! dispatch, and tagged with the registry write generation: registering,
//! composing or withdrawing any type, role candidate or pun moves it, and the
//! entry is not served again. Only arguments that are all plain type objects
//! (`ValueView::Package`) are keyed; a value argument (`R[5]`) is not memoized,
//! since its candidate selection can run a `where` clause.

use super::*;

/// The memo's entries for one `(role, method)`: each type-argument list with
/// the pun it dispatched to and the registry write generation it was
/// recorded under.
pub(crate) type ParamRolePunEntries = Vec<(Box<[Symbol]>, Symbol, u64)>;

/// The type-object names of `type_args`, or `None` when one is not a plain
/// type object.
// Cost: O(a), a = number of type arguments.
fn type_object_args(type_args: &[Value]) -> Option<Vec<Symbol>> {
    type_args
        .iter()
        .map(|arg| match arg.view() {
            ValueView::Package(name) => Some(name),
            _ => None,
        })
        .collect()
}

/// Whether `entry_args` names exactly the type objects of `type_args`.
// Cost: O(a), a = number of type arguments.
fn same_type_objects(entry_args: &[Symbol], type_args: &[Value]) -> bool {
    entry_args.len() == type_args.len()
        && entry_args
            .iter()
            .zip(type_args)
            .all(|(name, arg)| matches!(arg.view(), ValueView::Package(n) if n == *name))
}

impl Interpreter {
    /// The pun class a call of `method` on `base[type_args]` was re-dispatched
    /// to under the current registry write generation, if recorded.
    // Cost: O(e * a), e = parameterizations of `base` called with `method`,
    // a = number of type arguments; no allocation.
    pub(crate) fn param_role_pun_memo_get(
        &self,
        base: Symbol,
        type_args: &[Value],
        method: Symbol,
    ) -> Option<Symbol> {
        let generation = self.registry_write_generation();
        self.caches
            .param_role_pun_memo
            .get(&(base, method))?
            .iter()
            .find(|(args, _, entry_gen)| *entry_gen == generation && same_type_objects(args, type_args))
            .map(|(_, pun, _)| *pun)
    }

    /// Replace a parameterized role type object at `stack[receiver_idx]` with
    /// the pun class a call of `method` on it was re-dispatched to before
    /// (`param_role_pun_memo_get`), so the `CallMethod`/`CallMethodMut` site
    /// dispatches to the pun directly. Without it the call walked the site's
    /// whole probe chain, fell into `call_method_with_values`, walked its
    /// probes up to the memo, and only then re-dispatched to the pun through
    /// the general path again: ~45k instructions for a trivial method against
    /// ~15k on the pun itself (#17331). The re-dispatch is exactly what the
    /// memo hit there does, so the answer is the same.
    // Cost: O(e * a), as `param_role_pun_memo_get`.
    pub(crate) fn pun_memoized_parametric_receiver(&mut self, receiver_idx: usize, method: Symbol) {
        let Some(target) = self.stack.get(receiver_idx) else {
            return;
        };
        let ValueView::ParametricRole {
            base_name,
            type_args,
        } = target.view()
        else {
            return;
        };
        if let Some(pun) = self.param_role_pun_memo_get(base_name, type_args, method) {
            self.stack[receiver_idx] = Value::package(pun);
        }
    }

    /// Record that a call of `method` on `base[type_args]` re-dispatches to
    /// the pun class `pun`. A no-op when an argument is not a plain type
    /// object. An entry of the same arguments recorded under an older
    /// generation is replaced.
    // Cost: O(e * a), as `param_role_pun_memo_get`, plus one allocation.
    pub(crate) fn param_role_pun_memo_insert(
        &mut self,
        base: Symbol,
        type_args: &[Value],
        method: Symbol,
        pun: Symbol,
    ) {
        let Some(args) = type_object_args(type_args) else {
            return;
        };
        let generation = self.registry_write_generation();
        let entries = self
            .caches
            .param_role_pun_memo
            .entry((base, method))
            .or_default();
        entries.retain(|(entry_args, _, _)| entry_args.as_ref() != args.as_slice());
        entries.push((args.into_boxed_slice(), pun, generation));
    }
}
