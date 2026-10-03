//! A code object's `$!do`: its executable body, as `nqp::getattr` /
//! `nqp::bindattr` address it (#11207, ADR-11203).
//!
//! Rakudo keeps a routine's body in `Code.$!do`. Rebinding that attribute
//! replaces what every call of the routine runs, while the object's identity,
//! name, signature and mixins stay. Upstream NativeCall's backend-neutral path
//! relies on it:
//!
//! ```raku
//! my $do := nqp::getattr($replacement, Code, '$!do');
//! nqp::bindattr(self, Code, '$!do', $do);
//! ```
//!
//! mutsu already has one mechanism that makes every call path of a routine
//! (named call, `&f` value call, the name-keyed call caches, TRIR, the JIT)
//! run something other than its declared body: the `.wrap` chain. Rakudo's
//! `.wrap` is itself a `$!do` rebind, so a bound body is modelled as the
//! **innermost** entry of that chain, under the reserved handle
//! [`DO_BODY_HANDLE`]. Wrappers `.wrap`ped later still run around it, and
//! their `callsame` reaches it. The declared body is no longer reached, because
//! the bound body never defers to it.
//!
//! A `$!do` value is a direct code object: calling it runs that body without
//! entering any wrap chain (the `__mutsu_wrap_direct` marker `nextcallee`
//! also uses). Rakudo's `$!do` is a `ForeignCode`; mutsu's is the `Sub`/`Block`
//! itself, which is what callers do with it (call it, rename it, bind it).
//!
//! Rakudo's `Code.name` is `nqp::getcodename($!do)`: a routine and its body
//! share one name. A `$!do` copy therefore links back to the code object it
//! was copied from, and [`Interpreter::code_name_holder`] follows those links
//! (and a routine's bound body) to the one object whose name `.name` reports
//! and `set_name` / `nqp::setcodename` rewrite (#11462).

use super::*;
use crate::gc::Gc;
use crate::meta_ns::MetaNs;
use crate::value::SubData;

/// The wrap-chain handle id of a bound `$!do` body. `.wrap` handles count up
/// from 1, so 0 never names a user wrapper.
pub(crate) const DO_BODY_HANDLE: u64 = 0;

/// The routine an nqp attribute op addresses as `$!do`, when it does: the
/// attribute name is `$!do` and `obj` is a code object (a `Sub`, or a `Sub`
/// with roles mixed in, such as `$r does NativeCall::Native[...]`).
// Cost: O(1).
pub(crate) fn code_do_target(obj: &Value, name: &str) -> Option<Gc<SubData>> {
    if name != "$!do" {
        return None;
    }
    match Interpreter::unwrap_callable_mixin(obj.clone()).view() {
        ValueView::Sub(data) => Some(data.clone()),
        _ => None,
    }
}

/// The env key under which a `$!do` copy records the code object it was
/// copied from. Rakudo's `Code.name` is `nqp::getcodename($!do)`, so a routine
/// and its `$!do` share one name: renaming either renames both. mutsu's `$!do`
/// is a distinct copy, so the copy points back at the object that owns the
/// name (#11462).
const DO_OWNER_KEY: &str = "__mutsu_do_owner";

/// How many `$!do` / owner links a name lookup follows. Each link points at an
/// object that existed before the one holding it, so the links cannot form a
/// cycle; the bound only keeps a pathological chain from running long.
const NAME_LINK_LIMIT: usize = 64;

/// Whether `data` is a direct code object (runs without entering a wrap chain).
// Cost: O(1).
fn is_direct(data: &SubData) -> bool {
    matches!(
        data.env.get("__mutsu_wrap_direct").map(Value::view),
        Some(ValueView::Bool(true))
    )
}

/// `data` as a direct code object: the same body and captures, marked so a
/// call runs it without entering a wrap chain, and linked back to `data` for
/// its name. A code object that already is direct is answered as itself, so
/// `$!do` keeps its identity across a read and a bind.
// Cost: O(e), e = entries of the captured environment (a copy-on-write clone).
fn direct_code(data: &Gc<SubData>) -> Value {
    if is_direct(data) {
        return Value::sub_value(data.clone());
    }
    let mut direct = (**data).clone();
    direct
        .env
        .insert("__mutsu_wrap_direct".to_string(), Value::TRUE);
    direct
        .env
        .insert(DO_OWNER_KEY.to_string(), Value::sub_value(data.clone()));
    Value::sub_value(Gc::new(direct))
}

impl Interpreter {
    /// The wrap-chain key of routine `data`, plus its bare name for named-call
    /// dispatch. A routine already wrapped by name keeps the key its first
    /// wrap chose, because each `&foo` mention builds a fresh `Sub`. A routine
    /// redefined since then (its registration callable id changed) drops the
    /// stale chain and starts a new one under its own id.
    // Cost: O(w), w = routines currently wrapped (a scan of `wrap_sub_names`).
    pub(crate) fn routine_wrap_key(&mut self, data: &SubData) -> (u64, String) {
        let func_name = data
            .env
            .get("__mutsu_wrap_name")
            .map(Value::to_string_value)
            .unwrap_or_else(|| data.name.resolve());
        if func_name.is_empty() {
            return (data.id, func_name);
        }
        let key = MetaNs::CallableId.key_pair_for_strs(&self.current_package(), &func_name);
        let current_callable_id = self.registration_callable_id(key).or_else(|| {
            let key = MetaNs::CallableId.key_pair_for_strs("GLOBAL", &func_name);
            self.registration_callable_id(key)
        });
        let mut sub_id = data.id;
        if let Some((&old_id, _)) = self
            .dispatch
            .wrap_sub_names
            .iter()
            .find(|(_, n)| **n == func_name)
        {
            let stored_callable_id = self
                .dispatch
                .wrap_callable_ids
                .get(&func_name)
                .copied()
                .flatten();
            let same_sub = match (stored_callable_id, current_callable_id) {
                (Some(stored), Some(current)) => stored == current,
                // If we can't tell, assume same.
                _ => true,
            };
            if same_sub {
                sub_id = old_id;
            } else {
                // Redefined: clear the old wrap chain and mappings.
                crate::runtime::cow_table_mut(&mut self.dispatch.wrap_chains).remove(&old_id);
                self.dispatch.wrap_sub_names.remove(&old_id);
                self.dispatch.wrap_name_to_sub.remove(&func_name);
            }
        }
        crate::runtime::cow_table_mut(&mut self.dispatch.wrap_callable_ids)
            .insert(func_name.clone(), current_callable_id);
        (sub_id, func_name)
    }

    /// Record that the chain under `sub_id` now changes what routine
    /// `func_name` runs, so named calls dispatch through it. The name-keyed
    /// call caches bypass `wrap_chains`, so the resolution generation is
    /// bumped and the next call re-resolves.
    // Cost: O(1) amortized, plus the cache invalidation.
    pub(crate) fn note_routine_wrap_chain(
        &mut self,
        sub_id: u64,
        func_name: String,
        target: &Value,
    ) {
        self.invalidate_fn_resolution();
        if !func_name.is_empty() {
            self.dispatch
                .wrap_sub_names
                .insert(sub_id, func_name.clone());
            // Only the first Sub value for this name is kept: it carries the
            // chain's key.
            self.dispatch
                .wrap_name_to_sub
                .entry(func_name)
                .or_insert_with(|| target.clone());
        }
    }

    /// The body bound to routine `data`'s `$!do`, if one was bound. Read-only:
    /// it does not touch the wrap tables.
    // Cost: O(1) when nothing is wrapped; otherwise O(w + c), w = wrapped
    // routines, c = wrappers on this routine.
    fn bound_do_body(&self, data: &SubData) -> Option<Value> {
        if self.dispatch.wrap_chains.is_empty() {
            return None;
        }
        let name = data
            .env
            .get("__mutsu_wrap_name")
            .map(Value::to_string_value)
            .unwrap_or_else(|| data.name.resolve());
        let sub_id = self
            .dispatch
            .wrap_sub_names
            .iter()
            .find(|(_, n)| !name.is_empty() && **n == name)
            .map_or(data.id, |(&id, _)| id);
        let chain = self.dispatch.wrap_chains.get(&sub_id)?;
        chain
            .iter()
            .find(|(h, _)| *h == DO_BODY_HANDLE)
            .map(|(_, body)| body.clone())
    }

    /// `nqp::getattr($code, Code, '$!do')`: the body a call of `target` runs
    /// (the bound body if one was bound, else the declared one), as a direct
    /// code object. Read-only: it does not touch the wrap tables.
    // Cost: O(w + c + e), w = wrapped routines, c = wrappers on this routine,
    // e = entries of the captured environment.
    pub(crate) fn code_do_get(&self, data: &Gc<SubData>) -> Value {
        self.bound_do_body(data)
            .unwrap_or_else(|| direct_code(data))
    }

    /// The code object that holds `data`'s name. Rakudo answers `Code.name`
    /// from `$!do`, so the holder is found by following a routine to its
    /// bound `$!do` body, and a `$!do` copy back to the object it was copied
    /// from, until neither link applies.
    // Cost: O(l * (w + c)), l = links followed (at most `NAME_LINK_LIMIT`),
    // w = wrapped routines, c = wrappers per routine; O(1) for a code object
    // with no `$!do` history while nothing is wrapped.
    pub(crate) fn code_name_holder(&self, data: &Gc<SubData>) -> Gc<SubData> {
        let mut holder = data.clone();
        for _ in 0..NAME_LINK_LIMIT {
            let next = if is_direct(&holder) {
                holder.env.get(DO_OWNER_KEY).cloned()
            } else {
                self.bound_do_body(&holder)
            };
            let Some(next) = next else { break };
            let next = Self::unwrap_callable_mixin(next);
            let ValueView::Sub(next) = next.view() else {
                break;
            };
            holder = next.clone();
        }
        holder
    }

    /// `Code.name` of a `Sub`: the name of its [name holder](Self::code_name_holder).
    // Cost: as `code_name_holder`.
    pub(crate) fn code_name(&self, data: &Gc<SubData>) -> Symbol {
        self.code_name_holder(data).name
    }

    /// `Code.set_name` / `nqp::setcodename`: rename code object `target`
    /// together with the object that holds its name, and `target` itself when
    /// it is a `$!do` copy. A routine whose `$!do` was rebound keeps its own
    /// name field (it keys the wrap chain); its `.name` answers from the body.
    /// Returns `false` when `target` is not a code object.
    // Cost: as `code_name_holder`, plus O(n), n = chars of the new name.
    pub(crate) fn rename_code(&self, target: &Value, name: &str) -> bool {
        let ValueView::Sub(data) = target.view() else {
            return false;
        };
        let holder = self.code_name_holder(&data);
        crate::runtime::methods_sub::rename_code_object(&Value::sub_value(holder.clone()), name);
        if is_direct(&data) && !Gc::ptr_eq(&holder, &data) {
            crate::runtime::methods_sub::rename_code_object(target, name);
        }
        true
    }

    /// `nqp::bindattr($code, Code, '$!do', $body)`: every later call of
    /// `target` runs `body` with the call's arguments. It replaces any body
    /// bound earlier, and keeps the wrappers already `.wrap`ped around it.
    // Cost: O(w + c + e), as `code_do_get`.
    pub(crate) fn code_do_bind(
        &mut self,
        target: &Value,
        data: &SubData,
        body: &Value,
    ) -> Result<(), RuntimeError> {
        let body_code = Self::unwrap_callable_mixin(body.clone());
        let ValueView::Sub(body_data) = body_code.view() else {
            return Err(RuntimeError::new(format!(
                "nqp::bindattr: Code.$!do must be bound to a code object, got {}",
                crate::value::what_type_name(body)
            )));
        };
        let body = direct_code(&body_data);
        let (sub_id, func_name) = self.routine_wrap_key(data);
        let chain = crate::runtime::cow_table_mut(&mut self.dispatch.wrap_chains)
            .entry(sub_id)
            .or_default();
        chain.retain(|(h, _)| *h != DO_BODY_HANDLE);
        chain.insert(0, (DO_BODY_HANDLE, body));
        self.note_routine_wrap_chain(sub_id, func_name, target);
        Ok(())
    }

    /// The `$!do` case of the nqp attribute ops, shared by the generic op
    /// table, the VM's `NqpAttrC` site and TRIR. `None` when `obj`/`name` do
    /// not address a code object's `$!do`, and the caller's ordinary
    /// attribute body runs. A read answers the body; a bind answers `Ok(())`
    /// and the caller hands back what its op returns (the value, or the
    /// invocant for `p6bindattrinvres`).
    // Cost: O(1) when it declines; otherwise as `code_do_get` / `code_do_bind`.
    pub(crate) fn nqp_code_do_attr(
        &mut self,
        obj: &Value,
        name: &str,
        bind: Option<&Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        let data = code_do_target(obj, name)?;
        Some(match bind {
            None => Ok(self.code_do_get(&data)),
            Some(body) => self.code_do_bind(obj, &data, body).map(|()| body.clone()),
        })
    }
}
