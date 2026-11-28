//! A per-call cache of a routine-nested sub's free-variable bindings
//! (mutsu#17971).
//!
//! `sub tok($html) { my int $len = $html.chars; my sub scan($i) { $i < $len } }`:
//! `scan` reads `$len` through `GetGlobal`, and every read used to walk the
//! whole by-name resolution chain -- the rw-return and atomic lanes, the
//! loop-parameter test, the per-call-state / escaping-`our` / package-block
//! stores, and inside `get_env_with_main_alias_sym` the process-dynamic,
//! unmerged-package and routine-scope probes -- before `unit_lexical_slot`
//! reached the sub's alias cell (`vm_lexsub_aliases.rs`). That was ~2k
//! instructions per read; a character scanner reads two such variables per
//! character.
//!
//! The binding a read resolves to is fixed for the call: the alias names the
//! cell of the declaring activation, which the declaration bound before the
//! sub could run, and the probes ahead of it read state that is either
//! fixed for the call or re-tested on every cached read. So the first read
//! of each such name in a call records the cell, and later reads of that
//! call load it directly.
//!
//! **Filling** (`lexsub_free_cache_fill`) happens after the full chain
//! answered, and only when the answer provably was the alias:
//! - the running frame is a routine-nested sub with aliases, not a block;
//! - the name is a plain lexical (no sigil, twigil or qualifier) the running
//!   code never rebinds (`free_var_rebinds`);
//! - the frame was not called through a code object holding its own captures
//!   (`frame_capture_holds_name`), where `unit_lexical_slot` declines the
//!   alias;
//! - the per-read guards below hold;
//! - the alias binding's content (a shared cell's value, or the plain value
//!   an alias holds for a read-only parameter) is, bit for bit, the value
//!   the chain produced.
//!
//! **Reading** (`lexsub_free_cache_read`) re-tests, every time, each probe of
//! the chain whose answer can change during a call: an rw-return context, an
//! atomic variable of the name, a sigilless attribute alias, an escaping
//! `our` capture, and a `for` parameter of the name that is now running. A
//! cached Nil or lazy thunk is not served (the chain's default and force
//! steps own those).
//!
//! **Mainline and bare-block subs** (ADR-0024) are served the same way: their
//! free variable's binding is the cell captured into the routine's
//! `unit_lexicals` bucket, also fixed for the call (a `:=` seats the new
//! binding inside that cell rather than replacing it). ADR-18244 stage B.
//!
//! Entries are kept per frame, in call order: frames are told apart by their
//! invocation id, which every call path draws fresh, so an entry can never
//! answer for a later call, and a caller's entries survive the calls it makes.
//! Filling for a frame first drops the entries of frames that returned (one
//! whose routine-stack index now holds another invocation). A name's first
//! read in a call only leaves a placeholder; its second read fills it.

use super::*;

/// `LexicalState::lexsub_free_cache`: (frame invocation id, the frame's index
/// in the routine stack, name, binding cell). The index makes "is this
/// frame still live" one comparison.
pub(crate) type LexSubFreeCache = Vec<(u64, u32, Symbol, LexSubFreeSlot)>;

/// What a [`LexSubFreeCache`] entry knows about its name in its frame.
#[derive(Clone)]
pub(crate) enum LexSubFreeSlot {
    /// Read once: the next read resolves and fills it.
    Pending,
    /// The chain's answer is not a binding fixed for the call (a module
    /// file-scope store, a capture the frame's code object holds): every
    /// read takes the chain, without trying to fill again.
    Never,
    /// The binding: a shared cell, read through, or a plain read-only value.
    Bound(Value),
}

/// Entries kept per frame; a sub with more free variables reads the rest the
/// long way.
const LEXSUB_FREE_CACHE_CAP: usize = 32;

impl Interpreter {
    /// The probes of `GetGlobal`'s chain that run ahead of the alias and whose
    /// answer can change while one call runs. All must decline for a cached
    /// binding to stand.
    // Cost: O(l), l = names of the running `for` parameters.
    #[inline]
    pub(super) fn lexsub_free_cache_guards_hold(&self, name: &str) -> bool {
        !self.rw_return_context
            && !(self.atomic_var_seen() && Self::atomic_name_possible(name))
            && !self.threads.sigilless_attrs_active
            && self.lexicals.escaping_our_lexical_names.is_empty()
            && !self.topic_state.name_bound_by_loop_param(name)
    }

    /// The value of the free variable `name_sym` this call's first read
    /// resolved to its alias cell, read from the cell. `None` when nothing is
    /// cached for the running frame or a guard fails: the caller then takes
    /// the full chain.
    // Cost: O(c + l), c = the running frame's cached entries plus any a
    // returned callee left above them (at most 32), l = as the guards.
    #[inline]
    pub(super) fn lexsub_free_cache_read(&self, name: &str, name_sym: Symbol) -> Option<Value> {
        if self.lexicals.lexsub_free_cache.is_empty() {
            return None;
        }
        let frame = self.routine_stack().last()?;
        if frame.is_block {
            return None;
        }
        let id = frame.invocation_id;
        // Entries are in call order (a callee's invocation id is larger than
        // its caller's): this frame's are at the top, above them only a
        // returned callee's leftovers.
        let (_, _, _, LexSubFreeSlot::Bound(cell)) = self
            .lexicals
            .lexsub_free_cache
            .iter()
            .rev()
            .take_while(|(owner, _, _, _)| *owner >= id)
            .find(|(owner, _, sym, _)| *owner == id && *sym == name_sym)?
        else {
            return None;
        };
        if !self.lexsub_free_cache_guards_hold(name) {
            return None;
        }
        let val = cell.deref_clone();
        if val.is_nil() || val.is_lazy_thunk_value() {
            return None;
        }
        Some(val)
    }

    /// Record the alias cell of the free variable `name_sym`, after the full
    /// chain answered `val` for it, when the module docs' conditions show the
    /// answer was that cell.
    // Cost: O(1) when no routine captures an outer lexical; otherwise the
    // alias lookup, O(a + d), a = the sub's aliases, d = env chain depth, or
    // the bucket lookup, O(o), o = same-named routines that captured.
    pub(super) fn lexsub_free_cache_fill(
        &mut self,
        code: &CompiledCode,
        name: &str,
        name_sym: Symbol,
        val: &Value,
    ) {
        if (self.lexicals.lexsub_free_aliases.is_empty()
            && self.lexicals.mainline_lexical_subs.is_empty())
            || !crate::env::is_plain_user_lexical(name)
            || name.starts_with(['$', '@', '%', '&', '*', '!', '.', '^', '?'])
            || name == "_"
            || crate::qualified::is_qualified(name_sym)
            || code.free_var_rebinds.contains(&name_sym)
        {
            return;
        }
        let Some(frame) = self.routine_stack().last() else {
            return;
        };
        if frame.is_block {
            return;
        }
        // Only a frame whose free variables have a call-fixed binding: a
        // routine-nested sub with aliases, or a mainline / bare-block sub that
        // captured into an ADR-0024 bucket. Cost: two hash probes.
        let lexsub = self.lexicals.lexsub_free_aliases.contains_key(&frame.name);
        if !lexsub
            && (frame.package != crate::symbol::wk::global_package()
                || !self
                    .lexicals
                    .mainline_lexical_subs
                    .contains_key(frame.name.as_str()))
        {
            return;
        }
        let id = frame.invocation_id;
        // The first read of a name in a call only marks it (`Pending`, which
        // `lexsub_free_cache_read` never serves): the
        // lookup below costs about what one more chain walk does, so a name a
        // call reads once is not worth it. The second read fills the entry.
        let depth = self.routine_stack().len() - 1;
        // Drop the entries of frames that returned: a callee's (a larger id)
        // and an earlier sibling call's (a smaller id, but no longer at its
        // routine-stack index). A live caller's stay for when it runs again.
        while let Some(&(owner, at, _, _)) = self.lexicals.lexsub_free_cache.last() {
            let live = owner <= id
                && self
                    .routine_stack()
                    .get(at as usize)
                    .is_some_and(|f| f.invocation_id == owner);
            if live {
                break;
            }
            self.lexicals.lexsub_free_cache.pop();
        }
        let cache = &mut self.lexicals.lexsub_free_cache;
        let top = cache
            .iter()
            .rposition(|(owner, _, sym, _)| *owner != id || *sym == name_sym)
            .filter(|&i| cache[i].0 == id);
        let slot = match top {
            None => {
                if cache.len() < LEXSUB_FREE_CACHE_CAP {
                    cache.push((id, depth as u32, name_sym, LexSubFreeSlot::Pending));
                }
                return;
            }
            Some(i) if matches!(cache[i].3, LexSubFreeSlot::Pending) => i,
            Some(_) => return,
        };
        // A guard that fails now may hold on the next read: stay pending.
        if !self.lexsub_free_cache_guards_hold(name) {
            return;
        }
        let bound = self.lexsub_free_binding(code, name, name_sym, val, lexsub);
        if let Some(entry) = self.lexicals.lexsub_free_cache.get_mut(slot) {
            entry.3 = match bound {
                Some(cell) => LexSubFreeSlot::Bound(cell),
                None => LexSubFreeSlot::Never,
            };
        }
    }

    /// The binding the full chain just answered `val` from, when it is one
    /// fixed for the running call (see the module docs); `None` otherwise.
    // Cost: the alias lookup, O(a + d), a = the sub's aliases, d = env chain
    // depth, or the bucket lookup, O(o), o = same-named routines that captured.
    fn lexsub_free_binding(
        &self,
        code: &CompiledCode,
        name: &str,
        name_sym: Symbol,
        val: &Value,
        lexsub: bool,
    ) -> Option<Value> {
        if self.env().frame_capture_holds_name(name, Some(name_sym)) {
            return None;
        }
        // The binding the chain answered from: a routine-nested sub's alias
        // (mutsu#9111), or the cell a mainline / bare-block sub captured into
        // its ADR-0024 bucket. Either is fixed for the call; a module's
        // file-scope store is not (a `:=` there replaces the cell).
        let cell = if lexsub {
            self.lexsub_alias_slot_sym(name, Some(name_sym))?.clone()
        } else {
            let cell = self
                .active_unit_lexical_entry()
                .and_then(|(_, cells)| cells.get(name).cloned())?;
            // A never-written capture is held as its plain value; that is the
            // binding itself, as for a routine-nested sub's read-only alias,
            // unless this body writes the name (which a cell would carry).
            if !cell.is_container_ref() && code.free_var_writes.contains(&name_sym) {
                return None;
            }
            cell
        };
        // A shared cell (a variable the declaring frame boxed) is read through
        // on every hit; a plain value (a read-only parameter the alias holds
        // as is) is the binding itself.
        cell.deref_clone().same_binding(val).then_some(cell)
    }
}
