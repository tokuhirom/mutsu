//! The installing activation of an inline `CATCH`/`CONTROL` handler
//! (ADR-0072): the handler's bytecode belongs to the block that installed it,
//! but it runs at a deeper throw or `warn` site. These helpers give it that
//! block's slots and env for the run and route its writes back to them, for
//! both handler kinds (`catch_inline`, `control_inline`).
//!
//! The installing activation's live slots are the variables: a handler that
//! changes one writes it straight back there, and a write to one of its own
//! lexicals never lands in the env of the frame that threw, where a same-named
//! lexical of that frame would take it (ADR-12529 §3, slice 22).

use super::*;

/// Saved execution state for a handler run against the *installing* frame's
/// lexicals while `self.locals` belongs to the deep throw/raise site.
pub(crate) struct InstallingFrame {
    /// Absolute slot index of the installing frame's slot 0 when its live
    /// slots seeded the run, so the flush writes the handler's changes
    /// straight back into them.
    live_base: Option<usize>,
    saved_locals_base: crate::runtime::locals::CallerFrame,
    saved_upvalues: Vec<Option<Value>>,
    /// The reconstructed locals as seeded, so the flush writes back only slots
    /// the handler actually changed.
    seeded: Vec<Value>,
}

impl Interpreter {
    /// Swap `self.locals` for the locals of the frame that installed `code`,
    /// reconstructed from `env` by name (`code.locals[i]` names slot `i`).
    ///
    /// `installing_base` is the installing frame's base on the shared slot
    /// stack, when the caller knows it (a CATCH or CONTROL region records it). That
    /// frame is suspended below the throw site with its slots intact, so a
    /// non-`Nil` slot is taken from there: an env lookup by name at the throw
    /// site would find a same-named lexical of the dying routine instead.
    ///
    /// A handler's bytecode addresses the *installing* frame's slots, but at an
    /// inline run `self.locals` is the deep raise/throw site's array. `env` is the
    /// cross-frame-visible store, so it is where the installing frame's values can
    /// be found; this mirrors `reconcile_locals_from_env_at_site`. The upvalue
    /// array is cleared for the same reason: a `GetUpvalue` in the handler range
    /// must fall back to the env read rather than index the wrong frame's array.
    // Cost: O(l), l = the installing code's locals.
    pub(crate) fn enter_installing_frame(
        &mut self,
        code: &CompiledCode,
        installing: Option<(usize, usize)>,
    ) -> InstallingFrame {
        let installing_base = installing.map(|(base, _)| base);
        // The installing activation's upvalue array, saved by the call frame
        // it pushed next -- when that frame really is its own (its caller
        // base is the installing base). Without it a `GetUpvalue` in the
        // handler falls back to an env read at the throw site, where a
        // same-named lexical of the dying routine shadows the capture.
        let installing_upvalues = installing.and_then(|(base, depth)| {
            self.installing_call_frame(base, depth)
                .map(|f| f.saved_upvalues.clone())
        });
        // The installing frame lies wholly below the executing one, or its
        // region cannot be trusted (a code object that grew after install).
        let live_base =
            installing_base.filter(|&b| Self::installing_slots_live(code, b, self.locals.base()));
        let handler_locals: Vec<Value> = code
            .locals
            .iter()
            .enumerate()
            .map(|(i, name)| {
                if let Some(b) = live_base {
                    let v = &self.locals.all_slots()[b + i];
                    if !v.is_nil() {
                        return v.clone();
                    }
                }
                self.env().get(name).cloned().unwrap_or_else(|| {
                    name.strip_prefix('$')
                        .or_else(|| name.strip_prefix('@'))
                        .or_else(|| name.strip_prefix('%'))
                        .or_else(|| name.strip_prefix('&'))
                        .and_then(|bare| self.env().get(bare).cloned())
                        .unwrap_or(Value::NIL)
                })
            })
            .collect();
        let seeded = handler_locals.clone();
        // Live slots mean the handler runs on an env of its own
        // (`enter_handler_env`), whose tiers hold the installing activation's
        // locals only as of its last by-name write: its slots are the
        // variables. Show the handler's by-name reads (a `$x++` on an
        // env-synced local) the slot values, as the frame itself sees them.
        if live_base.is_some() {
            for (name, v) in code.locals_sym.iter().zip(&handler_locals) {
                // A dynamic resolves in the dynamic scope of the throw, which
                // the handler env already carries.
                if !v.is_nil()
                    && !name.with_str(str::is_empty)
                    && !name.is_dynamic_var_env_key()
                    && !name.is_frame_fixture()
                {
                    self.env_mut().insert_sym(*name, v.clone());
                }
            }
        }
        let saved_locals_base = self.locals.push_frame_from(&handler_locals);
        let saved_upvalues =
            std::mem::replace(&mut self.upvalues, installing_upvalues.unwrap_or_default());
        InstallingFrame {
            live_base,
            saved_locals_base,
            saved_upvalues,
            seeded,
        }
    }

    /// Restore the raise-site registers and flush back every slot the handler
    /// changed, so the installing frame (and any intervening by-name reader)
    /// observes the handler's writes. Only changed slots are written, to keep the
    /// blast radius minimal.
    ///
    /// When the installing activation's live slots seeded the run, a changed
    /// slot is written straight back into them, and that is the write: the
    /// by-name channels below exist to carry a value to a slot through the
    /// frames in between (each frame's return merge, then the installing
    /// frame's drain), and the drain would read that frame's env and put its
    /// stale value back over the slot once the merge no longer carries the
    /// write (ADR-12529 §3). The env is written only when it is the installing
    /// activation's own (`on_installing_env`, see `enter_installing_env`):
    /// the raise site's env belongs to the frame that raised, and a
    /// same-named lexical of that frame would take the write.
    pub(crate) fn leave_installing_frame(
        &mut self,
        code: &CompiledCode,
        st: InstallingFrame,
        on_installing_env: bool,
    ) {
        let InstallingFrame {
            live_base,
            saved_locals_base,
            saved_upvalues,
            seeded,
        } = st;
        // The flush below reads the handler's slots, so copy them out before
        // closing the frame — a frame's slots do not outlive it (ADR-0077).
        let handler_locals = self.locals.to_vec();
        self.locals.pop_frame(saved_locals_base);
        self.upvalues = saved_upvalues;
        for (i, name) in code.locals.iter().enumerate() {
            if name.is_empty() {
                continue;
            }
            if handler_locals[i] != seeded[i] {
                if let Some(b) = live_base {
                    *self.locals.absolute_slot_mut(b + i) = handler_locals[i].clone();
                    if on_installing_env {
                        self.env_mut()
                            .insert(name.clone(), handler_locals[i].clone());
                    }
                    continue;
                }
                self.env_mut()
                    .insert(name.clone(), handler_locals[i].clone());
                // The handler mutated the installing frame's lexical `name` by
                // writing `env` here. Record the name for the precise drain
                // (`apply_pending_rw_writeback`) every call site runs: drop-on-miss
                // for the same frame, plus retain-on-miss so a deeper raise site
                // carries it up to the installing frame.
                self.pending_rw_writeback_sources.push(name.clone());
                self.record_caller_var_writeback(name);
                // The value must also survive each frame exit between the raise
                // site and the installing frame (a callee whose argument is an
                // aggregate variable restores its caller's env on return and
                // would otherwise drop the write), which is what a run-time
                // named write needs.
                self.record_runtime_name_write(name);
                // This env write happened without a call opcode, so a leaf closure
                // between the raise site and the installing frame would otherwise
                // drop it on return. See `inline_control_env_writes`.
                self.inline_control_env_writes
                    .push(crate::symbol::Symbol::intern(name));
            }
        }
    }

    /// Whether the activation of `code` at slot base `base` lies wholly below
    /// the executing frame (slot base `executing_base`), so its slots are live
    /// and addressable. Not when it is the executing frame itself, or when
    /// `code` grew after the region was installed.
    // Cost: O(1).
    fn installing_slots_live(code: &CompiledCode, base: usize, executing_base: usize) -> bool {
        base + code.locals.len() <= executing_base
    }

    /// The call frame the installing activation (slot base `base`) pushed
    /// when it made its call at depth `depth`, if that is really its own.
    // Cost: O(1).
    fn installing_call_frame(&self, base: usize, depth: usize) -> Option<&crate::vm::VmCallFrame> {
        self.call_frames
            .get(depth)
            .filter(|f| f.saved_locals_base.as_ref().map(|c| c.base()) == Some(base))
    }

    /// Make the installing activation's env the executing one for an inline
    /// handler run, so the handler's by-name reads (`GetGlobal`,
    /// `GetHashVar`, ...) resolve its own lexicals: at the throw site a
    /// same-named lexical of the dying routine would shadow them. That env is
    /// the one the installing activation saved in the call frame it pushed
    /// next (`call_frames[depth]`, when its caller base proves it is that
    /// activation's). The dynamics declared on the way to the throw stay
    /// visible, copied into the handler's overlay: rakudo runs the handler in
    /// the dynamic scope of the throw. `None` when there is no such frame; the
    /// handler then runs in the throw site's env, as before.
    // Cost: O(e), e = entries in the throw site's env tiers.
    fn enter_installing_env(
        &mut self,
        base: usize,
        depth: usize,
        live_slots: bool,
    ) -> Option<HandlerEnv> {
        let installing = self.installing_call_frame(base, depth)?.saved_env.clone();
        let dynamics = self.env().tier_dynamic_entries();
        let mut env = crate::env::Env::scoped_child(installing);
        for (k, v) in &dynamics {
            env.insert_sym(*k, v.clone());
        }
        let throw_env = std::mem::replace(self.env_mut(), env);
        Some(HandlerEnv {
            throw_env,
            dynamics,
            on_installing: true,
            live_slots,
        })
    }

    /// The env an inline handler for `code` runs on, installed by the
    /// activation at slot base `base` whose next call frame is `depth`.
    ///
    /// The installing activation's own env when its call frame has it
    /// ([`Self::enter_installing_env`]). Otherwise, when that activation's
    /// slots are live below the throw site (so `leave_installing_frame`
    /// writes the handler's changes straight into them), a private overlay of
    /// the throw site's env: the handler reads through it, but a lexical it
    /// writes does not land in the env of the frame that threw, where a
    /// same-named lexical of that frame would take it. `None` leaves the
    /// handler on the throw site's env itself, as before.
    // Cost: O(e), e = entries in the throw site's env tiers.
    pub(crate) fn enter_handler_env(
        &mut self,
        code: &CompiledCode,
        (base, depth): (usize, usize),
    ) -> Option<HandlerEnv> {
        let live_slots = Self::installing_slots_live(code, base, self.locals.base());
        if let Some(st) = self.enter_installing_env(base, depth, live_slots) {
            return Some(st);
        }
        if !live_slots {
            return None;
        }
        let env = crate::env::Env::scoped_child(self.env().clone());
        let throw_env = std::mem::replace(self.env_mut(), env);
        Some(HandlerEnv {
            throw_env,
            dynamics: Vec::new(),
            on_installing: false,
            live_slots,
        })
    }

    /// Undo [`Self::enter_installing_env`] and route the handler's writes. A
    /// write to a lexical the throw site sees as the same variable also goes
    /// to the throw site's env (the by-name store the frames in between hand
    /// back up on return, as before); one the throw site shadows goes only to
    /// the installing activation's saved env. Dynamics go to the throw site.
    // Cost: O(w), w = names the handler wrote.
    pub(crate) fn leave_installing_env(
        &mut self,
        code: &CompiledCode,
        depth: usize,
        st: HandlerEnv,
    ) {
        let HandlerEnv {
            throw_env,
            dynamics,
            on_installing,
            live_slots,
        } = st;
        let handler_env = std::mem::replace(self.env_mut(), throw_env);
        let topic = crate::symbol::Symbol::intern("_");
        let bang = crate::symbol::Symbol::intern("!");
        for (k, v) in handler_env.overlay_iter() {
            if *k == topic || *k == bang || k.is_frame_fixture() {
                continue;
            }
            // On a private overlay, a write to one of the installing
            // activation's own locals already went to its slot; anything
            // else (a dynamic, a lexical it captured from further out) goes
            // to the throw site, which hands it back up on return as before.
            if !on_installing {
                if !code.locals_sym.contains(k) {
                    self.write_throw_env(*k, v.clone());
                }
                continue;
            }
            if k.is_dynamic_var_env_key() {
                if dynamics
                    .iter()
                    .any(|(dk, dv)| dk == k && dv.same_binding(v))
                {
                    continue;
                }
                self.write_throw_env(*k, v.clone());
                continue;
            }
            // "The same variable" is binding identity, never structural
            // equality: `PartialEq` walks container and object contents, so
            // two same-shaped cyclic object graphs (a tree whose nodes point
            // back at their parent) recursed until the stack overflowed (#11014).
            let installing = self.call_frames.get(depth).map(|f| &f.saved_env);
            let same_var = match (
                self.env().get_sym(*k),
                installing.and_then(|e| e.get_sym(*k)),
            ) {
                (None, _) => true,
                (Some(a), Some(b)) => a.same_binding(b),
                (Some(_), None) => false,
            };
            // A write to one of the installing activation's own locals went
            // to its live slot; on the throw site's env it would only shadow
            // the name for the frames in between, whose slot-only lexical of
            // that name the chain does not see.
            let own_live = live_slots && code.locals_sym.contains(k);
            if same_var && !own_live {
                self.write_throw_env(*k, v.clone());
            }
            if let Some(f) = self.call_frames.get_mut(depth) {
                f.saved_env.insert_sym(*k, v.clone());
            }
        }
    }

    /// A handler's write that lands on the throw site's env, where the frames
    /// in between hand it back up on return. It is logged for the carrier the
    /// throw site may be running in (`warn`, `EVAL`), so that frame
    /// reconciles its slot of that name; the handler's own run is not logged
    /// there (see [`Self::suspend_carrier_for_handler`]).
    // Cost: O(1).
    fn write_throw_env(&mut self, k: crate::symbol::Symbol, v: Value) {
        if self.carrier_writes.is_some() {
            k.with_str(|name| self.note_caller_env_write(name));
        }
        self.env_mut().insert_sym(k, v);
    }

    /// Set aside the throw site's carrier log while a handler runs on an env
    /// of its own (`enter_handler_env`): the handler's by-name writes belong to
    /// the installing activation, and logged there they would make the frame
    /// that threw reconcile its same-named slot from an env that does not hold
    /// that variable. `None` (nothing set aside) when the handler runs on the
    /// throw site's env. Undone by [`Self::resume_carrier_after_handler`].
    // Cost: O(1).
    pub(crate) fn suspend_carrier_for_handler(
        &mut self,
        own_env: bool,
    ) -> Option<Option<std::collections::HashSet<String>>> {
        own_env.then(|| self.carrier_writes.take())
    }

    // Cost: O(1).
    pub(crate) fn resume_carrier_after_handler(
        &mut self,
        saved: Option<Option<std::collections::HashSet<String>>>,
    ) {
        if let Some(log) = saved {
            self.carrier_writes = log;
        }
    }
}

/// The throw site's env, set aside while an inline handler runs in the
/// installing activation's (see `Interpreter::enter_installing_env`).
pub(crate) struct HandlerEnv {
    throw_env: crate::env::Env,
    /// The throw site's dynamics as copied into the handler's overlay, so only
    /// the ones the handler changed are written back.
    dynamics: Vec<(crate::symbol::Symbol, Value)>,
    /// Whether the handler runs on the installing activation's env, rather
    /// than on a private overlay of the throw site's (`enter_handler_env`).
    pub(crate) on_installing: bool,
    /// Whether the installing activation's slots are live below the throw
    /// site, so `leave_installing_frame` writes the handler's changes to its
    /// own locals straight into them and no env needs to carry them.
    live_slots: bool,
}
