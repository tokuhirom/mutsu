//! Which local slots a `++`/`--` may step as a bare native `int` (ADR-18357
//! D1, stage 1).
//!
//! `my int $p; $p++` runs the generic scalar `++`/`--` op, which asks, on
//! every step, everything a slot-held scalar *could* be:
//! - a readonly binding, through the readonly registry and the free-variable
//!   decision, by name;
//! - a shared cell or `Proxy` parked in the env under the name;
//! - a typed lexical whose store is not the identity;
//! - a `state` slot to publish;
//! - an env mirror to keep in sync.
//!
//! For a `my int` that this chunk declares itself, the compiler has already
//! settled each of these. Such a slot is never a parameter, a free variable or
//! a closure-written name (`fresh_local`), never readonly, never `state`, and
//! nothing reads it by name (`needs_env_sync`). Its declared constraint is
//! the native `int` itself, whose in-range store is the identity.
//!
//! What only the running program can decide stays a per-step check in the op:
//! the slot's tag (a NaN-boxed `Int`), overflow (a native `int` wraps where
//! the generic op knows how), and the process-wide store spoilers
//! (`set_local_scalar_fast_metadata_clear_except_type`). When any of them
//! fails, the op runs the generic body as before (ADR-18357 D3).
//!
//! The proof is a per-chunk bit read by the existing `PostIncrement` /
//! `PreIncrement` / ... arms rather than a separate opcode: the narrow form
//! replaces one op one-for-one, so every op-scanning analysis keeps seeing the
//! op it already understands (ADR-18357 D1).

use crate::binding_desc::DeclaredConstraint;
use crate::opcode::CompiledCode;

/// Whether the narrow forms are enabled at all: `MUTSU_NARROW_OPS=0` turns
/// every one of them off, so a parity test can run the same program through
/// the generic ops and compare (ADR-18357 D6). Read once per process.
// Cost: O(1) after the first call.
pub(crate) fn narrow_ops_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var("MUTSU_NARROW_OPS").ok().is_none_or(|v| v != "0"))
}

impl CompiledCode {
    /// Whether a `++`/`--` of slot `idx` may step it as a bare native `int`
    /// (see the module doc). The static half only: the op still checks the
    /// slot's tag, overflow and the store spoilers before it takes the
    /// narrow arm.
    // Cost: O(1) after the first call per chunk (a `OnceLock` acquire and an
    // indexed load); the first call is O(l + s), l = locals, s = `state` slots.
    #[inline]
    pub(crate) fn native_int_step_slot(&self, idx: usize) -> bool {
        let slots = self
            .native_int_step_slots
            .get_or_init(|| self.compute_native_int_step_slots());
        slots.get(idx).copied().unwrap_or(false)
    }

    // Cost: O(l + s), l = locals, s = `state` slots; once per chunk.
    fn compute_native_int_step_slots(&self) -> Box<[bool]> {
        if self.needs_reflective_capture || !narrow_ops_enabled() {
            return vec![false; self.locals.len()].into_boxed_slice();
        }
        (0..self.locals.len())
            .map(|i| {
                let Some(desc) = self.binding_descs.get(i) else {
                    return false;
                };
                desc.declared_constraint == DeclaredConstraint::NativeInt
                    && desc.fresh_local
                    && desc.flags.simple_scalar_local()
                    && !self.needs_env_sync.get(i).copied().unwrap_or(true)
                    && self.local_read_plain(i)
                    // A plain `$name`: no twigil (`$*x`, `$?x`, `$^x`, ...).
                    && self.locals[i]
                        .as_bytes()
                        .first()
                        .is_some_and(|b| b.is_ascii_alphabetic() || *b == b'_')
                    && !self.state_locals.iter().any(|(s, _)| *s == i)
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::binding_desc::{BindingDesc, BindingFlags, DeclaredConstraint};
    use crate::opcode::CompiledCode;

    fn chunk_with(desc: BindingDesc, name: &str, env_sync: bool) -> CompiledCode {
        let mut code = CompiledCode::new();
        code.locals.push(name.to_string());
        code.binding_descs.push(desc);
        code.needs_env_sync.push(env_sync);
        code
    }

    fn native_fresh() -> BindingDesc {
        let mut desc = BindingDesc::new(BindingFlags::new(true, true));
        desc.declared_constraint = DeclaredConstraint::NativeInt;
        desc.fresh_local = true;
        desc
    }

    #[test]
    fn a_fresh_native_int_local_steps_narrowly() {
        assert!(chunk_with(native_fresh(), "p", false).native_int_step_slot(0));
    }

    #[test]
    fn every_unproven_shape_takes_the_generic_op() {
        // Read by name from the env.
        assert!(!chunk_with(native_fresh(), "p", true).native_int_step_slot(0));
        // A parameter or a free variable.
        let mut desc = native_fresh();
        desc.fresh_local = false;
        assert!(!chunk_with(desc, "p", false).native_int_step_slot(0));
        // An untyped or boxed `Int` slot.
        let mut desc = native_fresh();
        desc.declared_constraint = DeclaredConstraint::Untyped;
        assert!(!chunk_with(desc, "p", false).native_int_step_slot(0));
        // A dynamic variable.
        assert!(!chunk_with(native_fresh(), "*p", false).native_int_step_slot(0));
        // A `state` slot.
        let mut code = chunk_with(native_fresh(), "p", false);
        code.state_locals
            .push((0, crate::symbol::Symbol::intern("__state_p")));
        assert!(!code.native_int_step_slot(0));
        // Out of range.
        assert!(!chunk_with(native_fresh(), "p", false).native_int_step_slot(1));
    }
}
