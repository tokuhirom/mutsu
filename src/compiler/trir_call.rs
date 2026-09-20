//! Emitting [`OpCode::CallTrir`] — ADR-0110 §3.3's static call linkage.
//!
//! A call is compiled to the direct form only when every one of these is a
//! compile-time fact: the callee is a routine THIS compile already registered
//! with a TRIR chunk, the arity matches its positional signature, and every
//! argument is a plain lexical of the caller's own frame. Anything else
//! compiles exactly as it did before, so the emit is purely additive.
//!
//! The runtime still re-checks that the callee's fingerprint is the one that
//! was resolved here (`TrCallSite::fingerprint`); a routine that has since
//! been replaced takes the cold by-name fallback rather than the wrong body.

use super::*;
use crate::trir::TrCallSite;

impl Compiler {
    /// Record that `key` names a routine with a TRIR chunk, so a later call
    /// site can resolve it. Called from the single site that attaches a chunk.
    pub(super) fn record_trir_routine(
        &mut self,
        name: &str,
        arity: usize,
        key: crate::symbol::Symbol,
        fingerprint: u64,
    ) {
        self.trir_routines
            .insert((name.to_string(), arity), (key, fingerprint));
    }

    /// Emit the direct call, answering whether it was emitted.
    pub(super) fn try_compile_trir_direct_call(
        &mut self,
        name: &crate::symbol::Symbol,
        args: &[Expr],
    ) -> bool {
        if self.trir_routines.is_empty() || args.len() > u8::MAX as usize {
            return false;
        }
        let Some(&(key, fingerprint)) =
            name.with_str(|n| self.trir_routines.get(&(n.to_string(), args.len())))
        else {
            return false;
        };
        let mut arg_slots = Vec::with_capacity(args.len());
        for a in args {
            let Expr::Var(n) = a else { return false };
            // The argument must be a plain lexical of THIS frame. A free
            // variable (no `local_map` entry) is resolved by name at run time
            // and has no slot to read.
            let Some(&slot) = self.local_map.get(n.as_str()) else {
                return false;
            };
            arg_slots.push(slot);
        }
        let site_idx = self.code.trir_call_sites.len() as u32;
        self.code.trir_call_sites.push(TrCallSite {
            key,
            fingerprint,
            name: *name,
            arg_slots,
        });
        self.code.emit(OpCode::CallTrir(site_idx));
        true
    }
}
