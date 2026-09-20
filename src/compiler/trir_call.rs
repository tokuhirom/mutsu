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
    /// Record what a bare call to `name` at this arity now resolves to.
    ///
    /// `Some` when the routine just declared has a TRIR chunk, `None` when it
    /// does not — and the `None` is load-bearing, not tidiness: a later
    /// routine of the same name and arity SHADOWS the earlier one, so leaving
    /// a stale entry would statically link a call site to a body the name no
    /// longer denotes. (`{ my sub f(int $a) {...} }; sub f($a) { $a * 100 }`
    /// answered 4 instead of 300 before this.) Block scoping is handled by
    /// `LexicalScopeSnapshot`, which restores the whole map on block exit.
    pub(super) fn record_trir_routine(
        &mut self,
        name: &str,
        arity: usize,
        chunk: Option<(crate::symbol::Symbol, u64)>,
    ) {
        match chunk {
            Some(target) => {
                self.trir_routines.insert((name.to_string(), arity), target);
            }
            None => {
                self.trir_routines.remove(&(name.to_string(), arity));
            }
        }
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
            // A plain `$`-sigil user lexical only: `is_plain_lexical_name`
            // excludes the topic, twigils, dynamics and attributes, whose
            // frame slot is not the authority the ordinary read op consults.
            if !Self::is_plain_lexical_name(n) {
                return false;
            }
            // The argument must be a lexical of THIS frame. A free variable
            // (no `local_map` entry) is resolved by name at run time and has
            // no slot to read.
            let Some(&slot) = self.local_map.get(n.as_str()) else {
                return false;
            };
            arg_slots.push(slot);
        }
        // The argument-source table the `CallFunc` this replaces would have
        // carried. The post-compile analyses read it to learn that these
        // locals reach a call and so may be written back through an `is rw`
        // parameter; without it a closure over one of them captures it by
        // value and never sees the writeback.
        let arg_sources_idx = self.add_arg_sources_constant(args);
        let site_idx = self.code.trir_call_sites.len() as u32;
        self.code.trir_call_sites.push(TrCallSite {
            key,
            fingerprint,
            name: *name,
            arg_slots,
        });
        self.code.emit(OpCode::CallTrir {
            site: site_idx,
            arg_sources_idx,
        });
        true
    }
}
