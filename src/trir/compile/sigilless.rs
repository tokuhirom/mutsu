//! Whether a sigilless parameter may bind its argument's value at entry.
//!
//! A sigilless parameter (`\text`) binds the caller's CONTAINER when handed a
//! variable, so a read of it sees every later write to that variable, even
//! one the callee makes indirectly (`sub f(\x) { $g = 5; x }; f($g)` is 5).
//! A TRIR slot holds a value, which is why the doors declined a variable
//! argument outright. But the two bindings differ only if the variable can
//! change between entry and a read, and the body is in hand: when every read
//! runs before the first op that could run foreign code or write anything
//! but this frame's own slots, and no jump can bring control back above a
//! read, the value at entry IS the value every read sees. `str-escape(\text)`
//! reads `text` once, as its first op (`text.NFD`), and is called with `$_`.

use super::TrirCompiler;
use crate::trir::TrOp;

impl TrirCompiler<'_> {
    /// Set [`crate::trir::TrParam::value_at_entry`] on each sigilless
    /// parameter whose reads all precede the body's first effect.
    ///
    /// Runs on the unoptimized ops, before the peephole pass, which only
    /// fuses and threads ops in place and so moves no read past an effect.
    // Cost: O(n + r·p), n = ops, r = sigilless reads, p = parameters.
    pub(super) fn mark_sigilless_value_at_entry(&mut self) {
        if self.sigilless_reads.is_empty() || !self.params.iter().any(|p| p.sigilless) {
            return;
        }
        let first_effect = self
            .ops
            .iter()
            .position(|op| !reads_only_own_frame(op))
            .unwrap_or(self.ops.len());
        let lowest_target = self
            .ops
            .iter()
            .filter_map(TrOp::target)
            .min()
            .map_or(usize::MAX, |t| t as usize);
        for p in self.params.iter_mut().filter(|p| p.sigilless) {
            let mut reads = self.sigilless_reads.iter().filter(|(s, _)| *s == p.slot);
            p.value_at_entry = reads.all(|&(_, at)| {
                at < first_effect && (lowest_target == usize::MAX || at < lowest_target)
            });
        }
    }
}

/// An op that reads and writes nothing but this frame's slots, its banks and
/// its constants, and runs no code of anyone else's. Deliberately short: an
/// op left off only makes `value_at_entry` rarer.
// Cost: O(1).
fn reads_only_own_frame(op: &TrOp) -> bool {
    matches!(
        op,
        TrOp::ConstI(_)
            | TrOp::ConstObj(_)
            | TrOp::LoadI(_)
            | TrOp::StoreI(_)
            | TrOp::LoadObj(_)
            | TrOp::StoreObj(_)
            | TrOp::DupObj
            | TrOp::PopObj
            | TrOp::PopI
            | TrOp::BoxI
            | TrOp::BoxN
    )
}
