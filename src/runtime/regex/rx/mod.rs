//! The compiled regex engine of ADR-0135: a `RegexPattern` compiled to a flat
//! instruction program ([`RxProgram`]) and run by one backtracking loop
//! (`rx_vm`).
//!
//! Slice A (#10251) covers the regular core of the language: one-grapheme
//! atoms, zero-width assertions, groups, capture groups whose body captures
//! nothing, and greedy / frugal / ratcheted / counted quantifiers over bodies
//! that capture nothing. A pattern holding anything else is declined as a
//! whole and keeps the tree walk (ADR-0135 D5); the reason is reported under
//! `MUTSU_VM_STATS`.
//!
//! What an atom *accepts* is never restated here (ADR-0135 D4): a consuming
//! atom is tested by `match_consuming_atom` and a zero-width assertion by
//! `regex_match_atom_in_pkg`, the functions the walk itself uses. Captures go
//! into the walk's own `CapStore` through the walk's own delta builders
//! (`capture_group_delta`, `store_apply_named_capture`), so a compiled match
//! produces the captures the walk would have produced.

mod rx_atom;
mod rx_compile;
mod rx_diff;
mod rx_vm;

use crate::runtime::regex_types::{RegexAtom, RegexToken};

/// One instruction. Register operands index [`RxProgram::nregs`] registers;
/// `u32` operands are program counters or table indexes.
#[derive(Clone, Copy, Debug)]
pub(super) enum RxOp {
    /// Match the one-grapheme atom `atoms[i]` at `pos`, advancing past it.
    Atom(u32),
    /// `atoms[atom]` repeated `min..=max` times (`max == u32::MAX`: no
    /// bound): every iteration is matched up front, then the loop exits at
    /// the longest count, giving back one iteration per backtrack down to
    /// `min` — or, when `possessive` (ratchet), never giving back. The
    /// frugal form stays a `Repeat` loop.
    AtomRun {
        atom: u32,
        min: u32,
        max: u32,
        possessive: bool,
    },
    /// Test the zero-width assertion `atoms[i]` at `pos`.
    Assert(u32),
    /// `pos` is the start of the subject (a nested pattern's leading `^`).
    AssertStart,
    /// `pos` is the end of the subject (a pattern's trailing `$`).
    AssertEnd,
    /// Push a choice point resuming at `alt`, continue at `prefer`.
    Split {
        prefer: u32,
        alt: u32,
    },
    Jmp(u32),
    /// `regs[r] = pos`.
    Mark(u16),
    /// `regs[r] =` the positional-capture count of the store.
    PosBase(u16),
    /// `regs[r] =` the backtrack-stack height.
    Height(u16),
    /// Drop every choice point above `regs[r]` (ratchet).
    Cut(u16),
    /// `regs[r] = 0`.
    CtrZero(u16),
    /// `regs[r] += 1`.
    CtrInc(u16),
    /// A quantifier's loop head over counter `ctr`: below `min` iterations the
    /// body is mandatory; at `max` the loop exits; in between both are tried,
    /// the body first when `greedy`.
    Repeat {
        ctr: u16,
        min: u32,
        max: u32,
        body: u32,
        exit: u32,
        greedy: bool,
    },
    /// Close a `( … )` whose body captures nothing, opened at `regs[start]`.
    CloseCapture {
        start: u16,
    },
    /// Apply `toks[tok]`'s `$<name>=` / `$N=` alias over `regs[start]..pos`,
    /// with the positional count at token start in `regs[pos_base]`.
    Named {
        tok: u32,
        start: u16,
        pos_base: u16,
    },
    /// The empty arm of `toks[tok]`'s `?`: reserve the atom's capture slots
    /// (Nil, or an empty list under a nested list quantifier), apply the
    /// alias where the walk does, and mark nested list-quantified names.
    ZeroArm {
        tok: u32,
        pos_base: u16,
    },
    /// Mark every capture name under the quantified `toks[tok]` quantified,
    /// before its first iteration.
    QuantNames {
        tok: u32,
    },
    /// Fold the quantified `toks[tok]`'s per-iteration capture slots, pushed
    /// since the positional count `regs[pos_base]`, into one list per slot.
    Fold {
        tok: u32,
        pos_base: u16,
    },
    /// A complete match ending at `pos`.
    Match,
}

/// A compiled pattern. A pure function of the pattern (Slice A compiles no
/// subrule call), memoized in its `PatternDerived`.
pub(crate) struct RxProgram {
    pub(super) ops: Vec<RxOp>,
    pub(super) atoms: Vec<RegexAtom>,
    pub(super) toks: Vec<RegexToken>,
    pub(super) nregs: usize,
    /// Per-atom printable-ASCII acceptance sets, probed on first run (see
    /// `rx_atom`).
    pub(super) ascii: std::sync::OnceLock<Box<[u128]>>,
}

/// `MUTSU_RX_VM=off` routes every pattern back to the tree walk.
pub(super) fn rx_vm_enabled() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| !matches!(std::env::var("MUTSU_RX_VM").as_deref(), Ok("off" | "0")))
}

/// `MUTSU_RX_DIFF=1` (ADR-0135 D6) runs every compiled match through the walk
/// as well and aborts on any disagreement.
pub(super) fn rx_diff_enabled() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| matches!(std::env::var("MUTSU_RX_DIFF").as_deref(), Ok("1" | "on")))
}
