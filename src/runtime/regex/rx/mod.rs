//! The compiled regex engine of ADR-0135: a `RegexPattern` compiled to a flat
//! instruction program ([`RxProgram`]) and run by one backtracking loop
//! (`rx_vm`).
//!
//! Slice A (#10251) covers the regular core of the language: one-grapheme
//! atoms, zero-width assertions, groups, capture groups (nested ones in
//! capture levels of their own, `rx_levels`), aliases, backreferences, the
//! `<(` / `)>` markers, sequential alternation (`||`), and greedy / frugal /
//! ratcheted / counted / separated quantifiers over any of these. Slice B
//! (#10252) adds `|` alternation, ranked by the walk's LTM key (`rx_ltm`),
//! lookaround, `:i` / `:m` and `&`. Slice C (#10253) adds the call-out atoms
//! that run Raku code on the caller's interpreter: `{ … }`, `<?{ … }>`,
//! `<!{ … }>` and `:my` declarations (`Code`, `VarDecl`), then the
//! interpolation atoms: a capture-isolated group (`<$rx>`), `$x` of an in-regex
//! `:my` lexical and `<{ … }>` (`DropCapture`, `CapAtom`). A pattern holding
//! anything else is declined as a whole and keeps the tree walk (ADR-0135
//! D5); the reason is reported under `MUTSU_VM_STATS`.
//!
//! What an atom *accepts* is never restated here (ADR-0135 D4): a consuming
//! atom is tested by `match_consuming_atom` and a zero-width assertion by
//! `regex_match_atom_in_pkg`, the functions the walk itself uses. Captures go
//! into the walk's own `CapStore` through the walk's own delta builders
//! (`capture_group_delta`, `store_apply_named_capture`), so a compiled match
//! produces the captures the walk would have produced.

mod rx_atom;
mod rx_capture_ops;
mod rx_compile;
mod rx_compile_compound;
mod rx_diff;
mod rx_levels;
mod rx_ltm;
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
    /// The end of one iteration of a nullable loop body that began at
    /// `regs[start]`: fail when it consumed nothing and, after `regs[ctr]`
    /// iterations, such an iteration no longer counts toward `min..=max`
    /// (`max == u32::MAX`: no bound) — the walk's `zero_width_iter_counts`.
    ZeroIter {
        ctr: u16,
        start: u16,
        min: u32,
        max: u32,
    },
    /// `pos` has moved past `regs[start]` (a separated quantifier's step).
    Advanced {
        start: u16,
    },
    /// `regs[ctr] >= min` (a separated quantifier's minimum count).
    AtLeast {
        ctr: u16,
        min: u32,
    },
    /// Open a capture level for a `( … )` whose body captures (`rx_levels`).
    OpenCapture,
    /// Open a capture level for a capture-isolated group (`<$rx>`): a regex of
    /// its own, so it inherits none of the enclosing level's `:my` lexicals.
    OpenIsolated,
    /// Close a `( … )` opened at `regs[start]`: its captures are the level
    /// `OpenCapture` opened when `nested`, else none.
    CloseCapture {
        start: u16,
        nested: bool,
    },
    /// Close the capture level `OpenCapture` opened for a capture-isolated
    /// group (`<$rx>`) and drop its captures.
    DropCapture,
    /// Match `atoms[i]`, whose match reads or writes captures (a
    /// backreference, a `<(` / `)>` marker) or runs a nested pattern (a
    /// lookaround), through the walk's own single-candidate matcher, and
    /// merge the capture delta it returns.
    CapAtom(u32),
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
    /// `regs[r] =` the number of collected separated-quantifier iterations.
    SepBase(u16),
    /// Close the innermost capture level as one collected iteration: a
    /// separator's when `sep`, else an atom's.
    Collect {
        sep: bool,
    },
    /// The end of the separated quantifier `toks[tok]`: fold the iterations
    /// collected since `regs[base]` into its capture delta.
    SepEmit {
        tok: u32,
        base: u16,
    },
    /// The end of a `&` conjunction `toks[tok]` whose first branch ran in
    /// the capture level opened at `regs[start]`: every other branch must
    /// match exactly `regs[start]..pos` (a nested run of its own program);
    /// all branches' captures then merge into the enclosing level.
    ConjTail {
        tok: u32,
        start: u16,
    },
    /// A `|` alternation, `ltm_alts[i]`: rank its branches at `pos` by the
    /// walk's LTM key and enter them best first, each lower-ranked one only
    /// when everything above it has failed (ADR-0135 D4).
    LtmAlt(u32),
    /// The end of one `||` branch: pad the alternation `alts[alt]`'s
    /// positional slot space past what the branch took since `regs[pos_base]`
    /// (unless `suppress_padding`), and mark its list-valued names quantified.
    AltTail {
        alt: u32,
        pos_base: u16,
        suppress_padding: bool,
    },
    /// A call-out: run the `{ … }` block, `<?{ … }>` or `<!{ … }>` assertion
    /// `atoms[i]` on the caller's interpreter and merge the capture delta it
    /// returns (`regex_code_atom`, the walk's own). Fails when an assertion
    /// fails or the block dies.
    Code(u32),
    /// A call-out: run the `:my` / `:our` / `:temp` / `:let` declaration
    /// `atoms[i]` and merge the lexicals it declared (`regex_var_decl_atom`).
    VarDecl(u32),
    /// A complete match ending at `pos`.
    Match,
}

/// A compiled pattern. A pure function of the pattern (Slice A compiles no
/// subrule call), memoized in its `PatternDerived`.
pub(crate) struct RxProgram {
    pub(super) ops: Vec<RxOp>,
    pub(super) atoms: Vec<RegexAtom>,
    /// Per atom: whether it is tested under `:i` (its pattern level's flag).
    pub(super) atom_ic: Vec<bool>,
    pub(super) toks: Vec<RegexToken>,
    /// One per `||`: its shared positional width and list-valued names.
    pub(super) alts: Vec<super::regex_helpers::AlternationListFlags>,
    pub(super) nregs: usize,
    /// Whether the program runs Raku code or reads an in-regex lexical (a
    /// `Code`, `VarDecl` or `<{ … }>` op, or a `$x` interpolation). The
    /// position-only matcher treats code atoms as inert and has no lexicals to
    /// read, so it must not run a program that has any.
    pub(super) has_code: bool,
    /// One per `|`: its token (in `toks`) and each branch's first op.
    pub(super) ltm_alts: Vec<LtmAltTable>,
    /// Per-atom printable-ASCII acceptance sets, probed on first run (see
    /// `rx_atom`).
    pub(super) ascii: std::sync::OnceLock<Box<[Option<u128>]>>,
}

/// A compiled `|`: the alternation token (`toks[tok]`, whose atom holds the
/// branches the LTM ranking measures) and the pc each branch starts at.
pub(super) struct LtmAltTable {
    pub(super) tok: u32,
    pub(super) pcs: Box<[u32]>,
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
