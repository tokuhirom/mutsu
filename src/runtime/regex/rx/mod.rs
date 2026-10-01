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
//! `:my` lexical and `<{ … }>` (`DropCapture`, `CapAtom`). Slice D (#10254) adds
//! the `<subrule>` call (`Call`): a plain rule or a proto runs as a frame in the
//! run's own loop (`rx_frame`, `rx_call`), any other callee goes through the
//! walk's producer, and a ratcheted `*` / `+` over a call keeps the walk's
//! possessive scan (`NamedRun`); `~` goal matches compile too. A pattern
//! holding anything else is declined as a whole and keeps the tree walk
//! (ADR-0135 D5); the reason is reported under `MUTSU_VM_STATS`.
//!
//! What an atom *accepts* is never restated here (ADR-0135 D4): a consuming
//! atom is tested by `match_consuming_atom` and a zero-width assertion by
//! `regex_match_atom_in_pkg`, the functions the walk itself uses. Captures go
//! into the walk's own `CapStore` through the walk's own delta builders
//! (`capture_group_delta`, `store_apply_named_capture`), so a compiled match
//! produces the captures the walk would have produced.

mod rx_atom;
mod rx_call;
mod rx_capture_ops;
mod rx_compile;
mod rx_compile_compound;
mod rx_diff;
mod rx_entry;
mod rx_frame;
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
    /// Evaluate the count code of the `** { … }` quantifier `toks[tok]` where the
    /// walk does, when the quantifier is reached (`regex_repeat_count`, the
    /// walk's own), and store its bounds in `regs[min]` and `regs[max]`
    /// (`usize::MAX`: no bound). Fails when the code does.
    RepeatCount {
        tok: u32,
        min: u16,
        max: u16,
    },
    /// [`RxOp::Repeat`] with its bounds read from registers.
    RepeatDyn {
        ctr: u16,
        min: u16,
        max: u16,
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
    /// alias where the walk does, and mark nested list-quantified names, as
    /// `zero_arms[plan]` says.
    ZeroArm {
        tok: u32,
        pos_base: u16,
        plan: u32,
    },
    /// Mark every capture name in `name_sets[names]` (the names under a
    /// quantified token) quantified, before its first iteration.
    QuantNames {
        names: u32,
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
    /// collected since `regs[base]` into its capture delta; `name_sets[names]`
    /// are the names under it.
    SepEmit {
        tok: u32,
        base: u16,
        names: u32,
    },
    /// `regs[r] =` the innermost level's capture-trail length (`CapStore::mark`).
    CapMark(u16),
    /// The end of a separated quantifier whose iterations filed their (named
    /// only) captures straight into the enclosing level since the trail length
    /// `regs[base]` (ADR-10488 D3): mark quantified every name in
    /// `name_sets[names]` and every name an iteration filed under, as folding
    /// the iterations' own levels would have.
    SepNames {
        base: u16,
        names: u32,
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
    /// An iteration of the `*` / `+` over the `<subrule>` `toks[tok]` committed:
    /// run its action now when an action-driven parse reads a `$*` variable that
    /// action may write (`maybe_run_reduce_time_dynvar_action`, the walk's own;
    /// a no-op for every other grammar).
    ReduceAction {
        tok: u32,
    },
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
    /// A call-out: run the `$( … )` / `@( … )` atom `atoms[i]`, whose code
    /// yields a pattern (or a list of them), and enter the ends it matches at
    /// `pos` highest priority first, each with its capture delta
    /// (`regex_code_interp_ends`, the walk's own). The lower-priority ends wait
    /// on the backtrack stack as one choice point.
    InterpEnds(u32),
    /// A `<subrule>` call, `atoms[atom]` (ADR-0135 D3). The callee is resolved
    /// when the call is reached: a plain rule whose program exists runs as an
    /// [`rx_frame::Frame`] in this same loop, and its ends are entered one at a
    /// time as it returns; any other callee (a proto, one with arguments, a
    /// left-recursive one, a rule of the walk) is asked for its ends by the
    /// walk's own producer (`regex_match_atom_all_with_capture_opts`) and they
    /// are entered highest priority first. `commit` (the call's token is
    /// ratcheted) drops every other end the moment the first one is entered.
    Call {
        atom: u32,
        commit: bool,
    },
    /// The ratcheted `*` (`min == 0`) / `+` (`min == 1`) of the `<subrule>`
    /// `atoms[atom]` as one possessive scan, when the walk's fast path applies
    /// to the call (`regex_named_ratchet_run`, the walk's own): the scan's
    /// captures are merged and the pc jumps to `skip`, past the general loop
    /// that follows. When it does not apply, the general loop runs. Fails when
    /// the scan matched fewer than `min` iterations.
    NamedRun {
        atom: u32,
        min: u32,
        skip: u32,
    },
    /// The end of a `~` goal match's goal, which ran in a capture level of its
    /// own after the inner pattern's (`Collect`ed since `regs[base]`): close it
    /// and merge both levels' captures into the enclosing one, the goal's first
    /// (the order the walk's `GoalMatch` arm merges them).
    GoalEnd {
        base: u16,
    },
    /// The goal matched: the failure handler the inner pattern's end pushed at
    /// `regs[height]` is not needed (the choice points above it, the goal's own
    /// other ends, stay).
    GoalOk {
        height: u16,
    },
    /// The goal found no match after an end of the inner pattern: record the
    /// failure (`record_goal_failure`) for the "expected goal" report, and fail.
    GoalFail {
        tok: u32,
    },
    /// A complete match ending at `pos`; in a callee frame, the return.
    Match,
}

/// A compiled pattern. A pure function of the pattern (a `<subrule>` compiles to
/// a call carrying its atom, resolved when the call is reached, so no program is
/// keyed by package), memoized in its `PatternDerived`.
pub(crate) struct RxProgram {
    pub(super) ops: Vec<RxOp>,
    pub(super) atoms: Vec<RegexAtom>,
    /// Per atom: whether it is tested under `:i` (its pattern level's flag).
    pub(super) atom_ic: Vec<bool>,
    pub(super) toks: Vec<RegexToken>,
    /// One per `||`: its shared positional width and list-valued names.
    pub(super) alts: Vec<super::regex_helpers::AlternationListFlags>,
    /// The capture-name sets `QuantNames`, `SepEmit` and `SepNames` mark,
    /// interned once when the pattern compiles: they are a function of the
    /// pattern, which the walk recomputes (as strings) at every execution.
    pub(super) name_sets: Vec<Box<[crate::symbol::Symbol]>>,
    /// One per `ZeroArm`: what the empty arm of a `?` writes.
    pub(super) zero_arms: Vec<ZeroArmPlan>,
    pub(super) nregs: usize,
    /// Whether the program runs Raku code or reads an in-regex lexical (a
    /// `Code`, `VarDecl` or `<{ … }>` op, or a `$x` interpolation). The
    /// position-only matcher treats code atoms as inert and has no lexicals to
    /// read, so it must not run a program that has any.
    pub(super) has_code: bool,
    /// Whether the program holds a `Call` op, so a run of it may switch frames.
    pub(super) has_call: bool,
    /// One per `|`: its token (in `toks`) and each branch's first op.
    pub(super) ltm_alts: Vec<LtmAltTable>,
    /// Per-atom printable-ASCII acceptance sets, probed on first run (see
    /// `rx_atom`).
    pub(super) ascii: std::sync::OnceLock<Box<[Option<u128>]>>,
}

/// What the empty arm of a `?` writes, computed when the pattern compiles.
pub(super) struct ZeroArmPlan {
    /// The atom's capture slots, each Nil or (`true`) an empty list
    /// (`capture_group_list_flags`).
    pub(super) flags: Box<[bool]>,
    /// The names under a nested list quantifier, which render as empty lists
    /// (`collect_nested_list_quantified_names`).
    pub(super) list_names: Box<[crate::symbol::Symbol]>,
    /// Whether the token's own alias is applied over the empty span.
    pub(super) named_zero_capture: bool,
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
