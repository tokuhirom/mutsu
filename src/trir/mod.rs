//! TRIR — the typed, resolved IR of [ADR-0110](../../docs/adr/0110-typed-resolved-ir-for-statically-typed-routines.md).
//!
//! A routine whose variables, types and callees are statically known is
//! compiled a second time, from its own AST, into this instruction set. Where
//! the ordinary bytecode carries a type as the *string* `"int"`, a variable as
//! a *name*, and a callee as a *name*, a [`TrChunk`] carries a slot index, a
//! slot kind settled at compile time, and (Stage 2) a resolved callee. That is
//! the whole of the difference, and per ADR-0110 §1.3 it is the whole of the
//! ~70x gap on `JSON::Fast`: mutsu emits about the right *number* of
//! operations and pays ~211 ns for each one re-discovering what the compiler
//! already knew.
//!
//! # Two banks, no raw words on the `Value` stack
//!
//! ADR-0110 §3.2 describes native operands as raw words sharing the untyped
//! operand stack, with a debug-build stack-kind verifier as the soundness
//! gate. This implementation keeps the *typing* and drops the sharing: native
//! `int`/`num` operands live in their own `Vec<i64>` bank
//! ([`exec::TrScratch::ints`]), boxed operands in the interpreter's own
//! `Vec<Value>` stack and frame. Nothing ever reads a raw word as a `Value`,
//! because no raw word is ever stored where a `Value` lives — the top risk in
//! ADR-0110 §5 is removed structurally rather than contained by a verifier,
//! which is what CLAUDE.md's definition of risk ("a mechanism that cannot go
//! flaky" over "an optimization correct only under a static analysis") asks
//! for. GC needs no change either: the int bank holds no references, and
//! boxed slots stay in [`crate::runtime::locals::Locals`], which is already a
//! root. The deviation is recorded in ADR-0110's implementation status.
//!
//! # What is NOT here
//!
//! TRIR is not a second interpreter for Raku. It executes only operations
//! whose operand kinds the compiler proved, and [`compile::TrirCompiler`]
//! declines the whole routine the moment it meets anything else — so there is
//! no fallback arm, no slow path, and no second copy of any Raku semantic. A
//! declined routine takes today's path, which is correct (ADR-0110 §4).

use crate::symbol::Symbol;
use crate::value::Value;

pub(crate) mod compile;
pub(crate) mod entry;
pub(crate) mod exec;
pub(crate) mod exec_call;
pub(crate) mod frame;
pub(crate) mod outers;
// `#[path]`-spelled so `scripts/check-panic-surface.py` recognizes the whole
// file as test scaffolding (see its doc comment) rather than charging its
// assertions to the production budget.
#[cfg(test)]
#[path = "tests.rs"]
mod tests;

/// The kind of a TRIR slot or operand, as the compiler proved it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TrKind {
    /// A native `int`: a raw `i64` in the int bank.
    Int,
    /// A native `num`: an `f64` carried through the int bank as `to_bits`.
    Num,
    /// Anything boxed — a `str` parameter, an untyped `my $x`, a list. Lives
    /// in the interpreter's own `Value` frame/stack.
    Obj,
}

impl TrKind {
    /// True when values of this kind live in the int bank.
    #[inline]
    pub(crate) fn is_native(self) -> bool {
        matches!(self, TrKind::Int | TrKind::Num)
    }
}

/// One TRIR instruction.
///
/// Operand order is the order the compiler pushed them, per bank: an op
/// reading one operand from each bank pops each bank's top independently, so
/// the two are not interleaved.
#[derive(Debug, Clone)]
pub(crate) enum TrOp {
    // ---- int bank: literals, slots ----
    /// Push a compile-time integer.
    ConstI(i64),
    /// Push native slot `n`.
    LoadI(u16),
    /// Pop into native slot `n`.
    StoreI(u16),
    /// `slot += 1`, then push the NEW value (Raku's prefix `++`).
    IncI(u16),
    /// `slot -= 1`, then push the new value.
    DecI(u16),
    /// `slot += 1` with the result discarded — the statement-position form.
    IncIVoid(u16),
    /// `slot -= 1`, result discarded.
    DecIVoid(u16),

    // ---- int bank: arithmetic (wrapping, per native `int` semantics) ----
    AddI,
    SubI,
    MulI,
    DivI,
    ModI,
    NegI,
    BitAndI,
    BitOrI,
    BitXorI,
    ShlI,
    ShrI,

    // ---- int bank: comparison, pushing 0/1 ----
    EqI,
    NeI,
    LtI,
    LeI,
    GtI,
    GeI,
    /// Logical negation of an int-bank truth value.
    NotI,

    // ---- num bank (carried in the int bank as bits) ----
    AddN,
    SubN,
    MulN,
    DivN,
    /// `f64` comparison pushing an INT 0/1.
    EqN,
    LtN,
    LeN,
    GtN,
    GeN,
    /// Widen an int-bank integer to a num-bank double.
    IntToNum,
    /// Truncate a num-bank double to an integer.
    NumToInt,

    // ---- control flow ----
    Jump(u32),
    /// Pop the int bank; jump when the value is 0.
    JumpIfFalseI(u32),
    /// Pop the int bank; jump when the value is non-zero.
    JumpIfTrueI(u32),
    /// PEEK the int bank; jump when the value is 0, leaving it in place.
    /// `a && b` yields an operand rather than a boolean, so the
    /// short-circuited result is the value already on the bank.
    JumpIfFalseKeepI(u32),
    /// PEEK the int bank; jump when the value is non-zero, leaving it.
    JumpIfTrueKeepI(u32),

    // ---- boxed bank ----
    /// Push constant `TrChunk::constants[i]`.
    ConstObj(u32),
    /// Push boxed slot `n` (clones the `Value`).
    LoadObj(u16),
    /// Pop into boxed slot `n`.
    StoreObj(u16),
    /// Pop the int bank, push it boxed.
    BoxI,
    /// Pop the num bank, push it boxed as a `Num`.
    BoxN,
    /// Pop a boxed value, push its integer. Errors if it is not one — the
    /// checked boundary op of ADR-0110 §3.2.
    UnboxI,
    /// Drop the top of the boxed bank.
    PopObj,
    /// Drop the top of the int bank.
    PopI,
    /// Push the cached value of pre-resolved outer lexical `n`
    /// ([`TrChunk::outers`]).
    LoadOuter(u16),
    /// Push the value a BAREWORD names — a type object (`Map`, `NFD`), a
    /// constant, a package. Resolved by name through the ordinary machinery:
    /// these appear as arguments to `nqp::getattr`/`istype`/`create`, which
    /// is a cold-ish position, and resolving one is not what the untyped
    /// path's per-opcode cost was.
    LoadBareWord(u32),
    /// Push the value of a dynamic variable (`$*ALLOW-JSONC`), or `Nil`.
    LoadDynamic(u32),

    // ---- fused, operand-direct forms ----
    //
    // These exist because the generic stack form of the same operation would
    // clone a `Value` (an atomic refcount pair) purely to read through it.
    // `nom-ws`'s loop is entirely these: ADR-0110's measured ~600 ns per
    // iteration is dominated by exactly that kind of traffic.
    /// `nqp::ordat(<boxed slot n>, <int bank>)`: pop a codepoint index, push
    /// the codepoint at it, or -1 past the end. The slot's characters are
    /// materialized once per frame (`TrScratch`'s char memo).
    OrdAtLocal(u16),
    /// `nqp::ordat(<outer n>, <int bank>)`.
    OrdAtOuter(u16),
    /// `nqp::atpos_i(<boxed slot n>, <int bank>)`.
    AtPosILocal(u16),
    /// `nqp::atpos_i(<outer n>, <int bank>)`.
    AtPosIOuter(u16),
    /// `nqp::chars(<boxed slot n>)`, pushing an int.
    CharsLocal(u16),

    // ---- generic (stack-operand) forms of the same reads ----
    /// Pop a boxed string and an int index; push the codepoint, or -1.
    OrdAt,
    /// Pop a boxed list and an int index; push the element as an int.
    AtPosI,
    /// Pop a boxed string; push its length in codepoints.
    CharsS,
    /// Pop `n` boxed values and push their concatenation — the lowering of
    /// `"a $b c"`. Runs the interpreter's own `StringConcat`, so a `.Str`
    /// override, a `Proxy` operand and the writeback reconcile behave
    /// identically to the untyped path.
    ConcatN(u16),
    /// Pop two boxed values and push `$a ~ $b`, through the interpreter's own
    /// `Concat` — which is where a user `infix:<~>` override is honoured.
    ConcatBin,
    /// Duplicate the top of the boxed bank.
    DupObj,
    /// Pop a boxed value and push 1 when it is DEFINED — `nqp::ifnull`'s
    /// test, which mutsu answers as "not undefined" because it has no
    /// VM-level null distinct from an undefined Raku value (the untyped
    /// `JumpIfNotNil` arm says the same).
    TruthyDefined,
    /// Push a FRESH empty `Hash`. A constant would be shared by every
    /// invocation, and `my %result;` declares a new container each time.
    NewHash,
    /// Push a fresh empty `Array`, for the same reason.
    NewArray,
    /// Pop a boxed value and push its truth as an int-bank 0/1, through the
    /// interpreter's own `eval_truthy` — so a `.Bool` override, a `Failure`
    /// being marked handled, and every other rule behave exactly as they do
    /// under `JumpIfFalse`.
    TruthyObj,

    // ---- `is rw` native parameters (ADR-0110 §3.3's `getlexref_i`) ----
    //
    // The slot holds the ABSOLUTE index of the native slot it aliases, so a
    // write lands in the frame that owns the variable — including when this
    // routine passes the parameter on to another.
    /// Push the value the reference in slot `n` names.
    GetRefI(u16),
    /// Pop and store through the reference in slot `n`.
    SetRefI(u16),
    /// `++` through the reference in slot `n`, pushing the new value.
    IncRefI(u16),
    /// `++` through the reference in slot `n`, result discarded.
    IncRefIVoid(u16),
    /// `--` through the reference in slot `n`, pushing the new value.
    DecRefI(u16),
    /// `--` through the reference in slot `n`, result discarded.
    DecRefIVoid(u16),

    /// Dispatch an `nqp::` op TRIR has no typed form for: pop `arity` boxed
    /// operands, run the ordinary implementation, push the boxed result.
    ///
    /// One opcode covers the whole `nqp::` namespace, which is what makes a
    /// routine written in it compile at all. The typed forms above exist for
    /// the handful that a scanner's inner loop actually executes; everything
    /// else is correct here at the cost of boxing, and boxing is not what the
    /// ~211 ns per opcode was.
    NqpOpGen {
        id: u16,
        arity: u8,
    },

    // ---- calls ----
    /// Call another TRIR routine, resolved at compile time. `site` indexes
    /// [`TrChunk::calls`].
    CallTr(u32),
    /// Call a routine TRIR did not resolve — a cold error helper, a routine
    /// in another compunit, anything. The arguments are boxed and handed to
    /// the ordinary dispatch, and the result comes back boxed.
    ///
    /// This is what keeps eligibility from collapsing on cold paths: a
    /// scanner routine whose hot loop is typed usually ends in a `die` helper
    /// that is arbitrary Raku, and refusing the whole routine for it would
    /// leave the hot loop untyped too.
    CallGen(u32),

    // ---- exits ----
    /// Return the top of the int bank, boxed.
    ReturnI,
    /// Return the top of the num bank, boxed.
    ReturnN,
    /// Return the top of the boxed bank.
    ReturnObj,
    /// Return `Nil`.
    ReturnNil,
}

/// How one argument of a call inside a TRIR body is supplied.
///
/// The distinction that matters is whether the argument NAMES A VARIABLE the
/// callee might write (through an `is rw` parameter) or is merely a computed
/// value. For a resolved TRIR callee the signature settles it at compile
/// time; for a generic one it cannot, so a named variable is passed as a
/// container and read back — the ordinary path's `WrapVarRef` reduced to what
/// TRIR's own slots can express.
#[derive(Debug, Clone, Copy)]
pub(crate) enum TrArg {
    /// Already evaluated onto the operand stack of this kind's bank.
    Value(TrKind),
    /// This frame's native slot.
    Native(u16),
    /// This frame's boxed slot.
    Obj(u16),
    /// A native reference this frame already holds — its own `is rw`
    /// parameter, handed on.
    Ref(u16),
}

/// Who a call inside a TRIR body reaches.
#[derive(Debug, Clone)]
pub(crate) enum TrCallee {
    /// Another TRIR routine, resolved at compile time exactly as a
    /// [`TrCallSite`] resolves one.
    Trir { key: Symbol, fingerprint: u64 },
    /// Anything else, dispatched by name through the ordinary machinery.
    Generic,
}

/// One call inside a TRIR body.
#[derive(Debug, Clone)]
pub(crate) struct TrInnerCall {
    pub(crate) callee: TrCallee,
    /// The callee's name — the dispatch key for a generic call, and the
    /// wrapper-table probe for a resolved one.
    pub(crate) name: Symbol,
    /// The arguments, in signature order.
    pub(crate) args: Vec<TrArg>,
    /// The kind the call leaves on a bank.
    pub(crate) result: TrKind,
}

/// One pre-resolved outer lexical (ADR-0110 §3.1).
///
/// Stage 1 resolves the NAME once per routine invocation rather than once per
/// access — `nom-ws`'s `GetGlobal("ws")` runs per loop iteration today. The
/// resolution result is additionally memoized on the routine across calls,
/// keyed by [`crate::runtime::Interpreter::unit_lexical_gen`], so a steady
/// state pays one integer comparison.
#[derive(Debug, Clone)]
pub(crate) struct TrOuter {
    /// The free variable's name, sigil-less, exactly as `GetGlobal` spells it.
    pub(crate) name: Symbol,
}

/// One TRIR parameter's binding plan.
#[derive(Debug, Clone)]
pub(crate) struct TrParam {
    /// Slot the bound value goes to — a native slot when `kind.is_native()`,
    /// a boxed slot otherwise.
    pub(crate) slot: u16,
    pub(crate) kind: TrKind,
    /// `is rw`. Only a native parameter may be `is rw` in Stage 1; the callee
    /// reads the caller's slot in, and writes it back on every exit
    /// ([`entry`]).
    pub(crate) is_rw: bool,
    /// The declared type's spelling, for the boundary coercion's error
    /// message. Empty when unconstrained.
    pub(crate) type_name: &'static str,
}

/// A compile-time-resolved call to a TRIR routine (ADR-0110 §3.3).
///
/// The whole of `nom-ws($text, $pos)` — the callee, the callee's signature,
/// and where each argument lives — is known where the call is written, so
/// [`crate::opcode::OpCode::CallTrir`] carries an index into this table and
/// nothing else. No name is resolved, no dispatch key is built, no argument
/// is pushed, and no `is rw` container is minted: the arguments are read
/// straight out of the caller's own frame slots and the `is rw` result is
/// written straight back to them.
#[derive(Debug, Clone)]
pub(crate) struct TrCallSite {
    /// The callee's `CompiledFns` key, as its declaration produced it.
    pub(crate) key: Symbol,
    /// The callee's body fingerprint at compile time. Re-checked per call, so
    /// a routine that has since been replaced (or `.wrap`ped into a different
    /// body) falls back instead of running the wrong chunk.
    pub(crate) fingerprint: u64,
    /// The callee's name, for the cold fallback and for error messages.
    pub(crate) name: Symbol,
    /// The CALLER's local slot holding each positional argument, in signature
    /// order. A call site whose arguments are not all plain caller lexicals is
    /// not compiled to a `CallTrir` at all.
    pub(crate) arg_slots: Vec<u32>,
}

/// A routine compiled to TRIR.
#[derive(Debug, Clone)]
pub(crate) struct TrChunk {
    /// This chunk's identity, for the interpreter's per-chunk free-variable
    /// cache. A monotonic counter rather than the chunk's ADDRESS: a dropped
    /// `CompiledFunction` (an `EVAL`, an on-the-fly compile) frees its chunk,
    /// and the allocator may hand the same address to the next one — which
    /// would silently serve one routine's cached bindings to another.
    pub(crate) id: u64,
    pub(crate) ops: Vec<TrOp>,
    /// Boxed constants the ops index.
    pub(crate) constants: Vec<Value>,
    /// Number of native (int/num) slots this frame needs.
    pub(crate) n_native: u16,
    /// Number of boxed slots this frame needs.
    pub(crate) n_obj: u16,
    /// Positional parameters, in signature order.
    pub(crate) params: Vec<TrParam>,
    /// Free variables resolved at entry.
    pub(crate) outers: Vec<TrOuter>,
    /// The routine's name, for error messages.
    pub(crate) name: Symbol,
    /// The calls this body makes, indexed by `CallTr`/`CallGen`.
    pub(crate) calls: Vec<TrInnerCall>,
    /// True when the body contains any call at all. A body that makes none
    /// cannot observe a free variable changing under it, which is what lets
    /// its free variables be read once at entry instead of re-read after
    /// every call.
    pub(crate) has_calls: bool,
}

/// The next chunk identity. Wrapping is unreachable in practice (a program
/// would have to compile 2^64 routines), and `TrChunk` ids are only ever
/// compared for equality within one process.
pub(crate) fn next_chunk_id() -> u64 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static NEXT: AtomicU64 = AtomicU64::new(1);
    NEXT.fetch_add(1, Ordering::Relaxed)
}

impl TrChunk {
    /// Whether `MUTSU_TRIR=off` has switched TRIR off for this process.
    ///
    /// The A/B tool of ADR-0110 §5: with it set, every eligible routine
    /// declines and takes the untyped path, so the differential test in
    /// `t/vm/trir/` can run each signature shape through both and assert the
    /// results and exception types are identical.
    pub(crate) fn enabled() -> bool {
        use std::sync::OnceLock;
        static ON: OnceLock<bool> = OnceLock::new();
        *ON.get_or_init(|| {
            !matches!(
                std::env::var("MUTSU_TRIR").as_deref(),
                Ok("off") | Ok("0") | Ok("false")
            )
        })
    }
}
