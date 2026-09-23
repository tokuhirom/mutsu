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
//! `int`/`num` operands live in their own `Vec<i64>` banks
//! ([`frame::TrStacks::nl`] and [`frame::TrStacks::ns`]), boxed ones in
//! `Vec<Value>` banks beside them. Nothing ever reads a raw word as a
//! `Value`, because no raw word is ever stored where a `Value` lives — the
//! top risk in ADR-0110 §5 is removed structurally rather than contained by a
//! verifier, which is what CLAUDE.md's definition of risk ("a mechanism that
//! cannot go flaky" over "an optimization correct only under a static
//! analysis") asks for. GC needs no change either: the native banks hold no
//! references, and the boxed ones are visited as roots
//! ([`frame::TrStacks::boxed_slots`]). The deviation is recorded in
//! ADR-0110's implementation status.
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
mod entry_values;
pub(crate) mod exec;
pub(crate) mod exec_call;
mod exec_str;
pub(crate) mod frame;
pub(crate) mod gen_link;
mod link;
mod op;
pub(crate) mod outers;
pub(crate) mod stats;

pub(crate) use link::TrLink;
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

pub(crate) use op::TrOp;

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
    Trir(TrLink),
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
    /// A nominal type constraint on a boxed parameter (`Uni:D \codes`),
    /// checked at bind time with the general binder's own type test. A
    /// failed check declines the TRIR call, so the untyped path raises the
    /// error the program should see.
    pub(crate) check: Option<TrParamCheck>,
    /// A sigilless parameter (`\codes`). It binds the caller's CONTAINER
    /// when handed a variable, which a boxed slot holding a value cannot
    /// stand in for, so every door declines such an argument and admits
    /// only a computed value.
    pub(crate) sigilless: bool,
}

/// A boxed parameter's nominal type check.
#[derive(Debug, Clone)]
pub(crate) struct TrParamCheck {
    /// The constraint without its smiley (`Uni` for `Uni:D`).
    pub(crate) base: String,
    /// `Some(true)` for `:D`, `Some(false)` for `:U`, `None` for neither.
    pub(crate) defined: Option<bool>,
}

/// One method call inside a TRIR body (`TrOp::MethodGen`).
#[derive(Debug, Clone)]
pub(crate) struct TrMethodCall {
    pub(crate) name: Symbol,
    /// Positional arguments after the receiver.
    pub(crate) arity: u8,
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
    /// The callee, resolved when the site was compiled. Re-checked per call
    /// against the table in hand ([`TrLink::current_in`]), so a routine that
    /// has since been replaced falls back instead of running the wrong chunk;
    /// a `.wrap`ped one is caught by the wrapper-table probe.
    pub(crate) link: TrLink,
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
    /// The method calls this body makes, indexed by `MethodGen`.
    pub(crate) methods: Vec<TrMethodCall>,
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
