//! [`TrOp`]: the TRIR instruction set.

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
    /// Wrap the top of the int bank to a sized native integer (`uint32`,
    /// `int8`, ...), exactly as storing into such a variable does: the low
    /// `bits` bits, sign-extended when `signed`.
    WrapI {
        bits: u8,
        signed: bool,
    },

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
    /// Pop a boxed `nqp::` result being STORED into a native integer
    /// variable, and push its integer — with the assignment's own check, so
    /// a type object dies ("Cannot unbox a type object (Nil) to int.") where
    /// `UnboxI`'s `iarg` coercion reads 0. Operand: the constant holding the
    /// variable's declared type name.
    NarrowStoreI(u32),
    /// Drop the top of the boxed bank.
    PopObj,
    /// Drop the top of the int bank.
    PopI,
    /// Push the cached value of pre-resolved outer lexical `n`
    /// ([`TrChunk::outers`](super::TrChunk::outers)).
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
    /// Pop an int `$len`, an int `$from` and a boxed string; push the
    /// codepoint substring, clamped exactly as `nqp::substr($s, $from,
    /// $len)`'s own implementation clamps it (negative/past-end `$from`
    /// clamps to the string, a `$len` that runs past the end truncates).
    SubstrS,
    /// Pop an int `$pos`, a boxed needle and a boxed haystack; push 1 when
    /// the needle occurs at exactly codepoint offset `$pos`, else 0 —
    /// `nqp::eqat`'s own answer.
    EqAtS,
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
    /// Pop `n` boxed values and push them as a `List`.
    ///
    /// A comma list in value position. `JSON::Fast`'s `parse-obj` has one:
    /// the parser reads `nqp::stmts(my $d := ..., nqp::stmts(...))` as a
    /// single list-valued argument, and the untyped path builds the list
    /// too — the elements' side effects, not the list, are the point.
    MakeListN(u16),
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
    /// `nqp::elems` of a boxed operand, answered on the native bank. A list,
    /// an `IterationBuffer` or a `Uni` is counted in place; anything else goes
    /// through the same op the dispatch table runs (ADR-0112 Step 3).
    ElemsO,
    /// `nqp::shift_i` of a boxed operand, answered on the native bank: the
    /// dispatch table's own removal (`Interpreter::nqp_shift_int`), without
    /// its argument vector, name walk and re-boxing. A store into a sized
    /// native (`my uint32 $o = ...`) wraps it as it wraps any native int.
    ShiftIO,
    /// `nqp::push_i(boxed, native)`, through the dispatch table's own body.
    /// Leaves the pushed value boxed, as `NqpOpGen` does.
    PushIO,

    // ---- calls ----
    /// Call another TRIR routine, resolved at compile time. `site` indexes
    /// [`TrChunk::calls`](super::TrChunk::calls).
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
    /// Call a method on a boxed receiver through the ordinary method
    /// dispatch: pop the arguments and the receiver, push the boxed result.
    /// `site` indexes [`TrChunk::methods`](super::TrChunk::methods). The method-call twin of
    /// `CallGen` (ADR-0112 Step 2).
    MethodGen(u32),

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
