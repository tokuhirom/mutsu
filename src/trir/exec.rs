//! Tier 0 of ADR-0110 §3.4: the switch-dispatched execution of a
//! [`TrChunk`].
//!
//! Every operand kind here was proved by [`super::compile`], so an arm is a
//! handful of instructions: no `view_kind`, no `Symbol`, no `Env`, no
//! per-instruction line/stat/trace bookkeeping. That is the whole claim of the
//! ADR — mutsu already emits about the right NUMBER of operations and pays
//! ~211 ns for each one re-discovering what the compiler knew.

use super::{TrChunk, TrOp};
use crate::value::{Value, ValueView};

/// One TRIR invocation's state.
///
/// The int bank is a plain `Vec<i64>` holding both the frame's native slots
/// (`[0 .. n_native)`) and the native operand stack (everything above). It
/// holds no references, so the collector never has to see it and frame
/// teardown is a `truncate`.
pub(crate) struct TrExecState<'a> {
    pub(crate) chunk: &'a TrChunk,
    /// Bit `n` is set once slot `n`'s character memo has been validated
    /// against the slot's current value in THIS frame. The memo survives the
    /// frame (see [`TrScratch::reset`]), so without this every
    /// `OrdAtLocal` would re-prove the same identity — 65 instructions to
    /// answer a question already answered this call.
    chars_checked: u64,
    /// The same, for the outer lexicals.
    outer_chars_checked: u64,
    /// The reusable buffers this invocation borrowed from the interpreter's
    /// pool. A TRIR frame is five vectors; allocating them per call cost more
    /// than the whole of `nom-ws`'s loop.
    pub(crate) buf: Box<TrScratch>,
}

/// The five per-invocation buffers, pooled on the interpreter
/// ([`crate::runtime::Interpreter::take_trir_scratch`]).
#[derive(Debug, Default)]
pub(crate) struct TrScratch {
    /// Native slots followed by the native operand stack.
    pub(crate) ints: Vec<i64>,
    /// Boxed slots.
    pub(crate) objs: Vec<Value>,
    /// Boxed operand stack.
    pub(crate) ostack: Vec<Value>,
    /// The outer lexicals resolved for this invocation, parallel to
    /// [`TrChunk::outers`].
    pub(crate) outers: Vec<Value>,
    /// Per-frame codepoint memo for the operand-direct string reads. Parallel
    /// to `objs`; an entry is filled on the slot's first `OrdAtLocal`.
    ///
    /// The compiler only emits those forms for a slot it proved is never
    /// reassigned, so an entry cannot go stale.
    chars: Vec<Option<CharMemo>>,
    /// Same memo for the outer lexicals.
    outer_chars: Vec<Option<CharMemo>>,
}

/// One string's codepoints, remembered with the string they came from.
///
/// The memo deliberately SURVIVES a frame: a scanner calls its `nom-ws` tens
/// of thousands of times over the same document, and collecting the
/// characters afresh each time was the single largest per-call cost measured
/// after the frame buffers were pooled (345 instructions plus a malloc/free
/// pair). `src` is what makes reuse safe — it is compared by the string's own
/// allocation identity, so a different string never reads another's memo.
#[derive(Debug)]
struct CharMemo {
    src: Value,
    chars: Vec<char>,
}

/// Whether two values are the SAME string allocation.
fn same_string(a: &Value, b: &Value) -> bool {
    match (a.view(), b.view()) {
        (ValueView::Str(x), ValueView::Str(y)) => std::sync::Arc::ptr_eq(&x, &y),
        _ => false,
    }
}

impl TrScratch {
    /// Return the frame buffers to an empty state, keeping their allocations.
    ///
    /// The two character memos are deliberately NOT cleared: each entry
    /// carries the string it was built from and is re-validated on use, so
    /// keeping them is what lets a repeated call over one document collect
    /// its characters once rather than once per call. Only the length is
    /// trimmed, by `TrExecState::new`, when a chunk needs fewer slots.
    pub(crate) fn reset(&mut self) {
        self.ints.clear();
        self.objs.clear();
        self.ostack.clear();
        self.outers.clear();
    }
}

/// What a TRIR chunk did.
pub(crate) enum TrOutcome {
    Value(Value),
    /// An operation met a value whose shape the compiler's proof did not
    /// cover — an `nqp::atpos_i` on something that is not a list, an
    /// `UnboxI` of a non-integer. The caller re-runs the routine on the
    /// untyped path, which raises whatever the program should see.
    ///
    /// This is NOT a fallback arm inside the instruction set: it is a bail
    /// before any observable effect, and the only ops that can raise it are
    /// the checked boundary ops.
    Bail,
}

#[inline]
fn f(bits: i64) -> f64 {
    f64::from_bits(bits as u64)
}

#[inline]
fn b(v: f64) -> i64 {
    v.to_bits() as i64
}

impl<'a> TrExecState<'a> {
    /// Open a frame in `buf`, which the caller took from the pool. `buf` is
    /// handed back with [`Self::finish`].
    pub(crate) fn new(chunk: &'a TrChunk, mut buf: Box<TrScratch>) -> Self {
        buf.reset();
        buf.ints.resize(chunk.n_native as usize, 0);
        buf.objs.resize(chunk.n_obj as usize, Value::NIL);
        if buf.chars.len() < chunk.n_obj as usize {
            buf.chars.resize_with(chunk.n_obj as usize, || None);
        }
        buf.outers.reserve(chunk.outers.len());
        TrExecState {
            chunk,
            chars_checked: 0,
            outer_chars_checked: 0,
            buf,
        }
    }

    /// Record one resolved outer lexical, in [`TrChunk::outers`] order.
    #[inline]
    pub(crate) fn push_outer(&mut self, v: Value) {
        self.buf.outers.push(v);
        if self.buf.outer_chars.len() < self.buf.outers.len() {
            self.buf.outer_chars.push(None);
        }
    }

    /// Hand the buffers back to the pool.
    pub(crate) fn finish(self) -> Box<TrScratch> {
        self.buf
    }

    #[inline]
    fn ipush(&mut self, v: i64) {
        self.buf.ints.push(v);
    }

    #[inline]
    fn ipop(&mut self) -> i64 {
        // The compiler balances every bank, so the stack is never empty here;
        // a hand-built chunk that got it wrong would read 0 rather than
        // panic, which keeps a bug from becoming a process abort.
        self.buf.ints.pop().unwrap_or(0)
    }

    /// Seed a native slot at bind time.
    #[inline]
    pub(crate) fn set_native_slot(&mut self, n: u16, v: i64) {
        self.buf.ints[n as usize] = v;
    }

    /// Read a native slot back at `is rw` writeback time.
    #[inline]
    pub(crate) fn native_slot(&self, n: u16) -> i64 {
        self.buf.ints[n as usize]
    }

    /// Seed a boxed slot at bind time.
    #[inline]
    pub(crate) fn set_obj_slot(&mut self, n: u16, v: Value) {
        self.buf.objs[n as usize] = v;
    }

    #[inline]
    fn slot(&self, n: u16) -> i64 {
        self.buf.ints[n as usize]
    }

    #[inline]
    fn set_slot(&mut self, n: u16, v: i64) {
        self.buf.ints[n as usize] = v;
    }

    /// Make slot `n`'s character memo current, answering whether the slot
    /// holds a string at all.
    fn fill_slot_chars(&mut self, n: u16) -> bool {
        let i = n as usize;
        let bit = 1u64 << (i & 63);
        if self.chars_checked & bit != 0 {
            return self.buf.chars[i].is_some();
        }
        self.chars_checked |= bit;
        if let Some(m) = &self.buf.chars[i]
            && same_string(&m.src, &self.buf.objs[i])
        {
            return true;
        }
        match self.buf.objs[i].as_str() {
            Some(s) => {
                let chars = s.chars().collect();
                self.buf.chars[i] = Some(CharMemo {
                    src: self.buf.objs[i].clone(),
                    chars,
                });
                true
            }
            None => false,
        }
    }

    /// The same memo for an outer lexical.
    fn fill_outer_chars(&mut self, n: u16) -> bool {
        let i = n as usize;
        let bit = 1u64 << (i & 63);
        if self.outer_chars_checked & bit != 0 {
            return self.buf.outer_chars[i].is_some();
        }
        self.outer_chars_checked |= bit;
        if let Some(m) = &self.buf.outer_chars[i]
            && same_string(&m.src, &self.buf.outers[i])
        {
            return true;
        }
        match self.buf.outers[i].as_str() {
            Some(s) => {
                let chars = s.chars().collect();
                self.buf.outer_chars[i] = Some(CharMemo {
                    src: self.buf.outers[i].clone(),
                    chars,
                });
                true
            }
            None => false,
        }
    }

    /// Element `idx` of a boxed native-int list, exactly as
    /// `runtime/nqp_ops.rs`'s own `atpos_i` reads it: 0 both past the end and
    /// for a target with no elements at all.
    fn atpos_i(v: &Value, idx: i64) -> Option<i64> {
        let Ok(i) = usize::try_from(idx) else {
            return Some(0);
        };
        // A plain `nqp::list_i` IS an array, and that is what every scanner's
        // lookup table is. Reading it directly skips `nqp_backing_array`'s
        // walk through the Buf/IterationBuffer/Uni shapes, which cost more
        // than the read (106 instructions of 177).
        if let ValueView::Array(items, _) = v.view() {
            return Some(items.get(i).and_then(|e| e.as_int()).unwrap_or(0));
        }
        let elem = crate::runtime::Interpreter::nqp_elem_at(v, i);
        Some(match elem {
            Some(e) => e.as_int().unwrap_or_else(|| crate::runtime::to_int(&e)),
            None => 0,
        })
    }

    /// Run the chunk.
    pub(crate) fn run(&mut self) -> TrOutcome {
        let ops = &self.chunk.ops;
        let mut ip = 0usize;
        loop {
            match &ops[ip] {
                TrOp::ConstI(v) => self.ipush(*v),
                TrOp::LoadI(n) => {
                    let v = self.slot(*n);
                    self.ipush(v);
                }
                TrOp::StoreI(n) => {
                    let v = self.ipop();
                    self.set_slot(*n, v);
                }
                TrOp::IncI(n) => {
                    let v = self.slot(*n).wrapping_add(1);
                    self.set_slot(*n, v);
                    self.ipush(v);
                }
                TrOp::DecI(n) => {
                    let v = self.slot(*n).wrapping_sub(1);
                    self.set_slot(*n, v);
                    self.ipush(v);
                }
                TrOp::IncIVoid(n) => {
                    let v = self.slot(*n).wrapping_add(1);
                    self.set_slot(*n, v);
                }
                TrOp::DecIVoid(n) => {
                    let v = self.slot(*n).wrapping_sub(1);
                    self.set_slot(*n, v);
                }
                TrOp::AddI => self.bin_i(i64::wrapping_add),
                TrOp::SubI => self.bin_i(i64::wrapping_sub),
                TrOp::MulI => self.bin_i(i64::wrapping_mul),
                TrOp::DivI => {
                    let r = self.ipop();
                    let l = self.ipop();
                    if r == 0 {
                        return TrOutcome::Bail;
                    }
                    self.ipush(l.wrapping_div(r));
                }
                TrOp::ModI => {
                    let r = self.ipop();
                    let l = self.ipop();
                    if r == 0 {
                        return TrOutcome::Bail;
                    }
                    // `nqp::mod_i` follows the dividend's sign like Rust's
                    // `%`; Raku's own `%` does not, which is why only the
                    // `nqp::` spelling reaches here.
                    self.ipush(l.wrapping_rem(r));
                }
                TrOp::NegI => {
                    let v = self.ipop();
                    self.ipush(v.wrapping_neg());
                }
                TrOp::BitAndI => self.bin_i(|a, b| a & b),
                TrOp::BitOrI => self.bin_i(|a, b| a | b),
                TrOp::BitXorI => self.bin_i(|a, b| a ^ b),
                TrOp::ShlI => self.bin_i(|a, b| a.wrapping_shl(b as u32)),
                TrOp::ShrI => self.bin_i(|a, b| a.wrapping_shr(b as u32)),
                TrOp::EqI => self.cmp_i(|a, b| a == b),
                TrOp::NeI => self.cmp_i(|a, b| a != b),
                TrOp::LtI => self.cmp_i(|a, b| a < b),
                TrOp::LeI => self.cmp_i(|a, b| a <= b),
                TrOp::GtI => self.cmp_i(|a, b| a > b),
                TrOp::GeI => self.cmp_i(|a, b| a >= b),
                TrOp::NotI => {
                    let v = self.ipop();
                    self.ipush((v == 0) as i64);
                }
                TrOp::AddN => self.bin_n(|a, b| a + b),
                TrOp::SubN => self.bin_n(|a, b| a - b),
                TrOp::MulN => self.bin_n(|a, b| a * b),
                TrOp::DivN => self.bin_n(|a, b| a / b),
                TrOp::EqN => self.cmp_n(|a, b| a == b),
                TrOp::LtN => self.cmp_n(|a, b| a < b),
                TrOp::LeN => self.cmp_n(|a, b| a <= b),
                TrOp::GtN => self.cmp_n(|a, b| a > b),
                TrOp::GeN => self.cmp_n(|a, b| a >= b),
                TrOp::IntToNum => {
                    let v = self.ipop();
                    self.ipush(b(v as f64));
                }
                TrOp::NumToInt => {
                    let v = f(self.ipop());
                    self.ipush(v as i64);
                }
                TrOp::Jump(t) => {
                    ip = *t as usize;
                    continue;
                }
                TrOp::JumpIfFalseI(t) => {
                    if self.ipop() == 0 {
                        ip = *t as usize;
                        continue;
                    }
                }
                TrOp::JumpIfTrueI(t) => {
                    if self.ipop() != 0 {
                        ip = *t as usize;
                        continue;
                    }
                }
                TrOp::ConstObj(i) => {
                    let v = self.chunk.constants[*i as usize].clone();
                    self.buf.ostack.push(v);
                }
                TrOp::LoadObj(n) => {
                    let v = self.buf.objs[*n as usize].clone();
                    self.buf.ostack.push(v);
                }
                TrOp::StoreObj(n) => {
                    let v = self.buf.ostack.pop().unwrap_or(Value::NIL);
                    self.buf.objs[*n as usize] = v;
                    // Force the memo to re-prove itself against the new value
                    // the next time this slot is read as a string.
                    self.chars_checked &= !(1u64 << (*n as usize & 63));
                }
                TrOp::BoxI => {
                    let v = self.ipop();
                    self.buf.ostack.push(Value::int(v));
                }
                TrOp::BoxN => {
                    let v = f(self.ipop());
                    self.buf.ostack.push(Value::num(v));
                }
                TrOp::UnboxI => {
                    let v = self.buf.ostack.pop().unwrap_or(Value::NIL);
                    match v.as_int() {
                        Some(i) => self.ipush(i),
                        None => return TrOutcome::Bail,
                    }
                }
                TrOp::PopObj => {
                    self.buf.ostack.pop();
                }
                TrOp::PopI => {
                    self.ipop();
                }
                TrOp::LoadOuter(n) => {
                    let v = self.buf.outers[*n as usize].clone();
                    self.buf.ostack.push(v);
                }
                TrOp::OrdAtLocal(n) => {
                    let pos = self.ipop();
                    if !self.fill_slot_chars(*n) {
                        return TrOutcome::Bail;
                    }
                    let cp = match &self.buf.chars[*n as usize] {
                        Some(m) => ord_at(&m.chars, pos),
                        None => return TrOutcome::Bail,
                    };
                    self.ipush(cp);
                }
                TrOp::OrdAtOuter(n) => {
                    let pos = self.ipop();
                    if !self.fill_outer_chars(*n) {
                        return TrOutcome::Bail;
                    }
                    let cp = match &self.buf.outer_chars[*n as usize] {
                        Some(m) => ord_at(&m.chars, pos),
                        None => return TrOutcome::Bail,
                    };
                    self.ipush(cp);
                }
                TrOp::AtPosILocal(n) => {
                    let idx = self.ipop();
                    match Self::atpos_i(&self.buf.objs[*n as usize], idx) {
                        Some(v) => self.ipush(v),
                        None => return TrOutcome::Bail,
                    }
                }
                TrOp::AtPosIOuter(n) => {
                    let idx = self.ipop();
                    match Self::atpos_i(&self.buf.outers[*n as usize], idx) {
                        Some(v) => self.ipush(v),
                        None => return TrOutcome::Bail,
                    }
                }
                TrOp::CharsLocal(n) => {
                    if !self.fill_slot_chars(*n) {
                        return TrOutcome::Bail;
                    }
                    let len = match &self.buf.chars[*n as usize] {
                        Some(m) => m.chars.len() as i64,
                        None => return TrOutcome::Bail,
                    };
                    self.ipush(len);
                }
                TrOp::OrdAt => {
                    let pos = self.ipop();
                    let s = self.buf.ostack.pop().unwrap_or(Value::NIL);
                    let Some(st) = s.as_str() else {
                        return TrOutcome::Bail;
                    };
                    let cp = usize::try_from(pos)
                        .ok()
                        .and_then(|p| st.chars().nth(p))
                        .map(|c| c as i64)
                        .unwrap_or(-1);
                    self.ipush(cp);
                }
                TrOp::AtPosI => {
                    let idx = self.ipop();
                    let v = self.buf.ostack.pop().unwrap_or(Value::NIL);
                    match Self::atpos_i(&v, idx) {
                        Some(e) => self.ipush(e),
                        None => return TrOutcome::Bail,
                    }
                }
                TrOp::CharsS => {
                    let v = self.buf.ostack.pop().unwrap_or(Value::NIL);
                    let Some(s) = v.as_str() else {
                        return TrOutcome::Bail;
                    };
                    let n = s.chars().count() as i64;
                    self.ipush(n);
                }
                TrOp::ReturnI => {
                    let v = self.ipop();
                    return TrOutcome::Value(Value::int(v));
                }
                TrOp::ReturnN => {
                    let v = f(self.ipop());
                    return TrOutcome::Value(Value::num(v));
                }
                TrOp::ReturnObj => {
                    return TrOutcome::Value(self.buf.ostack.pop().unwrap_or(Value::NIL));
                }
                TrOp::ReturnNil => return TrOutcome::Value(Value::NIL),
            }
            ip += 1;
        }
    }

    #[inline]
    fn bin_i(&mut self, f: fn(i64, i64) -> i64) {
        let r = self.ipop();
        let l = self.ipop();
        self.ipush(f(l, r));
    }

    #[inline]
    fn cmp_i(&mut self, p: fn(i64, i64) -> bool) {
        let r = self.ipop();
        let l = self.ipop();
        self.ipush(p(l, r) as i64);
    }

    #[inline]
    fn bin_n(&mut self, op: fn(f64, f64) -> f64) {
        let r = f(self.ipop());
        let l = f(self.ipop());
        self.ipush(b(op(l, r)));
    }

    #[inline]
    fn cmp_n(&mut self, p: fn(f64, f64) -> bool) {
        let r = f(self.ipop());
        let l = f(self.ipop());
        self.ipush(p(l, r) as i64);
    }
}

/// `nqp::ordat`'s answer for a codepoint index: the codepoint, or -1 past the
/// end (`runtime/nqp_ops_builtin.rs`'s own `unwrap_or(-1)`).
#[inline]
fn ord_at(chars: &[char], pos: i64) -> i64 {
    usize::try_from(pos)
        .ok()
        .and_then(|p| chars.get(p))
        .map(|&c| c as i64)
        .unwrap_or(-1)
}
