//! Tier 0 of ADR-0110 §3.4: the switch-dispatched execution of a
//! [`TrChunk`].
//!
//! Every operand kind here was proved by [`super::compile`], so an arm is a
//! handful of instructions: no `view_kind`, no `Symbol`, no `Env`, no
//! per-instruction line/stat/trace bookkeeping. That is the whole claim of
//! the ADR — mutsu already emits about the right NUMBER of operations and
//! pays ~211 ns for each one re-discovering what the compiler knew.
//!
//! The loop is a method on [`Interpreter`] rather than on a state struct
//! because a TRIR body may now call out (`CallGen`), and a call needs the
//! whole interpreter.

use super::frame::TrFrame;
use super::{TrChunk, TrOp};
use crate::opcode::CompiledFns;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

/// What a TRIR chunk did.
pub(crate) enum TrOutcome {
    Value(Value),
    /// An operation met a value whose shape the compiler's proof did not
    /// cover — an `nqp::atpos_i` on something that is not a list, an
    /// `UnboxI` of a non-integer, a callee that has been replaced. The caller
    /// re-runs the routine on the untyped path, which raises whatever the
    /// program should see.
    ///
    /// This is NOT a fallback arm inside the instruction set: it is a bail,
    /// and the compiler only ever places a bail-capable op where re-running
    /// the routine from the beginning is equivalent to never having started
    /// it (see [`super::compile`]'s `effectful` tracking).
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

impl Interpreter {
    /// Run `chunk` in `frame`, which the caller opened and will close.
    pub(crate) fn run_trir_chunk(
        &mut self,
        chunk: &TrChunk,
        frame: TrFrame,
        compiled_fns: &CompiledFns,
    ) -> Result<TrOutcome, RuntimeError> {
        let nbase = frame.nbase as usize;
        let obase = frame.obase as usize;
        let cbase = frame.outer_base as usize;
        let ops = &chunk.ops;
        let mut ip = 0usize;
        loop {
            match &ops[ip] {
                TrOp::ConstI(v) => self.trir.ns.push(*v),
                TrOp::LoadI(n) => {
                    let v = self.trir.nl[nbase + *n as usize];
                    self.trir.ns.push(v);
                }
                TrOp::StoreI(n) => {
                    let v = self.ipop();
                    self.trir.nl[nbase + *n as usize] = v;
                }
                TrOp::IncI(n) => {
                    let s = nbase + *n as usize;
                    let v = self.trir.nl[s].wrapping_add(1);
                    self.trir.nl[s] = v;
                    self.trir.ns.push(v);
                }
                TrOp::DecI(n) => {
                    let s = nbase + *n as usize;
                    let v = self.trir.nl[s].wrapping_sub(1);
                    self.trir.nl[s] = v;
                    self.trir.ns.push(v);
                }
                TrOp::IncIVoid(n) => {
                    let s = nbase + *n as usize;
                    self.trir.nl[s] = self.trir.nl[s].wrapping_add(1);
                }
                TrOp::DecIVoid(n) => {
                    let s = nbase + *n as usize;
                    self.trir.nl[s] = self.trir.nl[s].wrapping_sub(1);
                }

                // ---- `is rw` native parameters, through their reference ----
                TrOp::GetRefI(n) => {
                    let r = self.trir.nl[nbase + *n as usize] as usize;
                    let v = self.trir.nl[r];
                    self.trir.ns.push(v);
                }
                TrOp::SetRefI(n) => {
                    let v = self.ipop();
                    let r = self.trir.nl[nbase + *n as usize] as usize;
                    self.trir.nl[r] = v;
                }
                TrOp::IncRefI(n) => {
                    let r = self.trir.nl[nbase + *n as usize] as usize;
                    let v = self.trir.nl[r].wrapping_add(1);
                    self.trir.nl[r] = v;
                    self.trir.ns.push(v);
                }
                TrOp::IncRefIVoid(n) => {
                    let r = self.trir.nl[nbase + *n as usize] as usize;
                    self.trir.nl[r] = self.trir.nl[r].wrapping_add(1);
                }
                TrOp::DecRefI(n) => {
                    let r = self.trir.nl[nbase + *n as usize] as usize;
                    let v = self.trir.nl[r].wrapping_sub(1);
                    self.trir.nl[r] = v;
                    self.trir.ns.push(v);
                }
                TrOp::DecRefIVoid(n) => {
                    let r = self.trir.nl[nbase + *n as usize] as usize;
                    self.trir.nl[r] = self.trir.nl[r].wrapping_sub(1);
                }

                // ---- arithmetic (wrapping, per native `int` semantics) ----
                TrOp::AddI => self.bin_i(i64::wrapping_add),
                TrOp::SubI => self.bin_i(i64::wrapping_sub),
                TrOp::MulI => self.bin_i(i64::wrapping_mul),
                TrOp::DivI => {
                    let r = self.ipop();
                    let l = self.ipop();
                    if r == 0 {
                        // The same error `runtime/nqp_ops.rs` raises, rather
                        // than a bail: a bail re-runs the routine, and by
                        // this point the body may already have written
                        // through an `is rw` reference.
                        return Err(RuntimeError::new("nqp::div_i: division by zero"));
                    }
                    self.trir.ns.push(l.wrapping_div(r));
                }
                TrOp::ModI => {
                    let r = self.ipop();
                    let l = self.ipop();
                    if r == 0 {
                        return Err(RuntimeError::new("nqp::mod_i: division by zero"));
                    }
                    // `nqp::mod_i` follows the dividend's sign like Rust's
                    // `%`; Raku's own `%` does not, which is why only the
                    // `nqp::` spelling reaches here.
                    self.trir.ns.push(l.wrapping_rem(r));
                }
                TrOp::NegI => {
                    let v = self.ipop();
                    self.trir.ns.push(v.wrapping_neg());
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
                    self.trir.ns.push((v == 0) as i64);
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
                    self.trir.ns.push(b(v as f64));
                }
                TrOp::NumToInt => {
                    let v = f(self.ipop());
                    self.trir.ns.push(v as i64);
                }

                // ---- control flow ----
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
                TrOp::JumpIfFalseKeepI(t) => {
                    if self.trir.ns.last().copied().unwrap_or(0) == 0 {
                        ip = *t as usize;
                        continue;
                    }
                }
                TrOp::JumpIfTrueKeepI(t) => {
                    if self.trir.ns.last().copied().unwrap_or(0) != 0 {
                        ip = *t as usize;
                        continue;
                    }
                }

                // ---- boxed bank ----
                TrOp::ConstObj(i) => {
                    let v = chunk.constants[*i as usize].clone();
                    self.trir.os.push(v);
                }
                TrOp::LoadObj(n) => {
                    let v = self.trir.ol[obase + *n as usize].clone();
                    self.trir.os.push(v);
                }
                TrOp::StoreObj(n) => {
                    let v = self.opop();
                    self.trir.ol[obase + *n as usize] = v;
                }
                TrOp::BoxI => {
                    let v = self.ipop();
                    self.trir.os.push(Value::int(v));
                }
                TrOp::BoxN => {
                    let v = f(self.ipop());
                    self.trir.os.push(Value::num(v));
                }
                TrOp::UnboxI => {
                    // `nqp`'s own `iarg` coercion. The compiler only emits
                    // this for a value an `nqp::` op produced, where these
                    // ARE the semantics; it refuses to narrow an arbitrary
                    // boxed expression into a native slot, which the general
                    // binder would reject rather than coerce.
                    let v = self.opop();
                    let i = v.as_int().unwrap_or_else(|| crate::runtime::to_int(&v));
                    self.trir.ns.push(i);
                }
                TrOp::PopObj => {
                    self.trir.os.pop();
                }
                TrOp::PopI => {
                    self.ipop();
                }
                TrOp::LoadOuter(n) => {
                    let v = self.trir.outers[cbase + *n as usize].clone();
                    self.trir.os.push(v);
                }

                // ---- operand-direct string and list reads ----
                TrOp::OrdAtLocal(n) => {
                    let pos = self.ipop();
                    let src = self.trir.ol[obase + *n as usize].clone();
                    let cp = self.trir_ord_at(&src, pos);
                    self.trir.ns.push(cp);
                }
                TrOp::OrdAtOuter(n) => {
                    let pos = self.ipop();
                    let src = self.trir.outers[cbase + *n as usize].clone();
                    let cp = self.trir_ord_at(&src, pos);
                    self.trir.ns.push(cp);
                }
                TrOp::AtPosILocal(n) => {
                    let idx = self.ipop();
                    let v = Self::trir_atpos_i(&self.trir.ol[obase + *n as usize], idx);
                    self.trir.ns.push(v);
                }
                TrOp::AtPosIOuter(n) => {
                    let idx = self.ipop();
                    let v = Self::trir_atpos_i(&self.trir.outers[cbase + *n as usize], idx);
                    self.trir.ns.push(v);
                }
                TrOp::CharsLocal(n) => {
                    let src = self.trir.ol[obase + *n as usize].clone();
                    let len = self.trir_chars_len(&src);
                    self.trir.ns.push(len);
                }
                TrOp::OrdAt => {
                    let pos = self.ipop();
                    let s = self.opop();
                    let cp = self.trir_ord_at(&s, pos);
                    self.trir.ns.push(cp);
                }
                TrOp::AtPosI => {
                    let idx = self.ipop();
                    let v = self.opop();
                    let e = Self::trir_atpos_i(&v, idx);
                    self.trir.ns.push(e);
                }
                TrOp::ConcatN(n) => {
                    let base = self.trir.os.len().saturating_sub(*n as usize);
                    let vals: Vec<Value> = self.trir.os.drain(base..).collect();
                    self.stack.extend(vals);
                    self.exec_string_concat_op(*n as u32)?;
                    let v = self.stack.pop().unwrap_or(Value::NIL);
                    self.trir.os.push(v);
                }
                TrOp::DupObj => {
                    let v = self.trir.os.last().cloned().unwrap_or(Value::NIL);
                    self.trir.os.push(v);
                }
                TrOp::TruthyDefined => {
                    // The same test the untyped `JumpIfNotNil` arm makes.
                    let v = self.opop();
                    let defined = self.value_is_defined_dispatch(&v);
                    self.trir.ns.push(defined as i64);
                }
                TrOp::NewHash => self
                    .trir
                    .os
                    .push(Value::hash(crate::value::ValueMap::default())),
                TrOp::NewArray => self.trir.os.push(Value::real_array(Vec::new())),
                TrOp::MakeListN(n) => {
                    let base = self.trir.os.len().saturating_sub(*n as usize);
                    let items: Vec<Value> = self.trir.os.drain(base..).collect();
                    // `Value::array` IS the `List` kind (see its definition);
                    // `real_array` is the `Array` kind `my @a` declares.
                    self.trir.os.push(Value::array(items));
                }
                TrOp::TruthyObj => {
                    let v = self.opop();
                    let t = self.eval_truthy(&v);
                    self.trir.ns.push(t as i64);
                }
                TrOp::ConcatBin => {
                    let r = self.opop();
                    let l = self.opop();
                    self.stack.push(l);
                    self.stack.push(r);
                    self.exec_concat_op()?;
                    let v = self.stack.pop().unwrap_or(Value::NIL);
                    self.trir.os.push(v);
                }
                TrOp::CharsS => {
                    let v = self.opop();
                    let n = self.trir_chars_len(&v);
                    self.trir.ns.push(n);
                }
                TrOp::SubstrS => {
                    let want = self.ipop();
                    let from = self.ipop();
                    let src = self.opop();
                    let v = self.trir_substr(&src, from, want);
                    self.trir.os.push(v);
                }
                TrOp::EqAtS => {
                    let pos = self.ipop();
                    let needle = self.opop();
                    let haystack = self.opop();
                    let yes = self.trir_eqat(&haystack, &needle, pos);
                    self.trir.ns.push(yes);
                }

                TrOp::LoadBareWord(i) => {
                    let name = chunk.constants[*i as usize].to_string_value();
                    self.push_bare_word_value(&name, compiled_fns)?;
                    let v = self.stack.pop().unwrap_or(Value::NIL);
                    self.trir.os.push(v);
                }
                TrOp::LoadDynamic(i) => {
                    let name = chunk.constants[*i as usize].to_string_value();
                    let v = self.env().get(&name).cloned().unwrap_or(Value::NIL);
                    self.trir.os.push(v);
                }
                TrOp::NqpOpGen { id, arity } => {
                    let n = *arity as usize;
                    let base = self.trir.os.len().saturating_sub(n);
                    // The pure native ops (`crate::runtime::nqp_pure`) run
                    // straight off the boxed bank: no argument vector, no
                    // name, no table walk. TRIR has typed forms for most of
                    // these already, so what reaches here is the residue a
                    // chunk could not type — an `nqp::iseq_i` whose operand
                    // came back boxed from a `CallGen`, say (#8900).
                    let direct = crate::runtime::nqp_pure::pure_op(*id).and_then(|op| {
                        crate::runtime::nqp_pure::try_eval_native(op, &self.trir.os[base..])
                    });
                    match direct {
                        Some(v) => {
                            self.trir.os.truncate(base);
                            self.trir.os.push(v);
                        }
                        None => {
                            let args: Vec<Value> = self.trir.os.drain(base..).collect();
                            let v = self.dispatch_nqp_op_by_id(*id, &args)?;
                            self.trir.os.push(v);
                        }
                    }
                }

                // ---- calls ----
                TrOp::CallTr(site) => {
                    match self.exec_trir_inner_call(chunk, *site, frame, compiled_fns)? {
                        Some(()) => {}
                        None => return Ok(TrOutcome::Bail),
                    }
                    // A callee may have written a free variable this frame
                    // holds a copy of; re-reading the bindings is the sound
                    // alternative to proving it did not (`outers.rs`). One
                    // cell read per CALL, where the untyped path pays a
                    // by-name lookup per ACCESS.
                    if !self.trir_reseed_outers(chunk, frame) {
                        return Ok(TrOutcome::Bail);
                    }
                }
                TrOp::CallGen(site) => {
                    self.exec_trir_generic_call(chunk, *site, frame)?;
                    if !self.trir_reseed_outers(chunk, frame) {
                        return Ok(TrOutcome::Bail);
                    }
                }

                // ---- exits ----
                TrOp::ReturnI => {
                    let v = self.ipop();
                    return Ok(TrOutcome::Value(Value::int(v)));
                }
                TrOp::ReturnN => {
                    let v = f(self.ipop());
                    return Ok(TrOutcome::Value(Value::num(v)));
                }
                TrOp::ReturnObj => return Ok(TrOutcome::Value(self.opop())),
                TrOp::ReturnNil => return Ok(TrOutcome::Value(Value::NIL)),
            }
            ip += 1;
        }
    }

    /// Pop the native operand stack.
    ///
    /// The compiler balances every bank, so it is never empty here; a
    /// hand-built chunk that got it wrong reads 0 rather than panicking,
    /// which keeps an internal bug from becoming a process abort (#8186).
    #[inline]
    pub(super) fn ipop(&mut self) -> i64 {
        self.trir.ns.pop().unwrap_or(0)
    }

    /// Pop the boxed operand stack, with the same contract.
    #[inline]
    pub(super) fn opop(&mut self) -> Value {
        self.trir.os.pop().unwrap_or(Value::NIL)
    }

    /// Element `idx` of a list, exactly as `runtime/nqp_ops.rs`'s own
    /// `atpos_i` reads it: 0 both past the end and for a target with no
    /// elements at all.
    /// `nqp::ordat`'s answer, with the same non-string coercion
    /// `nqp_char_cache::cached_chars` performs.
    fn trir_ord_at(&mut self, src: &Value, pos: i64) -> i64 {
        match self.trir.chars.index_of(src) {
            Some(i) => ord_at(self.trir.chars.chars_at(i), pos),
            None => {
                let s = src.to_string_value();
                ord_at(&s.chars().collect::<Vec<char>>(), pos)
            }
        }
    }

    /// `nqp::chars`'s answer, with the same coercion.
    fn trir_chars_len(&mut self, src: &Value) -> i64 {
        match self.trir.chars.index_of(src) {
            Some(i) => self.trir.chars.chars_at(i).len() as i64,
            None => src.to_string_value().chars().count() as i64,
        }
    }

    /// `nqp::substr($src, $from, $want)`'s answer, with the same clamping as
    /// `runtime/nqp_ops_str.rs`'s own `substr` and the same per-frame
    /// codepoint memo `trir_ord_at`/`trir_chars_len` use.
    fn trir_substr(&mut self, src: &Value, from: i64, want: i64) -> Value {
        match self.trir.chars.index_of(src) {
            Some(i) => Value::str(substr_of(self.trir.chars.chars_at(i), from, want)),
            None => {
                let s = src.to_string_value();
                let chars: Vec<char> = s.chars().collect();
                Value::str(substr_of(&chars, from, want))
            }
        }
    }

    /// `nqp::eqat($haystack, $needle, $pos)`'s answer, with the same
    /// haystack memo `trir_substr`/`trir_ord_at` use. The needle is not
    /// memoized — it is a fresh expression at almost every call site (a
    /// string literal or a short computed slice), so `runtime/
    /// nqp_ops_text.rs`'s own `eqat` does not cache it either.
    fn trir_eqat(&mut self, haystack: &Value, needle: &Value, pos: i64) -> i64 {
        let needle_chars: Vec<char> = needle.to_string_value().chars().collect();
        let matches = match self.trir.chars.index_of(haystack) {
            Some(i) => eqat_of(self.trir.chars.chars_at(i), &needle_chars, pos),
            None => {
                let s = haystack.to_string_value();
                let chars: Vec<char> = s.chars().collect();
                eqat_of(&chars, &needle_chars, pos)
            }
        };
        matches as i64
    }

    pub(super) fn trir_atpos_i(v: &Value, idx: i64) -> i64 {
        let Ok(i) = usize::try_from(idx) else {
            return 0;
        };
        // A plain `nqp::list_i` IS an array, and that is what every scanner's
        // lookup table is. Reading it directly skips `nqp_backing_array`'s
        // walk through the Buf/IterationBuffer/Uni shapes, which cost more
        // than the read.
        if let ValueView::Array(items, _) = v.view() {
            return items.get(i).and_then(|e| e.as_int()).unwrap_or(0);
        }
        match Self::nqp_elem_at(v, i) {
            Some(e) => e.as_int().unwrap_or_else(|| crate::runtime::to_int(&e)),
            None => 0,
        }
    }

    #[inline]
    fn bin_i(&mut self, op: fn(i64, i64) -> i64) {
        let r = self.ipop();
        let l = self.ipop();
        self.trir.ns.push(op(l, r));
    }

    #[inline]
    fn cmp_i(&mut self, p: fn(i64, i64) -> bool) {
        let r = self.ipop();
        let l = self.ipop();
        self.trir.ns.push(p(l, r) as i64);
    }

    #[inline]
    fn bin_n(&mut self, op: fn(f64, f64) -> f64) {
        let r = f(self.ipop());
        let l = f(self.ipop());
        self.trir.ns.push(b(op(l, r)));
    }

    #[inline]
    fn cmp_n(&mut self, p: fn(f64, f64) -> bool) {
        let r = f(self.ipop());
        let l = f(self.ipop());
        self.trir.ns.push(p(l, r) as i64);
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

/// `nqp::substr($s, $from, $want)`'s clamping, exactly as
/// `runtime/nqp_ops_str.rs`'s own `substr` computes it: a negative or
/// past-the-end `$from` clamps rather than dying, and a `$want` that runs
/// past the end truncates.
#[inline]
fn substr_of(chars: &[char], from: i64, want: i64) -> String {
    let total = chars.len();
    let from = (from.max(0) as usize).min(total);
    let want = if want < 0 { 0 } else { want as usize };
    let end = from.saturating_add(want).min(total);
    chars[from..end].iter().collect()
}

/// `nqp::eqat($haystack, $needle, $pos)`'s answer: whether `needle` occurs at
/// exactly codepoint offset `pos`, exactly as `runtime/nqp_ops_text.rs`'s own
/// `eqat` computes it.
#[inline]
fn eqat_of(chars: &[char], needle: &[char], pos: i64) -> bool {
    usize::try_from(pos)
        .ok()
        .and_then(|p| chars.get(p..p.saturating_add(needle.len())))
        .map(|window| window == needle)
        .unwrap_or(false)
}
