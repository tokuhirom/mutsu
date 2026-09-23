//! String, list and narrowing helpers behind TRIR's typed ops.

use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

impl Interpreter {
    #[inline]
    pub(super) fn bin_i(&mut self, op: fn(i64, i64) -> i64) {
        let r = self.ipop();
        let l = self.ipop();
        self.trir.ns.push(op(l, r));
    }

    #[inline]
    pub(super) fn cmp_i(&mut self, p: fn(i64, i64) -> bool) {
        let r = self.ipop();
        let l = self.ipop();
        self.trir.ns.push(p(l, r) as i64);
    }

    #[inline]
    pub(super) fn bin_n(&mut self, op: fn(f64, f64) -> f64) {
        let r = f64::from_bits(self.ipop() as u64);
        let l = f64::from_bits(self.ipop() as u64);
        self.trir.ns.push(op(l, r).to_bits() as i64);
    }

    #[inline]
    pub(super) fn cmp_n(&mut self, p: fn(f64, f64) -> bool) {
        let r = f64::from_bits(self.ipop() as u64);
        let l = f64::from_bits(self.ipop() as u64);
        self.trir.ns.push(p(l, r) as i64);
    }

    /// The untyped path's check for a value stored into a native integer
    /// variable (`vm_misc_typecheck.rs`'s native-int arm), for a value that
    /// is not already an `Int`.
    pub(super) fn trir_narrow_store(
        &mut self,
        type_name: &str,
        v: Value,
    ) -> Result<i64, RuntimeError> {
        if v.is_nil() {
            return Err(RuntimeError::new(format!(
                "Cannot unbox a type object (Nil) to {type_name}."
            )));
        }
        self.stack.push(v);
        let checked = self
            .validate_native_int_assignment(type_name, &self.stack[self.stack.len() - 1].clone());
        let v = self.stack.pop().unwrap_or(Value::NIL);
        checked?;
        Ok(v.as_int().unwrap_or_else(|| crate::runtime::to_int(&v)))
    }

    /// Element `idx` of a list, exactly as `runtime/nqp_ops.rs`'s own
    /// `atpos_i` reads it: 0 both past the end and for a target with no
    /// elements at all.
    /// `nqp::ordat`'s answer, with the same non-string coercion
    /// `nqp_char_cache::cached_chars` performs.
    pub(super) fn trir_ord_at(&mut self, src: &Value, pos: i64) -> i64 {
        match self.trir.chars.index_of(src) {
            Some(i) => ord_at(self.trir.chars.chars_at(i), pos),
            None => {
                let s = src.to_string_value();
                ord_at(&s.chars().collect::<Vec<char>>(), pos)
            }
        }
    }

    /// `nqp::chars`'s answer, with the same coercion.
    pub(super) fn trir_chars_len(&mut self, src: &Value) -> i64 {
        match self.trir.chars.index_of(src) {
            Some(i) => self.trir.chars.chars_at(i).len() as i64,
            None => crate::builtins::grapheme_index::codepoint_count(src) as i64,
        }
    }

    /// `nqp::substr($src, $from, $want)`'s answer, with the same clamping as
    /// `runtime/nqp_ops_str.rs`'s own `substr` and the same per-frame
    /// codepoint memo `trir_ord_at`/`trir_chars_len` use.
    pub(super) fn trir_substr(&mut self, src: &Value, from: i64, want: i64) -> Value {
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
    pub(super) fn trir_eqat(&mut self, haystack: &Value, needle: &Value, pos: i64) -> i64 {
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

/// The low `bits` bits of `v`, sign-extended when `signed`: a store into a
/// sized native integer variable.
#[inline]
pub(super) fn wrap_sized(v: i64, bits: u8, signed: bool) -> i64 {
    let shift = 64 - u32::from(bits);
    if signed {
        (v << shift) >> shift
    } else {
        ((v as u64) << shift >> shift) as i64
    }
}
