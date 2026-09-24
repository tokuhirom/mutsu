//! String, list and narrowing helpers behind TRIR's typed ops.

use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value};

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
    /// `nqp::ordat`'s answer: the same routine the untyped op runs
    /// (`builtins::str_prim`, ADR-0117).
    pub(super) fn trir_ord_at(&mut self, src: &Value, pos: i64) -> i64 {
        crate::builtins::str_prim::nqp_ordat(src, pos)
    }

    /// `nqp::chars`'s answer, in graphemes.
    pub(super) fn trir_chars_len(&mut self, src: &Value) -> i64 {
        crate::builtins::str_prim::chars(src) as i64
    }

    /// `nqp::substr($src, $from, $want)`'s answer.
    pub(super) fn trir_substr(
        &mut self,
        src: &Value,
        from: i64,
        want: i64,
    ) -> Result<Value, RuntimeError> {
        crate::builtins::str_prim::nqp_substr(src, from, Some(want))
    }

    /// `nqp::eqat($haystack, $needle, $pos)`'s answer.
    pub(super) fn trir_eqat(&mut self, haystack: &Value, needle: &Value, pos: i64) -> i64 {
        crate::builtins::grapheme_index::with_str(needle, |needle| {
            crate::builtins::str_prim::nqp_eqat(
                haystack,
                needle,
                pos,
                crate::builtins::str_prim::Fold::Exact,
            )
        }) as i64
    }

    /// `nqp::atpos_i`'s answer: the op's own body (`nqp_backing::atpos_i`).
    pub(super) fn trir_atpos_i(v: &Value, idx: i64) -> Result<i64, RuntimeError> {
        crate::runtime::nqp_backing::atpos_i(v, idx)
    }
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
