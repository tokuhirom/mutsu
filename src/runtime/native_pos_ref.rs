//! `IntPosRef` / `UIntPosRef` / `NumPosRef`: the lvalue `nqp::atposref_i`,
//! `_u` and `_n` answer for an element of native storage (#11209).
//!
//! An element of a plain array is a `Value` slot, and `atposref_*` on one
//! hands out that slot's shared cell (`array_slot_ref`). An element of native
//! storage -- a `Buf`, or a `CArray` whose `nqp::create` gave it a
//! [`BufData`](crate::value::BufData) node -- is a few bytes, with no cell to
//! share. MoarVM answers with a native reference container (its
//! `IntPosRef`, `UIntPosRef`, `NumPosRef` types), whose read decodes the
//! element and whose write encodes into it. Upstream `NativeCall::Types`'
//! typed `CArray` roles return exactly that from `AT-POS ... is raw`, so a
//! write through `my $r := $a[1]` lands in the array.
//!
//! mutsu's container with code on read and write is `Proxy`, so the
//! reference is a `Proxy` subclass named after MoarVM's type, whose FETCH
//! and STORE are the two routines below and whose attributes are the
//! storage holder and the index. Both read the live storage on every access:
//! nothing is copied, so the reference never goes stale.

use super::*;
use crate::symbol::Symbol;

/// The routine every native positional reference FETCHes through.
const FETCH: &str = "__mutsu_native_posref_fetch";
/// The routine every native positional reference STOREs through.
const STORE: &str = "__mutsu_native_posref_store";

/// How a native reference reads and writes its element.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum NativeRefKind {
    Int,
    UInt,
    Num,
}

impl NativeRefKind {
    /// The kind `nqp::atposref_{i,u,n}` asks for.
    // Cost: O(1).
    pub(crate) fn of_op(op: &str) -> Self {
        match op {
            "atposref_u" => Self::UInt,
            "atposref_n" => Self::Num,
            _ => Self::Int,
        }
    }

    /// MoarVM's name for this reference type, which `.VAR.^name` reports.
    // Cost: O(1).
    fn type_name(self) -> &'static str {
        match self {
            Self::Int => "IntPosRef",
            Self::UInt => "UIntPosRef",
            Self::Num => "NumPosRef",
        }
    }

    /// The kind a reference type name stands for.
    // Cost: O(1).
    fn of_type_name(name: &str) -> Option<Self> {
        Some(match name {
            "IntPosRef" => Self::Int,
            "UIntPosRef" => Self::UInt,
            "NumPosRef" => Self::Num,
            _ => return None,
        })
    }

    /// `v` as this kind's native value.
    // Cost: O(1) for a machine-word value; O(d), d = digits of a BigInt.
    fn native(self, v: &Value) -> Value {
        match self {
            Self::Int => Value::int(crate::runtime::to_int(v)),
            Self::UInt => super::nqp_ops_native::uint64_value(v),
            Self::Num => Value::num(v.to_f64()),
        }
    }

    /// This kind's zero, what an element past the end reads as.
    // Cost: O(1).
    fn zero(self) -> Value {
        match self {
            Self::Num => Value::num(0.0),
            _ => Value::int(0),
        }
    }
}

/// Whether `name` is one of the two routines a native reference calls.
// Cost: O(1).
pub(crate) fn is_native_pos_ref_routine(name: &str) -> bool {
    name == FETCH || name == STORE
}

/// A reference to element `idx` of `holder`'s native storage.
// Cost: O(1).
pub(crate) fn native_pos_ref(holder: Value, idx: i64, kind: NativeRefKind) -> Value {
    let mut attrs = crate::value::ValueMap::default();
    attrs.insert("target".to_string(), holder);
    attrs.insert("index".to_string(), Value::int(idx));
    let routine =
        |name: &str| Value::routine_parts(Symbol::intern(""), Symbol::intern(name), false);
    Value::proxy_parts(
        routine(FETCH),
        routine(STORE),
        Some((
            Symbol::intern(kind.type_name()),
            std::sync::Arc::new(std::sync::Mutex::new(attrs)),
        )),
        false,
    )
}

/// The storage holder, index and kind a native reference addresses.
// Cost: O(1).
fn parts(proxy: &Value) -> Result<(Value, i64, NativeRefKind), RuntimeError> {
    if let ValueView::Proxy {
        subclass: Some((name, attrs)),
        ..
    } = proxy.view()
        && let Some(kind) = NativeRefKind::of_type_name(name.as_str())
    {
        let attrs = attrs.lock().unwrap_or_else(|e| e.into_inner());
        if let (Some(target), Some(index)) = (attrs.get("target"), attrs.get("index")) {
            return Ok((target.clone(), crate::runtime::to_int(index), kind));
        }
    }
    Err(RuntimeError::new(
        "a native positional reference was invoked without its element",
    ))
}

/// FETCH: the element's current value, read from the storage.
// Cost: O(1) (one element decoded at the storage's own width).
fn fetch(args: &[Value]) -> Result<Value, RuntimeError> {
    let (target, idx, kind) = parts(args.first().unwrap_or(&Value::NIL))?;
    Ok(crate::runtime::nqp_backing::elem_at(&target, idx)?
        .map_or_else(|| kind.zero(), |v| kind.native(&v)))
}

/// STORE: encode the value into the element (growing the storage when the
/// index is past the end, as `nqp::bindpos_*` does). Answers the value stored.
// Cost: O(1) amortized; O(i - e) when growing, i = index, e = elements.
fn store(args: &[Value]) -> Result<Value, RuntimeError> {
    let (target, idx, kind) = parts(args.first().unwrap_or(&Value::NIL))?;
    let val = kind.native(&args.get(1).cloned().unwrap_or(Value::NIL).deref_container());
    crate::runtime::nqp_backing::bind_elem("bindpos", &target, idx, val, kind.zero())
}

impl Interpreter {
    /// Dispatch one of the two routines a native reference calls; `None` for
    /// any other name.
    // Cost: O(1) per element access (see `fetch` / `store`).
    pub(crate) fn try_native_pos_ref_routine(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        match name {
            FETCH => Some(fetch(args)),
            STORE => Some(store(args)),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reference_writes_and_reads_the_storage() {
        let buf = crate::value::value_buf::make_buf(
            Symbol::intern("Buf[int32]"),
            vec![Value::int(1), Value::int(2)],
        );
        let r = native_pos_ref(buf.clone(), 1, NativeRefKind::Int);
        assert_eq!(fetch(std::slice::from_ref(&r)).unwrap(), Value::int(2));
        store(&[r.clone(), Value::int(-9)]).unwrap();
        assert_eq!(fetch(&[r]).unwrap(), Value::int(-9));
        // Past the end reads as zero.
        let far = native_pos_ref(buf, 5, NativeRefKind::Num);
        assert_eq!(fetch(&[far]).unwrap(), Value::num(0.0));
    }
}
