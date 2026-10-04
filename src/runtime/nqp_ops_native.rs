//! The `nqp::` ops for native (unsigned / num) boxes, native element
//! references and code-object bookkeeping.
//!
//! Last link of the chained `nqp::` tables (`... -> nqp_ops_list -> here`).
//! These are the non-FFI ops that rakudo's `NativeCall.rakumod` and
//! `NativeCall/Types.rakumod` use (ADR-11203, #11206): `unbox_n`/`unbox_u` and
//! `atpos_u`/`bindpos_u` for typed `CArray` element traffic, `atposref_{i,n,u}` for an
//! element's lvalue, and `setcodename`/`neverrepossess` for the routine body
//! NativeCall's backend-neutral path installs. The FFI ops themselves
//! (`nqp::buildnativecall` and kin, #11211) follow the Rakudo `p6*` ops
//! (`nqp_ops_p6.rs`) at the end of the chain, in `nativecall_nqp.rs`.

use super::*;
use crate::value::ValueView;
use num_bigint::BigInt as NumBigInt;

/// An operand with any argument-wrapper and container stripped: the `nqp::`
/// layer reads raw values (see `call_nqp_op`'s boundary note).
fn operand(args: &[Value], i: usize) -> Value {
    crate::runtime::types::unwrap_varref_value(args.get(i).cloned().unwrap_or(Value::NIL))
        .deref_container()
}

/// `$v` read as a native `uint64`: an `Int` of any size wraps modulo 2**64,
/// as MoarVM's unsigned unbox does (`-1` reads as `2**64 - 1`). A boxed Int
/// subclass contributes its native payload, as for `nqp::unbox_i`.
// Cost: O(1) for an Int that fits a machine word; O(d), d = digits of a BigInt.
pub(crate) fn uint64_value(v: &Value) -> Value {
    let wide = match v.view() {
        ValueView::Int(n) => NumBigInt::from(n),
        ValueView::BigInt(b) => NumBigInt::clone(&b),
        ValueView::Instance { attributes, .. } => {
            match attributes.as_map().get("__mutsu_int_value") {
                Some(payload) => payload.to_bigint(),
                None => NumBigInt::from(crate::runtime::to_int(v)),
            }
        }
        _ => NumBigInt::from(crate::runtime::to_int(v)),
    };
    let wrapped = crate::native_types::wrap_native_int("uint64", &wide);
    match i64::try_from(&wrapped) {
        Ok(n) => Value::int(n),
        Err(_) => Value::bigint(wrapped),
    }
}

impl Interpreter {
    /// Try a native-box / native-ref / code-object `nqp::` op. `None` means
    /// "not an op this table knows"; the caller then raises the loud
    /// unsupported-op error.
    pub(crate) fn call_nqp_op_native(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        Some(match op {
            // nqp::unbox_n($num): the native num inside a boxed `Num`. Like
            // MoarVM, only a Num unboxes to a num -- an `Int` is a different
            // REPR, and the caller is expected to coerce first.
            // Cost: O(1).
            "unbox_n" => {
                let v = operand(args, 0);
                match v.view() {
                    ValueView::Num(n) => Ok(Value::num(n)),
                    _ => Err(RuntimeError::new(format!(
                        "This type cannot unbox to a native number: P6opaque, {}",
                        crate::value::type_name::value_type_name(&v)
                    ))),
                }
            }
            // nqp::unbox_u($int): the native `uint64` inside a boxed Int.
            // Cost: O(1) for a machine-word Int; O(d), d = digits of a BigInt.
            "unbox_u" => Ok(uint64_value(&operand(args, 0))),
            // nqp::atpos_u($list, $i): element `$i` read as a native `uint64`;
            // 0 past the end, as for `atpos_i`.
            // Cost: O(1) on an array and on a Buf (one element decoded at its own width).
            "atpos_u" => {
                let target = operand(args, 0);
                let idx = args.get(1).map(crate::runtime::to_int).unwrap_or(0);
                crate::runtime::nqp_backing::elem_at(&target, idx)
                    .map(|e| e.map_or(Value::int(0), |v| uint64_value(&v)))
            }
            // nqp::bindpos_u($list, $i, $u): store an unsigned element. A
            // Buf/CArray encodes it at its own width; a native array stores
            // the value read as a `uint64`.
            // Cost: O(1) amortized; O(i - e) when growing, i = index, e = elements.
            "bindpos_u" => {
                let target = operand(args, 0);
                let idx = args.get(1).map(crate::runtime::to_int).unwrap_or(0);
                let val = uint64_value(&operand(args, 2));
                if self.carray_bind_before_start(&target, idx) {
                    return Some(Ok(val));
                }
                crate::runtime::nqp_backing::bind_elem(op, &target, idx, val, Value::int(0))
            }
            // nqp::atposref_i / _n / _u($list, $i): an lvalue for element `$i`
            // -- the same container a `:=` bind to `@list[$i]` produces, so a
            // write through it lands in the list. An element of native storage
            // (a Buf, a CArray) has no slot to share, so it gets a native
            // reference that reads and writes the bytes (`IntPosRef` & co.).
            // Cost: O(1) for an element in range (promoted to a shared cell once).
            "atposref_i" | "atposref_n" | "atposref_u" | "atposref_s" => {
                let target = operand(args, 0);
                if crate::value::value_buf::buf_target(&target).is_some()
                    || crate::runtime::CArrayView::of(&target).is_some()
                {
                    // A Buf holds numbers; it has no str element to refer to.
                    if op == "atposref_s" {
                        return Some(Err(RuntimeError::new(
                            "nqp::atposref_s: a Buf has no str elements",
                        )));
                    }
                    let len = Interpreter::nqp_elems_len_of(&target).unwrap_or(0);
                    let idx = args.get(1).map(crate::runtime::to_int).unwrap_or(0);
                    return Some(
                        crate::runtime::nqp_backing::resolve_index(idx, len).map(|i| {
                            crate::runtime::native_pos_ref::native_pos_ref(
                                target,
                                i as i64,
                                crate::runtime::native_pos_ref::NativeRefKind::of_op(op),
                            )
                        }),
                    );
                }
                let ValueView::Array(arr, _) = target.view() else {
                    return Some(Err(RuntimeError::new(format!(
                        "nqp::{op}: {} does not support positional references",
                        crate::value::type_name::value_type_name(&target)
                    ))));
                };
                let len = arr.len();
                let idx = args.get(1).map(crate::runtime::to_int).unwrap_or(0);
                match crate::runtime::nqp_backing::resolve_index(idx, len) {
                    Ok(i) => Ok(target.array_slot_ref(i, true).unwrap_or(Value::NIL)),
                    Err(e) => Err(e),
                }
            }
            // nqp::setcodename($code, $name): rename a code object in place,
            // the same write `Code.set_name` makes, so a routine whose `$!do`
            // is `$code` reports the new name too. Returns the code object.
            // Cost: as `Interpreter::rename_code`.
            "setcodename" => {
                let code = operand(args, 0);
                let name = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
                if self.rename_code(&code, &name) {
                    Ok(code)
                } else {
                    Err(RuntimeError::new(
                        "setcodename needs a code ref".to_string(),
                    ))
                }
            }
            // nqp::getcodename($code): the code object's name, which
            // `Code.name` also answers (one name holder, #11462) -- so a
            // `setcodename` on a routine's `$!do` reads back here.
            // Cost: as `Interpreter::code_name`.
            "getcodename" => {
                let code = operand(args, 0);
                match Interpreter::unwrap_callable_mixin(code).view() {
                    ValueView::Sub(data) => Ok(Value::str(self.code_name(&data).resolve())),
                    _ => Err(RuntimeError::new(
                        "getcodename requires a concrete code object".to_string(),
                    )),
                }
            }
            // nqp::takeclosure($block): capture the block's outer lexicals as
            // they are now. A mutsu block value already is a closure over its
            // creation-time environment, so the block itself is the answer --
            // as Rakudo's `nqp::eqaddr($b, nqp::takeclosure($b))` also says.
            // Cost: O(1).
            "takeclosure" => Ok(operand(args, 0)),
            // nqp::setdebugtypename($type, $name): name a type for the VM's
            // debugging output only (`.^name` is unchanged). mutsu has no
            // separate debug name, so the type comes back untouched.
            // Cost: O(1).
            "setdebugtypename" => Ok(operand(args, 0)),
            // nqp::neverrepossess($obj): exempt an object from repossession by
            // a later serialization context. mutsu does not serialize compiled
            // modules, so there is nothing to exempt it from.
            // Cost: O(1).
            "neverrepossess" => Ok(operand(args, 0)),
            // The coercion / conversion / value-test ops (`nqp_ops_coerce.rs`),
            // then the FFI ops (`nativecall_nqp.rs`).
            _ => return self.call_nqp_op_coerce(op, args),
        })
    }
}
