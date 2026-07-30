//! `nativecast(<Signature>, $ptr)` — turn a raw C function pointer into a
//! callable Raku routine.
//!
//! An `is native` sub binds a *named* symbol in a library at declaration time.
//! But a binding that looks a symbol up itself — `dlsym`, or Windows'
//! `GetProcAddress` — ends up holding only an address, and needs to attach a
//! signature to it after the fact. `NativeLibs::Loader.symbol` is exactly this:
//!
//! ```raku
//! my $dll = NativeLibs::Loader.load('libm.so.6');
//! say $dll.symbol('sin', :(num64 --> num64))(pi / 2);   # 1
//! ```
//!
//! The result is represented as a `Routine` value whose name is a synthetic key
//! into `native_call_specs`, so every call path that already dispatches a native
//! sub by name (the two VM call opcodes, `call_sub_value`, `vm_call_on_value`)
//! handles it with no further plumbing.

use crate::runtime::Interpreter;
use crate::runtime::nativecall::{CType, NativeCallSpec, ParamSpec};
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};

/// Prefix of the synthetic `native_call_specs` key. Not a legal Raku identifier
/// (`#`), so it can never collide with a declared sub.
const FNPTR_KEY_PREFIX: &str = "__mutsu_native_fnptr#";

impl Interpreter {
    /// Build a callable for the C function at `ptr`'s address, marshalled per
    /// the signature registered under Signature instance `sig_id`.
    pub(crate) fn native_callable_from_signature(
        &mut self,
        sig_id: u64,
        ptr: &Value,
    ) -> Result<Value, RuntimeError> {
        let Some(info) = crate::value::signature::lookup_sig_info(sig_id) else {
            return Err(RuntimeError::new(
                "nativecast(): cannot recover the signature to cast to",
            ));
        };
        let addr = crate::runtime::nativecall::value_c_address(ptr);
        if addr == 0 {
            return Err(RuntimeError::new(
                "nativecast(): cannot cast a NULL pointer to a Signature",
            ));
        }

        let mut params = Vec::with_capacity(info.params.len());
        for p in &info.params {
            // A `--> T`-only signature has no parameters; an invocant slot in a
            // cast signature is not a C argument (there is no invocant to pass).
            if p.is_invocant {
                continue;
            }
            let Some(tc) = p.type_constraint.as_deref() else {
                return Err(RuntimeError::new(format!(
                    "nativecast(): parameter '{}' has no type, so it cannot be marshalled to C",
                    p.name
                )));
            };
            params.push(self.param_spec_for_native_type(tc, p.traits.iter().any(|t| t == "rw"))?);
        }

        // `Mu` is what a signature with no `-->` reports, and it means "no
        // declared return type" here, i.e. C `void`.
        let ret_name = info.return_type.as_deref().filter(|t| *t != "Mu");
        let (ret, ret_struct) = match ret_name {
            None => (CType::Void, None),
            Some(rt) => {
                let spec = self.param_spec_for_native_type(rt, false)?;
                let ret_struct = if spec.ct == CType::Pointer && self.is_cstruct_class(rt) {
                    Some(rt.rsplit("::").next().unwrap_or(rt).to_string())
                } else {
                    None
                };
                (spec.ct, ret_struct)
            }
        };

        let key = format!("{FNPTR_KEY_PREFIX}{addr:x}");
        self.native_call_specs.insert(
            key.clone(),
            NativeCallSpec {
                library: None,
                symbol: key.clone(),
                params,
                ret,
                ret_struct,
                entry: Some(addr),
            },
        );
        Ok(Value::routine_parts(
            Symbol::intern("GLOBAL"),
            Symbol::intern(&key),
            false,
        ))
    }

    /// Map one signature type name to its C marshalling spec, resolving
    /// `constant` type aliases and treating a CStruct-ish class as a pointer —
    /// the same rules `register_native_call_routine` applies to a declared
    /// `is native` signature.
    fn param_spec_for_native_type(
        &mut self,
        type_name: &str,
        is_rw: bool,
    ) -> Result<ParamSpec, RuntimeError> {
        let base = type_name
            .strip_suffix(":D")
            .or_else(|| type_name.strip_suffix(":U"))
            .or_else(|| type_name.strip_suffix(":_"))
            .unwrap_or(type_name);
        let resolved = self.resolve_native_type_alias(base);
        let base = resolved.as_str();
        if let Some(inner) = base
            .strip_prefix("CArray[")
            .and_then(|s| s.strip_suffix(']'))
        {
            let inner = self.resolve_native_type_alias(inner);
            let Some(elem) = CType::from_type_name(&inner) else {
                return Err(RuntimeError::new(format!(
                    "nativecast(): CArray element type '{inner}' cannot be marshalled to C"
                )));
            };
            return Ok(ParamSpec {
                ct: CType::CArray,
                is_rw,
                elem: Some(elem),
            });
        }
        if base == "CArray" {
            return Ok(ParamSpec {
                ct: CType::CArray,
                is_rw,
                elem: None,
            });
        }
        // A typed `Pointer[T]` is still one pointer.
        let stem = base.split_once('[').map_or(base, |(b, _)| b);
        if let Some(ct) = CType::from_type_name(stem) {
            return Ok(ParamSpec {
                ct,
                is_rw,
                elem: None,
            });
        }
        // Any class held by reference in C (a CStruct / CPointer handle).
        if self.is_cstruct_class(base) || self.is_native_handle_class(base) {
            return Ok(ParamSpec {
                ct: CType::Pointer,
                is_rw,
                elem: None,
            });
        }
        Err(RuntimeError::new(format!(
            "nativecast(): type '{type_name}' cannot be marshalled to C"
        )))
    }
}
