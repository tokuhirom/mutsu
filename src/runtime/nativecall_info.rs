//! Upstream NativeCall's argument/return-info hashes, read into the
//! [`NativeCallSpec`] the FFI machinery marshals against, and the store that
//! holds a built call for its callsite (ADR-11203 §2.3, #11211).
//!
//! `NativeCall.rakumod`'s `param_hash_for` / `return_hash_for` describe each
//! parameter and the return value as an `nqp::hash`:
//!
//! | key | meaning |
//! | --- | --- |
//! | `type` | the NCI type code (`type_code_for`): `"int"`, `"ulong"`, `"utf8str"`, `"cpointer"`, `"carray"`, ... |
//! | `rw` | an `is rw` parameter: C receives a pointer to the value |
//! | `free_str` | a string argument's temporary copy is freed after the call |
//! | `typeobj` | the declared type object |
//! | `callback_args` | for a `callback`: the return hash, then one hash per parameter |
//! | `entry_point` | (return hash) a `Pointer` to call instead of resolving the symbol |
//!
//! MoarVM reads the same hashes in `MVM_nativecall_build`
//! (`src/core/nativecall.c`); the type-code table is
//! `MVM_nativecall_get_arg_type`'s.

use std::collections::HashMap;
use std::sync::{Arc, Mutex, OnceLock};

use crate::runtime::nativecall::{CType, CallbackSig, NativeCallSpec, ParamSpec};
use crate::value::{RuntimeError, Value, ValueView};

/// The C type an NCI type code names, on an LP64 platform (`long` and
/// `size_t` are 64-bit), as `MVM_nativecall_get_arg_type` maps them. `char`
/// is signed, as `type_code_for` treats `int8` -> `"char"`.
// Cost: O(1).
fn ctype_for_code(code: &str) -> Result<CType, RuntimeError> {
    Ok(match code {
        "void" => CType::Void,
        "char" | "bool" => CType::I8,
        "short" => CType::I16,
        "int" => CType::I32,
        "long" | "longlong" | "ssize_t" => CType::I64,
        "uchar" => CType::U8,
        "ushort" => CType::U16,
        "uint" => CType::U32,
        "ulong" | "ulonglong" | "size_t" => CType::U64,
        "float" => CType::F32,
        "double" => CType::F64,
        // A NUL-terminated `char*`. ASCII is a subset of UTF-8, so the same
        // encoder serves both.
        "utf8str" | "asciistr" => CType::Str,
        // Every by-reference aggregate is one pointer at the C boundary; its
        // address is what `value_c_address` reads off the argument.
        "cpointer" | "cstruct" | "cppstruct" | "cunion" => CType::Pointer,
        "carray" => CType::CArray,
        "vmarray" => CType::Buf,
        "callback" => CType::Callback,
        // TODO: `long double` has no libffi type in the marshaller yet, and a
        // UTF-16 string needs its own encoder and terminator; both are rare
        // enough in the bundled bindings that they report loudly for now.
        "longdouble" | "utf16str" => {
            return Err(RuntimeError::new(format!(
                "NativeCall: the '{code}' type is not supported yet"
            )));
        }
        other => {
            return Err(RuntimeError::new(format!(
                "NativeCall: unknown native type code '{other}'"
            )));
        }
    })
}

/// `$hash<$key>`, or `None` when absent or `$hash` is not a hash.
fn info_get(info: &Value, key: &str) -> Option<Value> {
    match info.view() {
        ValueView::Hash(map) => map.get(key).cloned(),
        _ => None,
    }
}

/// A hash's `type` code.
fn info_type_code(info: &Value) -> Result<String, RuntimeError> {
    info_get(info, "type")
        .map(|v| v.to_string_value())
        .ok_or_else(|| RuntimeError::new("NativeCall: an argument-info hash has no 'type'"))
}

/// The elements of an `nqp::list`.
fn list_items(v: &Value) -> Vec<Value> {
    match v.view() {
        ValueView::Array(items, _) => items.iter().cloned().collect(),
        _ => Vec::new(),
    }
}

/// The element C type of a `CArray[T]` type object, when the parameterisation
/// is visible in its name. `None` lets the marshaller read it off the argument
/// (which is also how the unparameterised `CArray` spelling works).
fn carray_elem_of(typeobj: Option<&Value>) -> Option<CType> {
    let name = match typeobj?.view() {
        ValueView::Package(n) => n.resolve(),
        _ => return None,
    };
    let short = crate::runtime::cstruct_layout::short_base_name(&name);
    let inner = short.strip_prefix("CArray[")?.strip_suffix(']')?;
    CType::from_type_name(crate::runtime::cstruct_layout::short_base_name(inner))
}

/// One parameter's marshalling, from its `param_hash_for` hash.
// Cost: O(c), c = parameters of a callback's signature; O(1) otherwise.
fn param_spec_from_info(info: &Value) -> Result<ParamSpec, RuntimeError> {
    let ct = ctype_for_code(&info_type_code(info)?)?;
    let is_rw = info_get(info, "rw").is_some_and(|v| v.truthy());
    Ok(match ct {
        CType::CArray => {
            ParamSpec::carray(carray_elem_of(info_get(info, "typeobj").as_ref()), is_rw)
        }
        CType::Callback => {
            let sig = callback_sig_from_info(info_get(info, "callback_args").as_ref())?;
            ParamSpec {
                callback: Some(Box::new(sig)),
                ..ParamSpec::scalar(CType::Callback, is_rw)
            }
        }
        _ => ParamSpec::scalar(ct, is_rw),
    })
}

/// A callback parameter's C signature: `callback_args` holds the return hash
/// first, then one hash per parameter.
// Cost: O(c), c = parameters of the callback's signature.
fn callback_sig_from_info(args: Option<&Value>) -> Result<CallbackSig, RuntimeError> {
    let items = args.map(list_items).unwrap_or_default();
    let Some((ret, params)) = items.split_first() else {
        return Err(RuntimeError::new(
            "NativeCall: a callback parameter has no signature information",
        ));
    };
    let params = params
        .iter()
        .map(|p| info_type_code(p).and_then(|code| ctype_for_code(&code)))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(CallbackSig {
        params,
        ret: ctype_for_code(&info_type_code(ret)?)?,
    })
}

/// Build the call descriptor `nqp::buildnativecall` records. `library` is
/// already resolved by upstream's `guess_library_name`; empty means the
/// process's own symbol space.
///
/// Calling conventions are not read: on the platforms mutsu ships (Linux and
/// macOS, x86-64 and arm64) there is one C calling convention, and
/// `is nativeconv` only selects among Windows-x86 ones.
// Cost: O(p), p = parameters (callback signatures included).
pub(crate) fn spec_from_info(
    library: &str,
    symbol: &str,
    arg_info: &Value,
    ret_info: &Value,
) -> Result<NativeCallSpec, RuntimeError> {
    if info_get(ret_info, "variadic").is_some_and(|v| v.truthy()) {
        // TODO: a variadic C function needs a variadic CIF (`ffi_prep_cif_var`)
        // built per call from the actual arguments.
        return Err(RuntimeError::new(format!(
            "NativeCall: variadic native function '{symbol}' is not supported yet"
        )));
    }
    let params = list_items(arg_info)
        .iter()
        .map(param_spec_from_info)
        .collect::<Result<Vec<_>, _>>()?;
    let entry = info_get(ret_info, "entry_point")
        .filter(crate::runtime::types::value_is_defined)
        .map(|p| crate::runtime::nativecall::value_c_address(&p));
    Ok(NativeCallSpec {
        library: (!library.is_empty()).then(|| library.to_string()),
        symbol: symbol.to_string(),
        params,
        ret: ctype_for_code(&info_type_code(ret_info)?)?,
        // Filled in per call from `nqp::nativecall`'s `$rettype`.
        ret_struct: None,
        entry,
    })
}

/// The object a built call belongs to: a NativeCall-REPR instance, or the
/// routine upstream passes as `self` (whose `$!call` attribute is its box
/// target in MoarVM).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum CallsiteKey {
    Instance(u64),
    Code(u64),
}

impl CallsiteKey {
    /// The callsite identity of `v`, or `None` for a value that cannot carry a
    /// NativeCall body.
    // Cost: O(1).
    pub(crate) fn of(v: &Value) -> Option<CallsiteKey> {
        match v.view() {
            ValueView::Instance { id, .. } => Some(CallsiteKey::Instance(id)),
            ValueView::Sub(data) => Some(CallsiteKey::Code(data.id)),
            ValueView::WeakSub(w) => w.upgrade().map(|s| CallsiteKey::Code(s.id)),
            ValueView::Mixin(inner, _) => CallsiteKey::of(inner),
            ValueView::Scalar(inner) => CallsiteKey::of(inner),
            _ => None,
        }
    }
}

/// Built calls, by callsite. Process-wide, like MoarVM's NativeCall bodies,
/// so a call built on one thread is callable from another. An entry lives as
/// long as the process: it is a few words per distinct callsite, the same
/// bounded leak `load_library_cached` and the callback closures accept.
fn callsites() -> &'static Mutex<HashMap<CallsiteKey, Arc<NativeCallSpec>>> {
    static CALLSITES: OnceLock<Mutex<HashMap<CallsiteKey, Arc<NativeCallSpec>>>> = OnceLock::new();
    CALLSITES.get_or_init(Default::default)
}

/// Record `spec` as `key`'s built call, replacing an earlier build.
// Cost: O(1) amortized.
pub(crate) fn store_callsite(key: CallsiteKey, spec: NativeCallSpec) {
    let mut table = callsites().lock().unwrap_or_else(|e| e.into_inner());
    table.insert(key, Arc::new(spec));
}

/// A copy of `key`'s built call, to be adjusted for one invocation.
// Cost: O(p), p = parameters (the descriptor is cloned).
pub(crate) fn load_callsite(key: CallsiteKey) -> Option<NativeCallSpec> {
    let table = callsites().lock().unwrap_or_else(|e| e.into_inner());
    table.get(&key).map(|spec| NativeCallSpec::clone(spec))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn type_codes_follow_lp64() {
        assert_eq!(ctype_for_code("long").unwrap(), CType::I64);
        assert_eq!(ctype_for_code("ulong").unwrap(), CType::U64);
        assert_eq!(ctype_for_code("int").unwrap(), CType::I32);
        assert_eq!(ctype_for_code("uchar").unwrap(), CType::U8);
        assert_eq!(ctype_for_code("cstruct").unwrap(), CType::Pointer);
        assert_eq!(ctype_for_code("vmarray").unwrap(), CType::Buf);
        assert!(ctype_for_code("utf16str").is_err());
        assert!(ctype_for_code("nonsense").is_err());
    }
}
