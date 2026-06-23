//! Minimal NativeCall (C FFI) support.
//!
//! A sub declared with the `is native(...)` trait and a `{ * }` body is not run
//! as Raku code; instead, at call time we `dlopen` the named shared library,
//! `dlsym` the symbol, build a libffi CIF from the declared parameter / return
//! types, marshal the Raku argument `Value`s into C values, perform the call,
//! and marshal the C return value back into a `Value`.
//!
//! This is the MVP: scalar C types (signed/unsigned 8/16/32/64-bit integers,
//! 32/64-bit floats), `Str` passed as a NUL-terminated `char*`, and an opaque
//! `Pointer`. Aggregates (`CStruct`/`CArray`/`CUnion`), callbacks, and typed
//! pointers are follow-up work.

use crate::value::{RuntimeError, Value};

/// A C type a native parameter or return value can take.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CType {
    Void,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    /// `Str` marshalled as a NUL-terminated `char*` (input only in the MVP).
    Str,
    /// Opaque `Pointer` / `Pointer[T]` — carried as an integer address.
    Pointer,
}

impl CType {
    /// Map a Raku type-constraint name (from a signature) to a C type.
    /// Returns `None` for an unrecognized / unsupported type name.
    pub fn from_type_name(name: &str) -> Option<CType> {
        Some(match name {
            "int8" => CType::I8,
            "int16" => CType::I16,
            "int32" => CType::I32,
            "int64" | "long" | "longlong" | "int" | "Int" => CType::I64,
            "uint8" | "byte" => CType::U8,
            "uint16" => CType::U16,
            "uint32" => CType::U32,
            "uint64" | "ulong" | "ulonglong" | "uint" | "size_t" => CType::U64,
            "num32" => CType::F32,
            "num64" | "num" | "Num" => CType::F64,
            "Str" => CType::Str,
            "Pointer" | "OpaquePointer" => CType::Pointer,
            _ => return None,
        })
    }
}

/// One native parameter: its C type plus whether it was declared `is rw`.
/// An `is rw` `Pointer` is an *out-parameter*: C receives a `void**` and writes
/// the resulting pointer back into the caller's `Pointer` object.
// Fields are read only in the `libffi` build (the wasm stub ignores them).
#[cfg_attr(not(feature = "libffi"), allow(dead_code))]
#[derive(Debug, Clone, Copy)]
pub struct ParamSpec {
    pub ct: CType,
    pub is_rw: bool,
}

/// The resolved descriptor for one `is native` sub: which library/symbol to
/// call and the C signature to marshal against.
// In a non-`libffi` build (wasm) the fields are only written, never read (the
// `call_native` stub just errors), so suppress the dead-code lint there.
#[cfg_attr(not(feature = "libffi"), allow(dead_code))]
#[derive(Debug, Clone)]
pub struct NativeCallSpec {
    /// Library path/name as written in `is native(...)`. `None` means the trait
    /// had no argument (look the symbol up in the C library / main program).
    pub library: Option<String>,
    /// C symbol name — from `is symbol('...')` if present, else the sub name.
    pub symbol: String,
    pub params: Vec<ParamSpec>,
    pub ret: CType,
}

/// Candidate OS library file names for a `is native(<arg>)` argument, applying
/// Raku's convention of supplying the platform prefix/suffix when only a bare
/// stem is given. Several candidates are returned because the unversioned dev
/// symlink (`libfoo.so`) is often a linker script or absent on a runtime-only
/// system, while the versioned runtime object (`libfoo.so.N`) is present.
#[cfg(feature = "libffi")]
fn resolve_library_candidates(library: &Option<String>) -> Vec<String> {
    let stem = match library {
        // No argument, or the conventional "c" alias → the C runtime.
        None => return vec!["libc.so.6".to_string()],
        Some(name) if name == "c" => return vec!["libc.so.6".to_string()],
        Some(name) if name == "m" => {
            // libm is merged into glibc; prefer the versioned objects.
            return vec![
                "libm.so.6".to_string(),
                "libc.so.6".to_string(),
                "libm.so".to_string(),
            ];
        }
        Some(name) => name,
    };
    // An explicit path or already-decorated name is used verbatim.
    if stem.contains('/') || stem.contains(".so") || stem.contains(".dylib") {
        return vec![stem.clone()];
    }
    // A bare stem like "sqlite3" → try the dev symlink and common runtime sonames.
    vec![
        format!("lib{stem}.so"),
        format!("lib{stem}.so.0"),
        format!("lib{stem}.so.1"),
        format!("lib{stem}.so.2"),
    ]
}

/// Process-lifetime cache of dlopen'd libraries, keyed by the candidate name
/// that successfully loaded. **Loading must persist across calls**: a `Library`
/// dropped at the end of a call `dlclose`s it, and for a library that was not
/// already resident (e.g. `libsqlite3`) that unloads it — invalidating every
/// pointer the program obtained from it (a `sqlite3*` handle becomes a
/// dangling pointer, so the next call segfaults). Leaking the handle to
/// `'static` keeps each library mapped for the program's lifetime, matching
/// Rakudo's NativeCall (which never unloads).
#[cfg(feature = "libffi")]
fn load_library_cached(
    candidates: &[String],
) -> Result<(&'static libloading::Library, &'static str), RuntimeError> {
    use std::collections::HashMap;
    use std::sync::{Mutex, OnceLock};
    static CACHE: OnceLock<Mutex<HashMap<String, &'static libloading::Library>>> = OnceLock::new();
    let cache = CACHE.get_or_init(|| Mutex::new(HashMap::new()));
    let mut guard = cache.lock().unwrap();

    let mut last_err = String::new();
    for cand in candidates {
        if let Some(lib) = guard.get(cand) {
            // Leak the key string too so the returned name is `'static`.
            let name: &'static str = Box::leak(cand.clone().into_boxed_str());
            return Ok((lib, name));
        }
        // SAFETY: dlopen of a user-named shared library is inherently unsafe (we
        // trust the declared signature). This is the documented NativeCall
        // contract. The handle is leaked to `'static` (never dlclosed).
        match unsafe { libloading::Library::new(cand) } {
            Ok(l) => {
                let leaked: &'static libloading::Library = Box::leak(Box::new(l));
                guard.insert(cand.clone(), leaked);
                let name: &'static str = Box::leak(cand.clone().into_boxed_str());
                return Ok((leaked, name));
            }
            Err(e) => last_err = format!("{cand}: {e}"),
        }
    }
    Err(RuntimeError::new(format!(
        "NativeCall: cannot load library {candidates:?}: {last_err}"
    )))
}

#[cfg(feature = "libffi")]
pub fn call_native(spec: &NativeCallSpec, args: &[Value]) -> Result<Value, RuntimeError> {
    use libffi::middle::{Arg, Cif, CodePtr, Type};

    if args.len() != spec.params.len() {
        return Err(RuntimeError::new(format!(
            "NativeCall: '{}' expects {} argument(s), got {}",
            spec.symbol,
            spec.params.len(),
            args.len()
        )));
    }

    let candidates = resolve_library_candidates(&spec.library);
    let (lib, lib_name) = load_library_cached(&candidates)?;
    let func_ptr: *const std::ffi::c_void = unsafe {
        let sym: libloading::Symbol<*const std::ffi::c_void> =
            lib.get(spec.symbol.as_bytes()).map_err(|e| {
                RuntimeError::new(format!(
                    "NativeCall: symbol '{}' not found in '{lib_name}': {e}",
                    spec.symbol
                ))
            })?;
        // The symbol's address IS the function entry point.
        *sym.into_raw()
    };

    // Owners that must outlive the libffi call (boxed scalars and CStrings the
    // Arg pointers reference).
    let mut owners: Vec<ArgOwner> = Vec::with_capacity(args.len());
    let mut ffi_args: Vec<Arg> = Vec::with_capacity(args.len());
    let mut arg_types: Vec<Type> = Vec::with_capacity(args.len());

    // Arg indices whose `is rw Pointer` out-slot must be written back into the
    // caller's Pointer object after the call.
    let mut writebacks: Vec<usize> = Vec::new();

    for (i, (ps, v)) in spec.params.iter().zip(args.iter()).enumerate() {
        let (ty, owner) = if ps.is_rw && ps.ct == CType::Pointer {
            // `Pointer is rw`: pass `void**` — a pointer to a slot holding the
            // current address; C writes the new pointer into the slot.
            let slot = Box::new(pointer_address(v));
            writebacks.push(i);
            (Type::pointer(), ArgOwner::new_out_ptr(slot))
        } else {
            marshal_arg(ps.ct, v).map_err(|msg| {
                RuntimeError::new(format!(
                    "NativeCall: argument {} to '{}': {msg}",
                    i + 1,
                    spec.symbol
                ))
            })?
        };
        arg_types.push(ty);
        owners.push(owner);
    }
    // Build the Arg pointers after all owners are in place (stable addresses).
    for owner in &owners {
        ffi_args.push(owner.as_arg());
    }

    let cif = Cif::new(arg_types, ret_ffi_type(spec.ret));
    let code = CodePtr(func_ptr as *mut _);

    // SAFETY: the CIF matches the declared signature; arg pointers outlive the
    // call via `owners`.
    let result = unsafe {
        match spec.ret {
            CType::Void => {
                cif.call::<()>(code, &ffi_args);
                Value::Nil
            }
            CType::I8 => Value::Int(cif.call::<i8>(code, &ffi_args) as i64),
            CType::I16 => Value::Int(cif.call::<i16>(code, &ffi_args) as i64),
            CType::I32 => Value::Int(cif.call::<i32>(code, &ffi_args) as i64),
            CType::I64 => Value::Int(cif.call::<i64>(code, &ffi_args)),
            CType::U8 => Value::Int(cif.call::<u8>(code, &ffi_args) as i64),
            CType::U16 => Value::Int(cif.call::<u16>(code, &ffi_args) as i64),
            CType::U32 => Value::Int(cif.call::<u32>(code, &ffi_args) as i64),
            CType::U64 => Value::Int(cif.call::<u64>(code, &ffi_args) as i64),
            CType::F32 => Value::Num(cif.call::<f32>(code, &ffi_args) as f64),
            CType::F64 => Value::Num(cif.call::<f64>(code, &ffi_args)),
            CType::Pointer => Value::Int(cif.call::<usize>(code, &ffi_args) as i64),
            CType::Str => {
                let ptr = cif.call::<*const std::ffi::c_char>(code, &ffi_args);
                if ptr.is_null() {
                    Value::Nil
                } else {
                    let cstr = std::ffi::CStr::from_ptr(ptr);
                    Value::str(cstr.to_string_lossy().into_owned())
                }
            }
        }
    };

    // Write `is rw Pointer` out-slots back into the caller's Pointer objects.
    // The `Value::Instance` shares its attribute cell with the caller's
    // variable, so the new address becomes visible there.
    for idx in writebacks {
        if let ArgOwner::OutPtr { slot, .. } = &owners[idx] {
            write_pointer_address(&args[idx], **slot);
        }
    }

    Ok(result)
}

/// Read the C address carried by a NativeCall argument: a `Pointer` object's
/// `address` attribute, a bare integer, or 0 for Nil / anything else. Unwraps a
/// `Scalar` / `ContainerRef` container first (a `$`-variable argument arrives
/// wrapped).
#[cfg(feature = "libffi")]
fn pointer_address(v: &Value) -> usize {
    match v {
        Value::Int(i) => *i as usize,
        Value::Instance { attributes, .. } => match attributes.as_map().get("address") {
            Some(Value::Int(i)) => *i as usize,
            _ => 0,
        },
        Value::Scalar(inner) => pointer_address(inner),
        Value::ContainerRef(cell) => cell.lock().ok().map(|g| pointer_address(&g)).unwrap_or(0),
        // An `is rw` argument arrives as a varref Capture carrying the bound
        // variable's current value.
        Value::Capture { named, .. } => match named.get("__mutsu_varref_value") {
            Some(inner) => pointer_address(inner),
            None => 0,
        },
        _ => 0,
    }
}

/// Write a resolved C address back into a `Pointer` object's `address`
/// attribute (in place, through the shared attribute cell). Unwraps a `Scalar`
/// / `ContainerRef` container so a `$`-variable out-argument updates the
/// caller's object.
#[cfg(feature = "libffi")]
fn write_pointer_address(v: &Value, addr: usize) {
    match v {
        Value::Instance { attributes, .. } => {
            attributes.insert("address".to_string(), Value::Int(addr as i64));
        }
        Value::Scalar(inner) => write_pointer_address(inner, addr),
        Value::ContainerRef(cell) => {
            if let Ok(g) = cell.lock() {
                write_pointer_address(&g, addr);
            }
        }
        Value::Capture { named, .. } => {
            if let Some(inner) = named.get("__mutsu_varref_value") {
                write_pointer_address(inner, addr);
            }
        }
        _ => {}
    }
}

/// Backing storage for one marshalled argument; keeps owned data alive for the
/// duration of the libffi call.
#[cfg(feature = "libffi")]
enum ArgOwner {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
    Ptr(*const std::ffi::c_void),
    /// A C string (kept alive only to back the pointer) plus the `char*` handed
    /// to libffi. The `CString` field is never read directly — it is an RAII
    /// keep-alive for the buffer the pointer references.
    CStr(
        #[allow(dead_code)] std::ffi::CString,
        *const std::ffi::c_char,
    ),
    /// An `is rw Pointer` out-parameter (`void**`): `slot` holds the address
    /// (C writes the new pointer here), `slot_ptr` is the `void**` value handed
    /// to libffi (the address of `slot`).
    OutPtr {
        slot: Box<usize>,
        slot_ptr: *mut std::ffi::c_void,
    },
}

#[cfg(feature = "libffi")]
impl ArgOwner {
    /// Build an out-pointer owner. `slot_ptr` points at the heap-allocated
    /// `slot`; the `Box` keeps that allocation stable across the move into the
    /// `owners` vector, so the pointer remains valid for the call.
    fn new_out_ptr(mut slot: Box<usize>) -> ArgOwner {
        let slot_ptr: *mut std::ffi::c_void = (slot.as_mut() as *mut usize).cast();
        ArgOwner::OutPtr { slot, slot_ptr }
    }

    fn as_arg(&self) -> libffi::middle::Arg {
        use libffi::middle::arg;
        match self {
            ArgOwner::I8(v) => arg(v),
            ArgOwner::I16(v) => arg(v),
            ArgOwner::I32(v) => arg(v),
            ArgOwner::I64(v) => arg(v),
            ArgOwner::U8(v) => arg(v),
            ArgOwner::U16(v) => arg(v),
            ArgOwner::U32(v) => arg(v),
            ArgOwner::U64(v) => arg(v),
            ArgOwner::F32(v) => arg(v),
            ArgOwner::F64(v) => arg(v),
            ArgOwner::Ptr(p) => arg(p),
            ArgOwner::CStr(_, p) => arg(p),
            ArgOwner::OutPtr { slot_ptr, .. } => arg(slot_ptr),
        }
    }
}

/// Unwrap a native-call argument to its underlying scalar value: a `$`-variable
/// argument arrives wrapped in a `Scalar` / `ContainerRef`, or — when bound to a
/// parameter — in an `is rw`-style varref `Capture`. Without this, `to_int` /
/// `to_string_value` on the wrapper yields garbage (a `Capture` coerces to 0),
/// so a `Pointer`/int held in a variable would be passed as 0.
#[cfg(feature = "libffi")]
fn resolve_arg(v: &Value) -> Value {
    match v {
        Value::Scalar(inner) => resolve_arg(inner),
        Value::ContainerRef(cell) => cell.lock().map(|g| resolve_arg(&g)).unwrap_or(Value::Nil),
        Value::Capture { named, .. } => match named.get("__mutsu_varref_value") {
            Some(inner) => resolve_arg(inner),
            None => v.clone(),
        },
        other => other.clone(),
    }
}

#[cfg(feature = "libffi")]
fn marshal_arg(ct: CType, raw: &Value) -> Result<(libffi::middle::Type, ArgOwner), String> {
    use libffi::middle::Type;
    let resolved = resolve_arg(raw);
    let v = &resolved;
    let int = || crate::runtime::to_int(v);
    let num = || crate::runtime::utils::to_float_value(v).unwrap_or(0.0);
    Ok(match ct {
        CType::I8 => (Type::i8(), ArgOwner::I8(int() as i8)),
        CType::I16 => (Type::i16(), ArgOwner::I16(int() as i16)),
        CType::I32 => (Type::i32(), ArgOwner::I32(int() as i32)),
        CType::I64 => (Type::i64(), ArgOwner::I64(int())),
        CType::U8 => (Type::u8(), ArgOwner::U8(int() as u8)),
        CType::U16 => (Type::u16(), ArgOwner::U16(int() as u16)),
        CType::U32 => (Type::u32(), ArgOwner::U32(int() as u32)),
        CType::U64 => (Type::u64(), ArgOwner::U64(int() as u64)),
        CType::F32 => (Type::f32(), ArgOwner::F32(num() as f32)),
        CType::F64 => (Type::f64(), ArgOwner::F64(num())),
        CType::Pointer => {
            // A by-value `Pointer` passes its current address as `void*`.
            let addr = pointer_address(v) as *const std::ffi::c_void;
            (Type::pointer(), ArgOwner::Ptr(addr))
        }
        CType::Str => {
            let s = v.to_string_value();
            let cstr = std::ffi::CString::new(s)
                .map_err(|_| "Str argument contains an embedded NUL byte".to_string())?;
            let ptr = cstr.as_ptr();
            (Type::pointer(), ArgOwner::CStr(cstr, ptr))
        }
        CType::Void => return Err("a parameter cannot have type void".to_string()),
    })
}

#[cfg(feature = "libffi")]
fn ret_ffi_type(ct: CType) -> libffi::middle::Type {
    use libffi::middle::Type;
    match ct {
        CType::Void => Type::void(),
        CType::I8 => Type::i8(),
        CType::I16 => Type::i16(),
        CType::I32 => Type::i32(),
        CType::I64 => Type::i64(),
        CType::U8 => Type::u8(),
        CType::U16 => Type::u16(),
        CType::U32 => Type::u32(),
        CType::U64 => Type::u64(),
        CType::F32 => Type::f32(),
        CType::F64 => Type::f64(),
        CType::Str | CType::Pointer => Type::pointer(),
    }
}

/// Stub used when the `libffi` feature is disabled (e.g. wasm builds).
#[cfg(not(feature = "libffi"))]
pub fn call_native(spec: &NativeCallSpec, _args: &[Value]) -> Result<Value, RuntimeError> {
    Err(RuntimeError::new(format!(
        "NativeCall is not available in this build (cannot call '{}')",
        spec.symbol
    )))
}
