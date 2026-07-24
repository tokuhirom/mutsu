//! Minimal NativeCall (C FFI) support.
//!
//! A sub declared with the `is native(...)` trait and a `{ * }` body is not run
//! as Raku code; instead, at call time we `dlopen` the named shared library,
//! `dlsym` the symbol, build a libffi CIF from the declared parameter / return
//! types, marshal the Raku argument `Value`s into C values, perform the call,
//! and marshal the C return value back into a `Value`.
//!
//! Supported so far: scalar C types (signed/unsigned 8/16/32/64-bit integers,
//! 32/64-bit floats), `Str` passed as a NUL-terminated `char*`, an opaque
//! `Pointer`, `CArray[T]` (a contiguous C buffer / `char**` for `CArray[Str]`)
//! and `Blob`/`Buf` (a byte buffer) as call arguments, with the C-side data
//! copied back into the Raku array/buffer after the call so an out-array /
//! out-buffer (`SSL_read` / `BIO_read`-style fill) is visible, and
//! `is repr('CStruct')` types as **opaque native handles passed by pointer**
//! (returned wrapped in an instance of the declared class, e.g. OpenSSL's
//! `SSL` / `SSL_CTX` / `SSL_METHOD`). The library name may be supplied by a code
//! object (`is native(&ssl-lib)`), resolved at bind time. By-value CStructs
//! (field layout marshalling) and callbacks remain follow-up work.

use crate::value::{RuntimeError, Value, ValueView};

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
    /// `CArray[T]` — a contiguous C buffer whose element C type is carried in
    /// the [`ParamSpec::elem`] field (a scalar numeric type, or `Str` for a
    /// `char**`). Passed as a pointer to the first element.
    CArray,
    /// `Blob` / `Buf` / `buf8` — a byte buffer passed as a pointer to its bytes
    /// (`char*` / `void*`). Unlike `Str`, it is not NUL-terminated and may carry
    /// embedded NULs, and the callee may write into it (an out-buffer, e.g.
    /// `SSL_read` / `BIO_read`), so the C bytes are copied back into the caller's
    /// buffer after the call.
    Buf,
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
            // A byte buffer passed as a `void*` to its raw bytes. `blob8`/`buf8`
            // are the `uint8` parametric spellings; the bracketed forms
            // (`Buf[uint8]`) are handled by the caller stripping to the stem.
            "Blob" | "Buf" | "buf8" | "blob8" => CType::Buf,
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
    /// Element C type when `ct == CType::CArray` (`None` otherwise).
    pub elem: Option<CType>,
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
    /// When the return type is a user-declared `is repr('CStruct')` class (an
    /// opaque native handle passed by pointer, e.g. OpenSSL's `SSL` / `SSL_CTX`
    /// / `SSL_METHOD`), `ret` is [`CType::Pointer`] and this holds the class
    /// name so the returned address is wrapped in an instance of that class
    /// (rather than a bare `Pointer`). `None` for a plain `Pointer` return.
    pub ret_struct: Option<String>,
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
    // Match Rakudo's message shape so code that inspects the failure text works
    // (e.g. zef's `!native-library-is-installed` does
    // `.payload.starts-with("Cannot locate native library")`). `candidates[0]`
    // is the primary (unversioned) library file name.
    let primary = candidates.first().map(String::as_str).unwrap_or("");
    Err(RuntimeError::new(format!(
        "Cannot locate native library '{primary}': {last_err}"
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
    // Arg indices holding a numeric `CArray` whose C buffer must be copied back
    // into the caller's Raku array after the call (an out-array fill).
    let mut carray_writebacks: Vec<usize> = Vec::new();
    // Arg indices holding a `Blob`/`Buf` whose C buffer must be copied back into
    // the caller's Buf after the call (an out-buffer, e.g. `SSL_read`).
    let mut buf_writebacks: Vec<usize> = Vec::new();

    for (i, (ps, v)) in spec.params.iter().zip(args.iter()).enumerate() {
        let (ty, owner) = if ps.is_rw && ps.ct == CType::Pointer {
            // `Pointer is rw`: pass `void**` — a pointer to a slot holding the
            // current address; C writes the new pointer into the slot.
            let slot = Box::new(pointer_address(v));
            writebacks.push(i);
            (Type::pointer(), ArgOwner::new_out_ptr(slot))
        } else {
            let (ty, owner) = marshal_arg(ps, v).map_err(|msg| {
                RuntimeError::new(format!(
                    "NativeCall: argument {} to '{}': {msg}",
                    i + 1,
                    spec.symbol
                ))
            })?;
            // A numeric CArray is backed by C memory in Raku, so any write the
            // callee performs must be reflected back into the caller's array.
            if matches!(owner, ArgOwner::CArrayNum { .. }) {
                carray_writebacks.push(i);
            }
            // A `Buf`/`Blob` out-buffer: reflect callee writes back into the Buf.
            if matches!(owner, ArgOwner::BufBytes { .. }) {
                buf_writebacks.push(i);
            }
            (ty, owner)
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
                Value::NIL
            }
            CType::I8 => Value::int(cif.call::<i8>(code, &ffi_args) as i64),
            CType::I16 => Value::int(cif.call::<i16>(code, &ffi_args) as i64),
            CType::I32 => Value::int(cif.call::<i32>(code, &ffi_args) as i64),
            CType::I64 => Value::int(cif.call::<i64>(code, &ffi_args)),
            CType::U8 => Value::int(cif.call::<u8>(code, &ffi_args) as i64),
            CType::U16 => Value::int(cif.call::<u16>(code, &ffi_args) as i64),
            CType::U32 => Value::int(cif.call::<u32>(code, &ffi_args) as i64),
            CType::U64 => Value::int(cif.call::<u64>(code, &ffi_args) as i64),
            CType::F32 => Value::num(cif.call::<f32>(code, &ffi_args) as f64),
            CType::F64 => Value::num(cif.call::<f64>(code, &ffi_args)),
            // A CArray return is mapped to `CType::Pointer` at registration
            // (a returned `CArray[T]` has no length to reify), so this arm is
            // unreachable in practice — treat it as an opaque pointer. A CStruct
            // return (`ret_struct` set) wraps the address in an instance of the
            // declared class so it round-trips as that native handle type.
            CType::Pointer | CType::CArray | CType::Buf => {
                let addr = cif.call::<usize>(code, &ffi_args);
                match &spec.ret_struct {
                    Some(class) => make_struct_value(class, addr),
                    None => make_pointer_value(addr),
                }
            }
            CType::Str => {
                let ptr = cif.call::<*const std::ffi::c_char>(code, &ffi_args);
                if ptr.is_null() {
                    Value::NIL
                } else {
                    let cstr = std::ffi::CStr::from_ptr(ptr);
                    Value::str(cstr.to_string_lossy().into_owned())
                }
            }
        }
    };

    // Write `is rw Pointer` out-slots back into the caller's Pointer objects.
    // The `Instance` shares its attribute cell with the caller's
    // variable, so the new address becomes visible there.
    for idx in writebacks {
        if let ArgOwner::OutPtr { slot, .. } = &owners[idx] {
            write_pointer_address(&args[idx], **slot);
        }
    }

    // Copy each numeric CArray's (possibly callee-modified) C buffer back into
    // the caller's Raku array, element by element, through the shared backing
    // node so the mutation is visible at the call site.
    for idx in carray_writebacks {
        if let ArgOwner::CArrayNum { buf, elem, .. } = &owners[idx]
            && let Some(arr) = resolve_array_value(&args[idx])
        {
            let sz = carray_elem_size(*elem);
            arr.with_array_inplace(|data, _kind| {
                for (i, cell) in data.items.iter_mut().enumerate() {
                    let off = i * sz;
                    if off + sz <= buf.len() {
                        *cell = decode_carray_elem(*elem, &buf[off..off + sz]);
                    }
                }
            });
        }
    }

    // Copy each `Buf`/`Blob` out-buffer's (possibly callee-modified) C bytes
    // back into the caller's Buf instance.
    for idx in buf_writebacks {
        if let ArgOwner::BufBytes { buf, .. } = &owners[idx] {
            write_buf_instance_bytes(&args[idx], buf);
        }
    }

    Ok(result)
}

/// Build a `Pointer` object holding the given C address. Used to marshal a
/// `void*` return value. The `Pointer` prelude class is registered whenever a
/// program references `Pointer` (which a `returns Pointer` signature does), so
/// method dispatch (`.Int`/`.gist`/…) on the result resolves.
#[cfg(feature = "libffi")]
fn make_pointer_value(addr: usize) -> Value {
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("address".to_string(), Value::int(addr as i64));
    Value::make_instance(crate::symbol::Symbol::intern("Pointer"), attrs)
}

/// Wrap a returned native pointer as an instance of a user-declared
/// `is repr('CStruct')` class (an opaque native handle, e.g. `SSL_CTX`). The
/// address is carried in an `address` attribute so it round-trips as a `void*`
/// when passed back to another native call (`pointer_address` reads it). A NULL
/// return becomes the class's type object (undefined) so `.defined` / boolean
/// checks — the OpenSSL binding's `try {...} || try {...}` fallback pattern —
/// behave like Rakudo, where a null CStruct return is a type object.
#[cfg(feature = "libffi")]
fn make_struct_value(class: &str, addr: usize) -> Value {
    if addr == 0 {
        return Value::package(crate::symbol::Symbol::intern(class));
    }
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("address".to_string(), Value::int(addr as i64));
    Value::make_instance(crate::symbol::Symbol::intern(class), attrs)
}

/// Read the C address carried by a NativeCall argument: a `Pointer` object's
/// `address` attribute, a bare integer, or 0 for Nil / anything else. Unwraps a
/// `Scalar` / `ContainerRef` container first (a `$`-variable argument arrives
/// wrapped).
#[cfg(feature = "libffi")]
fn pointer_address(v: &Value) -> usize {
    match v.view() {
        ValueView::Int(i) => i as usize,
        ValueView::Instance { attributes, .. } => {
            match attributes.as_map().get("address").map(Value::view) {
                Some(ValueView::Int(i)) => i as usize,
                _ => 0,
            }
        }
        ValueView::Scalar(inner) => pointer_address(inner),
        ValueView::ContainerRef(cell) => cell.lock().ok().map(|g| pointer_address(&g)).unwrap_or(0),
        // An `is rw` argument arrives as a `VarRef` carrying the bound
        // variable's current value.
        ValueView::VarRef { value, .. } => pointer_address(value),
        _ => 0,
    }
}

/// Write a resolved C address back into a `Pointer` object's `address`
/// attribute (in place, through the shared attribute cell). Unwraps a `Scalar`
/// / `ContainerRef` container so a `$`-variable out-argument updates the
/// caller's object.
#[cfg(feature = "libffi")]
fn write_pointer_address(v: &Value, addr: usize) {
    match v.view() {
        ValueView::Instance { attributes, .. } => {
            attributes.insert("address".to_string(), Value::int(addr as i64));
        }
        ValueView::Scalar(inner) => write_pointer_address(inner, addr),
        ValueView::ContainerRef(cell) => {
            if let Ok(g) = cell.lock() {
                write_pointer_address(&g, addr);
            }
        }
        ValueView::VarRef { value, .. } => write_pointer_address(value, addr),
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
    /// A numeric `CArray[T]`: `buf` is the contiguous element buffer (heap, so
    /// its address is stable across the move into `owners`); `data_ptr` is the
    /// `T*` handed to libffi. `elem` is the element C type (for writeback).
    CArrayNum {
        buf: Vec<u8>,
        data_ptr: *const std::ffi::c_void,
        elem: CType,
    },
    /// A `Blob` / `Buf` byte buffer (`void*`): `buf` is the contiguous bytes
    /// (heap, stable address), `data_ptr` is the pointer handed to libffi. The
    /// callee may write into `buf` (an out-buffer), so it is copied back into
    /// the caller's `Buf` instance after the call.
    BufBytes {
        buf: Vec<u8>,
        data_ptr: *const std::ffi::c_void,
    },
    /// A `CArray[Str]` (`char**`): `strings` keeps the NUL-terminated buffers
    /// alive, `ptrs` is the NULL-terminated `char*` array, `data_ptr` is the
    /// `char**` handed to libffi (the address of the first `char*`).
    CArrayStr {
        #[allow(dead_code)]
        strings: Vec<std::ffi::CString>,
        #[allow(dead_code)]
        ptrs: Vec<*const std::ffi::c_char>,
        data_ptr: *const std::ffi::c_void,
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

    fn as_arg(&self) -> libffi::middle::Arg<'_> {
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
            ArgOwner::CArrayNum { data_ptr, .. } => arg(data_ptr),
            ArgOwner::BufBytes { data_ptr, .. } => arg(data_ptr),
            ArgOwner::CArrayStr { data_ptr, .. } => arg(data_ptr),
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
    match v.view() {
        ValueView::Scalar(inner) => resolve_arg(inner),
        ValueView::ContainerRef(cell) => cell.lock().map(|g| resolve_arg(&g)).unwrap_or(Value::NIL),
        ValueView::VarRef { value, .. } => resolve_arg(value),
        _ => v.clone(),
    }
}

/// Unwrap a native-call argument to the underlying `Array` value (sharing the
/// same GC backing node), unwrapping a `Scalar` / `ContainerRef` / `VarRef`
/// container first. Used to write a numeric CArray's buffer back into the
/// caller's array in place.
#[cfg(feature = "libffi")]
fn resolve_array_value(v: &Value) -> Option<Value> {
    match v.view() {
        ValueView::Array(..) => Some(v.clone()),
        ValueView::Scalar(inner) => resolve_array_value(inner),
        ValueView::ContainerRef(cell) => cell.lock().ok().and_then(|g| resolve_array_value(&g)),
        ValueView::VarRef { value, .. } => resolve_array_value(value),
        _ => None,
    }
}

/// The size in bytes of one `CArray[T]` element for a scalar C element type.
#[cfg(feature = "libffi")]
fn carray_elem_size(elem: CType) -> usize {
    match elem {
        CType::I8 | CType::U8 => 1,
        CType::I16 | CType::U16 => 2,
        CType::I32 | CType::U32 | CType::F32 => 4,
        CType::I64 | CType::U64 | CType::F64 => 8,
        // A pointer-sized element (`Pointer`); Str is handled separately.
        CType::Pointer | CType::Str | CType::CArray | CType::Buf | CType::Void => {
            std::mem::size_of::<usize>()
        }
    }
}

/// Encode one Raku value into `dst` (exactly `carray_elem_size(elem)` bytes) as
/// a native-endian C scalar of the given element type.
#[cfg(feature = "libffi")]
fn encode_carray_elem(elem: CType, v: &Value, dst: &mut [u8]) {
    let int = crate::runtime::to_int(v);
    let num = crate::runtime::utils::to_float_value(v).unwrap_or(0.0);
    match elem {
        CType::I8 | CType::U8 => dst.copy_from_slice(&(int as u8).to_ne_bytes()),
        CType::I16 | CType::U16 => dst.copy_from_slice(&(int as u16).to_ne_bytes()),
        CType::I32 | CType::U32 => dst.copy_from_slice(&(int as u32).to_ne_bytes()),
        CType::I64 | CType::U64 => dst.copy_from_slice(&(int as u64).to_ne_bytes()),
        CType::F32 => dst.copy_from_slice(&(num as f32).to_ne_bytes()),
        CType::F64 => dst.copy_from_slice(&num.to_ne_bytes()),
        CType::Pointer | CType::Str | CType::CArray | CType::Buf | CType::Void => {
            dst.copy_from_slice(&(int as usize).to_ne_bytes())
        }
    }
}

/// Decode one C scalar element (native-endian bytes in `src`) back into a Raku
/// value. Integer elements become `Int`, float elements become `Num`.
#[cfg(feature = "libffi")]
fn decode_carray_elem(elem: CType, src: &[u8]) -> Value {
    fn arr<const N: usize>(src: &[u8]) -> [u8; N] {
        let mut a = [0u8; N];
        a.copy_from_slice(&src[..N]);
        a
    }
    match elem {
        CType::I8 => Value::int(i8::from_ne_bytes(arr(src)) as i64),
        CType::U8 => Value::int(u8::from_ne_bytes(arr(src)) as i64),
        CType::I16 => Value::int(i16::from_ne_bytes(arr(src)) as i64),
        CType::U16 => Value::int(u16::from_ne_bytes(arr(src)) as i64),
        CType::I32 => Value::int(i32::from_ne_bytes(arr(src)) as i64),
        CType::U32 => Value::int(u32::from_ne_bytes(arr(src)) as i64),
        CType::I64 => Value::int(i64::from_ne_bytes(arr(src))),
        CType::U64 => Value::int(u64::from_ne_bytes(arr::<8>(src)) as i64),
        CType::F32 => Value::num(f32::from_ne_bytes(arr(src)) as f64),
        CType::F64 => Value::num(f64::from_ne_bytes(arr(src))),
        CType::Pointer | CType::Str | CType::CArray | CType::Buf | CType::Void => {
            Value::int(usize::from_ne_bytes(arr(src)) as i64)
        }
    }
}

#[cfg(feature = "libffi")]
fn marshal_arg(ps: &ParamSpec, raw: &Value) -> Result<(libffi::middle::Type, ArgOwner), String> {
    use libffi::middle::Type;
    if ps.ct == CType::CArray {
        return marshal_carray_arg(ps, raw);
    }
    let ct = ps.ct;
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
            // An UNDEFINED `Str` argument (`Nil`, `Str`, any type object) is a
            // NULL `char*`, as in Rakudo. Stringifying it instead handed C a
            // pointer to a 1-byte buffer, and a callee that treats the argument
            // as a caller-provided output buffer then wrote past it and
            // corrupted the heap: OpenSSL's `ERR_error_string($e, Nil)` — where
            // NULL means "use your own static buffer" — writes up to 256 bytes
            // and aborted mutsu with "realloc(): invalid next size".
            if !crate::runtime::types::value_is_defined(v) {
                (Type::pointer(), ArgOwner::Ptr(std::ptr::null()))
            } else {
                let s = v.to_string_value();
                let cstr = std::ffi::CString::new(s)
                    .map_err(|_| "Str argument contains an embedded NUL byte".to_string())?;
                let ptr = cstr.as_ptr();
                (Type::pointer(), ArgOwner::CStr(cstr, ptr))
            }
        }
        CType::Buf => {
            // A `Blob`/`Buf` is passed as a `void*` to a contiguous copy of its
            // bytes (kept alive by the owner for the call). A NULL/absent buffer
            // becomes a null pointer.
            let buf = buf_instance_bytes(v).unwrap_or_default();
            let data_ptr = buf.as_ptr() as *const std::ffi::c_void;
            (Type::pointer(), ArgOwner::BufBytes { buf, data_ptr })
        }
        CType::Void => return Err("a parameter cannot have type void".to_string()),
        // Routed to `marshal_carray_arg` above; unreachable here.
        CType::CArray => return marshal_carray_arg(ps, raw),
    })
}

/// Read the bytes of a `Blob`/`Buf` native-call argument. A `Buf`/`Blob` is an
/// `Instance` whose `bytes` attribute is an `Array` of byte `Int`s; unwrap any
/// `Scalar`/`ContainerRef`/`VarRef` container first (a `$`-variable argument
/// arrives wrapped).
#[cfg(feature = "libffi")]
fn buf_instance_bytes(v: &Value) -> Option<Vec<u8>> {
    match v.view() {
        ValueView::Instance { attributes, .. } => {
            match attributes.as_map().get("bytes").map(|b| b.view()) {
                Some(ValueView::Array(items, ..)) => Some(
                    items
                        .iter()
                        .map(|b| crate::runtime::to_int(b) as u8)
                        .collect(),
                ),
                _ => None,
            }
        }
        ValueView::Scalar(inner) => buf_instance_bytes(inner),
        ValueView::ContainerRef(cell) => cell.lock().ok().and_then(|g| buf_instance_bytes(&g)),
        ValueView::VarRef { value, .. } => buf_instance_bytes(value),
        _ => None,
    }
}

/// Copy C-side bytes back into a `Blob`/`Buf` argument's `bytes` attribute after
/// a native call (an out-buffer such as `SSL_read` / `BIO_read`). The `Instance`
/// shares its attribute cell with the caller's variable, so the update is
/// visible at the call site. The buffer length is preserved (the callee reports
/// how many bytes it wrote via the return value; the caller slices accordingly).
#[cfg(feature = "libffi")]
fn write_buf_instance_bytes(v: &Value, bytes: &[u8]) {
    match v.view() {
        ValueView::Instance { attributes, .. } => {
            let byte_vals: Vec<Value> = bytes.iter().map(|b| Value::int(*b as i64)).collect();
            attributes.insert("bytes".to_string(), Value::array(byte_vals));
        }
        ValueView::Scalar(inner) => write_buf_instance_bytes(inner, bytes),
        ValueView::ContainerRef(cell) => {
            if let Ok(g) = cell.lock() {
                write_buf_instance_bytes(&g, bytes);
            }
        }
        ValueView::VarRef { value, .. } => write_buf_instance_bytes(value, bytes),
        _ => {}
    }
}

/// Marshal a `CArray[T]` argument: a Raku array of elements becomes a
/// contiguous C buffer (`T*`), or a `char**` (NULL-terminated) for
/// `CArray[Str]`. A null/Any argument becomes a null pointer.
#[cfg(feature = "libffi")]
fn marshal_carray_arg(
    ps: &ParamSpec,
    raw: &Value,
) -> Result<(libffi::middle::Type, ArgOwner), String> {
    use libffi::middle::Type;
    let elem = ps
        .elem
        .ok_or_else(|| "CArray parameter is missing its element type".to_string())?;
    let list = match resolve_array_value(raw) {
        Some(arr) => arr
            .with_array_inplace(|data, _| data.items.clone())
            .unwrap_or_default(),
        // A bare type object / Any becomes a null pointer.
        None => Vec::new(),
    };

    if elem == CType::Str {
        let mut strings = Vec::with_capacity(list.len());
        let mut ptrs: Vec<*const std::ffi::c_char> = Vec::with_capacity(list.len() + 1);
        for item in &list {
            let s = item.to_string_value();
            let cstr = std::ffi::CString::new(s)
                .map_err(|_| "CArray[Str] element contains an embedded NUL byte".to_string())?;
            ptrs.push(cstr.as_ptr());
            strings.push(cstr);
        }
        // C `char**` arrays are conventionally NULL-terminated.
        ptrs.push(std::ptr::null());
        let data_ptr = ptrs.as_ptr() as *const std::ffi::c_void;
        return Ok((
            Type::pointer(),
            ArgOwner::CArrayStr {
                strings,
                ptrs,
                data_ptr,
            },
        ));
    }

    let sz = carray_elem_size(elem);
    let mut buf = vec![0u8; list.len() * sz];
    for (i, item) in list.iter().enumerate() {
        encode_carray_elem(elem, item, &mut buf[i * sz..(i + 1) * sz]);
    }
    let data_ptr = buf.as_ptr() as *const std::ffi::c_void;
    Ok((
        Type::pointer(),
        ArgOwner::CArrayNum {
            buf,
            data_ptr,
            elem,
        },
    ))
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
        CType::Str | CType::Pointer | CType::CArray | CType::Buf => Type::pointer(),
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
