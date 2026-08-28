//! Outbound NativeCall callbacks — marshalling a Raku `Callable` to a real C
//! function pointer that re-enters the VM (ADR-0063).
//!
//! `nativecall_fnptr.rs` covers the *inbound* direction (`nativecast(:(...),
//! $ptr)` makes a C function pointer callable from Raku). This is the mirror:
//! a `&callback (Sig)` parameter of an `is native` sub accepts a Raku sub and
//! hands C an address it can call.
//!
//! Two policies are load-bearing and are the subject of ADR-0063:
//!
//! * **Ownership** — a libffi closure, its CIF and its userdata (which owns a
//!   strong reference to the Raku `Callable`) are allocated once and **never
//!   freed**. C routinely retains a callback past the call that installed it
//!   (libarchive keeps all three of `archive_write_open`'s callbacks for the
//!   lifetime of the archive handle), so a closure dropped when the installing
//!   call returns would leave C calling freed memory. Closures are interned by
//!   (callable identity, C signature) so a call in a loop reuses one, which is
//!   what bounds the leak. This mirrors the existing process-lifetime leaks in
//!   this subsystem (`load_library_cached`, `native_object_where`).
//!
//! * **Re-entrancy** — the trampoline reaches an `Interpreter` through a
//!   thread-local stack that `call_native_with_out_args` pushes for the
//!   duration of each native call. A callback that C invokes *during* a native
//!   call therefore runs on the very interpreter that made the call, on the
//!   same thread, with all state shared (this covers the synchronous case,
//!   including a callback fired from a *nested* native call such as
//!   libarchive's `archive_write_data`). A callback invoked with no native call
//!   in progress on this thread — C stashed the pointer and called it later, or
//!   from a thread of its own — has no interpreter to reach and is reported on
//!   stderr, returning a zero value rather than crashing.

use crate::runtime::Interpreter;
use crate::runtime::nativecall::{CType, CallbackSig};
use crate::value::{RuntimeError, Value, ValueView};

use std::cell::RefCell;

thread_local! {
    /// Interpreters that are currently inside a native call on this thread.
    /// Pushed by [`InterpreterGuard`] for exactly the duration of the libffi
    /// call, so a callback C fires during it re-enters the calling VM.
    static ACTIVE_INTERPRETERS: RefCell<Vec<*mut Interpreter>> =
        const { RefCell::new(Vec::new()) };
}

/// RAII registration of the interpreter that is making a native call, so a C
/// callback fired during it can find a VM to run on.
pub(crate) struct InterpreterGuard;

impl InterpreterGuard {
    /// # Safety
    /// `interp` must stay valid, and must not be otherwise accessed, for the
    /// lifetime of the guard. Callers derive the pointer from the live `&mut
    /// Interpreter` that is making the native call and do not touch it again
    /// until the call has returned, which is what makes the re-borrow inside a
    /// callback sound.
    pub(crate) unsafe fn push(interp: *mut Interpreter) -> InterpreterGuard {
        ACTIVE_INTERPRETERS.with(|s| s.borrow_mut().push(interp));
        InterpreterGuard
    }
}

impl Drop for InterpreterGuard {
    fn drop(&mut self) {
        ACTIVE_INTERPRETERS.with(|s| {
            s.borrow_mut().pop();
        });
    }
}

/// The interpreter a callback should run on: the innermost one currently inside
/// a native call on this thread.
fn active_interpreter() -> Option<*mut Interpreter> {
    ACTIVE_INTERPRETERS.with(|s| s.borrow().last().copied())
}

/// Everything the trampoline needs, leaked to `'static` (see the ownership
/// policy above). `callable` keeps the Raku sub alive for the process, which is
/// also what makes the pointer-identity intern key sound: the address can never
/// be reused while the entry exists.
struct CallbackData {
    callable: Value,
    sig: CallbackSig,
}

/// A stable identity for a Raku callable, used to intern one C closure per
/// (callable, signature) pair. `None` when the value is not callable at all.
fn callable_identity(v: &Value) -> Option<String> {
    match v.view() {
        ValueView::Sub(data) => Some(format!("S{:x}", (&**data) as *const _ as usize)),
        ValueView::WeakSub(w) => w
            .upgrade()
            .map(|s| format!("S{:x}", (&*s) as *const _ as usize)),
        ValueView::Routine { package, name, .. } => {
            Some(format!("R{}::{}", package.resolve(), name.resolve()))
        }
        ValueView::Mixin(inner, _) => callable_identity(inner),
        ValueView::Scalar(inner) => callable_identity(inner),
        ValueView::ContainerRef(cell) => cell.lock().ok().and_then(|g| callable_identity(&g)),
        ValueView::VarRef { value, .. } => callable_identity(value),
        _ => None,
    }
}

/// Strip the argument containers a `$`-variable / `is rw` argument arrives in,
/// leaving the callable itself.
fn unwrap_callable(v: &Value) -> Value {
    match v.view() {
        ValueView::Scalar(inner) => unwrap_callable(inner),
        ValueView::ContainerRef(cell) => cell
            .lock()
            .map(|g| unwrap_callable(&g))
            .unwrap_or(Value::NIL),
        ValueView::VarRef { value, .. } => unwrap_callable(value),
        _ => v.clone(),
    }
}

/// The C function-pointer address for `arg` marshalled against `sig`.
///
/// An **undefined** argument (a type object / `Nil` / `Any`) is a genuine NULL
/// callback, which is how a C API's "no handler" is spelled — libarchive passes
/// a null `open` callback when it has nothing to do at open time.
pub(crate) fn callback_code_address(sig: &CallbackSig, arg: &Value) -> Result<usize, RuntimeError> {
    use std::collections::HashMap;
    use std::sync::{Mutex, OnceLock};

    let callable = unwrap_callable(arg);
    if !crate::runtime::types::value_is_defined(&callable) {
        return Ok(0);
    }
    let Some(id) = callable_identity(&callable) else {
        return Err(RuntimeError::new(
            "NativeCall: a callback parameter needs a Callable argument",
        ));
    };
    let key = format!("{id}|{sig:?}");

    static CLOSURES: OnceLock<Mutex<HashMap<String, usize>>> = OnceLock::new();
    let cache = CLOSURES.get_or_init(|| Mutex::new(HashMap::new()));
    let mut guard = cache.lock().unwrap_or_else(|e| e.into_inner());
    if let Some(addr) = guard.get(&key) {
        return Ok(*addr);
    }
    let addr = build_closure(callable, sig)?;
    guard.insert(key, addr);
    Ok(addr)
}

/// Allocate (and deliberately leak) one libffi closure for `callable`.
fn build_closure(callable: Value, sig: &CallbackSig) -> Result<usize, RuntimeError> {
    use libffi::middle::{Cif, Closure};

    let arg_types: Vec<libffi::middle::Type> = sig.params.iter().map(|c| ffi_type(*c)).collect();
    let cif = Cif::new(arg_types, ffi_type(sig.ret));
    // Leaked on purpose (ADR-0063 ownership policy): C may retain the pointer
    // for the rest of the process, and the userdata owns the Raku callable.
    let data: &'static CallbackData = Box::leak(Box::new(CallbackData {
        callable,
        sig: sig.clone(),
    }));
    let closure: &'static Closure<'static> = Box::leak(Box::new(match sig.ret {
        CType::F32 => Closure::new(cif, trampoline_f32, data),
        CType::F64 => Closure::new(cif, trampoline_f64, data),
        _ => Closure::new(cif, trampoline_word, data),
    }));
    Ok(*closure.code_ptr() as usize)
}

fn ffi_type(ct: CType) -> libffi::middle::Type {
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
        CType::Str | CType::Pointer | CType::CArray | CType::Buf | CType::Callback => {
            Type::pointer()
        }
    }
}

/// Decode one C argument (libffi hands over a pointer to the value) into the
/// Raku value the callback's parameter should receive.
///
/// # Safety
/// `slot` must point at a live value of the C type `ct` describes.
unsafe fn decode_c_arg(ct: CType, slot: *const std::ffi::c_void) -> Value {
    unsafe {
        match ct {
            CType::Void => Value::NIL,
            CType::I8 => Value::int(*(slot as *const i8) as i64),
            CType::I16 => Value::int(*(slot as *const i16) as i64),
            CType::I32 => Value::int(*(slot as *const i32) as i64),
            CType::I64 => Value::int(*(slot as *const i64)),
            CType::U8 => Value::int(*(slot as *const u8) as i64),
            CType::U16 => Value::int(*(slot as *const u16) as i64),
            CType::U32 => Value::int(*(slot as *const u32) as i64),
            CType::U64 => Value::int(*(slot as *const u64) as i64),
            CType::F32 => Value::num(*(slot as *const f32) as f64),
            CType::F64 => Value::num(*(slot as *const f64)),
            CType::Str => {
                let p = *(slot as *const *const std::ffi::c_char);
                if p.is_null() {
                    Value::NIL
                } else {
                    Value::str(std::ffi::CStr::from_ptr(p).to_string_lossy().into_owned())
                }
            }
            // Every other C type reaches Raku as an opaque `Pointer`, which is
            // what a callback signature spells them as (`Pointer`, `CArray`,
            // `Buf` and a nested function pointer are all one machine word).
            CType::Pointer | CType::CArray | CType::Buf | CType::Callback => {
                crate::runtime::nativecall::make_pointer_object(*(slot as *const usize))
            }
        }
    }
}

/// Run the Raku callable for one C invocation and return its result value.
/// `None` means the callback could not run at all (no interpreter, or the body
/// died) — the trampoline then returns a zero-valued result, because unwinding
/// a Raku exception through a C frame is not allowed
/// (`Language/nativecall.rakudoc`: "It is not allowed to throw an exception out
/// of a native callback").
fn invoke(data: &CallbackData, args: *const *const std::ffi::c_void) -> Option<Value> {
    let Some(interp) = active_interpreter() else {
        eprintln!(
            "NativeCall: a callback fired with no native call in progress on this thread; \
             mutsu cannot re-enter the VM from a C-owned thread, so the callback was skipped"
        );
        return None;
    };
    let raku_args: Vec<Value> = data
        .sig
        .params
        .iter()
        .enumerate()
        // SAFETY: libffi guarantees `args` holds one pointer per CIF argument,
        // each pointing at a live value of the declared type.
        .map(|(i, ct)| unsafe { decode_c_arg(*ct, *args.add(i)) })
        .collect();
    let callable = data.callable.clone();
    // A panic must not unwind across the C frame libffi called us from.
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(move || {
        // SAFETY: the pointer was pushed by `InterpreterGuard` from the live
        // `&mut Interpreter` making the native call, which does not touch the
        // interpreter again until that call returns — so this re-borrow is the
        // only live access for the duration of the callback.
        let interp = unsafe { &mut *interp };
        interp.call_sub_value(callable, raku_args, false)
    }));
    match result {
        Ok(Ok(v)) => Some(v),
        Ok(Err(e)) => {
            eprintln!("NativeCall: callback died: {}", e.message);
            None
        }
        Err(_) => {
            eprintln!("NativeCall: callback panicked");
            None
        }
    }
}

/// Encode the callback's Raku result into the word-sized slot libffi expects
/// for an integer / pointer / void return.
fn encode_word_result(ret: CType, v: &Value) -> u64 {
    match ret {
        CType::Void => 0,
        CType::F32 => {
            (crate::runtime::utils::to_float_value(v).unwrap_or(0.0) as f32).to_bits() as u64
        }
        CType::F64 => crate::runtime::utils::to_float_value(v)
            .unwrap_or(0.0)
            .to_bits(),
        CType::Str => {
            // The C side owns nothing here and NativeCall does not free strings
            // handed to it, so the buffer is leaked for the process — the same
            // contract Rakudo documents for callback string returns.
            match std::ffi::CString::new(v.to_string_value()) {
                Ok(s) => s.into_raw() as usize as u64,
                Err(_) => 0,
            }
        }
        CType::Pointer | CType::CArray | CType::Buf | CType::Callback => {
            crate::runtime::nativecall::value_c_address(v) as u64
        }
        _ => {
            let unboxed = crate::runtime::native_types::unbox_bool_to_native_int(v.clone());
            crate::runtime::to_int(&unboxed) as u64
        }
    }
}

/// Trampoline for every integer / pointer / void return. libffi widens a
/// sub-word integer result to `ffi_arg` (one machine word) for closures, which
/// is exactly what this writes.
unsafe extern "C" fn trampoline_word(
    _cif: &libffi::low::ffi_cif,
    result: &mut u64,
    args: *const *const std::ffi::c_void,
    data: &CallbackData,
) {
    *result = match invoke(data, args) {
        Some(v) => encode_word_result(data.sig.ret, &v),
        None => 0,
    };
}

unsafe extern "C" fn trampoline_f32(
    _cif: &libffi::low::ffi_cif,
    result: &mut f32,
    args: *const *const std::ffi::c_void,
    data: &CallbackData,
) {
    *result = match invoke(data, args) {
        Some(v) => crate::runtime::utils::to_float_value(&v).unwrap_or(0.0) as f32,
        None => 0.0,
    };
}

unsafe extern "C" fn trampoline_f64(
    _cif: &libffi::low::ffi_cif,
    result: &mut f64,
    args: *const *const std::ffi::c_void,
    data: &CallbackData,
) {
    *result = match invoke(data, args) {
        Some(v) => crate::runtime::utils::to_float_value(&v).unwrap_or(0.0),
        None => 0.0,
    };
}
