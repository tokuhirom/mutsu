//! `explicitly-manage` — handing a string's C buffer over to the callee.
//!
//! ```raku
//! say set_version(explicitly-manage('1.0.0'));
//! ```
//!
//! A plain `Str` argument is marshalled into a temporary `char*` that lives only
//! for the duration of the call (`ArgOwner::CStr` in
//! [`runtime::nativecall`](crate::runtime::nativecall)). That is right for a
//! callee that copies the string, and wrong for one that *keeps* the pointer —
//! `Language/nativecall.rakudoc`'s `set_version` example segfaults on the second
//! call for exactly this reason. `explicitly-manage` is Rakudo's answer: it
//! returns an object whose buffer "will not be freed by the runtime's garbage
//! collector", so the C library owns it from then on.
//!
//! mutsu models that object as `NativeCall::CStr` (Rakudo's own name for it,
//! with `repr('CStr')`), declared in the NativeCall prelude and carrying the
//! address of a **deliberately leaked** NUL-terminated buffer. The user-visible
//! sub is Raku, in that same prelude — `explicitly-manage` is a NativeCall
//! export, not a builtin — and only the leak is native. Passing the result
//! where a `Str` parameter is declared hands C that stable address (see
//! `explicitly_managed_address`).
//!
//! The prelude encodes with `Str.encode($encoding)` before calling in, so the
//! documented `:$encoding` is honoured by construction rather than assumed to be
//! UTF-8.

use crate::value::{RuntimeError, Value};

use super::Interpreter;

/// The name the NativeCall prelude's `explicitly-manage` calls to obtain the
/// leaked buffer's address.
pub(crate) const EXPLICITLY_MANAGE: &str = "__mutsu_explicitly_manage";

/// The class an explicitly-managed string is wrapped in, matching Rakudo.
pub(crate) const CSTR_CLASS: &str = "NativeCall::CStr";

impl Interpreter {
    /// `__mutsu_explicitly_manage($encoded)` — copy an encoded `Blob`'s bytes
    /// into a fresh, NUL-terminated allocation that is never freed, and return
    /// its address. `None` for any other function name, so the caller falls
    /// through to its remaining dispatch.
    ///
    /// The leak is the feature: this is the one place in mutsu where memory is
    /// handed to C permanently, and `nativecall.rakudoc` says so outright —
    /// "all memory management for explicitly managed strings must be handled by
    /// the C library itself".
    pub(crate) fn try_explicitly_manage(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if name != EXPLICITLY_MANAGE {
            return None;
        }
        if args.len() != 1 {
            return Some(Err(RuntimeError::new(format!(
                "explicitly-manage() expects 1 argument, got {}",
                args.len()
            ))));
        }
        let arg = crate::runtime::types::unwrap_varref_value(args[0].clone());
        let bytes = match encoded_bytes(&arg) {
            Some(b) => b,
            None => {
                return Some(Err(RuntimeError::new(
                    "explicitly-manage() expects an encoded Blob",
                )));
            }
        };
        Some(Ok(Value::int(leak_c_string(&bytes) as i64)))
    }
}

/// The bytes of an encoded `Blob`/`Buf`, or `None` when the value is not one.
fn encoded_bytes(v: &Value) -> Option<Vec<u8>> {
    use crate::value::ValueView;
    match v.view() {
        ValueView::Instance { attributes, .. } => {
            let node = crate::value::value_buf::buf_storage_node(&attributes)?;
            Some(node.bytes.clone())
        }
        ValueView::Scalar(inner) => encoded_bytes(inner),
        ValueView::ContainerRef(cell) => cell.lock().ok().and_then(|g| encoded_bytes(&g)),
        ValueView::VarRef { value, .. } => encoded_bytes(value),
        _ => None,
    }
}

/// Copy `bytes` plus a terminating NUL into an allocation that is intentionally
/// never freed, and return its address.
fn leak_c_string(bytes: &[u8]) -> usize {
    let mut owned = Vec::with_capacity(bytes.len() + 1);
    owned.extend_from_slice(bytes);
    owned.push(0);
    // Leaking is the whole contract; the C library owns this buffer now.
    Box::leak(owned.into_boxed_slice()).as_ptr() as usize
}

/// The stable `char*` behind an explicitly-managed string, or `None` when `v` is
/// not one. Used by the `Str` parameter marshaller so
/// `f(explicitly-manage($s))` hands C the leaked buffer rather than a temporary.
pub(crate) fn explicitly_managed_address(v: &Value) -> Option<usize> {
    use crate::value::ValueView;
    match v.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name.resolve() == CSTR_CLASS => match attributes.as_map().get("address")?.view()
        {
            ValueView::Int(a) if a > 0 => Some(a as usize),
            _ => None,
        },
        ValueView::Scalar(inner) => explicitly_managed_address(inner),
        ValueView::ContainerRef(cell) => cell
            .lock()
            .ok()
            .and_then(|g| explicitly_managed_address(&g)),
        ValueView::VarRef { value, .. } => explicitly_managed_address(value),
        _ => None,
    }
}
