//! `is repr<CStr>`: a string object that owns a C string (#11209, ADR-11203 §2.4).
//!
//! Upstream NativeCall declares the class from inside `explicitly-manage`:
//!
//! ```raku
//! our sub explicitly-manage(Str $x, :$encoding = 'utf8') {
//!     my class CStr is repr<CStr> { method encoding() { $encoding } }
//!     $x does ExplicitlyManagedString;
//!     $x.cstr = nqp::box_s(nqp::unbox_s($x), CStr)
//! }
//! ```
//!
//! `nqp::box_s($str, CStr)` is MoarVM's CStr REPR at work: the object it makes
//! holds a NUL-terminated copy of the string that the runtime never frees, so
//! a callee that keeps the pointer (`putenv`) keeps seeing live memory. The
//! copy is UTF-8 whatever `.encoding` says (measured on Rakudo: `:encoding<ascii>`
//! and `<utf16>` hand C the same six bytes for `héllo`). `nqp::unbox_s` of the
//! object decodes it again, and a NULL one -- `nqp::create` of the class, with
//! nothing boxed -- answers the null string.
//!
//! The REPR is selected by the declaration ([`Registry::cstr_classes`], filled
//! from `is repr<CStr>`), not by the class name. An object's buffer address is
//! its one hidden attribute, [`CSTR_ADDRESS`]; an object that has none is NULL.
//! The marshaller reads it back ([`cstr_repr_address`]) when such an object, or
//! a `Str` that did `ExplicitlyManagedString` and so carries one in its `cstr`
//! attribute, is passed where C wants a `char*`.
//!
//! [`Registry::cstr_classes`]: super::registry::Registry

use super::*;

/// The hidden attribute holding the address of a CStr-REPR object's C string.
pub(crate) const CSTR_ADDRESS: &str = "__mutsu_cstr_address";

/// The role attribute upstream's `ExplicitlyManagedString` keeps the object in.
#[cfg(feature = "libffi")]
const MANAGED_STRING_ATTR: &str = "cstr";

/// Copy `bytes` plus a terminating NUL into an allocation that is intentionally
/// never freed, and return its address.
///
/// The leak is the feature: this is the one place in mutsu where memory is
/// handed to C permanently, and `nativecall.rakudoc` says so outright -- "all
/// memory management for explicitly managed strings must be handled by the C
/// library itself".
// Cost: O(n), n = bytes.
fn leak_c_string(bytes: &[u8]) -> usize {
    let mut owned = Vec::with_capacity(bytes.len() + 1);
    owned.extend_from_slice(bytes);
    owned.push(0);
    // Leaking is the whole contract; the C library owns this buffer now.
    Box::leak(owned.into_boxed_slice()).as_ptr() as usize
}

impl Interpreter {
    /// `nqp::box_s($str, $class)` for a `$class` declared `is repr<CStr>`: an
    /// object that owns a NUL-terminated UTF-8 copy of `s`, leaked on purpose
    /// (the C library manages it from now on).
    // Cost: O(n), n = bytes of the string (copied, and never freed).
    pub(crate) fn box_str_into_cstr(
        &mut self,
        class: &Value,
        s: &str,
    ) -> Result<Value, RuntimeError> {
        let object = self.nqp_create(class.clone())?;
        let addr = leak_c_string(s.as_bytes());
        Self::nqp_bindattr_value("bindattr_i", &object, CSTR_ADDRESS, Value::int(addr as i64))?;
        Ok(object)
    }

    /// What `nqp::unbox_s` answers for `obj` when it is a CStr-REPR object: the
    /// string its buffer holds, or the null string (`Nil`) for a NULL one.
    /// `None` when `obj` is not such an object.
    // Cost: O(n), n = bytes of the C string; O(m) for the registry probe, m = chars of the class name.
    pub(crate) fn cstr_object_string(&self, obj: &Value) -> Option<Value> {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = obj.view()
        else {
            return None;
        };
        if !self.registry().cstr_classes.contains(class_name.as_str()) {
            return None;
        }
        let addr = match attributes.as_map().get(CSTR_ADDRESS).map(Value::view) {
            Some(ValueView::Int(a)) if a > 0 => a as usize,
            _ => return Some(Value::NIL),
        };
        // SAFETY: the address was produced by `leak_c_string` for this object
        // (the attribute is hidden and written only by `box_str_into_cstr`): a
        // NUL-terminated allocation that is never freed or moved.
        let text = unsafe { std::ffi::CStr::from_ptr(addr as *const std::ffi::c_char) };
        Some(Value::str(text.to_string_lossy().into_owned()))
    }
}

/// The C string behind a CStr-REPR object, or behind a `Str` that did
/// `ExplicitlyManagedString` (its `cstr` attribute holds one): what a `char*`
/// parameter is handed instead of a temporary copy of the string.
#[cfg(feature = "libffi")]
// Cost: O(r), r = roles mixed into the value.
pub(crate) fn cstr_repr_address(v: &Value) -> Option<usize> {
    match v.view() {
        ValueView::Instance { attributes, .. } => {
            match attributes.as_map().get(CSTR_ADDRESS)?.view() {
                ValueView::Int(a) if a > 0 => Some(a as usize),
                _ => None,
            }
        }
        ValueView::Mixin(inner, mixins) => mixins
            .role_attribute_by_name(MANAGED_STRING_ATTR)
            .and_then(|cstr| cstr_repr_address(&cstr))
            .or_else(|| cstr_repr_address(inner)),
        ValueView::Scalar(inner) => cstr_repr_address(inner),
        ValueView::ContainerRef(cell) => cell.lock().ok().and_then(|g| cstr_repr_address(&g)),
        ValueView::VarRef { value, .. } => cstr_repr_address(value),
        _ => None,
    }
}
