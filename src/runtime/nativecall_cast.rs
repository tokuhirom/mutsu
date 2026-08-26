//! Reinterpreting a C address as a declared Raku type — the shared core of
//! `nativecast($type, $ptr)` and `Pointer[T].deref`.
//!
//! Rakudo defines the second in terms of the first (`NativeCall::Types`'
//! `method deref` is `nativecast(self.of, self)`), and keeping that identity
//! here is not cosmetic: the two used to disagree, and the disagreement was a
//! *crash*. `.deref` read a typed pointer as element 0 of the equivalent
//! `CArray[T]`, which is right for a numeric `T` but wrong for `Str` — a
//! `CArray[Str]` element is a `char*` **stored at** the address, so
//! `Pointer[Str].deref` loaded eight bytes of the string itself and
//! dereferenced them as a pointer. `strdup("Success!").deref` segfaulted.
//!
//! The rule the two now share is the C one: a cast to a *value* type reads
//! through the address, a cast to a *pointer-shaped* type reinterprets the
//! address itself.
//!
//! | target | result |
//! | --- | --- |
//! | `Str` | the NUL-terminated string **at** the address |
//! | `int32` / `num64` / … | the scalar **at** the address |
//! | `Pointer` / `OpaquePointer` | a `Pointer` holding the **same** address |
//! | `Pointer[T]` | ditto, remembering `T` in an `of` attribute (ADR-0056) |
//! | `CArray` / `CArray[T]` / a CStruct / CPointer / CUnion class | a handle on the same address |

use crate::value::{RuntimeError, Value, ValueView};

use super::Interpreter;
use super::cstruct_layout::{
    FieldLayout, FieldType, pointer_parameter, read_field, short_base_name,
};

/// The C field type a cast target reads **through** the address, or `None` for
/// a pointer-shaped target (which reinterprets the address itself) and for any
/// name that is not a native scalar at all.
fn read_through_field_type(short: &str) -> Option<FieldType> {
    if short.contains('[') {
        return None;
    }
    match short {
        // Pointer-shaped: `nativecast(Pointer, $p)` is `$p` again, not the
        // pointer stored at `$p`.
        "Pointer" | "OpaquePointer" | "CArray" | "Str" => None,
        // `is_known_struct` is `false` here on purpose: a class name is a
        // handle target, handled by the caller, not a scalar read.
        _ => FieldType::from_type_name(short, |_| false),
    }
}

impl Interpreter {
    /// Reinterpret `addr` as `target`, per the table in this module's docs.
    pub(crate) fn nativecast_address(&mut self, target: &str, addr: usize) -> Value {
        let short = short_base_name(target);
        // `nativecast(Str, $ptr)` reads the pointer as a NUL-terminated C
        // string (the same marshalling a `--> Str` native return uses), not as
        // an opaque handle tagged `Str`.
        if short == "Str" {
            if addr == 0 {
                return Value::NIL;
            }
            // SAFETY: the declaration says a NUL-terminated C string lives at
            // this address. That is the trust every NativeCall signature gets,
            // and a wrong declaration is undefined behaviour in Rakudo too.
            let cstr = unsafe { std::ffi::CStr::from_ptr(addr as *const std::ffi::c_char) };
            return Value::str(cstr.to_string_lossy().into_owned());
        }
        // `Pointer[T]` stays an ordinary `Pointer` object and remembers `T` in
        // an `of` attribute, rather than becoming an instance of a class named
        // "Pointer[T]" — every `Pointer` method (`.Int`, `.gist`, the
        // marshalling layer's `address` read) keeps working unchanged, and
        // `.of` / `.deref` read the parameter from there.
        if let Some(of) = pointer_parameter(short) {
            return crate::runtime::nativecall::make_typed_pointer(addr, of);
        }
        if addr != 0
            && !self.is_native_handle_class(target)
            && let Some(ty) = read_through_field_type(short)
        {
            let field = FieldLayout {
                name: String::new(),
                ty,
                offset: 0,
            };
            // SAFETY: as above — the caller's declaration vouches that a value
            // of this C type lives at this address.
            return unsafe { read_field(addr, &field) };
        }
        // Tag the handle with the class's **registered** name, not the short
        // one. A CStruct/CPointer/CUnion declared inside a module is registered
        // as `M::BB` while `short_base_name` says `BB`, and a handle carrying
        // the short name matches neither its own class for method resolution
        // (`MoarVM::Guts::REPRs`' hand-written `MVMArrayB.realstart` was
        // unreachable) nor raku's `.^name`. The shortening still applies to the
        // *parameter* forms above, which is what it was introduced for.
        let tag = match self.cstruct_class_name(target) {
            Some(registered) => registered,
            None if self.registry().classes.contains_key(target) => target.to_string(),
            None => short.to_string(),
        };
        crate::runtime::nativecall::make_native_handle(&tag, addr)
    }

    /// `$ptr.of` — what a typed `Pointer[T]` points at, `void` for an untyped
    /// one, as in Rakudo. `NativeHelpers::Blob`'s `blob-from-pointer` branches
    /// on exactly this (`ptr.of ~~ void ?? $type.of !! ptr.of`).
    ///
    /// `$ptr.deref` — the thing at the address, which is exactly
    /// `nativecast($ptr.of, $ptr)` (see the module docs). An untyped `Pointer`
    /// has nothing to cast to and cannot be dereferenced, matching Rakudo's
    /// "Internal error: unhandled target type".
    pub(crate) fn try_pointer_method(
        &mut self,
        target: &Value,
        method: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(method, "of" | "deref") {
            return None;
        }
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        else {
            return None;
        };
        // The prelude's `Pointer` picks up the enclosing package when it is
        // prepended inside a module (`Foo::Pointer`), so match on the last `::`
        // component — the same "one class, several spellings" problem
        // `cstruct_class_name` documents.
        if class_name.as_str().rsplit("::").next() != Some("Pointer") {
            return None;
        }
        let of: Option<String> = attributes
            .as_map()
            .get("of")
            .map(|v| match v.view() {
                ValueView::Package(n) => n.resolve(),
                _ => v.to_string_value(),
            })
            .filter(|n| !n.is_empty() && n != "void");
        if method == "of" {
            return Some(Ok(Value::package(crate::symbol::Symbol::intern(
                of.as_deref().unwrap_or("void"),
            ))));
        }
        let addr = attributes
            .as_map()
            .get("address")
            .map(|v| crate::runtime::to_int(v) as usize)
            .unwrap_or(0);
        let Some(of) = of else {
            // Rakudo: "Internal error: unhandled target type".
            return Some(Err(RuntimeError::new(
                "Cannot dereference an untyped Pointer (no `of` type to read)",
            )));
        };
        Some(Ok(self.nativecast_address(&of, addr)))
    }
}
