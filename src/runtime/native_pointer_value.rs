//! The object a C pointer reads back as, for a declared pointer type.
//!
//! A CStruct field `has Pointer[int8] $.err`, a `cglobal(..., Pointer)` and the
//! like name their type by its spelling. With upstream NativeCall loaded, that
//! spelling denotes upstream's `NativeCall::Types::Pointer` (a CPointer-REPR
//! class) or a `Pointer[T]` mixin `^parameterize` built, and the value has to
//! be an instance of exactly that type, as MoarVM boxes it (#11203).

use super::*;

impl Interpreter {
    /// The object of the pointer type spelled `declared` that holds C
    /// address `addr`. A NULL `addr` reads as the type object when
    /// `null_is_type_object` (a CStruct field, as in rakudo), and as a defined
    /// object holding 0 otherwise. `None` when `declared` does not resolve to a
    /// CPointer/CArray type, leaving the caller's own handling in place.
    // Cost: O(n) for the spelling plus `native_object_of_type`; a `C[T]`
    // spelling's `^parameterize` runs once (memoized).
    pub(crate) fn native_pointer_of_declared(
        &mut self,
        declared: &str,
        addr: usize,
        null_is_type_object: bool,
    ) -> Option<Result<Value, RuntimeError>> {
        let ty = if declared.contains('[') {
            self.meta_parameterized_type(declared)?
        } else {
            self.constraint_type_value(declared)?
        };
        if addr == 0 && null_is_type_object {
            // Only a type `native_object_of_type` would box answers here, so a
            // name that is not a pointer type keeps the caller's handling.
            return self
                .native_object_of_type(&ty, 0)
                .map(|built| built.map(|_| ty.clone()));
        }
        self.native_object_of_type(&ty, addr)
    }
}
