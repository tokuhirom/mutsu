//! The six `nqp::` ops that are MoarVM's FFI layer (ADR-11203 §2.3, #11211):
//! `buildnativecall`, `nativecall`, `nativecallcast`, `nativecallsizeof`,
//! `nativecallglobal` and `nativecallrefresh`.
//!
//! Upstream `NativeCall.rakumod` does all its Raku-level work itself (reading
//! the routine's signature, choosing a type code per parameter, guessing the
//! library file name) and hands the result to these ops. In MoarVM they are C
//! (`src/core/nativecall*.c`); here they are Rust built on the same machinery
//! the native `is native` path uses (`nativecall.rs`: libloading + libffi,
//! `nativecall_callback.rs`, `nativecall_cast.rs`, `nativecall_global.rs`). The
//! translation from upstream's argument/return-info hashes to that machinery's
//! [`NativeCallSpec`](crate::runtime::nativecall::NativeCallSpec) is in [`super::nativecall_info`].
//!
//! The `__mutsu_nativesizeof` / `__mutsu_nativecast` entry points of the
//! native provider share their bodies with `nqp::nativecallsizeof` /
//! `nqp::nativecallcast` (one implementation per primitive, ADR-0117).

use super::*;
use crate::runtime::nativecall_info::{self, CallsiteKey};

/// An operand with any argument wrapper and container stripped: the `nqp::`
/// layer reads raw values (see `call_nqp_op`'s boundary note).
fn operand(args: &[Value], i: usize) -> Value {
    crate::runtime::types::unwrap_varref_value(args.get(i).cloned().unwrap_or(Value::NIL))
        .deref_container()
}

/// The type name a type-object (or instance) operand denotes.
fn type_operand_name(v: &Value) -> Option<String> {
    match v.view() {
        ValueView::Package(n) => Some(n.resolve()),
        ValueView::Instance { class_name, .. } => Some(class_name.resolve()),
        _ => None,
    }
}

impl Interpreter {
    /// Try one of the FFI `nqp::` ops. `None` means "not an op this table
    /// knows"; the caller then raises the loud unsupported-op error.
    pub(crate) fn call_nqp_op_ffi(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        Some(match op {
            // nqp::buildnativecall($callsite, $libname, $symbol, $conv,
            // $arg_info, $ret_info): record how to call `$symbol` -- the
            // per-argument marshalling `param_hash_for` chose and the return
            // type `return_hash_for` chose -- in `$callsite`'s NativeCall body.
            // The library is opened (and the symbol resolved) on the first
            // call, through the same per-process library cache the native
            // path uses. Returns 0, as MoarVM does.
            // Cost: O(p), p = parameters described by $arg_info (callback signatures included).
            "buildnativecall" => self.nqp_buildnativecall(args),
            // nqp::nativecall($rettype, $callsite, $args): call the function
            // `$callsite` was built for with the arguments in the list `$args`,
            // boxing the C return value as `$rettype`.
            // Cost: O(p + n), p = arguments, n = bytes marshalled (strings, CArrays); plus the C call itself.
            "nativecall" => self.nqp_nativecall(args),
            // nqp::nativecallcast($target, $box-type, $source): reinterpret the
            // C address `$source` carries as `$target` (a scalar type reads
            // through it, a pointer-shaped type wraps it). `$box-type` is what
            // a scalar read is boxed as; mutsu's scalar reads already box to
            // the matching Int/Num/Str, so it adds nothing here.
            // Cost: O(1); O(n), n = chars, for a Str target.
            "nativecallcast" => {
                let target = operand(args, 0);
                let source = operand(args, 2);
                // An `is repr('Uninstantiable')` target (upstream's `void`, what
                // `Pointer.deref` casts an untyped pointer to) has nothing to
                // box into; MoarVM's cast rejects it.
                if let ValueView::Package(name) = target.view()
                    && self
                        .registry()
                        .uninstantiable_classes
                        .contains(name.as_str())
                {
                    return Some(Err(RuntimeError::new(
                        "Internal error: unhandled target type",
                    )));
                }
                self.nativecast_value(&target, &source)
            }
            // nqp::nativecallsizeof($type): the C size in bytes of a native,
            // CStruct, CUnion, CPointer or CArray type (or of an instance's type).
            // Cost: O(f), f = fields of a CStruct type (its layout is recomputed); O(1) otherwise.
            "nativecallsizeof" => self.native_sizeof_value(&operand(args, 0)),
            // nqp::nativecallglobal($libname, $symbol, $target, $box-type):
            // read the C global `$symbol` of `$libname` as `$target`. The
            // library name arrives already resolved (`guess_library_name`);
            // an empty name is the process's own symbol space.
            // Cost: O(1) after the library is loaded; O(n), n = chars, for a Str target.
            "nativecallglobal" => {
                let library = operand(args, 0);
                let symbol = operand(args, 1).to_string_value();
                let Some(target) = type_operand_name(&operand(args, 2)) else {
                    return Some(Err(RuntimeError::new(
                        "nqp::nativecallglobal expects a type object as its target",
                    )));
                };
                self.cglobal_fetch(&library, &symbol, &target)
            }
            // nqp::nativecallrefresh($obj): MoarVM drops the child objects a
            // CArray/CStruct caches for its reference members, so that the next
            // read sees what C wrote. mutsu keeps such children too -- a
            // CStruct's `__mutsu_cstruct_child_*` attributes, a reference-
            // element CArray's child table -- but they are handles onto C
            // memory, and a read answers one only while its slot still holds
            // that child's address (a slot C rewrote builds a fresh object from
            // the address). Scalar members and `Str` fields are decoded from
            // the memory on every read (`cstruct_field_value`). So a read never
            // sees stale contents, and there is nothing a refresh must drop:
            // doing so would only change the identity of a child, and would
            // free what a Raku-allocated parent's pointer still points at.
            // Returns its argument, as MoarVM does.
            // Cost: O(1).
            "nativecallrefresh" => Ok(operand(args, 0)),
            _ => return self.call_nqp_op_sys(op, args),
        })
    }

    fn nqp_buildnativecall(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // A routine that does upstream's `Native` role builds its call in the
        // role's `is box_target` attribute (`has Callsite $!call is box_target`).
        let target = self.box_target_operand("buildnativecall", operand(args, 0))?;
        let Some(key) = CallsiteKey::of(&target) else {
            return Err(RuntimeError::new(format!(
                "nqp::buildnativecall: cannot hold a NativeCall body in a {}",
                crate::value::type_name::value_type_name(&target)
            )));
        };
        let library = operand(args, 1).to_string_value();
        let symbol = operand(args, 2).to_string_value();
        let spec = nativecall_info::spec_from_info(
            &library,
            &symbol,
            &operand(args, 4),
            &operand(args, 5),
        )?;
        nativecall_info::store_callsite(key, spec);
        // A NativeCall-REPR object unboxes to a non-zero integer once it is
        // built (`return if nqp::unbox_i($!call)` in `Native!setup`).
        if matches!(target.view(), ValueView::Instance { .. }) {
            Self::nqp_bindattr_value("bindattr_i", &target, "__mutsu_int_value", Value::int(1))?;
        }
        Ok(Value::int(0))
    }

    fn nqp_nativecall(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let rettype = operand(args, 0);
        let target = self.box_target_operand("nativecall", operand(args, 1))?;
        let Some(mut spec) = CallsiteKey::of(&target).and_then(nativecall_info::load_callsite)
        else {
            return Err(RuntimeError::new(
                "nqp::nativecall: the callsite has not been built (nqp::buildnativecall)",
            ));
        };
        // A pointer-shaped return is boxed as `$rettype`: a `Pointer`
        // subclass, a typed `Pointer[T]`, a CStruct/CUnion/CArray class.
        let ret_type_name = type_operand_name(&rettype);
        if spec.ret == crate::runtime::nativecall::CType::Pointer {
            spec.ret_struct = ret_type_name
                .as_deref()
                .filter(|n| crate::runtime::cstruct_layout::short_base_name(n) != "Pointer")
                .map(str::to_string);
            self.resolve_native_ret_struct(&mut spec);
        }
        let call_args: Vec<Value> = match operand(args, 2).view() {
            ValueView::Array(items, _) => items.iter().cloned().collect(),
            _ => {
                return Err(RuntimeError::new(
                    "nqp::nativecall expects its arguments as a list",
                ));
            }
        };
        let (result, _out_args) =
            crate::runtime::nativecall::call_native_with_out_args(self, &spec, &call_args)?;
        // MoarVM answers a `void` function, and a NULL `char*`, with the
        // return type object itself (`Mu` for a routine with no `-->`).
        // A pointer return is boxed as `$rettype` itself when that is a
        // CPointer class or a mixin type (upstream's `Pointer`, `Pointer[T]`,
        // `CArray[T]`); NULL is the type object.
        if spec.ret == crate::runtime::nativecall::CType::Pointer && spec.ret_struct.is_none() {
            let addr = crate::runtime::nativecall::value_c_address(&result);
            if let Some(boxed) = self.native_object_of_type(&rettype, addr) {
                return if addr == 0 { Ok(rettype) } else { boxed };
            }
        }
        Ok(match spec.ret {
            crate::runtime::nativecall::CType::Void => rettype,
            crate::runtime::nativecall::CType::Str if result.is_nil() => rettype,
            _ => result,
        })
    }

    /// `nativesizeof($obj-or-type)`: how many bytes the argument's type takes
    /// in C. A type object (`nativesizeof(uint32)`) and an instance are both
    /// accepted, matching Rakudo. The body of `nqp::nativecallsizeof` and of
    /// the native provider's `__mutsu_nativesizeof`.
    // Cost: O(f), f = fields of a CStruct type; O(1) otherwise.
    pub(crate) fn native_sizeof_value(&mut self, arg: &Value) -> Result<Value, RuntimeError> {
        let Some(type_name) = type_operand_name(arg) else {
            return Err(RuntimeError::new(
                "nativesizeof() expects a native type or a native object",
            ));
        };
        let size = self
            .native_size_of_type(&type_name)
            .or_else(|| self.native_decl_size(&type_name));
        match size {
            Some(size) => Ok(Value::int(size as i64)),
            // Rakudo's wording, so a binding that greps the message still works.
            None => Err(RuntimeError::new(format!(
                "NativeCall op sizeof expected type with CPointer, CStruct, CArray, P6int or P6num representation, but got a P6opaque ({})",
                type_name
            ))),
        }
    }

    /// The size of a `native`-declared type from its recorded traits: `is
    /// nativesize(N)` is N bits, `is ctype<...>` names a C type. `None` for any
    /// other type.
    // Cost: O(1).
    fn native_decl_size(&self, type_name: &str) -> Option<usize> {
        let decl = self.native_decl(type_name)?;
        let is_num = decl.repr.as_deref() == Some("P6num");
        Some(match decl.nativesize? {
            bits if bits > 0 => (bits as usize).div_ceil(8),
            // MoarVM's C-type codes (see `native_decl::ctype_nativesize`).
            -1 if is_num => std::mem::size_of::<f32>(),
            -2 if is_num => std::mem::size_of::<f64>(),
            -1 => std::mem::size_of::<std::ffi::c_char>(),
            -2 => std::mem::size_of::<std::ffi::c_short>(),
            -3 => std::mem::size_of::<std::ffi::c_int>(),
            -4 => std::mem::size_of::<std::ffi::c_long>(),
            -5 => std::mem::size_of::<std::ffi::c_longlong>(),
            -6 => std::mem::size_of::<usize>(),
            -7 => std::mem::size_of::<bool>(),
            _ => return None,
        })
    }

    /// `nativecast($target-type, $source)`: reinterpret the C pointer carried
    /// by `$source` as `$target-type`. The only way to reach the fields of a
    /// struct a C function handed back as an opaque pointer
    /// (`nativecast(evp_cipher_st, $cipher).key_len`). The body of
    /// `nqp::nativecallcast` and of the native provider's `__mutsu_nativecast`.
    // Cost: O(1); O(n), n = chars, for a Str target.
    pub(crate) fn nativecast_value(
        &mut self,
        target: &Value,
        source: &Value,
    ) -> Result<Value, RuntimeError> {
        if let ValueView::Array(array, _) = source.view() {
            let elem_type = array
                .declared_type
                .as_deref()
                .and_then(|name| {
                    name.strip_prefix("array[")
                        .and_then(|s| s.strip_suffix(']'))
                })
                .or(array.value_type.as_deref());
            if let Some(elem_type) = elem_type
                && crate::runtime::native_types::is_native_array_element_type(elem_type)
            {
                // SAFETY: promoting a native array's storage replaces its element
                // vector with an equivalent native buffer before its address is
                // taken; no borrow of the elements is live across the call.
                unsafe { crate::value::gc_contents_mut(&array) }.promote_native_storage(elem_type);
            }
        }
        // `nativecast(:(num64 --> num64), $ptr)` — cast a raw C function pointer
        // to a *signature*, yielding something callable. This is how a symbol
        // looked up at runtime becomes a usable routine (`NativeLibs`'
        // `Loader.symbol($name, :(num64 --> num64))`), so there is no `is native`
        // declaration and no symbol name to bind — only the address.
        if let ValueView::Instance { class_name, id, .. } = target.view()
            && class_name.resolve() == "Signature"
        {
            return self.native_callable_from_signature(id, source);
        }
        let addr = self.carray_element_address(source);
        // Upstream's `Pointer[T]` / `CArray[T]` are mixin type objects, and a
        // class declared `is repr('CArray')` boxes a CArray over the address
        // by its REPR; a NULL cast answers the type object, as MoarVM does.
        let boxes_by_repr = match target.view() {
            ValueView::Mixin(..) => true,
            ValueView::Package(class) => self.is_carray_repr_class(class.as_str()),
            _ => false,
        };
        if boxes_by_repr {
            if addr == 0 {
                return Ok(target.clone());
            }
            if let Some(result) = self.native_object_of_type(target, addr) {
                return result;
            }
        }
        let Some(target) = type_operand_name(target) else {
            return Err(RuntimeError::new(
                "nativecast() expects a type object as its first argument",
            ));
        };
        // The address-to-value half is shared with `Pointer[T].deref`, which
        // Rakudo defines as `nativecast(self.of, self)` — see
        // `runtime::nativecall_cast`.
        Ok(self.nativecast_address(&target, addr))
    }

    /// The native provider's `__mutsu_nativesizeof($obj-or-type)`.
    ///
    /// The user-visible `nativesizeof` is an `our sub` in the NativeCall prelude
    /// (`NATIVECALL_SUB_PRELUDES`) that calls this. It is spelled `__mutsu_`
    /// here precisely so that it is *not* an ambient builtin: Rakudo exports
    /// `nativesizeof` from `NativeCall.rakumod`, so it must arrive with the
    /// module and be `&`-callable, not be visible to every program.
    pub(crate) fn try_nativesizeof(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if name != "__mutsu_nativesizeof" {
            return None;
        }
        if args.len() != 1 {
            return Some(Err(RuntimeError::new(format!(
                "nativesizeof() expects 1 argument, got {}",
                args.len()
            ))));
        }
        let arg = crate::runtime::types::unwrap_varref_value(args[0].clone());
        Some(self.native_sizeof_value(&arg))
    }

    /// The native provider's `__mutsu_nativecast($target-type, $source)`.
    ///
    /// As with `try_nativesizeof`, the user-visible `nativecast` is an `our sub`
    /// in the NativeCall prelude; this half is `__mutsu_`-prefixed so it is not
    /// an ambient builtin.
    pub(crate) fn try_nativecast(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if name != "__mutsu_nativecast" {
            return None;
        }
        let args: Vec<Value> = args
            .iter()
            .cloned()
            .map(crate::runtime::types::unwrap_varref_value)
            .collect();
        if args.len() != 2 {
            return Some(Err(RuntimeError::new(format!(
                "nativecast() expects 2 arguments, got {}",
                args.len()
            ))));
        }
        Some(self.nativecast_value(&args[0], &args[1]))
    }
}
