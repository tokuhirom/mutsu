//! C `struct` field layout for `is repr('CStruct')` classes.
//!
//! mutsu already passes a CStruct around as an **opaque native handle**: an
//! `Instance` of the declared class whose `address` attribute carries the C
//! pointer (see `runtime::nativecall`). That is enough to hand the pointer back
//! to C, but not to read a field out of it — and real bindings do exactly that.
//! `OpenSSL::SSL` declares the whole `SSL` struct and reads `$ssl.server`;
//! `OpenSSL::CryptTools` casts an `EVP_CIPHER*` with `nativecast` and reads
//! `$evp.key_len` to validate a key length.
//!
//! This module computes each field's byte offset from the class's declared
//! attributes using the platform's C alignment rules, and reads a field out of
//! the pointed-to memory.
//!
//! Reads and writes go through a pointer that C gave us. `HAS`-embedded
//! structs/arrays and allocating a struct from Raku (`MyStruct.new`) remain
//! follow-up work.

/// The C type of one CStruct field.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum FieldType {
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
    /// A `Str` field — a `char*` read as a NUL-terminated string.
    Str,
    /// Any pointer-shaped field: `Pointer`, `CArray[T]`, or another CStruct
    /// class (C holds those by reference). Read as an address.
    Pointer,
}

impl FieldType {
    /// Map a declared attribute type name to its C field type. Returns `None`
    /// for a type NativeCall cannot marshal into a struct field.
    pub(crate) fn from_type_name(
        name: &str,
        is_known_struct: impl Fn(&str) -> bool,
    ) -> Option<Self> {
        // A type written inside a module carries that module's package on the
        // way in (`MoarVM::Guts::REPRs::Pointer[Pointer]` for a plain
        // `Pointer[Pointer]`), so match on the base's last component — the same
        // "one class, several spellings" problem `cstruct_class_name` documents.
        // `is_known_struct` gets the name as written; it does its own matching.
        Some(match short_base_name(name) {
            "int8" => FieldType::I8,
            "int16" => FieldType::I16,
            "int32" => FieldType::I32,
            "int64" | "long" | "longlong" | "int" => FieldType::I64,
            "uint8" | "byte" => FieldType::U8,
            "uint16" => FieldType::U16,
            "uint32" => FieldType::U32,
            "uint64" | "ulong" | "ulonglong" | "uint" | "size_t" => FieldType::U64,
            "num32" => FieldType::F32,
            "num64" | "num" => FieldType::F64,
            "Str" => FieldType::Str,
            "Pointer" | "OpaquePointer" => FieldType::Pointer,
            other => {
                // `CArray[T]`, a typed `Pointer[T]`, and any class C holds by
                // reference (another CStruct, possibly package-qualified:
                // `OpenSSL::Bio::BIO`). A typed pointer is still one pointer —
                // missing it aborted the whole layout, so a struct with a single
                // `has Pointer[my_bool] $.error;` (DBIish's `MYSQL_BIND`) had no
                // layout at all and every field access on it failed.
                if other.starts_with("CArray[")
                    || other.starts_with("Pointer[")
                    || is_known_struct(name)
                {
                    FieldType::Pointer
                } else {
                    return None;
                }
            }
        })
    }

    /// The field's size in bytes.
    pub(crate) fn size(self) -> usize {
        match self {
            FieldType::I8 | FieldType::U8 => 1,
            FieldType::I16 | FieldType::U16 => 2,
            FieldType::I32 | FieldType::U32 | FieldType::F32 => 4,
            FieldType::I64 | FieldType::U64 | FieldType::F64 => 8,
            FieldType::Str | FieldType::Pointer => std::mem::size_of::<usize>(),
        }
    }

    /// The field's alignment. For every type C supports here this equals its
    /// size, which is what the SysV/Windows ABIs specify for scalars and
    /// pointers alike.
    pub(crate) fn align(self) -> usize {
        self.size()
    }
}

/// One laid-out field: its name (without sigil/twigil), C type and byte offset.
#[derive(Debug, Clone)]
pub(crate) struct FieldLayout {
    pub name: String,
    pub ty: FieldType,
    pub offset: usize,
}

/// Lay out `fields` (in declaration order) as a C struct, returning each
/// field's offset. A field whose type NativeCall cannot marshal aborts the
/// layout: continuing past it would give every later field a wrong offset, and
/// a wrong offset is a silent wild read.
pub(crate) fn layout_struct(
    fields: &[(String, String)],
    is_known_struct: impl Fn(&str) -> bool + Copy,
) -> Option<Vec<FieldLayout>> {
    let mut out = Vec::with_capacity(fields.len());
    let mut offset = 0usize;
    for (name, type_name) in fields {
        let ty = FieldType::from_type_name(type_name, is_known_struct)?;
        let align = ty.align();
        offset = offset.div_ceil(align) * align;
        out.push(FieldLayout {
            name: name.clone(),
            ty,
            offset,
        });
        offset += ty.size();
    }
    Some(out)
}

/// Read the field at `base + offset` out of native memory.
///
/// # Safety
/// `base` must be a valid pointer to a C struct of the laid-out type, obtained
/// from C and still alive. This is the same trust the rest of NativeCall
/// extends to a declared signature: a wrong declaration is undefined behaviour
/// in Rakudo too.
pub(crate) unsafe fn read_field(base: usize, field: &FieldLayout) -> crate::value::Value {
    use crate::value::Value;
    let ptr = (base + field.offset) as *const u8;
    unsafe {
        match field.ty {
            FieldType::I8 => Value::int(ptr.cast::<i8>().read_unaligned() as i64),
            FieldType::I16 => Value::int(ptr.cast::<i16>().read_unaligned() as i64),
            FieldType::I32 => Value::int(ptr.cast::<i32>().read_unaligned() as i64),
            FieldType::I64 => Value::int(ptr.cast::<i64>().read_unaligned()),
            FieldType::U8 => Value::int(ptr.read_unaligned() as i64),
            FieldType::U16 => Value::int(ptr.cast::<u16>().read_unaligned() as i64),
            FieldType::U32 => Value::int(ptr.cast::<u32>().read_unaligned() as i64),
            FieldType::U64 => Value::int(ptr.cast::<u64>().read_unaligned() as i64),
            FieldType::F32 => Value::num(ptr.cast::<f32>().read_unaligned() as f64),
            FieldType::F64 => Value::num(ptr.cast::<f64>().read_unaligned()),
            FieldType::Str => {
                let s = ptr.cast::<*const std::ffi::c_char>().read_unaligned();
                if s.is_null() {
                    Value::NIL
                } else {
                    Value::str(std::ffi::CStr::from_ptr(s).to_string_lossy().into_owned())
                }
            }
            FieldType::Pointer => Value::int(ptr.cast::<usize>().read_unaligned() as i64),
        }
    }
}

/// Shorten a possibly-parameterised type name to its last `::` component
/// **without touching the type argument**: `A::B::CArray[X::Y]` stays
/// `CArray[X::Y]`. Splitting on the last `::` of the whole string instead turned
/// `Pointer[MoarVM::Guts::REPRs::CStructB]` into the nonsense class `CStructB]`,
/// which is how a `nativecast` through a qualified body type silently produced
/// an unusable handle.
pub(crate) fn short_base_name(type_name: &str) -> &str {
    let base_end = type_name.find('[').unwrap_or(type_name.len());
    match type_name[..base_end].rfind("::") {
        Some(i) => &type_name[i + 2..],
        None => type_name,
    }
}

/// The element type of a parameterised `Pointer[T]` spelling, or `None` for a
/// plain `Pointer`. The base may be qualified (`NativeCall::Types::Pointer[T]`);
/// the parameter is returned exactly as written, since every consumer resolves
/// a qualified type name by its last component anyway.
fn pointer_parameter(type_name: &str) -> Option<&str> {
    short_base_name(type_name)
        .strip_prefix("Pointer[")
        .and_then(|rest| rest.strip_suffix(']'))
}

/// The address of a process-lifetime C string holding `s`, for a `Str`-typed
/// CStruct field.
///
/// A `char*` field stores a pointer, so the bytes have to outlive the
/// assignment — C reads them whenever it likes, and a `CString` dropped at the
/// end of the call would leave the struct pointing at freed memory. Rakudo keeps
/// the Raku `Str` alive through the struct's `child_objs`; mutsu has no such
/// back-reference, so the strings are interned by content and live for the rest
/// of the process. That bounds the arena by the number of *distinct* strings a
/// program writes into struct fields (a handful, in practice) instead of by the
/// number of writes — the same trade `nativecall::native_object_where` already
/// makes for `.WHERE` blocks.
fn interned_c_string(s: &str) -> *const std::ffi::c_char {
    use std::collections::HashMap;
    use std::sync::{Mutex, OnceLock};
    static STRINGS: OnceLock<Mutex<HashMap<String, usize>>> = OnceLock::new();
    let mut map = STRINGS
        .get_or_init(|| Mutex::new(HashMap::new()))
        .lock()
        .unwrap_or_else(|e| e.into_inner());
    let addr = *map.entry(s.to_string()).or_insert_with(|| {
        // A NUL in the middle truncates, as it does for every other `Str`
        // argument NativeCall marshals.
        let owned = std::ffi::CString::new(s)
            .unwrap_or_else(|e| {
                let bytes = e.into_vec();
                let upto = bytes.iter().position(|b| *b == 0).unwrap_or(bytes.len());
                std::ffi::CString::new(&bytes[..upto]).unwrap_or_default()
            })
            .into_raw();
        owned as usize
    });
    addr as *const std::ffi::c_char
}

/// Write `value` into the field at `base + offset` in native memory.
///
/// # Safety
/// Same contract as [`read_field`]: `base` must point at a live C struct of the
/// laid-out type. A wrong declaration corrupts memory here exactly as it does in
/// Rakudo.
pub(crate) unsafe fn write_field(base: usize, field: &FieldLayout, value: &crate::value::Value) {
    let to_int = crate::runtime::to_int;
    let to_num = |v: &crate::value::Value| crate::runtime::utils::to_float_value(v).unwrap_or(0.0);
    let ptr = (base + field.offset) as *mut u8;
    unsafe {
        match field.ty {
            FieldType::I8 => ptr.cast::<i8>().write_unaligned(to_int(value) as i8),
            FieldType::I16 => ptr.cast::<i16>().write_unaligned(to_int(value) as i16),
            FieldType::I32 => ptr.cast::<i32>().write_unaligned(to_int(value) as i32),
            FieldType::I64 => ptr.cast::<i64>().write_unaligned(to_int(value)),
            FieldType::U8 => ptr.write_unaligned(to_int(value) as u8),
            FieldType::U16 => ptr.cast::<u16>().write_unaligned(to_int(value) as u16),
            FieldType::U32 => ptr.cast::<u32>().write_unaligned(to_int(value) as u32),
            FieldType::U64 => ptr.cast::<u64>().write_unaligned(to_int(value) as u64),
            FieldType::F32 => ptr.cast::<f32>().write_unaligned(to_num(value) as f32),
            FieldType::F64 => ptr.cast::<f64>().write_unaligned(to_num(value)),
            FieldType::Str => {
                // An undefined value is a NULL `char*`, matching the way a `Str`
                // *argument* is marshalled.
                let s = if crate::runtime::types::value_is_defined(value) {
                    interned_c_string(&value.to_string_value())
                } else {
                    std::ptr::null()
                };
                ptr.cast::<*const std::ffi::c_char>().write_unaligned(s);
            }
            // A `Pointer`, another CStruct handle, a `CArray[T]` handle, or a
            // bare address as an `Int` — all carry their address the same way.
            FieldType::Pointer => ptr
                .cast::<usize>()
                .write_unaligned(crate::runtime::nativecall::value_c_address(value)),
        }
    }
}

impl crate::runtime::Interpreter {
    /// The registered name of the `is repr('CStruct')` class `name` refers to,
    /// or `None` if it is not one.
    ///
    /// A CStruct is reached under several spellings: the registry stores the
    /// declaration's storage name (`OpenSSL::SSL::SSL`), a native return value
    /// is tagged with the short name (`SSL`), and a field's declared type
    /// carries the package path it was written with (`OpenSSL::Bio::BIO`). All
    /// three name the same class, so matching falls back to the last `::`
    /// component on both sides.
    pub(crate) fn cstruct_class_name(&self, name: &str) -> Option<String> {
        let reg = self.registry();
        if reg.cstruct_classes.contains(name) {
            return Some(name.to_string());
        }
        let short = name.rsplit("::").next().unwrap_or(name);
        reg.cstruct_classes
            .iter()
            .find(|c| c.rsplit("::").next().unwrap_or(c) == short)
            .cloned()
    }

    /// Whether `name` is a class declared `is repr('CStruct')`.
    pub(crate) fn is_cstruct_class(&self, name: &str) -> bool {
        self.cstruct_class_name(name).is_some()
    }

    /// Whether a *field* of type `name` occupies one pointer inside an
    /// enclosing CStruct: any class NativeCall holds by reference, i.e. one
    /// declared `is repr('CStruct')`, `'CPointer'` or `'CUnion'`.
    fn is_native_handle_class(&self, name: &str) -> bool {
        let short = name.rsplit("::").next().unwrap_or(name);
        let reg = self.registry();
        [
            &reg.cstruct_classes,
            &reg.cpointer_classes,
            &reg.cunion_classes,
        ]
        .iter()
        .any(|set| {
            set.contains(name)
                || set
                    .iter()
                    .any(|c| c.rsplit("::").next().unwrap_or(c) == short)
        })
    }

    /// The C field layout of a `is repr('CStruct')` class, or `None` if the
    /// class is not a CStruct or declares a field NativeCall cannot marshal.
    pub(crate) fn cstruct_layout(&mut self, class_name: &str) -> Option<Vec<FieldLayout>> {
        let registered = self.cstruct_class_name(class_name)?;
        let attrs = self.collect_class_attributes(&registered);
        let fields: Vec<(String, String)> = attrs
            .iter()
            .map(|(name, ..)| {
                let ty = self
                    .get_attr_type_constraint(&registered, name)
                    .unwrap_or_default();
                (name.clone(), ty)
            })
            .collect();
        // `is_known_struct` cannot borrow `self` here (the layout call takes it
        // by value), so resolve the pointer-shaped field types up front.
        let handle_fields: std::collections::HashSet<&str> = fields
            .iter()
            .map(|(_, ty)| ty.as_str())
            .filter(|ty| self.is_native_handle_class(ty))
            .collect();
        layout_struct(&fields, |n| handle_fields.contains(n))
    }

    /// Read field `name` out of the C struct `target` points at, if `target` is
    /// a CStruct handle carrying an address and the class declares that field.
    ///
    /// A field whose declared type is another CStruct class comes back wrapped
    /// as an instance of that class, so `$ssl.method.version`-style chains keep
    /// working; a plain `Pointer` field comes back as a `Pointer`.
    pub(crate) fn cstruct_field_value(
        &mut self,
        target: &crate::value::Value,
        name: &str,
    ) -> Option<crate::value::Value> {
        use crate::value::ValueView;
        let (class_name, address) = match target.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } => {
                let addr = match attributes.as_map().get("address").map(|v| v.view()) {
                    Some(ValueView::Int(a)) if a > 0 => a as usize,
                    _ => return None,
                };
                (class_name.resolve().to_string(), addr)
            }
            _ => return None,
        };
        let registered = self.cstruct_class_name(&class_name)?;
        let layout = self.cstruct_layout(&registered)?;
        let field = layout.iter().find(|f| f.name == name)?;
        // SAFETY: `address` came from C as a pointer to a struct of this
        // declared type and the instance is alive, so the field is in bounds.
        let raw = unsafe { read_field(address, field) };
        if field.ty != FieldType::Pointer {
            return Some(raw);
        }
        let declared = self.get_attr_type_constraint(&registered, name)?;
        let addr = crate::runtime::to_int(&raw) as usize;
        // A `Pointer`-typed field is a `Pointer` object even when it is NULL:
        // unlike a CStruct handle (where a null return is a type object, so
        // `.defined` behaves like Rakudo's), `Pointer.new(0)` is a defined value
        // in Rakudo too, and reading a null field as a type object made
        // `$s.field.Int` empty instead of 0. A parameterised field keeps its
        // parameter, so `.of` / `.deref` work on the value that comes out.
        if declared == "Pointer" || declared.starts_with("Pointer[") {
            return Some(crate::runtime::nativecall::make_typed_pointer(
                addr,
                pointer_parameter(&declared).unwrap_or("void"),
            ));
        }
        Some(crate::runtime::nativecall::make_native_handle(
            if self.is_cstruct_class(&declared) {
                declared.rsplit("::").next().unwrap_or(&declared)
            } else {
                "Pointer"
            },
            addr,
        ))
    }

    /// Write `value` into field `name` of the C struct `target` points at.
    /// Returns `false` — leaving the caller to its ordinary attribute path — if
    /// `target` is not a CStruct handle carrying an address, or the class does
    /// not declare that field.
    ///
    /// This is the write half of [`Self::cstruct_field_value`]. Without it an
    /// assignment through a handle (`$bind.buffer = $addr`) reported success and
    /// went nowhere, because a CStruct handle stores no Raku attributes to
    /// receive it — the struct in C memory is the only storage there is.
    pub(crate) fn cstruct_field_assign(
        &mut self,
        target: &crate::value::Value,
        name: &str,
        value: &crate::value::Value,
    ) -> bool {
        use crate::value::ValueView;
        let (class_name, address) = match target.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } => {
                let addr = match attributes.as_map().get("address").map(|v| v.view()) {
                    Some(ValueView::Int(a)) if a > 0 => a as usize,
                    _ => return false,
                };
                (class_name.resolve().to_string(), addr)
            }
            _ => return false,
        };
        let Some(registered) = self.cstruct_class_name(&class_name) else {
            return false;
        };
        let Some(layout) = self.cstruct_layout(&registered) else {
            return false;
        };
        let Some(field) = layout.iter().find(|f| f.name == name) else {
            return false;
        };
        // SAFETY: `address` came from C as a pointer to a struct of this
        // declared type and the instance is alive, so the field is in bounds —
        // the same trust `cstruct_field_value` documents for the read.
        unsafe { write_field(address, field, value) };
        true
    }

    /// The number of bytes a value of `type_name` occupies in C: the width of a
    /// native scalar, one pointer for anything C holds by reference, or the
    /// padded total size of a `is repr('CStruct')` class. `None` for a type
    /// NativeCall cannot marshal.
    pub(crate) fn native_size_of_type(&mut self, type_name: &str) -> Option<usize> {
        // A CStruct is checked first: as a *field* it is one pointer, but
        // `nativesizeof` asks for the struct's own size.
        if self.is_cstruct_class(type_name) {
            let layout = self.cstruct_layout(type_name)?;
            let last = layout.last()?;
            let end = last.offset + last.ty.size();
            // C rounds a struct up to its strictest member's alignment, so an
            // array of them keeps every element aligned.
            let align = layout.iter().map(|f| f.ty.align()).max().unwrap_or(1);
            return Some(end.div_ceil(align) * align);
        }
        let short = type_name.rsplit("::").next().unwrap_or(type_name);
        FieldType::from_type_name(short, |n| self.is_native_handle_class(n)).map(FieldType::size)
    }

    /// Read element `index` of a `CArray[elem]` that is a **native handle** —
    /// what `nativecast(CArray[T], $ptr)` produces, a bare C pointer with no
    /// Raku-side storage. `None` when the element type is not one NativeCall can
    /// marshal, so the caller can fall back instead of reading garbage.
    ///
    /// A `CArray` carries no length in C, so there is no bound to check: this is
    /// the same trust `read_field` documents. Reading past the array is
    /// undefined behaviour here exactly as it is in Rakudo.
    pub(crate) fn native_carray_element(
        &mut self,
        elem: &str,
        base: usize,
        index: usize,
    ) -> Option<crate::value::Value> {
        if base == 0 {
            return None;
        }
        let ty = FieldType::from_type_name(elem, |n| self.is_native_handle_class(n))?;
        let field = FieldLayout {
            name: String::new(),
            ty,
            offset: index.checked_mul(ty.size())?,
        };
        // SAFETY: `base` came from C (or from `native_object_where`) as the start
        // of an array of `elem`, and the caller vouches for the index being in
        // bounds — the contract NativeCall extends to every declared signature.
        Some(unsafe { read_field(base, &field) })
    }

    /// `nativesizeof($obj-or-type)` — NativeCall's own helper, reporting how
    /// many bytes the argument's type takes in C. Both a type object
    /// (`nativesizeof(uint32)`) and an instance are accepted, matching Rakudo.
    pub(crate) fn try_nativesizeof(
        &mut self,
        name: &str,
        args: &[crate::value::Value],
    ) -> Option<Result<crate::value::Value, crate::value::RuntimeError>> {
        use crate::value::{RuntimeError, ValueView};
        if name != "nativesizeof" {
            return None;
        }
        if args.len() != 1 {
            return Some(Err(RuntimeError::new(format!(
                "nativesizeof() expects 1 argument, got {}",
                args.len()
            ))));
        }
        let arg = crate::runtime::types::unwrap_varref_value(args[0].clone());
        let type_name = match arg.view() {
            ValueView::Package(n) => n.resolve().to_string(),
            ValueView::Instance { class_name, .. } => class_name.resolve().to_string(),
            _ => {
                return Some(Err(RuntimeError::new(
                    "nativesizeof() expects a native type or a native object",
                )));
            }
        };
        Some(match self.native_size_of_type(&type_name) {
            Some(size) => Ok(crate::value::Value::int(size as i64)),
            // Rakudo's wording, so a binding that greps the message still works.
            None => Err(RuntimeError::new(format!(
                "NativeCall op sizeof expected type with CPointer, CStruct, CArray, P6int or P6num representation, but got a P6opaque ({})",
                type_name
            ))),
        })
    }

    /// `nativecast($target-type, $source)` — reinterpret the C pointer carried
    /// by `$source` as `$target-type`. NativeCall's own helper, and the only
    /// way to reach the fields of a struct a C function handed back as an
    /// opaque pointer (`nativecast(evp_cipher_st, $cipher).key_len`).
    pub(crate) fn try_nativecast(
        &mut self,
        name: &str,
        args: &[crate::value::Value],
    ) -> Option<Result<crate::value::Value, crate::value::RuntimeError>> {
        use crate::value::{RuntimeError, ValueView};
        if name != "nativecast" {
            return None;
        }
        let args: Vec<crate::value::Value> = args
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
        let target = match args[0].view() {
            ValueView::Package(n) => n.resolve().to_string(),
            ValueView::Instance { class_name, .. } => class_name.resolve().to_string(),
            _ => {
                return Some(Err(RuntimeError::new(
                    "nativecast() expects a type object as its first argument",
                )));
            }
        };
        let addr = crate::runtime::nativecall::value_c_address(&args[1]);
        let short = short_base_name(&target);
        // `Pointer[T]` stays an ordinary `Pointer` object and remembers `T` in
        // an `of` attribute, rather than becoming an instance of a class named
        // "Pointer[T]" — every `Pointer` method (`.Int`, `.gist`, the
        // marshalling layer's `address` read) keeps working unchanged, and `.of`
        // / `.deref` read the parameter from there.
        if let Some(of) = pointer_parameter(short) {
            return Some(Ok(crate::runtime::nativecall::make_typed_pointer(addr, of)));
        }
        Some(Ok(crate::runtime::nativecall::make_native_handle(
            short, addr,
        )))
    }

    /// `.REPR` / `.WHERE` for a **native handle** — an instance whose whole
    /// identity is a C address (a `nativecast`ed CStruct, CUnion or CArray).
    /// `None` for anything else, which keeps its ordinary answers.
    ///
    /// These two travel together on purpose. `MoarVM::Guts::REPRs`' `BODY_OF`
    /// dispatches on `.REPR` and then *dereferences* `.WHERE`, so answering
    /// `.REPR` honestly is a promise that a REPR body exists at `.WHERE`.
    /// Answering it before the body existed would hand a module the identity
    /// hash to dereference — see ADR-0015 §2.1.
    ///
    /// The body itself needs no new machinery. mutsu's `.WHERE` contract is
    /// "points straight at the payload, no object header" (`Offset` is 0), and
    /// `native_object_where` already hands out a zero-filled block whose first
    /// word is the address. That is byte-for-byte the CStruct body
    /// (`{void* cstruct; void** child_objs}`) and the CArray body
    /// (`{void* storage; void** child; i32 managed; i32 allocated; i32 elems}`)
    /// for an unmanaged cast: storage set, `managed`/`elems` zero, which is
    /// exactly what an unmanaged `CArray` handle is.
    ///
    /// A `Buf`/`Blob` qualifies too, by a different route: it has no `address`
    /// attribute, but its storage node *is* contiguous C memory, and the
    /// `MVMArrayB` body describing it is synthesised from that node (ADR-0015
    /// P2, `value::value_buf_repr`). This is the answer `NativeHelpers::Blob`'s
    /// `pointer-to` needs.
    ///
    /// A CStruct *constructed in Raku* deliberately does not qualify: it has no
    /// C storage yet, so it keeps `P6opaque` and `BODY_OF` keeps refusing it
    /// loudly instead of quietly reading a NULL body. Giving it real storage is
    /// ADR-0015's P3.
    pub(crate) fn try_native_handle_repr_where(
        &mut self,
        target: &crate::value::Value,
        method: &str,
    ) -> Option<crate::value::Value> {
        use crate::value::{Value, ValueView};
        if !matches!(method, "REPR" | "WHERE") {
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
        // A buffer with real element storage. A `Buf`-shaped instance that has
        // none — a type object reached through this path — falls through and
        // keeps `P6opaque`, so nothing ever gets an honest name without a body.
        if crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
            && let Some(body) = crate::value::value_buf::buf_repr_body_address(&attributes)
        {
            return Some(match method {
                "REPR" => Value::str_from("VMArray"),
                _ => Value::int(body as i64),
            });
        }
        let addr = match attributes.as_map().get("address").map(|v| v.view()) {
            Some(ValueView::Int(a)) if a > 0 => a as usize,
            _ => return None,
        };
        let name = class_name.resolve();
        let short = name.rsplit("::").next().unwrap_or(&name).to_string();
        let is_cunion = {
            let reg = self.registry();
            reg.cunion_classes.contains(&name)
                || reg
                    .cunion_classes
                    .iter()
                    .any(|c| c.rsplit("::").next().unwrap_or(c) == short)
        };
        let repr = if self.is_cstruct_class(&name) {
            "CStruct"
        } else if is_cunion {
            "CUnion"
        } else if short == "CArray" || short.starts_with("CArray[") {
            "CArray"
        } else {
            return None;
        };
        Some(match method {
            "REPR" => Value::str_from(repr),
            _ => Value::int(crate::runtime::nativecall::native_object_where(addr) as i64),
        })
    }

    /// `$ptr.of` — what a typed `Pointer[T]` points at, `void` for an untyped
    /// one, as in Rakudo. `NativeHelpers::Blob`'s `blob-from-pointer` branches
    /// on exactly this (`ptr.of ~~ void ?? $type.of !! ptr.of`).
    ///
    /// `$ptr.deref` — the thing at the address. A pointer to a struct yields a
    /// handle onto that same address (C holds structs by reference, so
    /// `nativecast(Pointer[SomeStruct], $p).deref.field` reads the struct in
    /// place); a pointer to a native scalar reads the value there, which is
    /// element 0 of the equivalent `CArray[T]`.
    pub(crate) fn try_pointer_method(
        &mut self,
        target: &crate::value::Value,
        method: &str,
    ) -> Option<Result<crate::value::Value, crate::value::RuntimeError>> {
        use crate::value::{RuntimeError, Value, ValueView};
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
        if self.is_cstruct_class(&of) || self.is_native_handle_class(&of) {
            let short = of.rsplit("::").next().unwrap_or(&of);
            return Some(Ok(crate::runtime::nativecall::make_native_handle(
                short, addr,
            )));
        }
        Some(match self.native_carray_element(&of, addr, 0) {
            Some(v) => Ok(v),
            None => Err(RuntimeError::new(format!(
                "Cannot dereference a Pointer[{}]: not a type NativeCall can read",
                of
            ))),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn no_structs(_: &str) -> bool {
        false
    }

    #[test]
    fn scalar_fields_are_padded_to_their_alignment() {
        let fields = [
            ("a".to_string(), "int8".to_string()),
            ("b".to_string(), "int32".to_string()),
            ("c".to_string(), "int8".to_string()),
            ("d".to_string(), "num64".to_string()),
        ];
        let layout = layout_struct(&fields, no_structs).unwrap();
        assert_eq!(layout[0].offset, 0);
        assert_eq!(layout[1].offset, 4, "int32 aligns to 4");
        assert_eq!(layout[2].offset, 8);
        assert_eq!(layout[3].offset, 16, "num64 aligns to 8");
    }

    #[test]
    fn a_struct_typed_field_is_a_pointer() {
        let fields = [
            ("v".to_string(), "int32".to_string()),
            ("m".to_string(), "OpenSSL::Method::SSL_METHOD".to_string()),
            ("n".to_string(), "int32".to_string()),
        ];
        let layout = layout_struct(&fields, |n| n == "OpenSSL::Method::SSL_METHOD").unwrap();
        assert_eq!(layout[1].ty, FieldType::Pointer);
        assert_eq!(layout[1].offset, 8, "the pointer aligns to 8");
        assert_eq!(layout[2].offset, 16);
    }

    #[test]
    fn an_unmarshallable_field_aborts_the_layout() {
        let fields = [
            ("a".to_string(), "int32".to_string()),
            ("b".to_string(), "SomeRakuClass".to_string()),
        ];
        assert!(layout_struct(&fields, no_structs).is_none());
    }
}
