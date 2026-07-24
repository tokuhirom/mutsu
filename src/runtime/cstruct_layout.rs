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
//! Only *reads* through a pointer that C gave us are supported. Writing fields,
//! `HAS`-embedded structs/arrays, and allocating a struct from Raku
//! (`MyStruct.new`) remain follow-up work.

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
        Some(match name {
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
                // `CArray[T]`, and any class C holds by reference (another
                // CStruct, possibly package-qualified: `OpenSSL::Bio::BIO`).
                if other.starts_with("CArray[") || is_known_struct(other) {
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
        Some(crate::runtime::nativecall::make_native_handle(
            if self.is_cstruct_class(&declared) {
                declared.rsplit("::").next().unwrap_or(&declared)
            } else {
                "Pointer"
            },
            addr,
        ))
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
        let short = target.rsplit("::").next().unwrap_or(&target);
        Some(Ok(crate::runtime::nativecall::make_native_handle(
            short, addr,
        )))
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
