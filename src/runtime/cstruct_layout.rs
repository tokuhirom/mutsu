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
//! Reads and writes go through a pointer that C gave us. A `HAS`-declared
//! member is laid out **by value** — its own bytes live inside the enclosing
//! struct — which is what NativeCall's `HAS` scope means; allocating a struct
//! from Raku (`MyStruct.new`) remains follow-up work.

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
    /// A `HAS`-declared member of another `is repr('CStruct')` class, stored
    /// **by value**: the member's own bytes are inlined here instead of a
    /// pointer to them. Carries the member's padded size and its alignment,
    /// both taken from that class's own layout — unlike every other variant,
    /// they are not a property of the variant itself.
    Embedded {
        size: usize,
        align: usize,
    },
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
            "int8" | "bool" => FieldType::I8,
            "int16" => FieldType::I16,
            "int32" => FieldType::I32,
            "int64" | "long" | "longlong" | "ssize_t" | "int" => FieldType::I64,
            "uint8" | "byte" => FieldType::U8,
            "uint16" => FieldType::U16,
            "uint32" => FieldType::U32,
            "uint64" | "ulong" | "ulonglong" | "uint" | "size_t" => FieldType::U64,
            "num32" => FieldType::F32,
            "num64" | "num" => FieldType::F64,
            "Str" => FieldType::Str,
            // A bare, unparameterised `CArray` field is still just a pointer in
            // C (`Compress::Zlib::Raw`'s `z_stream` declares `has CArray
            // $.next-in`). Missing it aborted the whole struct layout, which
            // surfaced as `nativesizeof` reporting the class as a P6opaque.
            "Pointer" | "OpaquePointer" | "CArray" => FieldType::Pointer,
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
            FieldType::Embedded { size, .. } => size,
        }
    }

    /// The field's alignment. For every type C supports here this equals its
    /// size, which is what the SysV/Windows ABIs specify for scalars and
    /// pointers alike.
    pub(crate) fn align(self) -> usize {
        match self {
            // An embedded struct aligns to its strictest member, which is not
            // its (padded) size: a `{ int64; int32 }` is 16 bytes but aligns
            // to 8.
            FieldType::Embedded { align, .. } => align,
            _ => self.size(),
        }
    }
}

/// One laid-out field: its name (without sigil/twigil), C type and byte offset.
#[derive(Debug, Clone)]
pub(crate) struct FieldLayout {
    pub name: String,
    pub ty: FieldType,
    pub offset: usize,
}

/// One declared field on the way into [`layout_struct`]: the attribute name,
/// the type it was declared with, and — for a `HAS`-declared member of another
/// CStruct — that member's own `(size, align)`, resolved by the caller (which
/// is the only side that can compute a nested layout).
#[derive(Debug, Clone)]
pub(crate) struct FieldDecl {
    pub name: String,
    pub type_name: String,
    /// `Some((size, align))` for a `HAS` member stored by value; `None` for an
    /// ordinary `has`, including a `HAS` on a type that is not a CStruct (the
    /// "Useless use of HAS scope" case, which rakudo also lays out as a plain
    /// field).
    pub embedded: Option<(usize, usize)>,
}

/// Lay out `fields` (in declaration order) as a C struct, returning each
/// field's offset. A field whose type NativeCall cannot marshal aborts the
/// layout: continuing past it would give every later field a wrong offset, and
/// a wrong offset is a silent wild read.
pub(crate) fn layout_struct(
    fields: &[FieldDecl],
    is_known_struct: impl Fn(&str) -> bool + Copy,
) -> Option<Vec<FieldLayout>> {
    let mut out = Vec::with_capacity(fields.len());
    let mut offset = 0usize;
    for FieldDecl {
        name,
        type_name,
        embedded,
    } in fields
    {
        let ty = match *embedded {
            Some((size, align)) => FieldType::Embedded { size, align },
            None => FieldType::from_type_name(type_name, is_known_struct)?,
        };
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
            // An embedded member IS the bytes at this offset, so its "value"
            // is where they start — the caller wraps that address in a handle
            // of the declared class, and reads through it land in the
            // enclosing struct's own storage.
            FieldType::Embedded { .. } => Value::int(ptr as i64),
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
pub(crate) fn pointer_parameter(type_name: &str) -> Option<&str> {
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
            // Assigning to an embedded member copies the member's bytes in, the
            // way a C `a.inner = b` does. A source that carries no address
            // (a type object, an `Int`) has nothing to copy, so the field is
            // left alone rather than filled with garbage.
            FieldType::Embedded { size, .. } => {
                let src = crate::runtime::nativecall::value_c_address(value);
                if src != 0 && src != base + field.offset {
                    std::ptr::copy_nonoverlapping(src as *const u8, ptr, size);
                }
            }
        }
    }
}

thread_local! {
    /// The classes whose layout is currently being computed, innermost last.
    ///
    /// `HAS` makes the layout recursive — a member's size comes from that
    /// member's own layout — so a struct that (directly or through a cycle)
    /// embeds itself would recurse until the stack ran out. C has no such type
    /// either, so the cycle is simply refused.
    static LAYOUT_IN_PROGRESS: std::cell::RefCell<Vec<String>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

/// Marks `class` as in-progress for as long as the guard lives. `None` when it
/// already is, i.e. the layout is cyclic.
struct LayoutGuard;

impl LayoutGuard {
    fn enter(class: &str) -> Option<LayoutGuard> {
        LAYOUT_IN_PROGRESS.with(|stack| {
            let mut stack = stack.borrow_mut();
            if stack.iter().any(|c| c == class) {
                return None;
            }
            stack.push(class.to_string());
            Some(LayoutGuard)
        })
    }
}

impl Drop for LayoutGuard {
    fn drop(&mut self) {
        LAYOUT_IN_PROGRESS.with(|stack| {
            stack.borrow_mut().pop();
        });
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
    pub(crate) fn is_native_handle_class(&self, name: &str) -> bool {
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

    /// Follow a `constant` type alias a field's declared type is spelled with.
    ///
    /// A C binding names its platform-dependent types once and reuses them:
    /// `DBDish::mysql::Native` declares `constant my_bool = int8` and
    /// `constant intptr = ptrsize == 8 ?? uint64 !! uint32`, then writes
    /// `has intptr $.length` / `has my_bool $.is_unsigned` inside `MYSQL_BIND`.
    /// The alias is not a C type name, so the field was unmappable and — because
    /// one bad field aborts the whole layout — the struct had *no* layout at all:
    /// `nativesizeof(MYSQL_BIND)` failed, which in turn killed the
    /// `LinearArray[MYSQL_BIND]` parameterisation that computes its stride from
    /// it. Signatures already follow these aliases
    /// ([`Self::resolve_native_type_alias`]); fields now do too.
    ///
    /// Only a name that is *not* already marshallable is followed, so a field
    /// typed with a real C type or with a class held by reference keeps its
    /// declared spelling.
    ///
    /// `owner` is the CStruct class declaring the field: when the constant is a
    /// module file-scope name whose env entry died with the frame that loaded
    /// the module (`require` inside a method), it is resolved from the owner's
    /// module scope instead (`module_scope_lexicals`).
    fn resolve_field_type_alias(&self, ty: &str, owner: &str) -> String {
        if ty.is_empty()
            || FieldType::from_type_name(ty, |n| self.is_native_handle_class(n)).is_some()
        {
            return ty.to_string();
        }
        self.resolve_native_type_alias_for_owner(ty, owner)
    }

    /// The C field layout of a `is repr('CStruct')` class, or `None` if the
    /// class is not a CStruct or declares a field NativeCall cannot marshal.
    pub(crate) fn cstruct_layout(&mut self, class_name: &str) -> Option<Vec<FieldLayout>> {
        let registered = self.cstruct_class_name(class_name)?;
        // A struct that embeds itself has no size in C either, and following it
        // here would recurse forever through `cstruct_size_align`.
        let _guard = LayoutGuard::enter(&registered)?;
        let attrs = self.collect_class_attributes(&registered);
        let mut fields: Vec<FieldDecl> = attrs
            .iter()
            .map(|attr| {
                let ty = self
                    .get_attr_type_constraint(&registered, &attr.name)
                    .unwrap_or_default();
                FieldDecl {
                    name: attr.name.clone(),
                    type_name: self.resolve_field_type_alias(&ty, &registered),
                    embedded: None,
                }
            })
            .collect();
        // A `HAS` member occupies its own storage inline, so its size and
        // alignment come from the embedded type's layout — which needs
        // `&mut self` and so is resolved here, before the layout call.
        for (field, attr) in fields.iter_mut().zip(attrs.iter()) {
            if !self.is_embedded_attribute(&registered, &field.name) {
                continue;
            }
            field.embedded = match (attr.sigil, attr.declared_shape.as_deref()) {
                // `HAS num32 @.mat[16] is CArray` — an inline array of 16
                // native floats, laid out end to end with no pointer in
                // between. `kazmath`'s `kmMat4` and `Image::Libexif`'s
                // `ExifData` are both shaped like this.
                ('@', Some(dims)) => {
                    let (elem_size, elem_align) = self.cstruct_size_align(&field.type_name)?;
                    Some((dims.iter().product::<usize>() * elem_size, elem_align))
                }
                // `HAS T @.x[N]` where `N` could not be resolved to a
                // dimension at all — attribute registration already tried a
                // literal extraction and, for a named `constant`/enum `N`, a
                // registration-time evaluation of the shape default too
                // (#8032); this is left only for a shape that depends on
                // instance state (`self`) or otherwise fails to evaluate.
                // Guessing the element count would put every later field at
                // a wrong offset, which is a silent wild read — abort the
                // layout instead, the same way an unmarshallable field does.
                ('@', None) => return None,
                // `HAS gsl_vector $.vector` — the member struct's own bytes.
                _ if self.is_cstruct_class(&field.type_name) => {
                    Some(self.cstruct_size_align(&field.type_name)?)
                }
                // `HAS` on anything C does not hold by value (a native scalar,
                // a `Str`, a `Pointer[T]`) lays out as an ordinary field;
                // rakudo warns "Useless use of HAS scope" and does the same.
                _ => None,
            };
        }
        // `is_known_struct` cannot borrow `self` here (the layout call takes it
        // by value), so resolve the pointer-shaped field types up front.
        let handle_fields: std::collections::HashSet<&str> = fields
            .iter()
            .map(|f| f.type_name.as_str())
            .filter(|ty| self.is_native_handle_class(ty))
            .collect();
        layout_struct(&fields, |n| handle_fields.contains(n))
    }

    /// Whether `attr_name` was declared with NativeCall's `HAS` scope on
    /// `class_name` or on one of its ancestors. Follows the MRO for the same
    /// reason [`Self::get_attr_type_constraint`] does: the layout of a subclass
    /// includes its parent's fields, declared in the parent's body.
    pub(crate) fn is_embedded_attribute(&self, class_name: &str, attr_name: &str) -> bool {
        self.mro_readonly(class_name).iter().any(|cls| {
            self.registry()
                .classes
                .get(cls)
                .is_some_and(|cd| cd.embedded_attributes.contains(attr_name))
        })
    }

    /// The `CArray[T]` spelling an inline `HAS T @.x[N] is CArray` member reads
    /// back as, or `None` when `attr_name` is not a shaped `@` attribute.
    fn embedded_array_tag(&mut self, class_name: &str, attr_name: &str) -> Option<String> {
        let attr = self
            .collect_class_attributes(class_name)
            .into_iter()
            .find(|a| a.name == attr_name)?;
        if attr.sigil != '@' || attr.declared_shape.is_none() {
            return None;
        }
        let elem = self.get_attr_type_constraint(class_name, attr_name)?;
        Some(format!("CArray[{}]", short_base_name(&elem)))
    }

    /// The `(padded size, alignment)` a value of `type_name` occupies in C.
    /// `None` for a type NativeCall cannot marshal.
    pub(crate) fn cstruct_size_align(&mut self, type_name: &str) -> Option<(usize, usize)> {
        // A CStruct is checked first: as a *field* it is one pointer, but this
        // asks for the struct's own footprint.
        if self.is_cstruct_class(type_name) {
            let layout = self.cstruct_layout(type_name)?;
            let last = layout.last()?;
            let end = last.offset + last.ty.size();
            // C rounds a struct up to its strictest member's alignment, so an
            // array of them keeps every element aligned.
            let align = layout.iter().map(|f| f.ty.align()).max().unwrap_or(1);
            return Some((end.div_ceil(align) * align, align));
        }
        let short = type_name.rsplit("::").next().unwrap_or(type_name);
        FieldType::from_type_name(short, |n| self.is_native_handle_class(n))
            .map(|ty| (ty.size(), ty.align()))
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
        // A `HAS` member reads as the address of its inline storage, so it goes
        // through the same wrapping as a pointer field: what comes back is a
        // handle of the declared class onto the bytes inside this struct.
        if !matches!(field.ty, FieldType::Pointer | FieldType::Embedded { .. }) {
            return Some(raw);
        }
        let declared = self.get_attr_type_constraint(&registered, name)?;
        let addr = crate::runtime::to_int(&raw) as usize;
        // An inline `HAS T @.x[N] is CArray` member reads back as a `CArray[T]`
        // onto its own storage — that handle is what makes `$s.x[2]` reach the
        // bytes inside this struct.
        if matches!(field.ty, FieldType::Embedded { .. })
            && let Some(tag) = self.embedded_array_tag(&registered, name)
        {
            return Some(crate::runtime::nativecall::make_native_handle(&tag, addr));
        }
        // A `CArray`-typed field is a `CArray` handle, not a bare `Pointer`:
        // being able to index it is the whole reason a binding declares the
        // field that way (`Compress::Zlib::Raw`'s `z_stream.next-in`).
        if short_base_name(&declared).starts_with("CArray") {
            return Some(crate::runtime::nativecall::make_native_handle(
                short_base_name(&declared),
                addr,
            ));
        }
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
        self.cstruct_size_align(type_name).map(|(size, _)| size)
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

    /// Mirror of `native_carray_element` for element assignment: write `value`
    /// into native memory at `base + index * sizeof(elem)`. `None` when the
    /// element type is not marshallable (caller falls back). Same
    /// trust-the-declaration contract as `write_field`.
    pub(crate) fn native_carray_element_assign(
        &mut self,
        elem: &str,
        base: usize,
        index: usize,
        value: &crate::value::Value,
    ) -> Option<()> {
        if base == 0 {
            return None;
        }
        let ty = FieldType::from_type_name(elem, |n| self.is_native_handle_class(n))?;
        let field = FieldLayout {
            name: String::new(),
            ty,
            offset: index.checked_mul(ty.size())?,
        };
        // SAFETY: as in `native_carray_element` — the address and index are the
        // caller's declaration-backed contract, exactly as in Rakudo.
        unsafe { write_field(base, &field, value) };
        Some(())
    }

    /// The primitive behind NativeCall's `nativesizeof($obj-or-type)`, reporting
    /// how many bytes the argument's type takes in C. Both a type object
    /// (`nativesizeof(uint32)`) and an instance are accepted, matching Rakudo.
    ///
    /// The user-visible `nativesizeof` is an `our sub` in the NativeCall prelude
    /// (`NATIVECALL_SUB_PRELUDES`) that calls this. It is spelled `__mutsu_`
    /// here precisely so that it is *not* an ambient builtin: Rakudo exports
    /// `nativesizeof` from `NativeCall.rakumod`, so it must arrive with the
    /// module and be `&`-callable, not be visible to every program.
    pub(crate) fn try_nativesizeof(
        &mut self,
        name: &str,
        args: &[crate::value::Value],
    ) -> Option<Result<crate::value::Value, crate::value::RuntimeError>> {
        use crate::value::{RuntimeError, ValueView};
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

    /// The primitive behind NativeCall's `nativecast($target-type, $source)` —
    /// reinterpret the C pointer carried by `$source` as `$target-type`. The
    /// only way to reach the fields of a struct a C function handed back as an
    /// opaque pointer (`nativecast(evp_cipher_st, $cipher).key_len`).
    ///
    /// As with `try_nativesizeof`, the user-visible `nativecast` is an `our sub`
    /// in the NativeCall prelude; this half is `__mutsu_`-prefixed so it is not
    /// an ambient builtin.
    pub(crate) fn try_nativecast(
        &mut self,
        name: &str,
        args: &[crate::value::Value],
    ) -> Option<Result<crate::value::Value, crate::value::RuntimeError>> {
        use crate::value::{RuntimeError, ValueView};
        if name != "__mutsu_nativecast" {
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
        if let ValueView::Array(array, _) = args[1].view() {
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
                unsafe { crate::value::gc_contents_mut(&array) }.promote_native_storage(elem_type);
            }
        }
        // `nativecast(:(num64 --> num64), $ptr)` — cast a raw C function pointer
        // to a *signature*, yielding something callable. This is how a symbol
        // looked up at runtime becomes a usable routine (`NativeLibs`'
        // `Loader.symbol($name, :(num64 --> num64))`), so there is no `is native`
        // declaration and no symbol name to bind — only the address.
        if let ValueView::Instance { class_name, id, .. } = args[0].view()
            && class_name.resolve() == "Signature"
        {
            return Some(self.native_callable_from_signature(id, &args[1]));
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
        // The address-to-value half is shared with `Pointer[T].deref`, which
        // Rakudo defines as `nativecast(self.of, self)` — see
        // `runtime::nativecall_cast`.
        Some(Ok(self.nativecast_address(&target, addr)))
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
        if let ValueView::Array(data, _) = target.view()
            && let Some(body) = data.native_repr_body_address()
        {
            return Some(match method {
                "REPR" => Value::str_from("VMArray"),
                _ => Value::int(body as i64),
            });
        }
        if let ValueView::Array(data, _) = target.view()
            && let Some(elem_type) = data
                .declared_type
                .as_deref()
                .and_then(|name| {
                    name.strip_prefix("array[")
                        .and_then(|s| s.strip_suffix(']'))
                })
                .or(data.value_type.as_deref())
            && crate::runtime::native_types::is_native_array_element_type(elem_type)
        {
            if method == "WHERE" {
                unsafe { crate::value::gc_contents_mut(&data) }.promote_native_storage(elem_type);
                if let Some(body) = data.native_repr_body_address() {
                    return Some(Value::int(body as i64));
                }
            }
            return Some(match method {
                "REPR" => Value::str_from("VMArray"),
                _ => Value::int(data.items().as_ptr() as i64),
            });
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
        // A native-backed `CArray[T]` (ADR-0015 P3). Same shape as the buffer
        // above, a different REPR body: `NativeHelpers::Blob`'s `pointer-to`
        // reads `$bb.storage` off a `CArrayB` where a `Blob`'s reads
        // `$bb.realstart` off an `MVMArrayB`. An array whose element type is a
        // reference (`CArray[Str]`) has no storage node and so keeps
        // `P6opaque`, which is the safe direction (§2.1).
        if crate::value::value_carray::is_native_carray_class(&class_name.resolve())
            && let Some(body) = crate::value::value_carray::carray_repr_body_address(&attributes)
        {
            return Some(match method {
                "REPR" => Value::str_from("CArray"),
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

    /// The representation a class was *declared* with (`is repr('CStruct')`).
    /// `None` for an ordinary class, whose representation is `P6opaque`.
    ///
    /// [`Self::try_native_handle_repr_where`] answers `.REPR` for a live handle
    /// — an instance that carries a C address. A **type object** has no address,
    /// so it fell through to `P6opaque`, and a binding that gates on the
    /// representation of its type parameter got the wrong answer:
    /// `NativeHelpers::CStruct`'s `LinearArray[::T]` opens with
    /// `die "Need a CStruct" unless T.REPR eq 'CStruct'`.
    pub(crate) fn declared_class_repr(&self, name: &str) -> Option<&'static str> {
        let short = name.rsplit("::").next().unwrap_or(name);
        let reg = self.registry();
        let holds = |set: &rustc_hash::FxHashSet<String>| {
            set.contains(name)
                || set
                    .iter()
                    .any(|c| c.rsplit("::").next().unwrap_or(c) == short)
        };
        if holds(&reg.cstruct_classes) {
            Some("CStruct")
        } else if holds(&reg.cunion_classes) {
            Some("CUnion")
        } else if holds(&reg.cpointer_classes) {
            Some("CPointer")
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn no_structs(_: &str) -> bool {
        false
    }

    fn plain(name: &str, type_name: &str) -> FieldDecl {
        FieldDecl {
            name: name.to_string(),
            type_name: type_name.to_string(),
            embedded: None,
        }
    }

    #[test]
    fn scalar_fields_are_padded_to_their_alignment() {
        let fields = [
            plain("a", "int8"),
            plain("b", "int32"),
            plain("c", "int8"),
            plain("d", "num64"),
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
            plain("v", "int32"),
            plain("m", "OpenSSL::Method::SSL_METHOD"),
            plain("n", "int32"),
        ];
        let layout = layout_struct(&fields, |n| n == "OpenSSL::Method::SSL_METHOD").unwrap();
        assert_eq!(layout[1].ty, FieldType::Pointer);
        assert_eq!(layout[1].offset, 8, "the pointer aligns to 8");
        assert_eq!(layout[2].offset, 16);
    }

    #[test]
    fn an_unmarshallable_field_aborts_the_layout() {
        let fields = [plain("a", "int32"), plain("b", "SomeRakuClass")];
        assert!(layout_struct(&fields, no_structs).is_none());
    }

    #[test]
    fn an_embedded_member_occupies_its_own_storage() {
        // `class Outer { HAS Inner $.i; has int32 $.t }` where `Inner` is
        // `{ int32; num64 }`: 16 bytes, aligned to 8. The member is laid out
        // by value, so the tail follows it rather than following a pointer.
        let fields = [
            FieldDecl {
                name: "i".to_string(),
                type_name: "Inner".to_string(),
                embedded: Some((16, 8)),
            },
            plain("t", "int32"),
        ];
        let layout = layout_struct(&fields, |n| n == "Inner").unwrap();
        assert_eq!(layout[0].offset, 0);
        assert_eq!(layout[0].ty, FieldType::Embedded { size: 16, align: 8 });
        assert_eq!(layout[1].offset, 16, "the tail follows the whole member");
    }

    #[test]
    fn an_embedded_member_pads_to_its_own_alignment() {
        let fields = [
            plain("head", "int8"),
            FieldDecl {
                name: "i".to_string(),
                type_name: "Inner".to_string(),
                embedded: Some((16, 8)),
            },
        ];
        let layout = layout_struct(&fields, |n| n == "Inner").unwrap();
        assert_eq!(layout[1].offset, 8, "aligned to 8, not to its 16-byte size");
    }
}
