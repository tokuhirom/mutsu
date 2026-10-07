//! `cglobal` — reading a library's exported (`extern`) variables.
//!
//! ```raku
//! my $errno := cglobal('libc.so.6', 'errno', int32);
//! ```
//!
//! Raku's `cglobal` returns a `Proxy` that "redirects all its accesses" to the
//! named symbol (`Language/nativecall.rakudoc`), so it re-reads on every fetch —
//! which is the whole point for a variable C keeps changing underneath you. That
//! `Proxy` is upstream NativeCall's; this module is the primitive behind its
//! `FETCH`, `nqp::nativecallglobal($libname, $symbol, $target-type)`.
//!
//! **It dereferences.** The symbol's address is where the variable *lives*, and
//! the value is read from it — `cglobal('libc.so.6', 'optind', int32)` is `1`,
//! not the address of `optind`. (Verified against Rakudo.) A missing library or
//! symbol throws, which is what lets the common existence probe work:
//!
//! ```raku
//! (try cglobal($candidate, $well-known-symbol, Pointer)) ~~ Pointer
//! ```
//!
//! That probe is how `NativeLibs::Searcher` finds a versioned shared object, and
//! through it how `DBIish`'s `mysql` and `Pg` drivers locate their client
//! libraries — the reason this exists. Note what it implies: the symbol probed
//! is usually a *function* (`mysql_init`), so the dereference reads the first
//! word of its machine code. That is meaningless as a pointer and deliberately
//! unused; only "did the lookup throw" is being asked.

use crate::value::{RuntimeError, Value, ValueView};

use super::Interpreter;

impl Interpreter {
    #[cfg(feature = "libffi")]
    pub(crate) fn cglobal_fetch(
        &mut self,
        library: &Value,
        symbol: &str,
        target: &str,
    ) -> Result<Value, RuntimeError> {
        use crate::runtime::cstruct_layout::{FieldLayout, FieldType, read_field, short_base_name};

        // `is native(&lib-name)` may supply the library through a code object;
        // `cglobal` takes the library "in the same ways that they can be to the
        // native trait" (nativecall.rakudoc), so resolve a callable the same way.
        //
        // The name is resolved exactly as `is native(...)` resolves its trait
        // argument, so an UNDEFINED library (`my $CLIB = Str; $CLIB.&cglobal(
        // 'malloc', Pointer)`, the standard idiom for a symbol already linked
        // into every process) means the process's own symbol space rather than
        // a file literally named after the type object.
        let lib_value = match library.view() {
            ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. } => {
                self.call_sub_value(library.clone(), Vec::new(), true)?
            }
            _ => library.clone(),
        };
        let lib_name = crate::runtime::nativecall::library_name_from_value(&lib_value);
        let (lib, lib_name) = crate::runtime::nativecall::load_declared_library(&lib_name)?;
        // SAFETY: looking a symbol up in a dlopen'd library. The handle is
        // leaked to `'static` by `load_library_cached`, so the address stays
        // valid for the rest of the process.
        let addr: usize = unsafe {
            let sym: libloading::Symbol<*const std::ffi::c_void> =
                lib.get(symbol.as_bytes()).map_err(|e| {
                    RuntimeError::new(format!(
                        "cglobal: symbol '{symbol}' not found in '{lib_name}': {e}"
                    ))
                })?;
            *sym.into_raw() as usize
        };

        let short = short_base_name(target);
        // A pointer-shaped target reads a `void*` out of the variable and wraps
        // it. `Pointer.new(0)` is a legitimate defined value, so unlike a native
        // call's NULL *return* (which is the class's type object) a null global
        // stays a defined `Pointer` holding 0.
        if short == "Pointer" || short.starts_with("Pointer[") {
            // SAFETY: as above — `addr` is a live symbol address, and the
            // declared type says a pointer lives there. Same trust every
            // NativeCall signature already gets.
            let held = unsafe { (addr as *const usize).read_unaligned() };
            // Upstream NativeCall's own Pointer type, when it is loaded.
            if let Some(built) = self.native_pointer_of_declared(target, held, false) {
                return built;
            }
            // No upstream type resolves: `Pointer` comes from `use NativeCall`,
            // which is also what exports `cglobal`.
            return Err(RuntimeError::new(format!(
                "cglobal: '{target}' is not a pointer type NativeCall can build"
            )));
        }
        // A CStruct/CUnion/CPointer target: the variable holds a pointer to the
        // struct, and the handle is that address wrapped as the declared class.
        if self.is_cstruct_class(target) {
            // SAFETY: as above.
            let held = unsafe { (addr as *const usize).read_unaligned() };
            return Ok(crate::runtime::nativecall::make_native_handle(short, held));
        }
        let ty =
            FieldType::from_type_name(target, |n| self.is_cstruct_class(n)).ok_or_else(|| {
                RuntimeError::new(format!(
                    "cglobal: '{target}' is not a type NativeCall can read"
                ))
            })?;
        let field = FieldLayout {
            name: symbol.to_string(),
            ty,
            offset: 0,
        };
        // SAFETY: as above. Reading the declared type out of the variable the
        // symbol names is exactly what `cglobal` is for; a wrong declaration is
        // undefined behaviour in Rakudo too.
        Ok(unsafe { read_field(addr, &field) })
    }

    #[cfg(not(feature = "libffi"))]
    pub(crate) fn cglobal_fetch(
        &mut self,
        _library: &Value,
        _symbol: &str,
        _target: &str,
    ) -> Result<Value, RuntimeError> {
        Err(RuntimeError::new(
            "cglobal() requires NativeCall support, which this build does not have",
        ))
    }
}

impl Interpreter {
    /// Resolve a NativeCallSpec's `ret_struct` to the name its class is
    /// actually registered under, at CALL time. Registration-time resolution
    /// (`registered_native_class_name`) covers most declarations, but a native
    /// sub declared INSIDE the class body it returns —
    /// `sub PQconnectdbParams(... --> PGconn)` inside `class PGconn` — runs
    /// its registration before the class exists, leaving the short name; by
    /// call time the class is registered package-qualified, and an instance
    /// tagged with the short name cannot dispatch the class's ordinary Raku
    /// methods (`PGconn.escapeBytea`). Falls back to a UNIQUE `::Short`
    /// suffix match among registered classes; an ambiguous short name is left
    /// alone.
    pub(crate) fn resolve_native_ret_struct(
        &mut self,
        spec: &mut crate::runtime::nativecall::NativeCallSpec,
    ) {
        let Some(name) = spec.ret_struct.clone() else {
            return;
        };
        // A `--> Pointer[T]` return keeps its whole parameterised spelling in
        // `ret_struct`; it names no class and must not be "resolved" to one.
        if crate::runtime::cstruct_layout::pointer_parameter(&name).is_some() {
            return;
        }
        if self.registry().classes.contains_key(&name) {
            return;
        }
        if let Some(ValueView::Package(sym)) = self.env.get(&name).map(Value::view) {
            let resolved = sym.resolve().to_string();
            if resolved != name && self.registry().classes.contains_key(&resolved) {
                spec.ret_struct = Some(resolved);
                return;
            }
        }
        let suffix = format!("::{name}");
        let found = {
            let registry = self.registry();
            let mut found: Option<String> = None;
            for k in registry.classes.keys() {
                if k.ends_with(suffix.as_str()) {
                    if found.is_some() {
                        return;
                    }
                    found = Some(k.clone());
                }
            }
            found
        };
        if let Some(f) = found {
            spec.ret_struct = Some(f);
        }
    }

}

impl Interpreter {
    /// Read a native-handle instance's declared CStruct fields out of C memory
    /// and into its attribute cell, so `$!field` inside the class's own methods
    /// resolves.
    ///
    /// A no-op — and one registry probe — for every ordinary class. Values are
    /// refreshed on each method entry rather than cached once, because the
    /// authoritative copy is the C struct and only that copy is written by the
    /// callee of a native call.
    pub(crate) fn seed_cstruct_fields_for_method(
        &mut self,
        receiver_class_name: &str,
        invocant: Option<&Value>,
    ) {
        let Some(invocant) = invocant else { return };
        let ValueView::Instance { attributes, .. } = invocant.view() else {
            return;
        };
        if !attributes.contains_key("address") {
            return;
        }
        let Some(registered) = self.cstruct_class_name(receiver_class_name) else {
            return;
        };
        let Some(layout) = self.cstruct_layout(&registered) else {
            return;
        };
        for field in &layout {
            let name = field.name.clone();
            if let Some(value) = self.cstruct_field_value(invocant, &name) {
                attributes.insert(name.as_str(), value);
            }
        }
    }
}
