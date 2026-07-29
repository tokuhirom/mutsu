//! `cglobal` — reading a library's exported (`extern`) variables.
//!
//! ```raku
//! my $errno := cglobal('libc.so.6', 'errno', int32);
//! ```
//!
//! Raku's `cglobal` returns a [`Proxy`] that "redirects all its accesses" to the
//! named symbol (`Language/nativecall.rakudoc`), so it re-reads on every fetch —
//! which is the whole point for a variable C keeps changing underneath you. That
//! `Proxy` is built in the NativeCall prelude; this module is the primitive
//! behind its `FETCH`, `__mutsu_cglobal_fetch($libname, $symbol, $target-type)`.
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

/// The name a `Proxy` in the NativeCall prelude calls to perform one fetch.
pub(crate) const CGLOBAL_FETCH: &str = "__mutsu_cglobal_fetch";

impl Interpreter {
    /// `__mutsu_cglobal_fetch($libname, $symbol, $target-type)` — one read of a
    /// C global. `None` for any other function name, so the caller falls
    /// through to its remaining dispatch.
    pub(crate) fn try_cglobal_fetch(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if name != CGLOBAL_FETCH {
            return None;
        }
        let args: Vec<Value> = args
            .iter()
            .cloned()
            .map(crate::runtime::types::unwrap_varref_value)
            .collect();
        if args.len() != 3 {
            return Some(Err(RuntimeError::new(format!(
                "cglobal() expects 3 arguments, got {}",
                args.len()
            ))));
        }
        let symbol = args[1].to_string_value();
        let target = match args[2].view() {
            ValueView::Package(n) => n.resolve().to_string(),
            ValueView::Instance { class_name, .. } => class_name.resolve().to_string(),
            _ => {
                return Some(Err(RuntimeError::new(
                    "cglobal() expects a type object as its third argument",
                )));
            }
        };
        Some(self.cglobal_fetch(&args[0], &symbol, &target))
    }

    #[cfg(feature = "libffi")]
    fn cglobal_fetch(
        &mut self,
        library: &Value,
        symbol: &str,
        target: &str,
    ) -> Result<Value, RuntimeError> {
        use crate::runtime::cstruct_layout::{FieldLayout, FieldType, read_field, short_base_name};

        // `is native(&lib-name)` may supply the library through a code object;
        // `cglobal` takes the library "in the same ways that they can be to the
        // native trait" (nativecall.rakudoc), so resolve a callable the same way.
        let lib_name = match library.view() {
            ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. } => self
                .call_sub_value(library.clone(), Vec::new(), true)?
                .to_string_value(),
            _ => library.to_string_value(),
        };
        let (lib, lib_name) = crate::runtime::nativecall::load_declared_library(&Some(lib_name))?;
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
            return Ok(crate::runtime::nativecall::make_pointer_object(held));
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
    fn cglobal_fetch(
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

    /// Route a resolved `is native(...)` **method** to NativeCall, with the
    /// invocant marshalled as the first C argument.
    ///
    /// `None` when `class.method` carries no native descriptor, which is every
    /// ordinary method — so this is one hash lookup on the method-dispatch path
    /// and nothing else.
    pub(crate) fn try_native_call_method(
        &mut self,
        class_name: &str,
        method: &str,
        invocant: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if self.native_call_specs.is_empty() {
            return None;
        }
        let spec = self
            .native_call_specs
            .get(&Self::native_method_key(class_name, method))
            .or_else(|| {
                let short = class_name.rsplit("::").next().unwrap_or(class_name);
                self.native_call_specs
                    .get(&Self::native_method_key(short, method))
            })?
            .clone();
        let mut spec = spec;
        self.resolve_native_ret_struct(&mut spec);
        let mut call_args = Vec::with_capacity(args.len() + 1);
        call_args.push(invocant.clone());
        call_args.extend(args.iter().cloned());
        let result = match crate::runtime::nativecall::call_native_with_out_args(&spec, &call_args)
        {
            Ok((v, out_args)) => {
                // An `is rw` numeric out-parameter must reach the caller's
                // variable (`$conn.PQescapeByteaConn($buf, $len, $sz)` leaves
                // the written length in `$sz`), the same as the native-sub VM
                // call site. A method argument arrives by value, so the
                // caller name comes from the dispatching CallMethod op's
                // arg-source list (a `VarRef` is honored too, for interpreter
                // routes that keep it). The queued source is applied to the
                // caller's local slot when the VM op returns.
                for (idx, val) in out_args {
                    let name = match call_args[idx].view() {
                        ValueView::VarRef { name, .. } => Some(name.resolve().to_string()),
                        _ => self
                            .pending_call_arg_sources()
                            // `call_args[0]` is the invocant, absent there.
                            .and_then(|sources| sources.get(idx.checked_sub(1)?))
                            .and_then(|s| s.clone()),
                    };
                    if let Some(n) = name {
                        self.env.insert(n.clone(), val);
                        self.pending_rw_writeback_sources.push(n);
                    }
                }
                Ok(v)
            }
            Err(e) => Err(e),
        };
        Some(result)
    }

    /// [`try_native_call_method`] from a call site that knows only the
    /// receiver, resolving the declaring class across the MRO the way ordinary
    /// method resolution does (a native method can be inherited).
    ///
    /// Guarded twice before it costs anything: no native descriptors at all, or
    /// a receiver that is neither an instance nor a type object, and it is a
    /// single `is_empty` check.
    pub(crate) fn try_native_method_on_receiver(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if self.native_call_specs.is_empty() {
            return None;
        }
        let class_name = match target.view() {
            ValueView::Instance { class_name, .. } => class_name.resolve().to_string(),
            ValueView::Package(name) => name.resolve().to_string(),
            _ => return None,
        };
        let mro: Vec<String> = self
            .class_mro(&class_name)
            .iter()
            .map(|s| s.resolve().to_string())
            .collect();
        for cn in mro {
            if let Some(result) = self.try_native_call_method(&cn, method, target, args) {
                return Some(result);
            }
        }
        None
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
