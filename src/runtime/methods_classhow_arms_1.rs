//! `Metamodel::*HOW` methods, part 1 (ADR-11276 slice 3G): one Interpreter method per
//! metamethod. The rows that reach them are `method_table/ctors_mop/class_how.rs`.

use super::methods_classhow_dispatch::{array_element_type_name, shorten_type_name};
use super::*;

impl Interpreter {
    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_mixin(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            // `.^mixin(Role)` on a slang-activation handle (ADR-0026):
            // record the composition without composing anything.
            if let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = args[0].view()
                && class_name.as_str().starts_with("Mutsu::Slang::")
            {
                return Ok(Self::slang_handle_mixin(
                    &class_name.resolve(),
                    &attributes.as_map(),
                    &args[1..],
                ));
            }
            // `$*LANG.HOW.mixin($*LANG.WHAT, $role)` — the same recording,
            // reached through the *type object* rather than a handle
            // instance. `.WHAT` on the `$*LANG` handle is the CompLang
            // type, so mixing a grammar role into the language itself
            // (rather than into a named slang) lands here; without this it
            // fell through to the generic `but`-style composition and the
            // role was lost, taking the whole slang registration with it.
            if let ValueView::Package(sym) = args[0].view()
                && sym.as_str().starts_with("Mutsu::Slang::")
            {
                return Ok(Self::slang_handle_mixin(
                    crate::runtime::slang_activation::GRAMMAR_HANDLE_CLASS,
                    &crate::value::AttrMap::default(),
                    &args[1..],
                ));
            }
            // Generic `.^mixin(R)`: same composition as infix `but`
            // (`Str.^mixin(R)` is the `Str+{R}` mixin type object). When
            // `R` is an actual role, route through the same role
            // composition `but`/`does` use (`eval_does_values`) rather
            // than `apply_but_mixin`'s generic by-type-name keying —
            // otherwise the mixin map is keyed by the bare role name
            // instead of the `__mutsu_role__<name>` marker every other
            // role-aware consumer (`.can`, `.^can`, `nqp::can`, `.does`)
            // expects, and (for a routine invocant) the composition is
            // never stored in the routine composition cell
            // on a later rebuild (see
            // news/2026-08/test-assertion-trait-is-not-introspectable.md).
            //
            // On a real object this is a rebless, exactly as for `does`
            // (Rakudo's `does` IS `.HOW.mixin`): every alias sees the
            // role, so `self.^mixin(R)` in a method changes the object
            // itself, not a copy the caller has to keep (Tinky's
            // `apply-workflow`).
            let mut result = args[0].clone();
            for role in &args[1..] {
                result = if self.is_role_application(role) {
                    self.eval_does_values_mutating(result, role.clone())?
                } else {
                    Self::apply_but_mixin(result, role.clone())?
                };
            }
            Ok(result)
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_set_name(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            // `$type.^set_name($name)` renames a metaobject (Rakudo's ClassHOW
            // method). It is most often applied to a freshly-composed
            // anonymous type, e.g. `Foo.new but role {...}`, to give it a
            // human-readable name for display. Persist the name so a later
            // `.^name` returns it.
            let new_name = args[1].to_string_value();
            // `.^set_name` on a `Metamodel::Primitives.create_type` type
            // object is `$type.HOW.set_name($type, $name)`, the write half
            // of the same `Metamodel::Naming` protocol `.^name` reads --
            // the name is state on the metaobject, so there is nothing to
            // record on the type itself. A HOW that composes no naming
            // role has nowhere to put the name; leave it anonymous rather
            // than inventing a store the metaobject cannot see.
            if let ValueView::CustomType(c) = args[0].view() {
                let how = (*c.how).clone();
                if self.value_can_method(&how, "set_name") {
                    self.call_method_with_values(
                        how,
                        "set_name",
                        vec![args[0].clone(), Value::str(new_name.clone())],
                    )?;
                }
                return Ok(Value::str(new_name));
            }
            match args[0].view() {
                ValueView::Mixin(inner, mixins) => {
                    // Resolve to the composition-keyed shared node
                    // (ADR-0060) — the SAME node whether `args[0]` came
                    // from `.WHAT` (`Hash::Restricted`'s
                    // `v.var.WHAT.^set_name(...)`) or is the mixed-in
                    // instance itself (`$obj.^set_name(...)`,
                    // `t/metamodel-set-name.t`'s `Foo.new but role {...}`
                    // scenario) — so a later `.^name` on ANY value with
                    // this exact composition, including one constructed
                    // after this call, observes the rename.
                    let overrides = self.mixin_instance_composition_overrides(inner, mixins)?;
                    // SAFETY: aliased in-place mutation of a shared container
                    // (see `gc_contents_mut`); no borrow into the map is live
                    // across the insert, and the insert does not re-enter the VM.
                    let map = unsafe { crate::gc::gc_contents_mut(&overrides) };
                    map.insert(
                        "__mutsu_type_name__".to_string(),
                        Value::str(new_name.clone()),
                    );
                    // A type object (`p.^mixin(TypedPointer[t])` renamed
                    // in upstream NativeCall's `^parameterize`) also
                    // carries the name itself, so the interpreter-free
                    // `.raku` / `.gist` render it (the name is not part
                    // of the composition key, so identity is unchanged).
                    if matches!(inner.view(), ValueView::Package(_))
                        && !crate::gc::Gc::ptr_eq(&overrides, mixins)
                    {
                        // SAFETY: as above -- an aliased in-place insert
                        // into the type object's own overrides node; no
                        // borrow into it is live and nothing re-enters.
                        let own = unsafe { crate::gc::gc_contents_mut(mixins) };
                        own.insert(
                            "__mutsu_type_name__".to_string(),
                            Value::str(new_name.clone()),
                        );
                    }
                }
                ValueView::Package(name) => {
                    let resolved = name.resolve();
                    // A builtin type's `Package` value (e.g. `Hash`, `Array`) is
                    // the SAME shared value for every variable of that type —
                    // it is not a fresh per-instance metaobject. Renaming it
                    // therefore renames the type process-wide for every value
                    // of it, not just the caller's — which matches real
                    // Rakudo: `Hash.^set_name("X"); say Hash.^name` reports
                    // "X" there too (verified against `raku`). A role-mixed
                    // native value's `.WHAT` (the `ValueView::Mixin` arm
                    // above) is what gives `Hash::Restricted` a distinct
                    // per-composition anonymous type object to rename
                    // instead, when the caller wants a scoped rename rather
                    // than a global one.
                    crate::runtime::cow_table_mut(&mut self.types.type_metadata)
                        .entry(resolved)
                        .or_default()
                        .insert("__set_name__".to_string(), Value::str(new_name.clone()));
                }
                ValueView::Instance { class_name, .. } => {
                    crate::runtime::cow_table_mut(&mut self.types.type_metadata)
                        .entry(class_name.resolve())
                        .or_default()
                        .insert("__set_name__".to_string(), Value::str(new_name.clone()));
                }
                _ => {}
            }
            Ok(Value::str(new_name))
    }

        // `Metamodel::Versioning`'s write side. `.^set_ver`/`.^set_auth`/
        // `.^set_api` are the runtime equivalents of the declarative
        // `class C:ver<1.0>:auth<foo>:api<2>` adverbs, and Rakudo stores
        // both in the same slot -- so they land in the very
        // `type_metadata` entry the `:ver(...)` adverb writes and the
        // `"ver"`/`"auth"`/`"api"` readers below already consult. They
        // stay callable after `.^compose` (Rakudo imposes no
        // post-composition lock on metadata), which is what makes the
        // documented `BEGIN { C.^set_ver: v0.0.1 }` idiom work.
    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_set_meta(&mut self, method: &str, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let key = method.trim_start_matches("set_").to_string();
            let name = self.mop_receiver_owner(&args[0]);
            let stored = if key == "ver" {
                Self::version_from_value(args[1].clone())
            } else {
                Value::str(args[1].to_string_value())
            };
            crate::runtime::cow_table_mut(&mut self.types.type_metadata)
                .entry(name)
                .or_default()
                .insert(key, stored.clone());
            Ok(stored)
    }

        // `Metamodel::AttributeContainer`: `.^set_rw` is what the `is rw`
        // class trait calls; `.^rw` reads the flag back as Rakudo's
        // native int (`1` / `0`), in the same `type_metadata` slot
        // `register_class_decl` writes for `class C is rw`.
        // Cost: O(1).
    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_set_rw(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let name = self.mop_receiver_owner(&args[0]);
            crate::runtime::cow_table_mut(&mut self.types.type_metadata)
                .entry(name)
                .or_default()
                .insert("rw".to_string(), Value::TRUE);
            Ok(Value::int(1))
    }

        // A role's HOW (`ParametricRoleGroupHOW`) is no
        // AttributeContainer, so `R.^rw` falls through to NotFound.
        // Cost: O(1).
    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_rw(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let name = self.mop_receiver_owner(&args[0]);
            let is_rw = self
                .types
                .type_metadata
                .get(&name)
                .and_then(|m| m.get("rw"))
                .is_some_and(|v| v.truthy());
            Ok(Value::int(i64::from(is_rw)))
    }

    /// Whether `rw` applies to these arguments (the arm's own guard).
    // Cost: O(1).
    pub(crate) fn mop_rw_applies(&self, args: &[Value]) -> bool {
        !self.is_role_reference_value(&args[0])
    }

        // `Metamodel::Documenting`: `.^set_why` attaches a pod object to
        // the METACLASS, so unlike an attribute write it is not blocked
        // once the type is composed. `.WHY` reads it back, both on the
        // HOW (`Documented.HOW.WHY`) and on the type object itself
        // (`Documented.WHY`, via `dispatch_why`).
    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_set_why(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let name = self.mop_receiver_owner(&args[0]);
            crate::runtime::cow_table_mut(&mut self.types.type_metadata)
                .entry(name)
                .or_default()
                .insert("__set_why__".to_string(), args[1].clone());
            Ok(args[1].clone())
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_why_read(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let name = self.mop_receiver_owner(&args[0]);
            if let Some(why) = self
                .types
                .type_metadata
                .get(&name)
                .and_then(|m| m.get("__set_why__"))
            {
                return Ok(why.clone());
            }
            let target = args[0].clone();
            self.dispatch_why(&target)
    }

        // `Metamodel::Trusting`: the list of types this class declared
        // `trusts` on, in declaration order. Rakudo answers with a `List`
        // of type objects (empty for a class with no `trusts`), and only
        // `ClassHOW` has the method at all -- a role's
        // `ParametricRoleGroupHOW` throws `X::Method::NotFound`, which is
        // what the `is_classhow_method` gate plus this arm's registry
        // check reproduce.
    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_trusts(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            // Rakudo composes `Metamodel::Trusting` into `ClassHOW` only
            // (and therefore into `GrammarHOW`, which subclasses it):
            // `module M {}; M.^trusts`, `enum E <a b>; E.^trusts` and
            // `subset S of Int; S.^trusts` all throw `X::Method::NotFound`
            // while `Int.^trusts` and `G.^trusts` answer `()`. Ask the
            // metaobject itself rather than re-deriving the taxonomy here,
            // so a new HOW kind cannot silently gain the method.
            let how = self.dispatch_how(&args[0], &[])?;
            let how_is_class_like = matches!(
                how.view(),
                ValueView::Instance { class_name, .. }
                    if matches!(
                        class_name.as_str(),
                        "Perl6::Metamodel::ClassHOW" | "Perl6::Metamodel::GrammarHOW"
                    )
            );
            if !how_is_class_like {
                let name = self.mop_receiver_owner(&args[0]);
                return Err(RuntimeError::meta_method_not_found("trusts", &name));
            }
            let name = self.mop_receiver_owner(&args[0]);
            let trusted = self
                .registry()
                .class_trusts
                .get(&name)
                .cloned()
                .unwrap_or_default();
            let types = trusted
                .iter()
                .map(|t| {
                    let canonical = self.resolve_private_class_name(&name, t);
                    Value::package(Symbol::intern(&canonical))
                })
                .collect::<Vec<_>>();
            Ok(Value::array(types))
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_name(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            if let ValueView::Mixin(inner, mixins) = args[0].view() {
                // Same composition-keyed shared node as `set_name`
                // (ADR-0060): a rename made via `.WHAT.^set_name(...)`
                // or directly on the instance is visible here either way.
                // A role-mixed value (`5 but Foo::Bar`, `$x does R`) not
                // renamed reports its base type with a `+{Role,...}` suffix,
                // e.g. `Int+{Foo::Bar}`: `what_type_name` builds this from
                // the recorded role keys; `value_type_name` (a
                // `&'static str`) cannot.
                let name = self.mixin_instance_type_name(&args[0], inner, mixins)?;
                return Ok(Value::str(name));
            }
            if matches!(args[0].view(), ValueView::CustomType(_)) {
                // Same `$type.HOW.name($type)` resolution the `.^name`
                // fast path performs -- one definition, not two.
                return self.dispatch_caret_name(&args[0]);
            }
            let name = match args[0].view() {
                ValueView::Package(name) => self
                    .types
                    .type_metadata
                    .get(&name.resolve())
                    .and_then(|m| m.get("__set_name__"))
                    .map(Value::to_string_value)
                    .unwrap_or_else(|| {
                        crate::value::user_facing_type_name(&name.resolve()).to_string()
                    }),
                ValueView::Instance { class_name, .. } => self
                    .types
                    .type_metadata
                    .get(&class_name.resolve())
                    .and_then(|m| m.get("__set_name__"))
                    .map(Value::to_string_value)
                    .unwrap_or_else(|| {
                        crate::value::user_facing_type_name(&class_name.resolve()).to_string()
                    }),
                ValueView::ParametricRole {
                    base_name,
                    type_args,
                } => {
                    crate::value::parametric_role_display_name(&base_name.resolve(), type_args)
                }
                // A concrete builtin value (`5`, `"x"`, `%h`, ...): honor
                // a process-wide rename of its type via
                // `Hash.^set_name(...)` etc. — see
                // `Interpreter::builtin_display_name`, the same helper
                // `dispatch_caret_name`'s equivalent fallback uses.
                _ => {
                    let owner = self.dispatch_owner_name(&args[0]);
                    self.builtin_display_name(owner)
                }
            };
            Ok(Value::str(name))
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_array_type(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            // A type that recorded one (`is array_type(T)`, a composed
            // role's trait -- a mixin's included --, `.^set_array_type`)
            // answers it.
            if let Some(recorded) = self.type_array_type(&args[0])? {
                return Ok(recorded);
            }
            // The element type of a native array-ish container. Derived from
            // the same name `.^name` reports — `dispatch_caret_name`, which
            // is where a `CArray[int32]` / `array[uint8]` gets its
            // parameterised spelling from the container metadata — so the
            // two can never disagree. `NativeHelpers::Blob` asks every
            // container it is handed for this and feeds the answer to
            // `nativesizeof` and `nativecast(Pointer[T], …)`.
            let name = self.dispatch_caret_name(&args[0])?.to_string_value();
            Ok(Value::package(crate::symbol::Symbol::intern(
                array_element_type_name(&name),
            )))
    }

        // Cost: O(1).
    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_set_array_type(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let owner = self.mop_receiver_owner(&args[0]);
            self.set_array_type(&owner, args[1].clone());
            Ok(Value::NIL)
    }

    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_shortname(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let full = self
                .dispatch_classhow_method("name", vec![args[0].clone()])?
                .to_string_value();
            Ok(Value::str(shorten_type_name(&full)))
    }

        // `S.^refinement`: the `where` predicate as a callable; `Mu` for a
        // subset without one. `UInt` is a builtin, not in the registry.
        // Cost: O(1) for a declared subset.
    // Cost: O(m), m = the members (methods, attributes, parents) of the type the call reads.
    pub(crate) fn mop_refinement(&mut self, args: Vec<Value>) -> Result<Value, RuntimeError> {
            let name = self.mop_receiver_owner(&args[0]);
            if name == "UInt" {
                // TODO: compile to bytecode -- a builtin has no
                // declaration site to build the callable from.
                return self.eval_eval_string("-> $_ { $_ >= 0 }");
            }
            let Some(def) = self.registry().subsets.get(&name).cloned() else {
                let how = self.dispatch_how(&args[0], &[])?;
                let how_name = match how.view() {
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => "Mu".to_string(),
                };
                return Err(crate::runtime::did_you_mean::method_not_found(
                    "refinement",
                    &how_name,
                ));
            };
            Ok(def
                .refinement
                .clone()
                .unwrap_or_else(|| Value::package(Symbol::intern("Mu"))))
    }
}
