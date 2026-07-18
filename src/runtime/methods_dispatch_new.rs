use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Dispatch "new", "bless", and related constructor methods.
    /// Returns Some(result) if the method was handled, None to fall through.
    pub(super) fn dispatch_new_and_constructors(
        &mut self,
        target: &Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        match method {
            // `Range.new($min, $max)`, with the `:excludes-min` / `:excludes-max` adverbs.
            // The same range `$min..$max` builds, just spelled as a constructor.
            "new" if matches!(target.view(), ValueView::Package(name) if name == "Range") => {
                let adverb = |key: &str| {
                    args.iter().any(|a| match a.view() {
                        ValueView::Pair(k, v) => k == key && v.truthy(),
                        ValueView::ValuePair(k, v) => k.to_string_value() == key && v.truthy(),
                        _ => false,
                    })
                };
                let positional: Vec<Value> = args
                    .iter()
                    .filter(|a| !matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
                    .cloned()
                    .collect();
                if positional.len() != 2 {
                    return Some(Err(RuntimeError::new(
                        "Range.new requires a minimum and a maximum".to_string(),
                    )));
                }
                let (excl_min, excl_max) = (adverb("excludes-min"), adverb("excludes-max"));
                if !excl_min && !excl_max {
                    return Some(Ok(Self::make_inclusive_range_value(
                        positional[0].clone(),
                        positional[1].clone(),
                    )));
                }
                Some(Ok(Value::generic_range(
                    positional[0].clone(),
                    positional[1].clone(),
                    excl_min,
                    excl_max,
                )))
            }
            "new" if matches!(target.view(), ValueView::Package(name) if name == "IO::ArgFiles") => {
                // IO::ArgFiles.new(@files): an $*ARGFILES-like handle that reads
                // from an explicit file list rather than the global @*ARGS.
                let paths: Vec<String> = args
                    .iter()
                    .filter(|a| !matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
                    .flat_map(|a| match a.view() {
                        ValueView::Array(items, ..) => items
                            .iter()
                            .map(|v| v.to_string_value())
                            .collect::<Vec<_>>(),
                        ValueView::Seq(items) => items
                            .iter()
                            .map(|v| v.to_string_value())
                            .collect::<Vec<_>>(),
                        _ => vec![a.to_string_value()],
                    })
                    .collect();
                let handle = self.create_handle(
                    crate::runtime::IoHandleTarget::ArgFiles,
                    crate::runtime::IoHandleMode::Read,
                    None,
                );
                let _ = self.with_handle_mut_opt(&handle, |state| {
                    state.argfiles_paths = Some(paths);
                    Ok(())
                });
                Some(Ok(handle))
            }
            "new" if matches!(target.view(), ValueView::Package(name) if matches!(name.resolve().as_str(), "ObjAt" | "ValueObjAt")) =>
            {
                // Pure data assembly — shared with the VM's `try_native_builtin_construct`.
                let class_name = match target.view() {
                    ValueView::Package(n) => n,
                    _ => unreachable!(),
                };
                Some(Self::build_native_objat_value(class_name, &args))
            }
            "new" if matches!(target.view(), ValueView::Package(name) if matches!(name.resolve().as_str(), "IntStr" | "NumStr" | "RatStr" | "ComplexStr")) =>
            {
                // Pure data assembly — shared with the VM's `try_native_builtin_construct`.
                let type_name = match target.view() {
                    ValueView::Package(n) => n.resolve(),
                    _ => unreachable!(),
                };
                Some(Self::build_native_allomorph_value(&type_name, &args))
            }
            "new" if matches!(target.view(), ValueView::Package(name) if name == "Failure") => {
                // Pure data assembly (reads `$!` / MRO) — shared with the VM's
                // `try_native_failure_construct`.
                Some(Ok(self.build_native_failure_value(&args)))
            }
            "handled" if matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Failure") =>
            {
                if args.is_empty() {
                    // Read the handled state
                    Some(Ok(Value::truth(target.is_failure_handled())))
                } else if args.len() == 1 {
                    // Set the handled state: $failure.handled = value
                    let val = args[0].truthy();
                    if let ValueView::Instance { id, .. } = target.view() {
                        crate::value::set_failure_handled(id, val);
                    }
                    Some(Ok(Value::truth(val)))
                } else {
                    None
                }
            }
            "new-from-pairs" => Some(self.dispatch_new_from_pairs(target, args)),
            "new" => Some(self.dispatch_new(target.clone(), args)),
            // self.Mu::new(...)  –  qualified call to Mu's default constructor.
            "Mu::new" => Some(self.call_method_with_values(target.clone(), "bless", args)),
            "bless" => Some(self.dispatch_bless(target, args)),
            _ => None,
        }
    }

    /// Handle new-from-pairs: creates a Set/Bag/Mix from Pair arguments.
    /// Pair keys become element keys, Pair values become weights/counts.
    /// Duplicate keys have weights summed. Zero-weight entries are removed.
    fn dispatch_new_from_pairs(
        &mut self,
        target: &Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let type_name = match target.view() {
            ValueView::Package(name) => name.resolve(),
            _ => {
                return Err(RuntimeError::new(
                    "new-from-pairs can only be called on a type object",
                ));
            }
        };
        // Check for lazy iterables
        for arg in &args {
            if Self::is_lazy_for_set_ops(arg) {
                let what = type_name.split('[').next().unwrap_or(&type_name);
                let mut err = RuntimeError::new(format!("Cannot .{} a lazy list", what));
                err.exception = Some(Box::new(Value::make_instance(
                    Symbol::intern("X::Cannot::Lazy"),
                    [("what".to_string(), Value::str(what.to_string()))]
                        .into_iter()
                        .collect::<AttrMap>(),
                )));
                return Err(err);
            }
        }
        // Flatten a single list argument
        let items: Vec<Value> = if args.len() == 1 {
            Self::value_to_list(&args[0])
        } else {
            args
        };
        match type_name.as_str() {
            "Mix" | "MixHash" => {
                let mut weights: HashMap<String, f64> = HashMap::new();
                for item in &items {
                    let (key, weight) = match item.view() {
                        ValueView::Pair(k, v) => (
                            k.clone(),
                            crate::builtins::quanthash_coerce::mix_pair_weight(v)?,
                        ),
                        ValueView::ValuePair(k, v) => (
                            k.to_string_value(),
                            crate::builtins::quanthash_coerce::mix_pair_weight(v)?,
                        ),
                        _ => (item.to_string_value(), 1.0),
                    };
                    *weights.entry(key).or_insert(0.0) += weight;
                }
                Ok(if type_name == "MixHash" {
                    Value::mix_hash(weights)
                } else {
                    Value::mix(weights)
                })
            }
            "Bag" | "BagHash" => {
                let mut counts: HashMap<String, i64> = HashMap::new();
                for item in &items {
                    let (key, count) = match item.view() {
                        ValueView::Pair(k, v) => {
                            let c = match v.view() {
                                ValueView::Int(i) => i,
                                ValueView::Num(n) => n as i64,
                                _ => 1,
                            };
                            (k.clone(), c)
                        }
                        ValueView::ValuePair(k, v) => {
                            let c = match v.view() {
                                ValueView::Int(i) => i,
                                ValueView::Num(n) => n as i64,
                                _ => 1,
                            };
                            (k.to_string_value(), c)
                        }
                        _ => (item.to_string_value(), 1),
                    };
                    *counts.entry(key).or_insert(0) += count;
                }
                // Remove zero-count entries
                counts.retain(|_, v| *v > 0);
                Ok(if type_name == "BagHash" {
                    Value::bag_hash(counts)
                } else {
                    Value::bag(counts)
                })
            }
            "Set" | "SetHash" => {
                let mut elems = std::collections::HashSet::new();
                for item in &items {
                    match item.view() {
                        ValueView::Pair(k, v) => {
                            if v.truthy() {
                                elems.insert(k.clone());
                            }
                        }
                        ValueView::ValuePair(k, v) => {
                            if v.truthy() {
                                elems.insert(k.to_string_value());
                            }
                        }
                        ValueView::Hash(h) => {
                            for (k, v) in h.iter() {
                                if v.truthy() {
                                    elems.insert(k.clone());
                                }
                            }
                        }
                        _ => {
                            elems.insert(item.to_string_value());
                        }
                    }
                }
                Ok(if type_name == "SetHash" {
                    Value::set_hash(elems)
                } else {
                    Value::set(elems)
                })
            }
            _ => Err(RuntimeError::new(format!(
                "new-from-pairs not supported on {}",
                type_name
            ))),
        }
    }

    /// Handle the "bless" method: creates a new instance with attributes from named args.
    fn dispatch_bless(&mut self, target: &Value, args: Vec<Value>) -> Result<Value, RuntimeError> {
        // self.bless(:attr1($val1), :attr2($val2), ...)
        // Creates a new instance of the invocant's class with attributes from named args
        let class_name = match target.view() {
            ValueView::Package(name) => name,
            ValueView::Instance { class_name, .. } => class_name,
            _ => {
                return Err(RuntimeError::new(
                    "bless can only be called on a class or instance",
                ));
            }
        };
        if matches!(
            class_name.resolve().as_str(),
            "Sub" | "Routine" | "Method" | "Code" | "Block"
        ) {
            return Err(RuntimeError::new(
                "getcodename requires a concrete code object",
            ));
        }
        // Initialize with default attribute values. The attribute defs and the
        // BUILD/TWEAK probes come from the cached per-class NativeCtorPlan
        // (shape-only data, invalidated with the dispatch caches) instead of
        // being re-collected on every bless.
        let plan = self.native_ctor_plan(class_name);
        let cn_resolved = class_name.as_str();
        let mut attributes = AttrMap::new();
        for ((attr_name, _is_public, default, _is_rw, _, sigil, _), &attr_sym) in
            plan.class_attrs.iter().zip(plan.attr_syms.iter())
        {
            // A `@`/`%` attribute with no declared default that a bless named
            // argument provides would get an empty container here only for the
            // override loop below to immediately replace it — skip the throwaway
            // allocation (and the GC-candidate churn its drop incurs). The args
            // scan is cheap (bless calls carry few named args) relative to a
            // HashData/ArrayData allocation per unfilled construction.
            if default.is_none()
                && (*sigil == '@' || *sigil == '%')
                && args
                    .iter()
                    .any(|a| matches!(a.view(), ValueView::Pair(k, _) if k == attr_name))
            {
                continue;
            }
            let val = if let Some(Expr::Literal(lit_val)) = default {
                // Fast path: simple literal defaults (e.g. native type
                // defaults like Int(0)) don't need interpretation.
                lit_val.clone()
            } else if let Some(expr) = default {
                self.eval_block_value(&[Stmt::Expr(expr.clone())])?
            } else if *sigil == '@' {
                // A `@`-sigil attribute with no default is an empty Array,
                // not Nil (matches `dispatch_new`). Leaving it Nil makes
                // `@!attr.elems` return 1 (Any.elems) and corrupts guards.
                let mut arr = Value::real_array(Vec::new());
                if let Some(tc) = plan.type_constraints.get(attr_name).cloned() {
                    arr = self.tag_container_metadata(
                        arr,
                        super::ContainerTypeInfo {
                            value_type: tc,
                            key_type: None,
                            declared_type: None,
                        },
                    );
                }
                arr
            } else if *sigil == '%' {
                // A `%`-sigil attribute with no default is an empty Hash.
                Value::hash(HashMap::new())
            } else {
                // Native types have zero/empty defaults instead of Nil.
                // The plan's type_constraints carry the same MRO-wide map
                // `get_attr_type_constraint` would walk per attribute.
                match plan.type_constraints.get(attr_name).map(String::as_str) {
                    Some(
                        "int" | "int8" | "int16" | "int32" | "int64" | "uint" | "uint8" | "uint16"
                        | "uint32" | "uint64" | "byte" | "atomicint",
                    ) => Value::int(0),
                    Some("num" | "num32" | "num64") => Value::num(0.0),
                    Some("str") => Value::str("".to_string()),
                    _ => Value::NIL,
                }
            };
            attributes.insert(attr_sym, val);
        }
        // Override with named args from bless call. A key that names a declared
        // attribute reuses its pre-interned Symbol (the common case — `|%_`
        // passthrough in a user `new`); anything else interns as before.
        for arg in &args {
            if let ValueView::Pair(key, value) = arg.view() {
                match plan
                    .class_attrs
                    .iter()
                    .position(|(n, ..)| n.as_str() == &**key)
                {
                    Some(i) => {
                        // Sigil-coerce like `dispatch_new` does: a `%`-attribute
                        // provided as a (list of) Pair(s) becomes a Hash, a
                        // `@`-attribute provided as a List becomes an Array
                        // (META6's `multi method new(*%items) { self.bless(|%items) }`
                        // passes `provides => ("Test::META" => "lib/...",)`).
                        let sigil = plan.class_attrs[i].5;
                        attributes.insert(
                            plan.attr_syms[i],
                            Self::coerce_attr_value_by_sigil(value.clone(), sigil),
                        )
                    }
                    None => attributes.insert(key.clone(), value.clone()),
                };
            }
        }
        // Array-subclass construction: an `is Array` subclass reaches the base
        // `Array.new(1, 2, 3)` semantics through `nextwith(|@values)` in its own
        // `new`, which lands here as `bless` with positional (non-Pair) args.
        // Those positional args become the elements of the backing array
        // storage (which `Array` methods on the instance delegate to). Only
        // touch the storage when there are positional args, so a plain
        // named-args `bless` keeps the empty storage seeded by `dispatch_new`.
        if self.class_mro(cn_resolved).iter().any(|n| n == "Array") {
            let elems: Vec<Value> = args
                .iter()
                .filter(|a| !matches!(a.view(), ValueView::Pair(..)))
                .cloned()
                .collect();
            if !elems.is_empty() || !attributes.contains_key("__mutsu_array_storage") {
                attributes.insert("__mutsu_array_storage", Value::real_array(elems));
            }
        }
        // Embed `is default(...)` element defaults into `@`/`%` containers.
        self.apply_container_attribute_defaults(cn_resolved, &mut attributes);
        // Build the instance BEFORE the BUILD/TWEAK phases and thread its shared
        // attribute cell through them (raku semantics: `self` inside BUILD/TWEAK
        // IS the constructed object — same identity, mutations through a stored
        // `self` alias stay visible). This also drops the former per-phase-step
        // AttrMap clones + phantom intermediate instances (each of which was
        // queued for DESTROY).
        let inv = Value::make_instance(class_name, attributes);
        // Run BUILD/TWEAK submethods in MRO order (base-first). The whole BUILD
        // walk (per-MRO-class registry probes + role-submethod ordering) is
        // skipped when the plan says no class or composed role declares one.
        if plan.has_build {
            self.run_bless_build_phase(class_name, &inv, &args)?;
        }
        // `bless` passes its named arguments through to BUILD and TWEAK (as
        // `BUILDALL` does), so a `submethod BUILD(:$!attr!)` with a required named
        // parameter binds -- the args are also pre-folded into `attributes` above
        // for the common no-explicit-BUILD case.
        if plan.has_tweak {
            self.run_tweak_phase(class_name, &inv, &args)?;
        }
        Ok(inv)
    }

    /// Native `bless` entry for the VM: dispatch straight to `dispatch_bless`,
    /// skipping the interpreter's generic method-dispatch scan (lever A).
    /// Only a registered class with no user-defined (or role-composed) `bless`
    /// override qualifies; everything else returns `None` and falls back to
    /// the interpreter, whose scan handles user overrides / builtin receivers.
    pub(crate) fn try_native_bless(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let class_name = match target.view() {
            ValueView::Package(name) => name,
            ValueView::Instance { class_name, .. } => class_name,
            _ => return None,
        };
        if !self.registry().classes.contains_key(class_name.as_str()) {
            return None;
        }
        // The user-`bless`-override probe is cached in the per-class plan (an
        // MRO + registry scan otherwise paid on every construction).
        if self.native_ctor_plan(class_name).has_custom_bless {
            return None;
        }
        Some(self.dispatch_bless(target, args.to_vec()))
    }

    /// The BUILD phase of `bless`: invoke every BUILD submethod (own and
    /// role-composed) across the MRO in base-first order against `inv`'s shared
    /// attribute cell (every step sees — and mutates — the same live object).
    /// Extracted from `dispatch_bless` so the `plan.has_build` gate can skip it
    /// wholesale. (Unlike `run_build_phase`, a `fail` inside BUILD propagates as
    /// an error here — the historical `bless` behavior.)
    fn run_bless_build_phase(
        &mut self,
        class_name: Symbol,
        inv: &Value,
        args: &[Value],
    ) -> Result<(), RuntimeError> {
        let Some(cell) = Self::self_instance_attrs(inv) else {
            return Ok(());
        };
        let mut probe_attrs = cell.to_map();
        let refresh_probe = probe_attrs
            .keys()
            .any(|k| k.starts_with("__mutsu_attr_alias::"));
        let mro = self.class_mro(&class_name.resolve());
        // Determine the class's language revision for submethod dispatch rules.
        let class_lang_rev = self
            .type_metadata
            .get(&class_name.resolve())
            .and_then(|m| m.get("language-revision"))
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| {
                let version = crate::parser::current_language_version();
                if let Some(rest) = version.strip_prefix("6.") {
                    rest.chars().next().unwrap_or('c').to_string()
                } else {
                    "c".to_string()
                }
            });
        let class_is_6e = class_lang_rev != "c";
        for mro_class in mro.iter().rev().map(|s| s.as_str()) {
            if mro_class == "Any" || mro_class == "Mu" {
                continue;
            }
            // Skip role entries in MRO
            if self.registry().roles.contains_key(mro_class)
                && !self.registry().classes.contains_key(mro_class)
            {
                continue;
            }
            // Check if the class itself has a BUILD submethod
            let class_has_own_build = self
                .registry()
                .classes
                .get(mro_class)
                .and_then(|def| def.methods.get("BUILD"))
                .map(|overloads| overloads.iter().any(|md| md.role_origin.is_none()))
                .unwrap_or(false);
            // Call BUILD submethods from composed roles.
            // Rules:
            // - Always call role BUILD submethods (both 6.c and 6.e classes)
            // - In 6.c: if class has own BUILD, skip role submethods from same
            //   revision (6.c), but still call submethods from other revisions (6.e+)
            // - In 6.e+: always call all role submethods regardless
            let role_order = self.ordered_role_submethods_for_class(mro_class, "BUILD");
            for (role_name, method_def) in role_order {
                // Determine the role's language revision
                let role_base = role_name
                    .split_once('[')
                    .map(|(b, _)| b)
                    .unwrap_or(&role_name);
                let role_lang_rev = self
                    .type_metadata
                    .get(role_base)
                    .and_then(|m| m.get("language-revision"))
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "c".to_string());
                // In 6.c class with own BUILD: skip 6.c role submethods
                if !class_is_6e && class_has_own_build && role_lang_rev == "c" {
                    continue;
                }
                if refresh_probe {
                    probe_attrs = cell.to_map();
                }
                let (_v, updated) = self.run_resolved_method_celled(
                    &class_name.resolve(),
                    &role_name,
                    "BUILD",
                    method_def,
                    &probe_attrs,
                    args.to_vec(),
                    Some(inv.clone()),
                )?;
                if let Some(m) = updated {
                    cell.commit_attrs(m);
                }
            }
            // Call the class's BUILD if it has one that wasn't already handled
            // by ordered_role_submethods_for_class. Role submethods (is_my=true,
            // role_origin=Some) were already called above. Regular methods
            // (is_my=false) from roles still need to go through run_instance_method.
            let has_non_submethod_build = self
                .registry()
                .classes
                .get(mro_class)
                .and_then(|def| def.methods.get("BUILD"))
                .map(|overloads| {
                    overloads
                        .iter()
                        .any(|md| md.role_origin.is_none() || !md.is_my)
                })
                .unwrap_or(false);
            if has_non_submethod_build {
                if refresh_probe {
                    probe_attrs = cell.to_map();
                }
                let (_v, updated) = self.run_instance_method_celled(
                    mro_class,
                    &probe_attrs,
                    "BUILD",
                    args.to_vec(),
                    Some(inv.clone()),
                )?;
                if let Some(m) = updated {
                    cell.commit_attrs(m);
                }
            }
        }
        Ok(())
    }

    /// Run the TWEAK phase of construction: invoke every TWEAK submethod (own and
    /// role-composed) across the MRO in base-first order against `inv`'s shared
    /// attribute cell (every step sees — and mutates — the same live object).
    /// `tweak_args` are the constructor's named arguments, passed through to each
    /// TWEAK so a `submethod TWEAK(:$y)` signature binds them (the `.new` path
    /// passes the original args; `self.bless` passes none). Extracted so the
    /// native default constructor can reuse the exact same TWEAK
    /// ordering/dispatch instead of duplicating it (Track A ③ ctor: a class whose
    /// only non-simple feature is TWEAK is native-default constructible, then
    /// runs TWEAK here).
    pub(crate) fn run_tweak_phase(
        &mut self,
        class_name: Symbol,
        inv: &Value,
        tweak_args: &[Value],
    ) -> Result<(), RuntimeError> {
        let Some(cell) = Self::self_instance_attrs(inv) else {
            return Ok(());
        };
        let mut probe_attrs = cell.to_map();
        let refresh_probe = probe_attrs
            .keys()
            .any(|k| k.starts_with("__mutsu_attr_alias::"));
        let cn = class_name.resolve();
        let mro = self.class_mro(&cn);
        let class_lang_rev = self
            .type_metadata
            .get(&cn)
            .and_then(|m| m.get("language-revision"))
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| {
                let version = crate::parser::current_language_version();
                if let Some(rest) = version.strip_prefix("6.") {
                    rest.chars().next().unwrap_or('c').to_string()
                } else {
                    "c".to_string()
                }
            });
        let class_is_6e = class_lang_rev != "c";
        for mro_class in mro.iter().rev().map(|s| s.as_str()) {
            if mro_class == "Any" || mro_class == "Mu" {
                continue;
            }
            // Skip role entries in MRO
            if self.registry().roles.contains_key(mro_class)
                && !self.registry().classes.contains_key(mro_class)
            {
                continue;
            }
            // Check if the class itself has a TWEAK submethod
            let class_has_own_tweak = self
                .registry()
                .classes
                .get(mro_class)
                .and_then(|def| def.methods.get("TWEAK"))
                .map(|overloads| overloads.iter().any(|md| md.role_origin.is_none()))
                .unwrap_or(false);
            // Call TWEAK submethods from composed roles (same rules as BUILD)
            let role_order = self.ordered_role_submethods_for_class(mro_class, "TWEAK");
            for (role_name, method_def) in role_order {
                let role_base = role_name
                    .split_once('[')
                    .map(|(b, _)| b)
                    .unwrap_or(&role_name);
                let role_lang_rev = self
                    .type_metadata
                    .get(role_base)
                    .and_then(|m| m.get("language-revision"))
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "c".to_string());
                // In 6.c class with own TWEAK: skip 6.c role submethods
                if !class_is_6e && class_has_own_tweak && role_lang_rev == "c" {
                    continue;
                }
                if refresh_probe {
                    probe_attrs = cell.to_map();
                }
                let (_v, updated) = self.run_resolved_method_celled(
                    &cn,
                    &role_name,
                    "TWEAK",
                    method_def,
                    &probe_attrs,
                    tweak_args.to_vec(),
                    Some(inv.clone()),
                )?;
                if let Some(m) = updated {
                    cell.commit_attrs(m);
                }
            }
            // Call the class's TWEAK if it has one that wasn't already handled
            // by ordered_role_submethods_for_class. Same logic as BUILD above.
            let has_non_submethod_tweak = self
                .registry()
                .classes
                .get(mro_class)
                .and_then(|def| def.methods.get("TWEAK"))
                .map(|overloads| {
                    overloads
                        .iter()
                        .any(|md| md.role_origin.is_none() || !md.is_my)
                })
                .unwrap_or(false);
            if has_non_submethod_tweak {
                if refresh_probe {
                    probe_attrs = cell.to_map();
                }
                let (_v, updated) = self.run_instance_method_celled(
                    mro_class,
                    &probe_attrs,
                    "TWEAK",
                    tweak_args.to_vec(),
                    Some(inv.clone()),
                )?;
                if let Some(m) = updated {
                    cell.commit_attrs(m);
                }
            }
        }
        Ok(())
    }

    /// Run the BUILD phase of construction: invoke every BUILD submethod (own and
    /// role-composed) across the MRO in base-first order against `inv`'s shared
    /// attribute cell, passing the constructor's named args. Mirrors the BUILD
    /// loop in the full `.new` path, including its `fail` semantics: a `fail`
    /// inside BUILD does not propagate as an error but yields a `Failure`
    /// instance to return from `.new`. The result is therefore `Ok(Ok(()))` on
    /// success, `Ok(Err(failure))` when a BUILD failed (the caller returns that
    /// `Failure` value), or `Err(e)` for a real error. Extracted so the native
    /// default constructor can reuse it (Track A ③ ctor).
    #[allow(clippy::type_complexity)]
    pub(crate) fn run_build_phase(
        &mut self,
        class_name: Symbol,
        inv: &Value,
        build_args: &[Value],
    ) -> Result<Result<(), Value>, RuntimeError> {
        let Some(cell) = Self::self_instance_attrs(inv) else {
            return Ok(Ok(()));
        };
        let mut probe_attrs = cell.to_map();
        let refresh_probe = probe_attrs
            .keys()
            .any(|k| k.starts_with("__mutsu_attr_alias::"));
        let cn = class_name.resolve();
        let mro = self.class_mro(&cn);
        let class_lang_rev = self
            .type_metadata
            .get(&cn)
            .and_then(|m| m.get("language-revision"))
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| {
                let version = crate::parser::current_language_version();
                if let Some(rest) = version.strip_prefix("6.") {
                    rest.chars().next().unwrap_or('c').to_string()
                } else {
                    "c".to_string()
                }
            });
        let class_is_6e = class_lang_rev != "c";
        for mro_class in mro.iter().rev().map(|s| s.as_str()) {
            if mro_class == "Any" || mro_class == "Mu" {
                continue;
            }
            if self.registry().roles.contains_key(mro_class)
                && !self.registry().classes.contains_key(mro_class)
            {
                continue;
            }
            let class_has_own_build = self
                .registry()
                .classes
                .get(mro_class)
                .and_then(|def| def.methods.get("BUILD"))
                .map(|overloads| overloads.iter().any(|md| md.role_origin.is_none()))
                .unwrap_or(false);
            let role_order = self.ordered_role_submethods_for_class(mro_class, "BUILD");
            for (role_name, method_def) in role_order {
                let role_base = role_name
                    .split_once('[')
                    .map(|(b, _)| b)
                    .unwrap_or(&role_name);
                let role_lang_rev = self
                    .type_metadata
                    .get(role_base)
                    .and_then(|m| m.get("language-revision"))
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "c".to_string());
                if !class_is_6e && class_has_own_build && role_lang_rev == "c" {
                    continue;
                }
                if refresh_probe {
                    probe_attrs = cell.to_map();
                }
                let (_v, updated) = self.run_resolved_method_celled(
                    &cn,
                    &role_name,
                    "BUILD",
                    method_def,
                    &probe_attrs,
                    build_args.to_vec(),
                    Some(inv.clone()),
                )?;
                if let Some(m) = updated {
                    cell.commit_attrs(m);
                }
            }
            let has_non_submethod_build = self
                .registry()
                .classes
                .get(mro_class)
                .and_then(|def| def.methods.get("BUILD"))
                .map(|overloads| {
                    overloads
                        .iter()
                        .any(|md| md.role_origin.is_none() || !md.is_my)
                })
                .unwrap_or(false);
            if has_non_submethod_build {
                if refresh_probe {
                    probe_attrs = cell.to_map();
                }
                match self.run_instance_method_celled(
                    mro_class,
                    &probe_attrs,
                    "BUILD",
                    build_args.to_vec(),
                    Some(inv.clone()),
                ) {
                    Ok((_v, updated)) => {
                        if let Some(m) = updated {
                            cell.commit_attrs(m);
                        }
                    }
                    Err(err) if err.is_fail() => {
                        // `fail` inside BUILD yields a Failure, not an error.
                        let ex = if let Some(exception) = err.exception {
                            *exception
                        } else {
                            let mut ex_attrs = HashMap::new();
                            ex_attrs.insert("message".to_string(), Value::str(err.message));
                            Value::make_instance(Symbol::intern("X::AdHoc"), ex_attrs)
                        };
                        let mut failure_attrs = HashMap::new();
                        failure_attrs.insert("exception".to_string(), ex);
                        return Ok(Err(Value::make_instance(
                            Symbol::intern("Failure"),
                            failure_attrs,
                        )));
                    }
                    Err(err) => return Err(err),
                }
            }
        }
        Ok(Ok(()))
    }

    /// Dispatch Enum .new — should throw X::Constructor::BadType.
    /// Returns Some(err) if target is an enum, None otherwise.
    pub(super) fn dispatch_enum_new_check(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let _ = args;
        let enum_name = match target.view() {
            ValueView::Package(name) if name == "Bool" => Some("Bool".to_string()),
            ValueView::Bool(_) => Some("Bool".to_string()),
            ValueView::Package(name)
                if self.registry().enum_types.contains_key(&name.resolve()) =>
            {
                Some(name.resolve())
            }
            ValueView::Enum { enum_type, .. } => Some(enum_type.resolve()),
            _ => None,
        };
        if let Some(ename) = enum_name {
            let msg = format!(
                "Enum '{}' is insufficiently type-like to be instantiated.  Did you mean 'class'?",
                ename
            );
            let mut attrs = HashMap::new();
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            let ex = Value::make_instance(Symbol::intern("X::Constructor::BadType"), attrs);
            let mut err = RuntimeError::new(msg);
            err.exception = Some(Box::new(ex));
            return Some(Err(err));
        }
        None
    }

    /// Dispatch the CREATE method for CustomType and Package targets.
    pub(super) fn dispatch_create(
        &mut self,
        target: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
        match target.view() {
            ValueView::CustomType(c) => Some(Ok(Value::custom_type_instance(
                c.id,
                c.how.clone(),
                c.repr.clone(),
                c.name,
                std::sync::Arc::new(HashMap::new()),
                crate::value::next_instance_id(),
            ))),
            ValueView::Package(class_name) => {
                // `CREATE` allocates a bare instance with all declared attribute
                // slots present (in their type-default empty state). It does NOT
                // run default-value expressions or BUILD/TWEAK — those belong to
                // `bless`/`BUILDALL`. Allocating the slots is what makes a later
                // `$!attr = ...` (e.g. in a `self.CREATE!SET-SELF: ...` private
                // builder, as MIME::Types uses) persist: the attribute write-back
                // only updates keys that already exist on the instance.
                let attributes = self.create_default_attr_slots(&class_name.resolve());
                Some(Ok(Value::make_instance(class_name, attributes)))
            }
            _ => None,
        }
    }

    /// Build the attribute map for a freshly `CREATE`d instance: every declared
    /// attribute keyed by its bare name with a type-default empty value (native
    /// numerics → 0, `str` → "", everything else → `Nil`). Unlike `bless`, this
    /// does not evaluate `has $.x = EXPR` default expressions.
    fn create_default_attr_slots(&mut self, class_name: &str) -> AttrMap {
        let mut attributes = AttrMap::new();
        if self.registry().classes.contains_key(class_name) {
            for (attr_name, _is_public, _default, _is_rw, _, _, _) in
                self.collect_class_attributes(class_name)
            {
                let type_constraint = self.get_attr_type_constraint(class_name, &attr_name);
                let val = match type_constraint.as_deref() {
                    Some(
                        "int" | "int8" | "int16" | "int32" | "int64" | "uint" | "uint8" | "uint16"
                        | "uint32" | "uint64" | "byte" | "atomicint",
                    ) => Value::int(0),
                    Some("num" | "num32" | "num64") => Value::num(0.0),
                    Some("str") => Value::str("".to_string()),
                    _ => Value::NIL,
                };
                attributes.insert(attr_name, val);
            }
        }
        attributes
    }

    /// Dispatch the "now" method for DateTime subclasses.
    pub(super) fn dispatch_datetime_now(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Package(class_name) = target.view() else {
            return None;
        };
        if !self
            .class_mro(&class_name.resolve())
            .iter()
            .any(|name| name == "DateTime")
        {
            return None;
        }
        use crate::builtins::methods_0arg::temporal;
        // Default timezone comes from $*TZ (the dynamic local timezone variable).
        let default_tz = self
            .env
            .get("*TZ")
            .and_then(|v| {
                if let ValueView::Int(n) = v.view() {
                    Some(n)
                } else {
                    None
                }
            })
            .unwrap_or(0i64);
        let mut timezone = default_tz;
        let mut formatter: Option<Value> = None;
        for arg in args {
            if let ValueView::Pair(key, value) = arg.view() {
                match key.as_str() {
                    "timezone" => timezone = value.to_f64() as i64,
                    "formatter" => formatter = Some(value.clone()),
                    _ => {}
                }
            }
        }
        let secs = crate::value::current_time_secs_f64() + timezone as f64;
        let total_i = secs.floor() as i64;
        let frac = secs - total_i as f64;
        let day_secs = total_i.rem_euclid(86400);
        let epoch_days = (total_i - day_secs) / 86400;
        let (y, m, d) = temporal::epoch_days_to_civil(epoch_days);
        let h = day_secs / 3600;
        let mi = (day_secs % 3600) / 60;
        let s = (day_secs % 60) as f64 + frac;
        let dt = temporal::make_datetime(y, m, d, h, mi, s, timezone);
        if let Some(formatter_value) = formatter
            && let ValueView::Instance {
                class_name,
                attributes,
                id,
            } = dt.view()
        {
            let mut attrs = attributes.to_map();
            attrs.insert("formatter".to_string(), formatter_value.clone());
            let dt_with_formatter = Value::write_back_sharing(&attributes, class_name, attrs, id);
            let saved_env = self.env().clone();
            let saved_readonly = self.enter_readonly_frame();
            let rendered =
                match self.eval_call_on_value(formatter_value, vec![dt_with_formatter.clone()]) {
                    Ok(v) => v.to_string_value(),
                    Err(e) => return Some(Err(e)),
                };
            *self.env_mut() = saved_env;
            self.exit_readonly_frame(saved_readonly);
            if let ValueView::Instance {
                class_name,
                attributes,
                id,
            } = dt_with_formatter.view()
            {
                let mut updated = attributes.to_map();
                updated.insert("__formatter_rendered".to_string(), Value::str(rendered));
                return Some(Ok(Value::write_back_sharing(
                    &attributes,
                    class_name,
                    updated,
                    id,
                )));
            }
        }
        if class_name.resolve() != "DateTime" {
            return Some(self.dispatch_new(target.clone(), vec![dt]));
        }
        Some(Ok(dt))
    }
}
