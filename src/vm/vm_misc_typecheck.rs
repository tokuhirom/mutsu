use super::vm_misc_ops::*;
use super::*;
use crate::value::ValueMap;

impl Interpreter {
    pub(super) fn exec_type_check_op_inner(
        &mut self,
        code: &CompiledCode,
        tc_idx: u32,
        var_name_idx: Option<u32>,
        bind_mode: bool,
        has_explicit_initializer: bool,
        smiley_from_pragma: bool,
    ) -> Result<(), RuntimeError> {
        let var_name: Option<&str> = var_name_idx.map(|idx| Self::const_str(code, idx));
        // Set again below only when this check fully matched the value, so the
        // declaration store right after it can skip a second match.
        self.decl_typechecked_context().set(false);
        if !bind_mode
            && (self.type_check_native_scalar_fast(code, tc_idx, var_name)
                || self.type_check_plain_scalar_fast(code, tc_idx, var_name)?)
        {
            return Ok(());
        }
        // A `use variables :D/:U` smiley is already part of the constraint:
        // the compiler applies the (lexical) pragma to the declaration.
        let effective_constraint = std::borrow::Cow::Borrowed(Self::const_str(code, tc_idx));
        // A `constant` type alias stands for the type it names, so resolve it
        // to that target once, here, before anything below reads the
        // constraint. Everything downstream then sees `Int` rather than
        // `MyInt`: the native/known-type check runs, and the type-check error
        // names the target, which is what rakudo reports ("expected Int but got
        // Str"). Without this the alias reached the unknown-type arm and, since
        // env holds a `Package` under the alias, was reported as a package
        // "insufficiently type-like to qualify a variable" (#8131).
        //
        // Gated on the constraint not already being a known type, so the
        // overwhelmingly common `my Int $x` pays a static `matches!` here
        // rather than an env lookup per typed declaration.
        let effective_constraint = {
            let (base, smiley) = crate::runtime::types::strip_type_smiley(&effective_constraint);
            let resolved = if runtime::is_known_type_constraint(base) || is_core_raku_type(base) {
                None
            } else {
                self.resolve_type_alias_chain(base)
                    .or_else(|| self.package_relative_type(base))
                    .map(|target| format!("{}{}", target, smiley.unwrap_or("")))
            };
            match resolved {
                Some(target) => std::borrow::Cow::Owned(target),
                None => effective_constraint,
            }
        };
        let constraint: &str = &effective_constraint;
        let (base_constraint, _) = crate::runtime::types::strip_type_smiley(constraint);
        let declared_constraint = base_constraint
            .split_once('(')
            .map_or(base_constraint, |(target, _)| target);
        let mut value = self.stack.last().expect("TypeCheck: empty stack").clone();
        if var_name.is_some_and(|name| name.starts_with('%')) {
            return Ok(());
        }
        // `my T $x = <Proxy>`: `=` reads its RHS in value context, so the
        // declaration checks (and stores) the FETCHed value, not the Proxy
        // container (ADR-0040's store boundary, `fetch_proxy_for_store`).
        // `my K $x = $obj[0]` over an `AT-POS ... is rw` returning a Proxy
        // (`CArray[CStruct]`) otherwise failed the check against `Any (Proxy)`.
        if !bind_mode && has_explicit_initializer && value.is_proxy_value() {
            value = self.fetch_proxy_for_store(value)?;
            if let Some(top) = self.stack.last_mut() {
                *top = value.clone();
            }
        }
        // The following bind store checks a Proxy's FETCH value and installs
        // the Proxy container. Leave that check to the store so FETCH runs
        // once, including when the RHS is wrapped in a VarRef.
        if bind_mode
            && var_name.is_some_and(|name| name.starts_with('$'))
            && value.unwrap_varref().is_proxy_value()
        {
            return Ok(());
        }
        // A *non-lazy* lazy-positional RHS (e.g. a plain `gather` coroutine, whose
        // view is `LazyList`, not `Array`/`Seq`) is reified here so both native and
        // boxed typed arrays check/coerce its elements through the normal eager-list
        // paths below. A single-shot coroutine must be reified exactly once, so
        // replace the stack value too (the later SetLocal reuses it). A *genuinely*
        // lazy list is left untouched: native arrays reject it just below as
        // X::Cannot::Lazy, and boxed arrays keep it lazy (Rakudo checks its
        // elements only when reified at access time).
        if var_name.is_some_and(|name| name.starts_with('@'))
            && let ValueView::LazyList(list) = value.view()
            && !list.is_genuinely_lazy()
        {
            let items = self.force_lazy_list_vm(&list)?;
            value = Value::real_array(items);
            *self.stack.last_mut().unwrap() = value.clone();
        }
        // A parallel sequence RHS (`.hyper`/`.race`) or a `Slip` bound to a typed
        // array is reified to a real array here so the eager-array element check
        // below inspects each element, instead of type-checking the whole
        // `HyperSeq`/`RaceSeq`/`Slip` against the element type ("expected T, got
        // HyperSeq"). Replace the stack value too so the following SetLocal stores
        // the reified array. Mirrors the `LazyList` reify just above.
        if var_name.is_some_and(|name| name.starts_with('@')) {
            let reified = match value.view() {
                ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => Some(items.to_vec()),
                ValueView::Slip(items) => Some(items.to_vec()),
                // A finite Range (`my Int @a = 1..7`) is reified here too, so
                // the per-element check further below inspects the actual
                // values. Without this, a finite Range fell through to the
                // sentinel spot-check near the end of this function, which
                // tests the constraint against a fixed endpoint/zero value —
                // wrong for a subset whose `where` clause excludes exactly
                // that sentinel (e.g. `subset DoW of Int where { 0 < $_ < 8
                // }`, from the Date::Utils ecosystem distribution: every
                // element of `1..7` is a valid DoW, but the sentinel `0` is
                // not, so the whole assignment was rejected).
                ValueView::Range(a, b)
                | ValueView::RangeExcl(a, b)
                | ValueView::RangeExclStart(a, b)
                | ValueView::RangeExclBoth(a, b)
                    if b != i64::MAX && a != i64::MIN =>
                {
                    Some(runtime::value_to_list(&value))
                }
                ValueView::GenericRange { end, .. }
                    if !matches!(end.as_ref().view(), ValueView::Num(n) if n.is_infinite())
                        && !matches!(
                            end.as_ref().view(),
                            ValueView::Whatever | ValueView::HyperWhatever
                        ) =>
                {
                    Some(runtime::value_to_list(&value))
                }
                _ => None,
            };
            if let Some(items) = reified {
                value = Value::real_array(items);
                *self.stack.last_mut().unwrap() = value.clone();
            }
        }
        // A Buf/Blob assigned to a NATIVE typed array spreads element-wise:
        // `my uint32 @W = $blob32` gives one element per buffer element (rakudo's
        // `array[uint32].STORE` reads the buffer directly). This is specific to
        // native arrays — a boxed `my Int @b = $blob` still sees the Blob as a
        // single element and fails its element type check, as in rakudo.
        // Digest::SHA1's message schedule (`my uint32 @W = $M`) needs this.
        if var_name.is_some_and(|name| name.starts_with('@'))
            && crate::runtime::native_types::is_native_array_element_type(base_constraint)
        {
            let spread = match value.view() {
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } if crate::runtime::utils::is_native_elems_class(&class_name.resolve()) => {
                    crate::value::value_buf::buf_elems_as_array(
                        &attributes.as_map(),
                        crate::value::ArrayKind::List,
                    )
                }
                _ => None,
            };
            if let Some(list) = spread {
                value = list;
                *self.stack.last_mut().unwrap() = value.clone();
            }
        }
        // Lazy values cannot be stored in native typed arrays
        if var_name.is_some_and(|name| name.starts_with('@'))
            && crate::runtime::native_types::is_native_array_element_type(base_constraint)
            && crate::builtins::methods_0arg::is_value_lazy(&value)
        {
            let declared = format!("array[{}]", base_constraint);
            return Err(RuntimeError::typed(
                "X::Cannot::Lazy",
                [
                    (
                        "message".to_string(),
                        Value::str(format!("Cannot store a lazy list onto a {}", declared)),
                    ),
                    ("action".to_string(), Value::str_from("store")),
                ]
                .into_iter()
                .collect(),
            ));
        }
        // A genuinely-lazy list bound to a *boxed* typed array stays lazy; accept it
        // without eager element checking (its elements are checked on reification).
        if var_name.is_some_and(|name| name.starts_with('@'))
            && matches!(value.view(), ValueView::LazyList(_))
        {
            return Ok(());
        }
        if var_name.is_some_and(|name| name.starts_with('@'))
            && matches!(value.view(), ValueView::Array(..) | ValueView::Seq(_))
        {
            if !self.array_elements_match_constraint(constraint, &value) {
                return Err(self.typed_array_element_error(
                    var_name,
                    base_constraint,
                    constraint,
                    &value,
                ));
            }
            return Ok(());
        }
        // When the constraint is a container type (List, Array, Positional, Seq, Cool, Any, Mu),
        // an Array value directly satisfies it — do NOT descend into element-level matching.
        // Element-level matching is for declarations like `my Int @x = 1, 2, 3`.
        // Also skip element-level matching for subset types whose base type is a container type
        // (e.g., `subset NumArray of Array where { ... }`).
        // Only an `Array` value consults it, and the subset walk allocates, so
        // it is asked lazily rather than for every scalar declaration.
        let is_container_constraint = || {
            matches!(
                declared_constraint,
                "List" | "Array" | "Positional" | "Seq" | "Cool" | "Any" | "Mu" | "Iterable"
            ) || {
                let ultimate_base = self.resolve_subset_base_type(declared_constraint);
                matches!(
                    ultimate_base.as_str(),
                    "List"
                        | "Array"
                        | "Positional"
                        | "Seq"
                        | "Cool"
                        | "Any"
                        | "Mu"
                        | "Iterable"
                        | "Hash"
                        | "Map"
                        | "Pair"
                )
            }
        };
        if let ValueView::Array(..) = value.view()
            && !is_container_constraint()
            // Element-level matching is for `@`-sigil typed arrays (`my Int @a`),
            // whose constraint is the ELEMENT type. A `$`-scalar holding an array
            // (`my Array[Numeric] $x = …`, `my Array[Numeric] constant c .= new`)
            // matches the WHOLE value against the (parameterized) type, not its
            // elements — fall through to the whole-value type check below.
            && !var_name.is_some_and(|n| n.starts_with('$'))
        {
            if !self.array_elements_match_constraint(constraint, &value) {
                return Err(self.typed_array_element_error(
                    var_name,
                    base_constraint,
                    constraint,
                    &value,
                ));
            }
            return Ok(());
        }
        // For finite Range values assigned to typed arrays (my Int @a = ^5),
        // check if the range elements match the element type constraint.
        // Integer ranges always contain Int elements.
        {
            let is_finite_int_range = matches!(
                value.view(),
                ValueView::Range(a, b) | ValueView::RangeExcl(a, b) |
                ValueView::RangeExclStart(a, b) | ValueView::RangeExclBoth(a, b)
                if b != i64::MAX && a != i64::MIN
            );
            let is_finite_generic_range = matches!(
                value.view(),
                ValueView::GenericRange { end, .. }
                if !matches!(end.as_ref().view(), ValueView::Num(n) if n.is_infinite())
                    && !matches!(end.as_ref().view(), ValueView::Whatever | ValueView::HyperWhatever)
            );
            let range_ok = if is_finite_int_range {
                loan_env!(self, type_matches_value(constraint, &Value::int(0)))
            } else if is_finite_generic_range {
                match value.view() {
                    ValueView::GenericRange { start, end, .. } => {
                        self.type_matches_value(constraint, start)
                            && self.type_matches_value(constraint, end)
                    }
                    _ => false,
                }
            } else {
                false
            };
            if range_ok {
                return Ok(());
            }
            // Infinite ranges and non-matching types fall through to normal check
        }
        if value.is_nil() && self.is_definite_constraint(constraint) {
            if has_explicit_initializer {
                // The declaration DID write an initializer expression — it just
                // evaluated to Nil at runtime (`my Int:D $i = f()` where `f`
                // returns `Nil`). That is a genuine failing assignment, not a
                // missing one: rakudo raises X::TypeCheck::Assignment here, the
                // same error a later `$i = Nil` reassignment gets.
                let nominal = loan_env!(self, nominal_type_object_name_for_constraint(constraint));
                let reset_value = Value::package(Symbol::intern(&nominal));
                return Err(runtime::utils::definite_type_check_assignment_error(
                    var_name.unwrap_or("variable"),
                    constraint,
                    &reset_value,
                ));
            }
            // A subset (named or anon-from-`where`) whose base is `:D` does not
            // require an initializer — only an explicit `:D` smiley on the declared
            // type does. Only raise MissingInitializer when one is truly required;
            // otherwise the Nil (type-object) default is allowed at declaration.
            if self.constraint_requires_initializer(constraint) {
                // A `:D` the source did not write came from `use variables :D`
                // (`Compiler::variables_pragma_constraint` added it), and
                // rakudo reports it as `implicit`.
                let implicit = smiley_from_pragma
                    .then(|| crate::runtime::types::strip_type_smiley(constraint).1)
                    .flatten()
                    .map(|smiley| format!("{smiley} by pragma"));
                return Err(RuntimeError::missing_initializer(
                    constraint,
                    "variable",
                    implicit.as_deref(),
                ));
            }
            return Ok(());
        }
        // Native integer type check: validate value is an integer in range.
        // Native types cannot hold Nil/type objects — reject them.
        // A user subset spelled like a native type shadows it (#12359).
        let shadowed_by_subset = self.constraint_is_user_subset(base_constraint);
        if !shadowed_by_subset && crate::runtime::native_types::is_native_int_type(base_constraint)
        {
            if value.is_nil() {
                return Err(RuntimeError::new(format!(
                    "Cannot unbox a type object (Nil) to {}.",
                    base_constraint
                )));
            }
            self.validate_native_int_assignment(base_constraint, &value)?;
            return Ok(());
        }
        // Native num/str types cannot hold type objects — reject Nil and Package values.
        if !shadowed_by_subset && matches!(base_constraint, "num" | "num32" | "num64" | "str") {
            if matches!(value.view(), ValueView::Nil | ValueView::Package(_)) {
                return Err(RuntimeError::new(format!(
                    "Cannot unbox a type object to {}.",
                    base_constraint
                )));
            }
            // A native `num32` scalar narrows to IEEE-754 single precision
            // immediately at the declaration/store, mirroring the native-int
            // branch just above (which mutates the stack value via
            // `validate_native_int_assignment`) — not just later when
            // something happens to coerce it. This is the single place both
            // the statement-form (`my num32 $x = …;`) and expression-context
            // (`f((my num32 $x = …))`, e.g. `nqp::iseq_n($_, (my num32
            // $num32 = $_))`) declarations both compile through, so fixing it
            // here (rather than in the SetLocal/SetGlobal store paths, which
            // only cover the statement form and run too late for the
            // expression form) covers both uniformly. `CBOR::Simple`'s float
            // encoder relies on exactly this: it decides "can this Num
            // shrink to a 4-byte CBOR float?" by writing into a `num32`
            // temporary and checking `$_ == $num32` for a lossless
            // round-trip — without truncation that check was always true, so
            // every double got wrongly encoded as a 4-byte float.
            if base_constraint == "num32"
                && let ValueView::Num(f) = value.view()
            {
                *self.stack.last_mut().unwrap() = Value::num(f as f32 as f64);
            }
            return Ok(());
        }
        // The known-type arm's verdict, reused by the general check below so
        // a known constraint is matched once, not twice (#11467; a subset's
        // `where` block shadowing the name would also have run twice).
        let mut known_matched: Option<bool> = None;
        // Why a subset rejected the value, if its `where` failed by throwing.
        let mut why = None;
        if runtime::is_known_type_constraint(base_constraint) {
            // A `:=` source is a VarRef/container wrapper; the constraint
            // applies to the value it carries (`my Positional[Int] $z := $x`).
            let probe = if bind_mode && value.is_varref() {
                value.unwrap_varref().deref_container()
            } else {
                value.clone()
            };
            let matched =
                !value.is_nil() && self.type_matches_value_why(constraint, &probe, &mut why);
            known_matched = Some(matched);
            if matched {
                self.decl_typechecked_context().set(true);
            }
            if !value.is_nil() && !matched {
                // A subset `where { … or fail "msg" }` that failed by throwing
                // surfaces its own exception (custom message) rather than the
                // generic type-check error.
                if let Some(fail) = why.take() {
                    return Err(*fail);
                }
                if base_constraint == "Int"
                    && matches!(value.view(), ValueView::Num(f) if f.is_nan() || f.is_infinite())
                {
                    let mut attrs = ValueMap::default();
                    attrs.insert("value".to_string(), value.clone());
                    attrs.insert(
                        "vartype".to_string(),
                        Value::package(Symbol::intern(base_constraint)),
                    );
                    let desc = if matches!(value.view(), ValueView::Num(f) if f.is_nan()) {
                        "Cannot convert NaN to Int"
                    } else {
                        "Cannot assign a literal of type Num (Inf) to a variable of type Int"
                    };
                    attrs.insert("message".to_string(), Value::str(desc.to_string()));
                    return Err(RuntimeError::typed("X::Syntax::Number::LiteralType", attrs));
                }
                if bind_mode {
                    return Err(self.type_check_binding_failure(base_constraint, &value));
                }
                let coerced = match base_constraint {
                    "Str" => Some(Value::str(crate::runtime::utils::coerce_to_str(&value))),
                    _ => None,
                };
                if let Some(new_val) = coerced {
                    *self.stack.last_mut().unwrap() = new_val;
                } else {
                    // When assigning an unhandled Failure to a typed variable,
                    // explode the Failure first (Raku behavior)
                    if let Some(err) = self.failure_to_runtime_error_if_unhandled(&value) {
                        return Err(err);
                    }
                    return Err(if let Some(var_name) = var_name {
                        if self.is_definite_constraint(constraint) {
                            crate::runtime::utils::definite_type_check_assignment_error(
                                var_name, constraint, &value,
                            )
                        } else {
                            self.type_check_assignment_failure(var_name, constraint, &value)
                        }
                    } else {
                        self.typecheck_assignment_failure(constraint, &value, None)
                    });
                }
            }
        } else if (!self.has_type(declared_constraint)
            // A module's own type is a declared type only where that module
            // is merged (ADR-11136).
            || self.module_name_hidden_here(declared_constraint))
            && !is_core_raku_type(declared_constraint)
            && !loan_env!(self, has_type_capture_binding(declared_constraint))
            // A role body statement runs under the composing class's package,
            // but its short type names (`my Level $x` with `enum Level` in the
            // class enclosing the role) resolve through the role's own chain.
            && !self.lexicals.nested_capture_owners.last().is_some_and(|owner| {
                let resolved = self
                    .resolve_type_name_for_owner(owner.as_str(), declared_constraint.to_string());
                resolved != declared_constraint && self.has_type_direct(&resolved)
            })
        {
            // Check if this is a suppressed nested class name that can be resolved
            if self.resolve_suppressed_type(declared_constraint).is_none() {
                // A `package`/`module` declared with this name exists but is not
                // type-like enough to constrain a variable (only `class`/`role`/
                // `enum`/`subset` are): throw X::Syntax::Variable::BadType, not the
                // generic "not declared" error.
                if self.is_declared_package(declared_constraint) {
                    let msg = format!(
                        "Package '{}' is insufficiently type-like to qualify a variable.  Did you mean 'class'?",
                        constraint
                    );
                    let mut attrs = ValueMap::default();
                    attrs.insert("type".to_string(), Value::str(constraint.to_string()));
                    attrs.insert("message".to_string(), Value::str(msg));
                    return Err(RuntimeError::typed("X::Syntax::Variable::BadType", attrs));
                }
                // Unknown user-defined type — reject it.
                // In Raku this is a compile-time failure grouped into an
                // X::Comp::Group whose `.sorrows` holds the underlying
                // X::Undeclared (with type "Did you mean" suggestions).
                let suggestions = self.suggest_type_names(constraint);
                let mut undecl_msg = format!("Type '{}' is not declared.", constraint);
                if suggestions.len() == 1 {
                    undecl_msg.push_str(&format!(" Did you mean '{}'?", suggestions[0]));
                } else if suggestions.len() > 1 {
                    let quoted: Vec<String> =
                        suggestions.iter().map(|s| format!("'{}'", s)).collect();
                    undecl_msg.push_str(&format!(
                        " Did you mean any of these: {}?",
                        quoted.join(", ")
                    ));
                }
                let mut undecl_attrs = std::collections::HashMap::new();
                undecl_attrs.insert("what".to_string(), Value::str("type".to_string()));
                undecl_attrs.insert("symbol".to_string(), Value::str(constraint.to_string()));
                undecl_attrs.insert(
                    "suggestions".to_string(),
                    Value::array(suggestions.iter().cloned().map(Value::str).collect()),
                );
                undecl_attrs.insert("message".to_string(), Value::str(undecl_msg.clone()));
                let sorrow = Value::make_instance(
                    crate::symbol::Symbol::intern("X::Undeclared"),
                    undecl_attrs,
                );
                // Group message mirrors Raku: the sorrow message plus "Malformed my".
                let group_msg = format!("{}\nMalformed my", undecl_msg);
                let mut group_attrs = ValueMap::default();
                group_attrs.insert("sorrows".to_string(), Value::array(vec![sorrow]));
                group_attrs.insert("worries".to_string(), Value::array(vec![]));
                group_attrs.insert("panic".to_string(), Value::NIL);
                group_attrs.insert("message".to_string(), Value::str(group_msg));
                return Err(RuntimeError::typed("X::Comp::Group", group_attrs));
            }
        }
        let matched = match known_matched {
            Some(matched) => matched,
            None => !value.is_nil() && self.type_matches_value_why(constraint, &value, &mut why),
        };
        if matched {
            self.decl_typechecked_context().set(true);
        }
        if !value.is_nil() && !matched && !self.is_container_subclass(constraint) {
            // A subset `where { … or fail "msg" }` that failed by throwing surfaces
            // its own exception (custom message), not the generic type-check error.
            if let Some(fail) = why.take() {
                return Err(*fail);
            }
            // A generic type capture (`sub c(::T $x, ...) { my T $zz = ... }`)
            // is checked against the type `::T` was BOUND to, so the error has
            // to name that type too: rakudo says "expected Int but got Str",
            // not "expected T". `resolved_type_capture_name` is the same
            // resolution the check above performs internally, and it returns
            // the constraint unchanged when no capture of that name is bound.
            let reported = self.resolved_type_capture_name(constraint);
            if bind_mode {
                return Err(self.type_check_binding_failure(&reported, &value));
            }
            return Err(self.typecheck_assignment_failure(&reported, &value, var_name));
        }
        if !value.is_nil() {
            let coerced = loan_env!(
                self,
                try_coerce_value_for_constraint(constraint, value.clone())
            )?;
            *self.stack.last_mut().unwrap() = coerced;
        }
        Ok(())
    }

    /// The common declaration check — a `$` scalar typed with a plain,
    /// unshadowed, non-native builtin name (`my Str $c = "a"`) receiving a
    /// value that matches it — answered without the general check's
    /// preamble. Returns `Ok(true)` when it settled the check, `Ok(false)` to
    /// hand it to the general path (which then re-derives everything).
    ///
    /// For such a constraint the general path does exactly this once the value
    /// matches: no alias to resolve (the name is a known type), no element
    /// check (not an `@`/`%` variable), no native-width validation, no
    /// definiteness error (the value is not `Nil`), then the match, the
    /// "already type-checked" mark for the store, and the coercion step. A
    /// value that does not match falls back, so every error is still raised
    /// by the general path. Every `value.view()` of the preamble it skips
    /// costs a refcount round trip on a `Str` (#11467).
    // Cost: O(1) on a memo hit, plus the match itself.
    /// The native counterpart of [`Self::type_check_plain_scalar_fast`]: a
    /// plain `int`/`int64`/`str`/`num`/`num64` scalar declaration whose value
    /// already carries exactly that tag. The general path validates the width
    /// (a `BigInt` or a narrower type could fail) and returns without vouching;
    /// for these constraints and an `Int`/`Str`/`Num` payload there is nothing
    /// to validate, so the check is the tag test (#12151). Anything else,
    /// including every mismatch, falls to the general path and its errors.
    // Cost: O(1) on a memo hit.
    fn type_check_native_scalar_fast(
        &self,
        code: &CompiledCode,
        tc_idx: u32,
        var_name: Option<&str>,
    ) -> bool {
        if !var_name.is_some_and(|n| n.starts_with('$')) {
            return false;
        }
        let constraint = Self::const_str(code, tc_idx);
        let Some(value) = self.stack.last() else {
            return false;
        };
        matches!(
            (constraint, value.view()),
            ("int" | "int64", ValueView::Int(_))
                | ("str", ValueView::Str(_))
                | ("num" | "num64", ValueView::Num(_))
        ) && self.registry().subsets.is_empty()
            && self.type_decl_constraint_is_plain_builtin(code, tc_idx, constraint)
    }

    fn type_check_plain_scalar_fast(
        &mut self,
        code: &CompiledCode,
        tc_idx: u32,
        var_name: Option<&str>,
    ) -> Result<bool, RuntimeError> {
        let constraint = Self::const_str(code, tc_idx);
        if !var_name.is_some_and(|n| n.starts_with('$'))
            || !self.type_decl_constraint_is_plain_builtin(code, tc_idx, constraint)
            || crate::runtime::native_types::is_native_int_type(constraint)
            || matches!(constraint, "num" | "num32" | "num64" | "str")
        {
            return Ok(false);
        }
        let Some(value) = self.stack.last() else {
            return Ok(false);
        };
        if value.is_nil() {
            return Ok(false);
        }
        let value = value.clone();
        if !self.type_matches_value(constraint, &value) {
            return Ok(false);
        }
        self.decl_typechecked_context().set(true);
        let coerced = loan_env!(self, try_coerce_value_for_constraint(constraint, value))?;
        *self.stack.last_mut().unwrap() = coerced;
        Ok(true)
    }

    pub(super) fn exec_indirect_type_lookup_op(&mut self) {
        let name_val = self.stack.pop().unwrap_or(Value::NIL);
        let name = name_val.to_string_value();
        self.stack
            .push(loan_env!(self, resolve_indirect_type_name(&name)));
    }

    /// A qualified constraint written relative to the enclosing package
    /// (`my Globber::Match $m` inside `class IO::Glob`, where the type is
    /// `IO::Glob::Globber::Match`) that does not name a type on its own.
    /// Unqualified names resolve elsewhere; this only fills the gap for a
    /// relative qualified one.
    // Cost: O(d), d = package nesting depth (memoized ancestor walk).
    fn package_relative_type(&self, base: &str) -> Option<String> {
        if !crate::qualified::is_qualified(crate::symbol::Symbol::intern(base))
            || self.has_type(base)
        {
            return None;
        }
        self.resolve_type_in_current_package(base)
            .filter(|resolved| resolved != base && self.has_type(resolved))
    }
}
