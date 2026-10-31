//! The bodies of `Code`'s and `Routine`'s rows `assuming`, `rw`, `prec`,
//! `precedence`, `associative` and `is-implementation-detail` (ADR-11276
//! §9.166): one per code-object kind, a `&name` handle and a `Sub`.

use super::*;
use crate::symbol::Symbol;
use crate::value::ValueMap;
use super::methods_sub::prec_field_of;

impl Interpreter {
    /// `assuming` of a `&name` handle: a `Sub` that re-dispatches by name.
    // Cost: O(a), a = assumed arguments.
    pub(crate) fn routine_handle_assuming(
        &mut self,
        target: &Value,
        name: &str,
        package: &str,
        method: &str,
        args: Vec<Value>,
        is_regex: bool,
    ) -> Option<Result<Value, RuntimeError>> {
        let _ = (target, name, package, &args, is_regex);
        if method == "assuming" {
            // Create a wrapper Sub that delegates to the multi-dispatch routine
            let mut sub_data = crate::value::SubData {
                fixtures: None,
                package: Symbol::intern(package),
                name: Symbol::intern(name),
                params: crate::value::empty_params(),
                param_defs: crate::value::empty_param_defs(),
                body: std::sync::Arc::new(vec![]),
                is_rw: false,
                is_raw: false,
                env: self.env().clone(),
                assumed_positional: Vec::new(),
                assumed_named: ValueMap::default(),
                id: crate::value::next_instance_id(),
                is_direct_code: false,
                empty_sig: false,
                is_bare_block: false,
                compiled_code: None,
                compiled_fns: None,
                compiled_routine: None,
                is_decl_expr_thunk: false,
                source_line: None,
                source_file: None,
                owned_captures: Vec::new(),
                authoritative_captures: Vec::new(),
                own_cell_captures: Vec::new(),
                upvalues: Vec::new(),
                captured_fatal_mode: false,
                param_name_syms_cache: std::sync::OnceLock::new(),
                source_file_sym_cache: std::sync::OnceLock::new(),
                _state_scope_guard: None,
                captured_readonly: None,
                routine_cell: Default::default(),
            };
            // Store the routine name so call_sub_value can dispatch
            sub_data.env.insert(
                "__mutsu_routine_name".to_string(),
                Value::str(name.to_string()),
            );
            // Apply assumed args
            for arg in &args {
                if let ValueView::Pair(key, boxed) = arg.view() {
                    if key == "__mutsu_test_callsite_line" {
                        continue;
                    }
                    sub_data.assumed_named.insert(key.clone(), boxed.clone());
                } else {
                    sub_data.assumed_positional.push(arg.clone());
                }
            }
            return Some(Ok(Value::sub_value(crate::gc::Gc::new(sub_data))));
        }
        None
    }

    /// `assuming` of a regex value: a `Sub` named `assumed.` that calls the
    /// regex with the assumed arguments (it has no routine name to
    /// re-dispatch by, so the regex rides as the callee marker).
    // Cost: O(a), a = assumed arguments.
    pub(crate) fn regex_assuming(
        &mut self,
        target: &Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let wrapper = self
            .routine_handle_assuming(target, "assumed.", "GLOBAL", "assuming", args, true)
            .expect("`assuming` always builds a wrapper")?;
        let ValueView::Sub(data) = wrapper.view() else {
            unreachable!("assuming wrapper must be a Sub");
        };
        let mut data = (**data).clone();
        data.env.remove("__mutsu_routine_name");
        data.env
            .insert_sym(crate::symbol::wk::assuming_callee_marker(), target.clone());
        Ok(Value::sub_value(crate::gc::Gc::new(data)))
    }

    /// `assuming` of a `Sub`: a primed copy, or a `Failure` when the assumed
    /// arguments cannot bind.
    // Cost: O(a + p), a = assumed arguments, p = parameters.
    pub(crate) fn sub_assuming(
        &mut self,
        target: &Value,
        data: &crate::value::SubData,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        let _ = (target, data, &args);
        if method == "assuming" {
            let mut next = (*data).clone();
            let make_failure =
                |sub_data: &crate::value::SubData, expected: Value, symbol: String| {
                    let mut ex_attrs = std::collections::HashMap::new();
                    ex_attrs.insert("expected".to_string(), expected.clone());
                    ex_attrs.insert("symbol".to_string(), Value::str(symbol));
                    ex_attrs.insert(
                        "payload".to_string(),
                        Value::str(expected.to_string_value()),
                    );
                    let exception =
                        Value::make_instance(Symbol::intern("X::TypeCheck::Binding"), ex_attrs);
                    let mut failure_attrs = std::collections::HashMap::new();
                    failure_attrs.insert("exception".to_string(), exception);
                    failure_attrs.insert("handled".to_string(), Value::FALSE);
                    let failure = Value::make_instance(Symbol::intern("Failure"), failure_attrs);
                    let mut mixins = ValueMap::default();
                    mixins.insert("Failure".to_string(), failure);
                    Value::mixin(
                        Value::sub_value(crate::gc::Gc::new(sub_data.clone())),
                        mixins,
                    )
                };
            let mut incoming_named = std::collections::HashMap::new();
            for arg in args {
                let pair = if let ValueView::Pair(key, boxed) = arg.view() {
                    Some((key.clone(), boxed.clone()))
                } else {
                    None
                };
                if let Some((key, value)) = pair {
                    if key == "__mutsu_test_callsite_line" {
                        continue;
                    }
                    incoming_named.insert(key.clone(), value.clone());
                    next.assumed_named.insert(key, value);
                } else {
                    next.assumed_positional.push(arg);
                }
            }
            if next.param_defs.is_empty() && !incoming_named.is_empty() {
                return Some(Ok(make_failure(
                    &next,
                    Value::str_from("Unexpected"),
                    String::new(),
                )));
            }
            let mut known_named: std::collections::HashSet<String> =
                std::collections::HashSet::new();
            Self::collect_named_param_keys(&next.param_defs, &mut known_named);
            let allows_extra_named = Self::has_named_slurpy_param(&next.param_defs);
            for name in incoming_named.keys() {
                if !allows_extra_named && !known_named.contains(name) {
                    return Some(Ok(make_failure(
                        &next,
                        Value::str_from("Unexpected"),
                        String::new(),
                    )));
                }
            }
            for pd in next.param_defs.iter().filter(|pd| pd.named) {
                if let Some(value) = incoming_named.get(&pd.name)
                    && let Some(constraint) = &pd.type_constraint
                    && !self.type_matches_value(constraint, value)
                {
                    return Some(Ok(make_failure(
                        &next,
                        Value::package(Symbol::intern("X::TypeCheck::Assignment")),
                        format!("${}", pd.name),
                    )));
                }
            }
            // Check positional type constraints (only for $-sigil params,
            // not @- or %-sigil where the constraint applies to elements)
            {
                let is_placeholder = |v: &Value| {
                    matches!(v.view(), ValueView::Whatever)
                        || matches!(v.view(), ValueView::Num(f) if f.is_infinite())
                        || matches!(v.view(), ValueView::Rat(_, 0))
                };
                // Type captures (`::T $a`) are declared by one parameter and
                // referenced by later ones as a bare `T`, which is not a real
                // type. Rakudo defers checking such capture-constrained params at
                // priming time, so collect the declared capture names and skip
                // them below (checking against the literal "T" would spuriously
                // fail the bind).
                let capture_decls: std::collections::HashSet<&str> = next
                    .param_defs
                    .iter()
                    .filter_map(|pd| pd.captured_type_name())
                    .collect();
                for (pos_idx, pd) in next
                    .param_defs
                    .iter()
                    .filter(|pd| !pd.named && !pd.slurpy)
                    .enumerate()
                {
                    if pos_idx >= next.assumed_positional.len() {
                        break;
                    }
                    let assumed = &next.assumed_positional[pos_idx];
                    if is_placeholder(assumed) || assumed.is_nil() {
                        continue;
                    }
                    // Skip type check for @-sigil and %-sigil params where the
                    // constraint applies to container elements, not the whole value.
                    if pd.name.starts_with('@') || pd.name.starts_with('%') {
                        continue;
                    }
                    if let Some(constraint) = &pd.type_constraint
                        && !crate::qualified::is_type_capture(constraint)
                        && !capture_decls.contains(constraint.as_str())
                        && !self.type_matches_value(constraint, assumed)
                    {
                        // Add $ sigil to the symbol name if it's bare
                        let symbol = if pd.name.starts_with('$')
                            || pd.name.starts_with('@')
                            || pd.name.starts_with('%')
                        {
                            pd.name.clone()
                        } else {
                            format!("${}", pd.name)
                        };
                        return Some(Ok(make_failure(
                            &next,
                            Value::package(Symbol::intern(constraint)),
                            symbol,
                        )));
                    }
                }
                // Check too many positionals (skip if there's a slurpy or capture param)
                // Also skip for multi-dispatch wrapper Subs where actual arity
                // depends on which candidate is selected at call time.
                let is_multi_dispatch = next.env.contains_key("__mutsu_multi_dispatch_name")
                    || next.env.contains_key("__mutsu_multi_dispatch_candidates")
                    || next.env.contains_key("__mutsu_routine_name");
                let has_slurpy = next.param_defs.iter().any(|pd| {
                    pd.slurpy
                        || pd.double_slurpy
                        || pd.onearg
                        || pd.sub_signature.is_some()
                        || pd.outer_sub_signature.is_some()
                });
                let positional_param_count = next
                    .param_defs
                    .iter()
                    .filter(|pd| !pd.named && !pd.slurpy)
                    .count();
                let non_placeholder_count = next
                    .assumed_positional
                    .iter()
                    .filter(|v| !is_placeholder(v))
                    .count();
                if !is_multi_dispatch
                    && !has_slurpy
                    && non_placeholder_count > positional_param_count
                {
                    let mut ex_attrs = std::collections::HashMap::new();
                    ex_attrs.insert(
                        "payload".to_string(),
                        Value::str("Too many positionals passed".to_string()),
                    );
                    let exception = Value::make_instance(Symbol::intern("X::AdHoc"), ex_attrs);
                    let mut failure_attrs = std::collections::HashMap::new();
                    failure_attrs.insert("exception".to_string(), exception);
                    failure_attrs.insert("handled".to_string(), Value::FALSE);
                    let failure = Value::make_instance(Symbol::intern("Failure"), failure_attrs);
                    let mut mixins = ValueMap::default();
                    mixins.insert("Failure".to_string(), failure);
                    return Some(Ok(Value::mixin(
                        Value::sub_value(crate::gc::Gc::new(next)),
                        mixins,
                    )));
                }
            }
            return Some(Ok(Value::sub_value(crate::gc::Gc::new(next))));
        }
        None
    }

    /// `rw` (and mutsu's `readonly`) of a `&name` handle.
    // Cost: O(a), a = attributes of the class.
    pub(crate) fn routine_handle_rw(
        &mut self,
        target: &Value,
        name: &str,
        package: &str,
        method: &str,
        args: Vec<Value>,
        is_regex: bool,
    ) -> Option<Result<Value, RuntimeError>> {
        let _ = (target, name, package, &args, is_regex);
        if matches!(method, "rw" | "readonly") && args.is_empty() {
            // A `Routine` handle reached through `.^can` on an auto-generated
            // attribute accessor (`has $.x is rw`) carries no `SubData` (see
            // `collect_can_methods`'s `Value::routine_parts` construction), so
            // `dispatch_sub_method`'s `data.is_rw` never runs for it and this
            // fell through to "No such method" (Object::Permission's
            // `trait_mod:<is>` calls `$meth.rw` on exactly this shape). Look
            // the accessor's own `is rw` up from the class attribute table
            // instead; a name with no matching public attribute (an ordinary
            // method reached this way, e.g. via the Mixin/RakuAst branches of
            // `collect_can_methods`) defaults to not-rw, matching Raku's
            // default for a method with no `is rw`.
            let is_rw = self
                .collect_class_attributes(package)
                .into_iter()
                .find(|attr| attr.is_public && attr.name == name)
                .is_some_and(|attr| attr.is_rw);
            return Some(Ok(Value::truth(if method == "rw" {
                is_rw
            } else {
                !is_rw
            })));
        }
        None
    }

    /// `rw` of a `Sub`.
    // Cost: O(1).
    pub(crate) fn sub_rw(
        &mut self,
        target: &Value,
        data: &crate::value::SubData,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        let _ = (target, data, &args);
        if method == "rw" && args.is_empty() {
            return Some(Ok(Value::truth(data.is_rw)));
        }
        None
    }

    /// `prec`, `precedence`, `associative` and `is-implementation-detail` of a
    /// `&name` handle.
    // Cost: O(1), registry lookups.
    pub(crate) fn routine_handle_prec(
        &mut self,
        target: &Value,
        name: &str,
        package: &str,
        method: &str,
        args: Vec<Value>,
        is_regex: bool,
    ) -> Option<Result<Value, RuntimeError>> {
        let _ = (target, name, package, &args, is_regex);
        // `is implementation-detail` -- see the matching arm in
        // `dispatch_sub_method` below (a bare `&name` reference reaches this
        // function instead, as a `ValueView::Routine`, so both need the arm).
        if method == "prec" && args.is_empty() {
            return Some(Ok(
                self.routine_prec(Symbol::intern(package), Symbol::intern(name), None)
            ));
        }
        if let Some(field) = prec_field_of(method).filter(|_| args.is_empty()) {
            return Some(Ok(self.routine_prec_field(
                Symbol::intern(package),
                Symbol::intern(name),
                None,
                field,
            )));
        }
        if method == "is-implementation-detail" && args.is_empty() {
            let key = crate::qualified::qualified(Symbol::intern(package), Symbol::intern(name));
            let is_impl_detail = self
                .registry()
                .functions
                .get(&key)
                .is_some_and(|def| def.is_implementation_detail);
            return Some(Ok(Value::truth(is_impl_detail)));
        }
        None
    }

    /// `prec`, `precedence`, `associative` and `is-implementation-detail` of a
    /// `Sub`.
    // Cost: O(1), registry lookups.
    pub(crate) fn sub_prec(
        &mut self,
        target: &Value,
        data: &crate::value::SubData,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        let _ = (target, data, &args);
        // `is implementation-detail` -- read back from the registered
        // `FunctionDef` (see `registration_sub.rs`'s `is_implementation_detail:
        // custom_traits.iter().any(...)`). A builtin like `&say` has no
        // registry entry under its own name and answers `False`, matching
        // real Raku rather than raising "No such method".
        if method == "prec" && args.is_empty() {
            return Some(Ok(self.routine_prec(data.package, data.name, Some(&data.routine_cell))));
        }
        if let Some(field) = prec_field_of(method).filter(|_| args.is_empty()) {
            return Some(Ok(self.routine_prec_field(
                data.package,
                data.name,
                Some(&data.routine_cell),
                field,
            )));
        }
        if method == "is-implementation-detail" && args.is_empty() {
            let key = crate::qualified::qualified(data.package, data.name);
            let is_impl_detail = self
                .registry()
                .functions
                .get(&key)
                .is_some_and(|def| def.is_implementation_detail);
            return Some(Ok(Value::truth(is_impl_detail)));
        }
        None
    }


    /// `Routine.yada`: whether the routine's body is nothing but a yada stub
    /// (`...`/`!!!`/`???`, a lone `__mutsu_stub_die`/`__mutsu_stub_warn` call).
    /// A body-less routine `Sub` (plan-derived, ADR-0019 C6e-3) and a `&name`
    /// handle answer from the registered def's `is_stub` fact; anything still
    /// carrying a body keeps the exact AST judgment. `None` for another value.
    // Cost: O(1), one registry lookup or a look at the body's one statement.
    pub(crate) fn code_yada(&mut self, target: &Value) -> Option<Value> {
        let data = match target.view() {
            ValueView::Sub(data) => data.clone(),
            ValueView::WeakSub(weak) => weak.upgrade()?,
            ValueView::Routine { package, name, .. } => {
                let key = crate::qualified::qualified(package, name);
                let is_stub = self.registry().functions.get(&key).is_some_and(|def| def.is_stub);
                return Some(Value::truth(is_stub));
            }
            _ => return None,
        };
        if data.body.is_empty() && !data.name.is_empty() {
            let key = crate::qualified::qualified_text(&data.package, &data.name)
                .as_str()
                .to_string();
            if let Some(def) = self.registry().functions.get(&Symbol::intern(&key)) {
                return Some(Value::truth(def.is_stub));
            }
        }
        Some(Value::truth(Self::sub_body_is_yada_stub(&data.body)))
    }
}
