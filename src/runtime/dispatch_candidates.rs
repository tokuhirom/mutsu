use super::*;
use crate::runtime::types::is_coercion_constraint;

impl Interpreter {
    fn ambiguous_multi_dispatch_error(
        &self,
        name: &str,
        args: &[Value],
        defs: &[Arc<FunctionDef>],
    ) -> RuntimeError {
        let arg_types = args
            .iter()
            .filter(|v| !matches!(v, Value::Pair(..)))
            .map(super::value_type_name)
            .collect::<Vec<_>>()
            .join(", ");
        let mut err = RuntimeError::new(format!("Ambiguous call to '{}({})'", name, arg_types));
        let mut attrs = std::collections::HashMap::new();
        attrs.insert(
            "message".to_string(),
            Value::str(format!(
                "Ambiguous call to {}({}); these signatures all match: {}",
                name,
                arg_types,
                defs.iter()
                    .map(|def| {
                        crate::value::signature::make_signature_value(
                            crate::value::signature::param_defs_to_sig_info(
                                &def.param_defs,
                                def.return_type.clone(),
                            ),
                        )
                        .to_string_value()
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            )),
        );
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::Multi::Ambiguous"),
            attrs,
        )));
        err
    }

    pub(super) fn choose_best_matching_candidate(
        &mut self,
        name: &str,
        args: &[Value],
        candidates: Vec<(String, Arc<FunctionDef>)>,
    ) -> Option<Arc<FunctionDef>> {
        let mut matches = Vec::new();
        let mut seen = std::collections::HashSet::new();
        for (_, def) in candidates {
            // For auto-param subs ($^a, $^b) with empty param_defs but
            // non-empty params, check arity against params.len() since
            // args_match_param_types cannot handle this case.
            let type_ok = if def.param_defs.is_empty()
                && !def.params.is_empty()
                && def.params.iter().all(|p| p.starts_with('^'))
            {
                let positional_arg_count = args
                    .iter()
                    .filter(|arg| !matches!(arg, Value::Pair(..) | Value::ValuePair(..)))
                    .count();
                positional_arg_count == def.params.len()
            } else {
                self.args_match_param_types(args, &def.param_defs)
            };
            if type_ok {
                let fingerprint =
                    crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body);
                if !seen.insert(fingerprint) {
                    continue;
                }
                matches.push(def);
            }
        }
        if matches.len() <= 1 {
            return matches.into_iter().next();
        }

        // Sort matches by specificity rank (primary, DESC), type hierarchy
        // distance (secondary, ASC), then required named count (tertiary, DESC)
        // so that subset types win over plain types, more specific types
        // (e.g. Str:D) beat less specific ones (e.g. Any), and among
        // equally-specific candidates, those with required named params win.
        {
            let mut ranked: Vec<(usize, _)> = matches
                .iter()
                .enumerate()
                .map(|(i, def)| {
                    let rank = self.candidate_specificity_rank(def);
                    let dist = self.candidate_type_distance(args, def);
                    let req_named = Self::candidate_required_named_count(def);
                    (i, (rank, dist, req_named))
                })
                .collect();
            ranked.sort_by(|a, b| {
                // Higher rank first, then lower distance, then higher required named
                b.1.0
                    .cmp(&a.1.0)
                    .then(a.1.1.cmp(&b.1.1))
                    .then(b.1.2.cmp(&a.1.2))
            });
            let sorted_matches: Vec<Arc<FunctionDef>> =
                ranked.iter().map(|(i, _)| matches[*i].clone()).collect();
            matches = sorted_matches;
        }

        let best_rank = self.candidate_specificity_rank(&matches[0]);
        let best_shape = self.candidate_dispatch_shape(&matches[0]);
        let best_distance = self.candidate_type_distance(args, &matches[0]);
        let best_req_named = Self::candidate_required_named_count(&matches[0]);
        let tied: Vec<Arc<FunctionDef>> = matches
            .iter()
            .filter(|def| {
                self.candidate_specificity_rank(def) == best_rank
                    && self.candidate_type_distance(args, def) == best_distance
                    && Self::candidate_required_named_count(def) == best_req_named
            })
            .cloned()
            .collect();
        // Compare dispatch shapes as sorted multisets so that candidates with
        // the same set of typed parameters in a different order are still
        // detected as tied (e.g. `foo(S $a, T $b)` vs `foo(T $a, S $b)`).
        let mut best_shape_sorted = best_shape.clone();
        best_shape_sorted.sort();
        if tied.len() > 1
            && tied
                .iter()
                .all(|def| !self.candidate_uses_order_sensitive_dispatch(def))
            && tied.iter().all(|def| {
                let mut shape = self.candidate_dispatch_shape(def);
                shape.sort();
                shape == best_shape_sorted
            })
        {
            // `is default` trait tie-breaking: if exactly one tied candidate
            // has `is_default`, prefer it over the others.
            let default_candidates: Vec<&Arc<FunctionDef>> =
                tied.iter().filter(|def| def.is_default).collect();
            if default_candidates.len() == 1 {
                return Some(default_candidates[0].clone());
            }
            self.pending_dispatch_error =
                Some(self.ambiguous_multi_dispatch_error(name, args, &tied));
            return None;
        }

        Some(matches.remove(0))
    }

    pub(super) fn candidate_specificity_rank(
        &self,
        def: &FunctionDef,
    ) -> (usize, usize, usize, usize, usize, usize, usize) {
        let params = Self::dispatch_visible_params(def);
        let literal_value_count = params.iter().filter(|p| p.literal_value.is_some()).count();
        let where_count = params
            .iter()
            .filter(|p| p.where_constraint.is_some())
            .count();
        let subset_type_count = params
            .iter()
            .filter(|p| {
                p.type_constraint
                    .as_deref()
                    .map(Self::constraint_base_name)
                    .map(|base| self.registry().subsets.contains_key(base))
                    .unwrap_or(false)
            })
            .count();
        let typed_param_count = params
            .iter()
            .filter(|p| {
                p.type_constraint.is_some()
                    || (!p.slurpy
                        && (p.name.starts_with('@')
                            || p.name.starts_with('%')
                            || p.name.starts_with('&')))
            })
            .count();
        let subsig_count = params.iter().filter(|p| p.sub_signature.is_some()).count();
        let named_count = params.iter().filter(|p| p.named).count();
        let trait_count = params
            .iter()
            .filter(|p| {
                p.traits
                    .iter()
                    .any(|t| matches!(t.as_str(), "rw" | "raw" | "copy"))
            })
            .count();
        (
            literal_value_count,
            where_count,
            subset_type_count,
            typed_param_count,
            subsig_count,
            named_count,
            trait_count,
        )
    }

    /// Count required named parameters — used as a tertiary tiebreaker
    /// AFTER rank and type distance, so it only matters when type constraints
    /// are equally specific.
    fn candidate_required_named_count(def: &FunctionDef) -> usize {
        Self::dispatch_visible_params(def)
            .iter()
            .filter(|p| p.named && p.required)
            .count()
    }

    /// Compute the total type hierarchy distance between a candidate's type
    /// constraints and the actual argument types.  Lower means more specific.
    /// Each parameter contributes the number of MRO levels between the
    /// constraint and the actual type; unconstrained parameters contribute a
    /// large constant so that constrained candidates are always preferred.
    fn candidate_type_distance(&self, args: &[Value], def: &FunctionDef) -> usize {
        let mut total = 0usize;
        let params: Vec<&ParamDef> = Self::dispatch_visible_params(def);
        let mut pos_idx = 0usize;
        for pd in params.iter() {
            if let Some(constraint) = &pd.type_constraint {
                let base = Self::constraint_base_name(constraint);
                // For named params, find the matching Pair arg
                if pd.named {
                    let bare_name = pd.name.trim_start_matches(|c: char| "$@%&".contains(c));
                    let named_val = args.iter().find_map(|a| {
                        if let Value::Pair(key, val) = a {
                            if key == bare_name {
                                Some(val.as_ref().clone())
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    });
                    if let Some(val) = named_val {
                        total += self.type_hierarchy_distance_with_var_type(base, &val, None);
                    } else {
                        total += 1000;
                    }
                    continue;
                }
                if pos_idx < args.len() {
                    // Skip Pair args when looking for positional args
                    while pos_idx < args.len() && matches!(&args[pos_idx], Value::Pair(..)) {
                        pos_idx += 1;
                    }
                    if pos_idx >= args.len() {
                        total += 1000;
                        continue;
                    }
                    // Unwrap VarRef Capture wrappers and check the source
                    // variable's declared type constraint for native type dispatch.
                    let (mut arg, var_type) = self.unwrap_varref_for_dispatch(&args[pos_idx]);
                    pos_idx += 1;
                    if pd.name.starts_with('&')
                        && let Some(return_type) = self.callable_return_type(&arg)
                    {
                        total += self.type_hierarchy_distance(
                            base,
                            &Value::Package(Symbol::intern(&return_type)),
                        );
                        continue;
                    }
                    if is_coercion_constraint(constraint)
                        && let Value::Str(s) = &arg
                        && matches!(base, "Int" | "Num" | "Rat" | "Complex" | "Numeric" | "Real")
                        && let Some(parsed) =
                            crate::runtime::str_numeric::parse_raku_str_to_numeric(s)
                    {
                        arg = parsed;
                    }
                    if (pd.name.starts_with('@') || pd.name.starts_with('%'))
                        && let Some(info) = self.container_type_metadata(&arg)
                        && !info.value_type.is_empty()
                    {
                        total += self.type_hierarchy_distance(
                            base,
                            &Value::Package(Symbol::intern(&info.value_type)),
                        );
                        continue;
                    }
                    if (pd.name.starts_with('@') || pd.name.starts_with('%'))
                        && let Some(var_type) = var_type.as_deref()
                    {
                        total += self.type_hierarchy_distance(
                            base,
                            &Value::Package(Symbol::intern(var_type)),
                        );
                        continue;
                    }
                    total +=
                        self.type_hierarchy_distance_with_var_type(base, &arg, var_type.as_deref());
                }
            } else {
                total += 1000;
                if !pd.named {
                    pos_idx += 1;
                }
            }
        }
        total
    }

    /// Unwrap a VarRef Capture wrapper to get the inner value and the source
    /// variable's declared type constraint (if any) for dispatch.
    fn unwrap_varref_for_dispatch(&self, value: &Value) -> (Value, Option<String>) {
        if let Value::Capture { positional, named } = value
            && positional.is_empty()
            && let Some(Value::Str(var_name)) = named.get("__mutsu_varref_name")
            && let Some(inner) = named.get("__mutsu_varref_value")
        {
            let var_type = self.var_type_constraint_fast(var_name).cloned();
            (inner.clone(), var_type)
        } else {
            (value.clone(), None)
        }
    }

    /// Return how many MRO levels separate `constraint` from the actual type
    /// of `value`.  0 means exact match; larger means less specific.
    fn type_hierarchy_distance(&self, constraint: &str, value: &Value) -> usize {
        let value_type = super::value_type_name(value);
        // Strip :D/:U smiley
        let base = if constraint.ends_with(":D") || constraint.ends_with(":U") {
            &constraint[..constraint.len() - 2]
        } else {
            constraint
        };
        if base == "Inf" {
            return match value {
                Value::Num(n) if n.is_infinite() && n.is_sign_positive() => 0,
                _ => 500,
            };
        }
        if base == "NaN" {
            return match value {
                Value::Num(n) if n.is_nan() => 0,
                _ => 500,
            };
        }
        if base == value_type {
            return 0;
        }
        // For instances, use the class MRO
        if let Value::Instance { class_name, .. } = value {
            let cn = class_name.resolve();
            if base == cn.as_str() {
                return 0;
            }
            // Use a non-mutable copy of the MRO (classes field lookup)
            if let Some(class_def) = self.registry().classes.get(cn.as_str()) {
                for (i, ancestor) in class_def.mro.iter().enumerate() {
                    if ancestor == base {
                        return i;
                    }
                }
            }
            // Check composed roles
            if let Some(roles) = self.registry().class_composed_roles.get(cn.as_str())
                && roles.iter().any(|r| r == base)
            {
                return 1;
            }
        }
        if let Value::Package(name) = value {
            let cn = name.resolve();
            if base == cn.as_str() {
                return 0;
            }
            if let Some(class_def) = self.registry().classes.get(cn.as_str()) {
                for (i, ancestor) in class_def.mro.iter().enumerate() {
                    if ancestor == base {
                        return i;
                    }
                }
            }
            if let Some(roles) = self.registry().class_composed_roles.get(cn.as_str())
                && roles.iter().any(|r| r == base)
            {
                return 1;
            }
        }
        // Built-in type hierarchy (approximation of Raku MRO depths)
        // Bool -> Int -> Cool -> Any -> Mu
        // but also Bool -> Int -> Numeric/Real -> ...
        let builtin_mro: &[&str] = match value_type {
            "Bool" => &["Bool", "Int", "Numeric", "Real", "Cool", "Any", "Mu"],
            "Int" => &["Int", "Numeric", "Real", "Cool", "Any", "Mu"],
            "Num" => &["Num", "Numeric", "Real", "Cool", "Any", "Mu"],
            "Rat" | "FatRat" => &["Rat", "Numeric", "Real", "Cool", "Any", "Mu"],
            "Complex" => &["Complex", "Numeric", "Cool", "Any", "Mu"],
            "Str" => &["Str", "Stringy", "Cool", "Any", "Mu"],
            "Array" => &["Array", "List", "Positional", "Cool", "Any", "Mu"],
            "List" => &["List", "Positional", "Cool", "Any", "Mu"],
            "Hash" => &["Hash", "Map", "Associative", "Cool", "Any", "Mu"],
            "Pair" => &["Pair", "Associative", "Cool", "Any", "Mu"],
            "Range" => &["Range", "Positional", "Cool", "Any", "Mu"],
            "Set" => &["Set", "Setty", "QuantHash", "Associative", "Any", "Mu"],
            "Bag" => &["Bag", "Baggy", "QuantHash", "Associative", "Any", "Mu"],
            "Mix" => &[
                "Mix",
                "Mixy",
                "Baggy",
                "QuantHash",
                "Associative",
                "Any",
                "Mu",
            ],
            "Sub" => &["Sub", "Routine", "Block", "Code", "Callable", "Any", "Mu"],
            "Seq" => &["Seq", "Positional", "Cool", "Any", "Mu"],
            "Regex" => &[
                "Regex", "Method", "Routine", "Block", "Code", "Callable", "Any", "Mu",
            ],
            "Junction" => &["Junction", "Mu"],
            _ => &[],
        };
        for (i, &ancestor) in builtin_mro.iter().enumerate() {
            if ancestor == base {
                return i;
            }
        }
        // Not found in hierarchy; return a large distance
        500
    }

    /// Like `type_hierarchy_distance`, but also considers the source variable's
    /// declared type constraint for native type dispatch.
    /// When a variable declared as `int` is passed to a multi candidate expecting
    /// `int`, that should be distance 0 (exact native match). The boxed `Int`
    /// candidate should be distance 1 (autoboxing penalty).
    fn type_hierarchy_distance_with_var_type(
        &self,
        constraint: &str,
        value: &Value,
        var_type: Option<&str>,
    ) -> usize {
        let base = if constraint.ends_with(":D") || constraint.ends_with(":U") {
            &constraint[..constraint.len() - 2]
        } else {
            constraint
        };
        // If the source variable has a native type constraint, use it for dispatch.
        if let Some(vt) = var_type
            && Self::is_native_type_name(vt)
        {
            if base == vt {
                // Exact native type match (e.g., int var → int param)
                return 0;
            }
            // Native type matches its boxed equivalent with a penalty
            // (e.g., int var → Int param) so the native candidate wins.
            let boxed = Self::native_to_boxed(vt);
            if base == boxed {
                return 1;
            }
        }
        self.type_hierarchy_distance(constraint, value)
    }

    /// Check if a type name is a native type.
    fn is_native_type_name(name: &str) -> bool {
        matches!(
            name,
            "int"
                | "int8"
                | "int16"
                | "int32"
                | "int64"
                | "uint"
                | "uint8"
                | "uint16"
                | "uint32"
                | "uint64"
                | "byte"
                | "num"
                | "num32"
                | "num64"
                | "str"
        )
    }

    /// Map native type names to their boxed equivalents.
    fn native_to_boxed(native: &str) -> &'static str {
        match native {
            "int" | "int8" | "int16" | "int32" | "int64" | "uint" | "uint8" | "uint16"
            | "uint32" | "uint64" | "byte" => "Int",
            "num" | "num32" | "num64" => "Num",
            "str" => "Str",
            _ => "Any",
        }
    }
}
