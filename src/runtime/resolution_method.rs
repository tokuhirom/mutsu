use super::*;

impl Interpreter {
    pub(super) fn resolve_method(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
    ) -> Option<MethodDef> {
        self.resolve_method_with_owner(class_name, method_name, arg_values)
            .map(|(_, def)| def)
    }

    pub(crate) fn method_args_match_for_invocant(
        &mut self,
        class_name: &str,
        def: &MethodDef,
        arg_values: &[Value],
        role_bindings: Option<&HashMap<String, Value>>,
        invocant: Option<&Value>,
    ) -> bool {
        let saved_env = self.env.clone();
        if let Some(bindings) = role_bindings {
            for (name, value) in bindings {
                self.env.insert(name.clone(), value.clone());
            }
        }
        for pd in &def.param_defs {
            if !(pd.is_invocant || pd.traits.iter().any(|t| t == "invocant")) {
                continue;
            }
            if let Some(constraint) = pd.type_constraint.as_deref() {
                if let Some(captured_name) = constraint.strip_prefix("::") {
                    self.bind_type_capture(
                        captured_name,
                        &Value::Package(Symbol::intern(class_name)),
                    );
                }
                // Check type constraint on the invocant (including :U/:D smileys).
                // Resolve ::?CLASS to the actual class name for matching.
                // Pure type captures (e.g. ::T) don't constrain the invocant.
                if let Some(inv) = invocant {
                    let resolved = constraint.replace("::?CLASS", class_name);
                    let is_type_capture = resolved.starts_with("::");
                    if !is_type_capture && !self.type_matches_value(&resolved, inv) {
                        self.env = saved_env;
                        return false;
                    }
                }
            }
        }
        let args_match = self.method_args_match(arg_values, &def.param_defs);
        self.env = saved_env;
        args_match
    }

    pub(crate) fn resolve_method_with_owner(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
    ) -> Option<(String, MethodDef)> {
        self.resolve_method_with_owner_impl(class_name, method_name, arg_values, None)
    }

    pub(crate) fn resolve_method_with_owner_invocant(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
        invocant: &Value,
    ) -> Option<(String, MethodDef)> {
        self.resolve_method_with_owner_impl(class_name, method_name, arg_values, Some(invocant))
    }

    fn resolve_method_with_owner_impl(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
        invocant: Option<&Value>,
    ) -> Option<(String, MethodDef)> {
        self.dispatch_ambiguous = false;
        let role_bindings = self.registry().get_role_param_bindings(class_name);
        let mro = self.class_mro(class_name);
        // Collect all matching multi candidates across the MRO, then pick the
        // most specific one by type hierarchy distance.
        let mut all_matches: Vec<(String, MethodDef)> = Vec::new();
        // Track whether a non-multi submethod was found on an ancestor
        // (submethods block MRO search for their class but not for
        // descendants).
        let mut submethod_blocks = false;
        for cn in &mro {
            // Hoist the clone to a `let` so the registry read guard drops here,
            // before method_args_match_for_invocant re-enters (&mut self). An
            // `if let` scrutinee would otherwise keep the temporary guard alive
            // across the whole block.
            let overloads = self
                .registry()
                .get_method_overloads(cn.as_str(), method_name);
            if let Some(overloads) = overloads {
                let any_multi = overloads.iter().any(|d| d.is_multi);
                let mut first_visible_non_multi: Option<MethodDef> = None;
                // Check if all overloads are submethods on an ancestor class
                let all_submethods = overloads.iter().all(|d| d.is_my);
                let is_ancestor = cn != class_name;
                for def in overloads {
                    if def.is_private {
                        continue;
                    }
                    // Submethods are NOT inherited: skip if defined on an
                    // ancestor class rather than the receiver's own class.
                    if def.is_my && is_ancestor {
                        continue;
                    }
                    if !any_multi && first_visible_non_multi.is_none() {
                        first_visible_non_multi = Some(def.clone());
                    }
                    let args_match = self.method_args_match_for_invocant(
                        class_name,
                        &def,
                        arg_values,
                        role_bindings.as_ref(),
                        invocant,
                    );
                    if args_match {
                        if !any_multi && all_matches.is_empty() {
                            // Non-multi: return the first match immediately,
                            // but only if no multi candidates were already
                            // collected from a child class in the MRO.
                            return Some((cn.clone(), def));
                        }
                        all_matches.push((cn.clone(), def));
                    }
                }
                // A non-multi submethod on an ancestor blocks the MRO search
                // (the method name exists but no candidate matched and
                // submethods are not inherited).
                if !any_multi && is_ancestor && all_submethods {
                    submethod_blocks = true;
                    continue;
                }
                // Method name is present on this class, but no candidate matched.
                // For non-multi methods, stop here — a subclass override
                // hides the parent's version.
                if !any_multi && all_matches.is_empty() {
                    return first_visible_non_multi.map(|def| (cn.clone(), def));
                }
            }
        }
        if all_matches.len() <= 1 {
            return all_matches.into_iter().next();
        }
        // Pick the candidate with the smallest type hierarchy distance
        let mut best_idx = 0;
        let mut best_dist = self.method_candidate_type_distance(arg_values, &all_matches[0].1);
        for (i, (_, def)) in all_matches.iter().enumerate().skip(1) {
            let dist = self.method_candidate_type_distance(arg_values, def);
            if dist < best_dist {
                best_dist = dist;
                best_idx = i;
            }
        }
        // `is default` trait tie-breaking: among candidates with the same
        // best distance, prefer the one marked `is default`.
        let tied: Vec<usize> = all_matches
            .iter()
            .enumerate()
            .filter(|(_, (_, def))| {
                self.method_candidate_type_distance(arg_values, def) == best_dist
            })
            .map(|(i, _)| i)
            .collect();
        if tied.len() > 1 {
            let best_where = tied
                .iter()
                .map(|&i| {
                    all_matches[i]
                        .1
                        .param_defs
                        .iter()
                        .filter(|p| p.where_constraint.is_some())
                        .count()
                })
                .max()
                .unwrap_or(0);
            let narrowed: Vec<usize> = tied
                .iter()
                .copied()
                .filter(|&i| {
                    all_matches[i]
                        .1
                        .param_defs
                        .iter()
                        .filter(|p| p.where_constraint.is_some())
                        .count()
                        == best_where
                })
                .collect();
            if let Some(&i) = narrowed.first() {
                best_idx = i;
            }
            // Invocant specificity tie-break: candidates whose argument-type
            // distance is equal can still differ in how derived their owning
            // class is. A `multi method` defined on the receiver's own class is
            // more specific than one inherited from an ancestor, so the
            // most-derived owner (earliest in the MRO) wins rather than being
            // treated as ambiguous.
            let mro_index = |owner: &str| -> usize {
                mro.iter()
                    .position(|cn| cn.as_str() == owner)
                    .unwrap_or(usize::MAX)
            };
            let best_mro = narrowed
                .iter()
                .map(|&i| mro_index(all_matches[i].0.as_str()))
                .min()
                .unwrap_or(usize::MAX);
            let mro_narrowed: Vec<usize> = narrowed
                .iter()
                .copied()
                .filter(|&i| mro_index(all_matches[i].0.as_str()) == best_mro)
                .collect();
            if let Some(&i) = mro_narrowed.first() {
                best_idx = i;
            }
            let mut default_winner = false;
            for &i in &tied {
                if all_matches[i].1.is_default {
                    best_idx = i;
                    default_winner = true;
                    break;
                }
            }
            // Ambiguous dispatch: two or more candidates remain equally specific
            // (same type distance, same number of `where` constraints, and the
            // same most-derived owner class) and none is marked `is default`.
            // Raku raises X::Multi::Ambiguous here.
            if !default_winner && mro_narrowed.len() > 1 {
                self.dispatch_ambiguous = true;
            }
        }
        let _ = submethod_blocks; // used for control flow above
        Some(all_matches.remove(best_idx))
    }

    /// Compute the type distance of a method's param constraints from the
    /// actual arguments.  Lower distance = more specific match.
    fn method_candidate_type_distance(&self, args: &[Value], def: &MethodDef) -> usize {
        let mut total = 0usize;
        let mut arg_idx = 0;
        for pd in &def.param_defs {
            if pd.is_invocant || pd.named || pd.name.starts_with("__type_capture__") {
                continue;
            }
            // Slurpy parameters (`*@x`, `*%h`, `**@x`) are less specific than a
            // required positional of the same type. When a single Positional
            // argument matches both `(@x)` and `(*@x)`, the non-slurpy candidate
            // must win rather than being treated as equally specific (ambiguous).
            if pd.slurpy || pd.double_slurpy {
                total += 2000;
                arg_idx += 1;
                continue;
            }
            if let Some(tc) = &pd.type_constraint {
                let base = Self::constraint_base_for_distance(tc);
                if arg_idx < args.len() {
                    total += Self::builtin_type_distance(base, &args[arg_idx]);
                }
            } else {
                total += 1000;
            }
            arg_idx += 1;
        }
        total
    }

    fn constraint_base_for_distance(constraint: &str) -> &str {
        let s = if constraint.ends_with(":D") || constraint.ends_with(":U") {
            &constraint[..constraint.len() - 2]
        } else {
            constraint
        };
        s.split('(').next().unwrap_or(s)
    }

    /// Compute the type hierarchy distance between a constraint and a value.
    /// 0 = exact match, larger = less specific.
    fn builtin_type_distance(constraint: &str, value: &Value) -> usize {
        let value_type = super::value_type_name(value);
        if constraint == value_type {
            return 0;
        }
        if let Value::Instance { class_name, .. } = value
            && constraint == class_name.resolve().as_str()
        {
            return 0;
        }
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
            if ancestor == constraint {
                return i;
            }
        }
        500
    }

    pub(crate) fn resolve_all_methods_with_owner(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
    ) -> Vec<(String, MethodDef)> {
        let role_bindings = self.registry().get_role_param_bindings(class_name);
        let mro = self.class_mro(class_name);
        let mut matches = Vec::new();
        for cn in &mro {
            // An MRO entry may be a regular class or a punned role used as a
            // parent (`class Foo is R1 { ... }`). Role methods are stored in
            // `self.registry().roles`, so fall back there when the entry is not a class.
            // Single guard for both the class and role method tables; clone the
            // matched overload Vec out so the guard drops before re-entry below.
            let overloads = {
                let registry = self.registry();
                registry
                    .classes
                    .get(cn.as_str())
                    .and_then(|c| c.methods.get(method_name))
                    .or_else(|| {
                        registry
                            .roles
                            .get(cn.as_str())
                            .and_then(|r| r.methods.get(method_name))
                    })
                    .cloned()
            };
            if let Some(overloads) = overloads {
                let is_ancestor = cn != class_name;
                for def in overloads {
                    if def.is_private {
                        continue;
                    }
                    // Submethods are NOT inherited
                    if def.is_my && is_ancestor {
                        continue;
                    }
                    if self.method_args_match_for_invocant(
                        class_name,
                        &def,
                        arg_values,
                        role_bindings.as_ref(),
                        None,
                    ) {
                        matches.push((cn.clone(), def));
                    }
                }
            }
        }
        matches
    }

    /// Resolve methods for .*/,+ dispatch: one resolution per MRO level.
    /// For multi methods, dispatches through the proto at each level.
    /// Returns empty vec if any level has the method but no candidate matches.
    pub(crate) fn resolve_methods_per_mro_level(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
    ) -> Vec<(String, MethodDef)> {
        let mro = self.class_mro(class_name);
        let mut defining_levels: Vec<String> = Vec::new();
        for cn in &mro {
            let is_ancestor = cn != class_name;
            if let Some(overloads) = self
                .registry()
                .get_method_overloads(cn.as_str(), method_name)
            {
                let has_visible = overloads
                    .iter()
                    .any(|d| !d.is_private && (!d.is_my || !is_ancestor));
                if has_visible {
                    defining_levels.push(cn.clone());
                }
            }
        }
        if defining_levels.is_empty() {
            return Vec::new();
        }
        let any_multi = defining_levels.iter().any(|cn| {
            self.registry()
                .get_method_overloads(cn.as_str(), method_name)
                .is_some_and(|ovs| ovs.iter().any(|d| d.is_multi))
        });
        if !any_multi {
            // Non-multi: use the standard resolution
            return self.resolve_all_methods_with_owner(class_name, method_name, arg_values);
        }
        // Multi methods: dispatch through proto at each level
        let mut matches = Vec::new();
        let mut any_failed = false;
        for cn in &defining_levels {
            if let Some(resolved) =
                self.resolve_method_with_owner_impl(cn, method_name, arg_values, None)
            {
                matches.push(resolved);
            } else {
                any_failed = true;
            }
        }
        if any_failed {
            return Vec::new();
        }
        matches
    }
}
