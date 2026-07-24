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
        role_bindings: Option<&rustc_hash::FxHashMap<String, Value>>,
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
                // `::?CLASS` / `::?ROLE` are pseudo-types (the current class), NOT
                // type captures — `constraint.strip_prefix("::")` would otherwise
                // read them as a capture named `?CLASS`/`?ROLE:U` and both bind a
                // bogus capture and skip the invocant check, so a role's
                // `multi method m(::?ROLE:U:)` / `(::?ROLE:D:)` pair became an
                // ambiguous call once composed into a class. Only a genuine
                // `::T` capture (not starting with `?`) binds here.
                if let Some(captured_name) = constraint.strip_prefix("::")
                    && !captured_name.starts_with('?')
                {
                    self.bind_type_capture(
                        captured_name,
                        &Value::package(Symbol::intern(class_name)),
                    );
                }
                // Check type constraint on the invocant (including :U/:D smileys).
                // Resolve the `::?CLASS`/`::?ROLE` pseudo-types to the actual
                // class name for matching. Pure type captures (e.g. ::T) don't
                // constrain the invocant.
                if let Some(inv) = invocant {
                    let resolved = constraint
                        .replace("::?CLASS", class_name)
                        .replace("::?ROLE", class_name);
                    let is_type_capture = resolved.starts_with("::");
                    if !is_type_capture && !self.type_matches_value(&resolved, inv) {
                        self.env = saved_env;
                        return false;
                    }
                }
            }
        }
        let args_match = self.method_args_match(arg_values, &def.param_defs);
        if def.param_defs.iter().any(|p| p.where_constraint.is_some()) {
            // A `where` clause is user code: its writes to *dynamic* variables
            // are observable side effects (`multi method f($ where { $*checked
            // ~= "d1"; ... })`, A01-limits/misc.t) and must survive the
            // restore, exactly as on the sub-dispatch path.
            self.restore_env_preserving_dynamics(saved_env);
        } else {
            self.env = saved_env;
        }
        args_match
    }

    /// Restore `self.env` to `saved` while keeping any *dynamic*-variable
    /// (`$*name`) writes made since — the observable side effects of user code
    /// (e.g. a `where` clause) run during a speculative dispatch match whose
    /// bindings are otherwise rolled back.
    pub(crate) fn restore_env_preserving_dynamics(&mut self, saved: crate::env::Env) {
        let dyn_writes: Vec<(crate::symbol::Symbol, Value)> = self
            .env
            .iter()
            .filter(|(k, v)| {
                k.with_str(|name| name.starts_with('*') || name.starts_with("$*"))
                    && saved.get_sym(**k) != Some(*v)
            })
            .map(|(k, v)| (*k, v.clone()))
            .collect();
        self.env = saved;
        for (k, v) in dyn_writes {
            self.env.insert_sym(k, v);
        }
    }

    pub(crate) fn resolve_method_with_owner(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
    ) -> Option<(Symbol, MethodDef)> {
        self.resolve_method_with_owner_impl(class_name, method_name, arg_values, None)
    }

    pub(crate) fn resolve_method_with_owner_invocant(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
        invocant: &Value,
    ) -> Option<(Symbol, MethodDef)> {
        self.resolve_method_with_owner_impl(class_name, method_name, arg_values, Some(invocant))
    }

    fn resolve_method_with_owner_impl(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
        invocant: Option<&Value>,
    ) -> Option<(Symbol, MethodDef)> {
        self.dispatch_ambiguous = false;
        let role_bindings = self.registry().get_role_param_bindings(class_name);
        let mro = self.class_mro(class_name);
        // Collect all matching multi candidates across the MRO, then pick the
        // most specific one by type hierarchy distance.
        let mut all_matches: Vec<(Symbol, MethodDef)> = Vec::new();
        // Track whether a non-multi submethod was found on an ancestor
        // (submethods block MRO search for their class but not for
        // descendants).
        let mut submethod_blocks = false;
        for cn in mro.iter() {
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
                let is_ancestor = cn.as_str() != class_name;
                // Single-visible-candidate fast return: with exactly one visible
                // non-multi candidate (and no multi collected from a more-derived
                // class), the outcome is invariant to the speculative argument
                // match — a successful match returns this def, and a failed match
                // falls through to the same def via `first_visible_non_multi`.
                // Skip the match (env snapshot + speculative binding) entirely.
                // Excluded: params carrying a `where` clause (user code whose
                // dynamic-variable writes are observable side effects of the
                // match), a type constraint (a subset's predicate is user code
                // too), or a sub-signature. This is the hot shape of every
                // BUILD/TWEAK construction dispatch.
                if !any_multi && all_matches.is_empty() {
                    let mut visible = overloads
                        .iter()
                        .filter(|d| !(d.is_private || (d.is_my && is_ancestor)));
                    if let Some(only) = visible.next()
                        && visible.next().is_none()
                        && only.param_defs.iter().all(|pd| {
                            pd.where_constraint.is_none()
                                && pd.type_constraint.is_none()
                                && pd.sub_signature.is_none()
                        })
                    {
                        return Some((*cn, only.clone()));
                    }
                }
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
                            return Some((*cn, def));
                        }
                        all_matches.push((*cn, def));
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
                    return first_visible_non_multi.map(|def| (*cn, def));
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
            // "Narrowness" tie-break: among equally-typed candidates, prefer the
            // one whose params carry more narrowing constraints. Ranked as a tuple
            // (higher wins lexicographically):
            //   .0 = `where` clauses OR subset types (a subset is narrower than its
            //        base, whose nominal distance was resolved to the base above);
            //   .1 = sigilled params (`@`/`%`/`&`) — the sigil imposes an implicit
            //        Positional/Associative/Callable constraint, so `(@x, @y)` is
            //        narrower than `($a, $b)` for two array args (matching the sub
            //        dispatch's `typed_param_count`, which also counts sigils).
            let narrowness = |def: &MethodDef| -> (usize, usize) {
                let where_subset = def
                    .param_defs
                    .iter()
                    .filter(|p| {
                        p.where_constraint.is_some()
                            || p.type_constraint.as_deref().is_some_and(|tc| {
                                self.registry()
                                    .subsets
                                    .contains_key(Self::constraint_base_for_distance(tc))
                            })
                    })
                    .count();
                let sigil_typed = def
                    .param_defs
                    .iter()
                    .filter(|p| {
                        !p.is_invocant
                            && !p.slurpy
                            && !p.double_slurpy
                            && (p.name.starts_with('@')
                                || p.name.starts_with('%')
                                || p.name.starts_with('&'))
                    })
                    .count();
                (where_subset, sigil_typed)
            };
            let best_where = tied
                .iter()
                .map(|&i| narrowness(&all_matches[i].1))
                .max()
                .unwrap_or((0, 0));
            let mut narrowed: Vec<usize> = tied
                .iter()
                .copied()
                .filter(|&i| narrowness(&all_matches[i].1) == best_where)
                .collect();
            if let Some(&i) = narrowed.first() {
                best_idx = i;
            }
            // Explicit-named preference (rakudo): among otherwise-tied
            // candidates, one that declares an explicit (non-slurpy) named
            // parameter is narrower than one that does not — `(Int $a, :$x)`
            // beats `(Int $a)` for `f(1)` regardless of declaration order.
            let has_explicit_named = |def: &MethodDef| -> bool {
                def.param_defs
                    .iter()
                    .any(|p| p.named && !p.slurpy && !p.double_slurpy)
            };
            if narrowed
                .iter()
                .any(|&i| has_explicit_named(&all_matches[i].1))
                && narrowed
                    .iter()
                    .any(|&i| !has_explicit_named(&all_matches[i].1))
            {
                narrowed.retain(|&i| has_explicit_named(&all_matches[i].1));
                if let Some(&i) = narrowed.first() {
                    best_idx = i;
                }
            }
            // Named parameters never make a dispatch ambiguous (rakudo): tied
            // candidates that all declare explicit named params resolve by
            // declaration order (`(Any :$file!)` vs `(Str :$file!)` picks the
            // first; named types/requiredness are not compared). Only a tie
            // among candidates with no explicit named params raises
            // X::Multi::Ambiguous (`(Int $a)` x2, `(Int $a, *%o)` x2).
            let all_named = narrowed
                .iter()
                .all(|&i| has_explicit_named(&all_matches[i].1));
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
            // Only a resolution that HAS the invocant can legitimately declare an
            // ambiguity: without it, `method_args_match_for_invocant` skips every
            // invocant type/definedness constraint, so a `:U:`/`:D:` multi pair
            // (or any invocant-narrowed candidates) all "match" and tie spuriously.
            // Such invocant-less resolutions come from introspection/speculative
            // callers (`has_user_method`, `Bool`/`Str` existence probes) that never
            // consume `dispatch_ambiguous`; leaving the flag set here instead leaks
            // a false ambiguity into the NEXT real dispatch (a defined-instance
            // `.gist`/`.Str` call poisoning a following `self.pairs`). The real
            // dispatch always re-resolves WITH the invocant, so deferring the
            // decision to it loses nothing.
            if !default_winner && mro_narrowed.len() > 1 && !all_named && invocant.is_some() {
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
            if pd.is_invocant || pd.name.starts_with("__type_capture__") {
                continue;
            }
            // Positional slurpy parameters (`*@x`, `**@x`) are less specific than
            // a required positional of the same type. When a single Positional
            // argument matches both `(@x)` and `(*@x)`, the non-slurpy candidate
            // must win rather than being treated as equally specific (ambiguous).
            //
            // A named-hash slurpy (`*%h`) is NOT penalized here. It competes with
            // other candidates only over *named* arguments, and a bare zero-arg
            // candidate is really `(*%_)` in Raku — equivalent, not narrower — so
            // a flat distance penalty would wrongly let `()` beat `(Bool :$b, *%h)`
            // (`HTTP::Request.new(GET => $url)` picking the wrong `new`). Named
            // slurpies are instead ranked by the explicit-named tie-break below:
            // `(IO::Path :$file!)` still beats `(*%items)` because it declares an
            // explicit named param, and `()` vs `(*%h)` correctly ties (ambiguous),
            // matching rakudo. The implicit method `*%_` carries no penalty either.
            if (pd.slurpy || pd.double_slurpy) && pd.name != "%_" && pd.name != "_" {
                if !pd.named && !pd.name.starts_with('%') {
                    // positional slurpy
                    total += 2000;
                    arg_idx += 1;
                }
                continue;
            }
            if pd.named || pd.slurpy || pd.double_slurpy {
                continue;
            }
            if let Some(tc) = &pd.type_constraint {
                let base = Self::constraint_base_for_distance(tc);
                // A subset's nominal distance is that of its ultimate base type
                // (`subset T of Any` ranks like `Any`, not as an unknown type at
                // distance 500 — which would wrongly lose to a bare `Any`). The
                // subset's extra narrowness is applied as a tie-break below.
                let resolved = if self.registry().subsets.contains_key(base) {
                    self.resolve_subset_base_type(base)
                } else {
                    base.to_string()
                };
                if arg_idx < args.len() {
                    total += Self::builtin_type_distance(&resolved, &args[arg_idx]);
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
        // `value_type_name(Nil)` reports "Any", so a Nil argument needs its own
        // MRO here: a `(Nil)` candidate must out-narrow e.g. `(Str() $s)` for a
        // literal Nil argument (URI's `authority(Nil)`).
        if matches!(value.view(), ValueView::Nil) {
            let nil_mro: &[&str] = &["Nil", "Cool", "Any", "Mu"];
            for (i, &ancestor) in nil_mro.iter().enumerate() {
                if ancestor == constraint {
                    return i;
                }
            }
            return 500;
        }
        let value_type = super::value_type_name(value);
        if constraint == value_type {
            return 0;
        }
        if let ValueView::Instance { class_name, .. } = value.view()
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

    /// Count the visible (non-private, non-ancestor-submethod) overloads of
    /// `method_name` across `class_name`'s MRO — the same visibility filter as
    /// `resolve_all_methods_with_owner`, but without cloning any overload Vec
    /// or running signature matching, and with an early-out at 2. Used to skip
    /// the remaining-candidates pass entirely for single-candidate submethod
    /// dispatch (BUILD/TWEAK on every construction).
    pub(crate) fn count_visible_method_candidates(
        &mut self,
        class_name: &str,
        method_name: &str,
    ) -> usize {
        let mro = self.class_mro(class_name);
        let registry = self.registry();
        let mut count = 0usize;
        for cn in mro.iter() {
            let is_ancestor = cn.as_str() != class_name;
            let overloads = registry
                .classes
                .get(cn.as_str())
                .and_then(|c| c.methods.get(method_name))
                .or_else(|| {
                    registry
                        .roles
                        .get(cn.as_str())
                        .and_then(|r| r.methods.get(method_name))
                });
            if let Some(ovs) = overloads {
                count += ovs
                    .iter()
                    .filter(|d| !(d.is_private || (d.is_my && is_ancestor)))
                    .count();
                if count > 1 {
                    return count;
                }
            }
        }
        count
    }

    pub(crate) fn resolve_all_methods_with_owner(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
    ) -> Vec<(Symbol, MethodDef)> {
        let role_bindings = self.registry().get_role_param_bindings(class_name);
        let mro = self.class_mro(class_name);
        let mut matches = Vec::new();
        for cn in mro.iter() {
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
                let is_ancestor = cn.as_str() != class_name;
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
                        matches.push((*cn, def));
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
    ) -> Vec<(Symbol, MethodDef)> {
        let mro = self.class_mro(class_name);
        let mut defining_levels: Vec<Symbol> = Vec::new();
        for cn in mro.iter() {
            let is_ancestor = cn.as_str() != class_name;
            if let Some(overloads) = self
                .registry()
                .get_method_overloads(cn.as_str(), method_name)
            {
                let has_visible = overloads
                    .iter()
                    .any(|d| !d.is_private && (!d.is_my || !is_ancestor));
                if has_visible {
                    defining_levels.push(*cn);
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
                self.resolve_method_with_owner_impl(cn.as_str(), method_name, arg_values, None)
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
