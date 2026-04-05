use super::*;

impl Interpreter {
    pub(in crate::runtime) fn type_arg_value_from_name(&self, name: &str) -> Value {
        let trimmed = name.trim().trim_start_matches('(').trim_end_matches(')');
        if let Some((base, args)) = Self::parse_parametric_type_name(trimmed)
            && self.is_role(&base)
        {
            return Value::ParametricRole {
                base_name: Symbol::intern(&base),
                type_args: args
                    .iter()
                    .map(|arg| self.type_arg_value_from_name(arg))
                    .collect(),
            };
        }
        Value::Package(Symbol::intern(trimmed))
    }

    pub(in crate::runtime) fn normalize_type_capture_value(&self, value: Value) -> Value {
        match value {
            Value::Str(name) if self.is_resolvable_type(&name) => {
                self.type_arg_value_from_name(&name)
            }
            other => other,
        }
    }

    pub(crate) fn resolved_type_capture_name(&self, constraint: &str) -> String {
        if self.has_type_capture_binding(constraint)
            && let Some(value) = self.env.get(constraint)
        {
            return match value {
                Value::Package(name) => name.resolve(),
                Value::ParametricRole {
                    base_name,
                    type_args,
                } => format!(
                    "{}[{}]",
                    base_name.resolve(),
                    type_args
                        .iter()
                        .map(|arg| match arg {
                            Value::Package(name) => name.resolve(),
                            other => other.to_string_value(),
                        })
                        .collect::<Vec<_>>()
                        .join(",")
                ),
                other => other.to_string_value(),
            };
        }
        constraint.to_string()
    }

    pub(in crate::runtime) fn typed_container_param_expected(
        &self,
        name: &str,
        constraint: &str,
    ) -> Option<String> {
        let constraint = self.resolved_type_capture_name(constraint);
        if name.starts_with('@') {
            Some(format!("Positional[{constraint}]"))
        } else if name.starts_with('%') {
            Some(format!("Associative[{constraint}]"))
        } else {
            None
        }
    }

    pub(in crate::runtime) fn typed_container_param_matches(
        &mut self,
        name: &str,
        constraint: &str,
        value: &Value,
        source_name: Option<&str>,
        source_constraint: Option<&str>,
    ) -> bool {
        let resolved_constraint = self.resolved_type_capture_name(constraint);
        if let Some(source_name) = source_name {
            if source_name.starts_with('@') && name.starts_with('%') {
                return false;
            }
            if source_name.starts_with('%') && name.starts_with('@') {
                return false;
            }
        }
        let container_kind_matches = if name.starts_with('@') {
            matches!(value, Value::Array(..) | Value::Slip(..))
        } else if name.starts_with('%') {
            matches!(value, Value::Hash(..) | Value::Array(..))
        } else {
            false
        };
        if container_kind_matches && let Some(source) = source_constraint {
            return self.type_matches_value(
                &resolved_constraint,
                &Value::Package(Symbol::intern(source)),
            );
        }
        if let Some(metadata) = self.container_type_metadata(value)
            && !metadata.value_type.is_empty()
            && container_kind_matches
        {
            return self.type_matches_value(
                &resolved_constraint,
                &Value::Package(Symbol::intern(&metadata.value_type)),
            );
        }

        if name.starts_with('@') {
            return match value {
                Value::Array(items, ..) => {
                    if items.is_empty() {
                        return source_constraint.is_some_and(|source| {
                            self.type_matches_value(
                                &resolved_constraint,
                                &Value::Package(Symbol::intern(source)),
                            )
                        });
                    }
                    !items.is_empty()
                        && items
                            .iter()
                            .all(|item| self.type_matches_value(&resolved_constraint, item))
                }
                Value::Slip(items) => {
                    if items.is_empty() {
                        return source_constraint.is_some_and(|source| {
                            self.type_matches_value(
                                &resolved_constraint,
                                &Value::Package(Symbol::intern(source)),
                            )
                        });
                    }
                    !items.is_empty()
                        && items
                            .iter()
                            .all(|item| self.type_matches_value(&resolved_constraint, item))
                }
                _ => false,
            };
        }

        if name.starts_with('%') {
            return match value {
                Value::Hash(map) => {
                    if map.is_empty() {
                        return source_constraint.is_some_and(|source| {
                            self.type_matches_value(
                                &resolved_constraint,
                                &Value::Package(Symbol::intern(source)),
                            )
                        });
                    }
                    !map.is_empty()
                        && map
                            .values()
                            .all(|item| self.type_matches_value(&resolved_constraint, item))
                }
                Value::Array(items, ..) => {
                    if items.is_empty() {
                        return source_constraint.is_some_and(|source| {
                            self.type_matches_value(
                                &resolved_constraint,
                                &Value::Package(Symbol::intern(source)),
                            )
                        });
                    }
                    !items.is_empty()
                        && items.iter().all(|item| {
                            if let Value::Pair(_, value) = item {
                                self.type_matches_value(&resolved_constraint, value)
                            } else {
                                false
                            }
                        })
                }
                _ => false,
            };
        }

        false
    }

    /// Resolve a potentially qualified role name to its registered key.
    /// If `name` is in `self.roles`, returns it as-is. Otherwise, if `name`
    /// contains `::`, tries the short name (after the last `::`).
    pub(in crate::runtime) fn resolve_role_key(&self, name: &str) -> Option<String> {
        if self.roles.contains_key(name) {
            return Some(name.to_string());
        }
        if let Some(Value::Package(pkg)) = self.env.get(name) {
            let resolved = pkg.resolve();
            if self.roles.contains_key(&resolved) {
                return Some(resolved);
            }
        }
        if let Some(short) = name.rsplit("::").next()
            && self.roles.contains_key(short)
        {
            return Some(short.to_string());
        }
        None
    }

    pub(crate) fn type_matches_value(&mut self, constraint: &str, value: &Value) -> bool {
        if let Value::Scalar(inner) = value {
            return self.type_matches_value(constraint, inner.as_ref());
        }
        if constraint == "Inf" {
            return matches!(value, Value::Num(n) if n.is_infinite() && n.is_sign_positive());
        }
        if constraint == "NaN" {
            return matches!(value, Value::Num(n) if n.is_nan());
        }
        if constraint == "UInt" {
            return match value {
                Value::Int(i) => *i >= 0,
                Value::BigInt(n) => n.sign() != num_bigint::Sign::Minus,
                Value::Nil => true,
                Value::Package(name) => {
                    let name = name.resolve();
                    name == "UInt" || name == "Int"
                }
                _ => false,
            };
        }
        if let Some((base, _)) = constraint.split_once(':')
            && base == "Variable"
            && varref_from_value(value).is_some()
        {
            return true;
        }
        if constraint == "Variable" && varref_from_value(value).is_some() {
            return true;
        }
        if let Value::Enum { enum_type, .. } = value {
            let enum_name = enum_type.resolve();
            if constraint == enum_name || Self::type_matches(constraint, &enum_name) {
                return true;
            }
        }
        let package_matches_type = |package_name: &str, type_name: &str| -> bool {
            let package_base = package_name.split('[').next().unwrap_or(package_name);
            let (package_base, _) = strip_type_smiley(package_base);
            if Self::type_matches(type_name, package_base) {
                return true;
            }
            if let Some(class_def) = self.classes.get(package_base) {
                return class_def
                    .parents
                    .clone()
                    .iter()
                    .any(|parent| Self::type_matches(type_name, parent));
            }
            false
        };
        if let Value::Package(package_name) = value
            && let Some((lhs_base, lhs_inner)) =
                Self::parse_generic_constraint(&package_name.resolve())
            && let Some((rhs_base, rhs_inner)) = Self::parse_generic_constraint(constraint)
            && Self::type_matches(rhs_base, lhs_base)
        {
            let (lhs_inner_base, lhs_inner_smiley) = strip_type_smiley(lhs_inner);
            let (rhs_inner_base, rhs_inner_smiley) = strip_type_smiley(rhs_inner);
            if !Self::type_matches(rhs_inner_base, lhs_inner_base) {
                return false;
            }
            return match rhs_inner_smiley {
                Some(":D") => lhs_inner_smiley == Some(":D"),
                Some(":U") => lhs_inner_smiley != Some(":D"),
                _ => true,
            };
        }
        if let Value::Package(package_name) = value
            && let Some((pkg_target, pkg_source)) = parse_coercion_type(&package_name.resolve())
            && (Self::type_matches(constraint, pkg_target)
                || pkg_source.is_some_and(|src| Self::type_matches(constraint, src)))
        {
            return true;
        }
        if let Value::Package(package_name) = value
            && let Some((target, source)) = parse_coercion_type(constraint)
        {
            let pkg_resolved = package_name.resolve();
            let target_ok = package_matches_type(&pkg_resolved, target);
            let source_ok = source.is_some_and(|src| self.type_matches_value(src, value));
            return target_ok || source_ok;
        }
        if let Value::Package(package_name) = value
            && package_matches_type(&package_name.resolve(), constraint)
        {
            return true;
        }
        // Handle coercion types: Int() matches anything, Int(Rat) matches Rat
        if let Some((target, source)) = parse_coercion_type(constraint) {
            return if let Some(src) = source {
                self.type_matches_value(src, value) || self.type_matches_value(target, value)
            } else {
                true
            };
        }
        if let Some((base, inner)) = Self::parse_generic_constraint(constraint) {
            match base {
                "Array" | "List" | "Positional" => {
                    if let Value::Array(items, ..) = value {
                        return items.iter().all(|v| self.type_matches_value(inner, v));
                    }
                    return false;
                }
                "buf8" | "blob8" | "buf16" | "buf32" | "buf64" | "blob16" | "blob32" | "blob64" => {
                    if let Value::Instance { class_name, .. } = value {
                        let cn = class_name.resolve();
                        if cn == "Buf"
                            || cn == "Blob"
                            || cn.starts_with("Buf[")
                            || cn.starts_with("Blob[")
                            || cn.starts_with("buf")
                            || cn.starts_with("blob")
                        {
                            return true;
                        }
                    }
                    return false;
                }
                "Hash" | "Associative" => {
                    if let Value::Hash(map) = value {
                        return map.values().all(|v| self.type_matches_value(inner, v));
                    }
                    if let Value::Array(items, ..) = value {
                        return items.iter().all(|item| {
                            if let Value::Pair(_, v) = item {
                                self.type_matches_value(inner, v)
                            } else {
                                false
                            }
                        });
                    }
                    return false;
                }
                "Buf" | "Blob" => {
                    if let Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } = value
                    {
                        let class = class_name.resolve();
                        let class_ok = if base == "Buf" {
                            class == "Buf" || class.starts_with("Buf[") || class.starts_with("buf")
                        } else {
                            class == "Blob"
                                || class == "Buf"
                                || class.starts_with("Blob[")
                                || class.starts_with("blob")
                                || class.starts_with("Buf[")
                                || class.starts_with("buf")
                        };
                        if !class_ok {
                            return false;
                        }
                        if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                            return items.iter().all(|v| self.type_matches_value(inner, v));
                        }
                    }
                    return false;
                }
                _ => {}
            }
        }
        if let Some(Value::Package(bound)) = self.env.get(constraint).cloned()
            && bound != *constraint
        {
            return self.type_matches_value(&bound.resolve(), value);
        }
        // Handle type smileys (:U, :D, :_)
        let (base_constraint, smiley) = strip_type_smiley(constraint);
        if let Some(smiley) = smiley {
            let type_ok = self.type_matches_value(base_constraint, value);
            if !type_ok {
                return false;
            }
            return match smiley {
                ":U" => !value_is_defined(value),
                ":D" => value_is_defined(value),
                ":_" => true,
                _ => true,
            };
        }

        if let Some(subset) = self.subsets.get(constraint).cloned() {
            if !self.type_matches_value(&subset.base, value) {
                return false;
            }
            let predicate_value = self.coerce_value_for_constraint(&subset.base, value.clone());
            let ok = if let Some(pred) = &subset.predicate {
                // Check if the predicate is a block/lambda (AnonSub, AnonSubParams, etc.)
                let is_callable_expr = matches!(
                    pred,
                    Expr::AnonSub { .. } | Expr::AnonSubParams { .. } | Expr::Block(_)
                );
                if is_callable_expr {
                    // Evaluate to get a callable, then call with the value
                    match self.eval_block_value(&[Stmt::Expr(pred.clone())]) {
                        Ok(callable @ Value::Sub(_)) => self
                            .call_sub_value(callable, vec![predicate_value], false)
                            .map(|v| v.truthy())
                            .unwrap_or(false),
                        Ok(v) => v.truthy(),
                        Err(_) => false,
                    }
                } else {
                    // Bare expression: set $_ and evaluate directly
                    let saved = self.env.get("_").cloned();
                    self.env.insert("_".to_string(), predicate_value);
                    let result = self
                        .eval_block_value(&[Stmt::Expr(pred.clone())])
                        .map(|v| self.smart_match(value, &v))
                        .unwrap_or(false);
                    if let Some(old) = saved {
                        self.env.insert("_".to_string(), old);
                    } else {
                        self.env.remove("_");
                    }
                    result
                }
            } else {
                true
            };
            return ok;
        }
        if let Some((constraint_base, constraint_args)) =
            Self::parse_parametric_type_name(constraint)
            && self.is_role(&constraint_base)
            && let Value::Mixin(_, mixins) = value
        {
            let key = format!(
                "__mutsu_role_typeargs__{}",
                Symbol::intern(&constraint_base)
            );
            if value.does_check(&constraint_base)
                && let Some(Value::Array(actual_args, ..)) = mixins.get(&key)
            {
                let expected_args = constraint_args
                    .iter()
                    .map(|arg| self.type_arg_value_from_name(arg))
                    .collect::<Vec<_>>();
                if actual_args.len() == expected_args.len()
                    && actual_args
                        .iter()
                        .zip(expected_args.iter())
                        .all(|(actual, expected)| self.parametric_arg_subtypes(actual, expected))
                {
                    return true;
                }
            }
        }
        if let Value::ParametricRole {
            base_name,
            type_args,
        } = value
        {
            if let Some((constraint_base, constraint_args)) =
                Self::parse_parametric_type_name(constraint)
            {
                let actual_args = if base_name.resolve() == constraint_base {
                    Some(type_args.clone())
                } else {
                    self.role_parent_args_for(&base_name.resolve(), type_args, &constraint_base)
                };
                if let Some(actual_args) = actual_args {
                    let expected_args = constraint_args
                        .iter()
                        .map(|arg| self.type_arg_value_from_name(arg))
                        .collect::<Vec<_>>();
                    if actual_args.len() == expected_args.len()
                        && actual_args
                            .iter()
                            .zip(expected_args.iter())
                            .all(|(actual, expected)| {
                                self.parametric_arg_subtypes(actual, expected)
                            })
                    {
                        return true;
                    }
                }
            }
            let constraint_base = Self::parse_parametric_type_name(constraint)
                .map(|(base, _)| base)
                .unwrap_or_else(|| Self::optional_type_object_name(constraint));
            if base_name.resolve() == constraint_base
                || self.role_is_subtype(&base_name.resolve(), &constraint_base)
                || self
                    .role_parent_args_for(&base_name.resolve(), type_args, &constraint_base)
                    .is_some()
            {
                return true;
            }
        }
        // Role constraints should accept composed role instances/mixins.
        // Use resolve_role_key to handle qualified names (e.g. GH2613::R1 -> R1).
        let resolved_constraint = self.resolve_role_key(constraint).map(|s| s.to_string());
        let effective_constraint = resolved_constraint.as_deref().unwrap_or(constraint);
        if resolved_constraint.is_some() && value.does_check(effective_constraint) {
            return true;
        }
        // Type-object checks: Package values should respect declared class/role ancestry.
        if let Value::Package(package_name) = value {
            if Self::type_matches(constraint, &package_name.resolve()) {
                return true;
            }
            if let Some((actual_base, actual_args)) =
                Self::parse_parametric_type_name(&package_name.resolve())
                && let Some((constraint_base, constraint_args)) =
                    Self::parse_parametric_type_name(constraint)
            {
                let actual_args = actual_args
                    .iter()
                    .map(|arg| self.type_arg_value_from_name(arg))
                    .collect::<Vec<_>>();
                if let Some(parent_args) =
                    self.role_parent_args_for(&actual_base, &actual_args, &constraint_base)
                {
                    let expected_args = constraint_args
                        .iter()
                        .map(|arg| self.type_arg_value_from_name(arg))
                        .collect::<Vec<_>>();
                    if parent_args.len() == expected_args.len()
                        && parent_args
                            .iter()
                            .zip(expected_args.iter())
                            .all(|(actual, expected)| {
                                self.parametric_arg_subtypes(actual, expected)
                            })
                    {
                        return true;
                    }
                }
            }
            if let Some(subset_base) = self
                .subsets
                .get(&package_name.resolve())
                .map(|subset| subset.base.clone())
                && (constraint == package_name.resolve()
                    || self.type_matches_value(
                        constraint,
                        &Value::Package(Symbol::intern(&subset_base)),
                    ))
            {
                return true;
            }
            let mro = self.class_mro(&package_name.resolve());
            if mro
                .iter()
                .any(|parent| Self::type_matches(effective_constraint, parent))
            {
                return true;
            }
            // Check transitive role composition through class_composed_roles and role_parents
            if resolved_constraint.is_some() {
                let mut role_stack: Vec<String> = Vec::new();
                let mut seen_roles = HashSet::new();
                // Collect all composed roles from the class and its parents
                for cn in &mro {
                    if let Some(composed) = self.class_composed_roles.get(cn.as_str()) {
                        for cr in composed {
                            let base = cr.split_once('[').map(|(b, _)| b).unwrap_or(cr.as_str());
                            role_stack.push(base.to_string());
                        }
                    }
                }
                while let Some(role_name) = role_stack.pop() {
                    if !seen_roles.insert(role_name.clone()) {
                        continue;
                    }
                    if Self::type_matches(effective_constraint, &role_name) {
                        return true;
                    }
                    if let Some(rparents) = self.role_parents.get(&role_name) {
                        for rp in rparents {
                            let rp_base = rp.split_once('[').map(|(b, _)| b).unwrap_or(rp.as_str());
                            if self.resolve_role_key(rp_base).is_some() {
                                role_stack.push(rp_base.to_string());
                            }
                        }
                    }
                }
            }
            if let Some(constraint_base) = constraint.split_once('[').map(|(base, _)| base)
                && mro.iter().any(|parent| {
                    parent == constraint_base || parent.starts_with(&format!("{constraint_base}["))
                })
            {
                return true;
            }
            let pkg_resolved = package_name.resolve();
            let pkg_base = pkg_resolved
                .split_once('[')
                .map(|(b, _)| b)
                .unwrap_or(&pkg_resolved);
            if self.roles.contains_key(pkg_base) {
                // For role groups, use candidate-specific parents:
                // - Curried roles (e.g. R[Int]): use parametric candidate's parents
                // - Bare role name: use non-parametric candidate's parents
                let candidate_parents = self.role_candidates.get(pkg_base).and_then(|candidates| {
                    if pkg_resolved.contains('[') {
                        // Curried: find the parametric candidate
                        candidates
                            .iter()
                            .find(|c| !c.type_params.is_empty())
                            .map(|c| c.parents.clone())
                    } else {
                        // Bare: find the non-parametric candidate
                        candidates
                            .iter()
                            .find(|c| c.type_params.is_empty())
                            .map(|c| c.parents.clone())
                    }
                });
                let initial_parents = candidate_parents
                    .or_else(|| self.role_parents.get(pkg_base).cloned())
                    .unwrap_or_default();
                let mut stack: Vec<String> = initial_parents;
                let mut seen = HashSet::new();
                while let Some(parent) = stack.pop() {
                    if !seen.insert(parent.clone()) {
                        continue;
                    }
                    if Self::type_matches(effective_constraint, &parent) {
                        return true;
                    }
                    // Walk parent's MRO for transitive parent matching
                    let parent_mro = self.class_mro(&parent);
                    if parent_mro
                        .iter()
                        .any(|p| Self::type_matches(effective_constraint, p))
                    {
                        return true;
                    }
                    // Also check composed roles of the parent class
                    if let Some(composed) = self.class_composed_roles.get(&parent) {
                        for cr in composed {
                            let cr_base = cr.split_once('[').map(|(b, _)| b).unwrap_or(cr.as_str());
                            if Self::type_matches(effective_constraint, cr_base) {
                                return true;
                            }
                        }
                    }
                    if let Some(rparents) = self.role_parents.get(&parent) {
                        for rp in rparents {
                            stack.push(rp.clone());
                        }
                    }
                }
            }
        }
        // Check Instance class name against constraint (including parent classes)
        if let Value::Instance { class_name, .. } = value {
            if Self::type_matches(constraint, &class_name.resolve()) {
                return true;
            }
            // Buf/Blob hierarchy: Buf[uint8] isa Buf, buf8 isa Buf, etc.
            let cn = class_name.resolve();
            if (constraint == "Buf" || constraint == "Blob")
                && crate::runtime::utils::is_buf_or_blob_class(&cn)
            {
                // Buf constraint accepts any Buf-like, Blob constraint accepts any Blob-like
                if constraint == "Buf" && crate::runtime::utils::is_buf_like_class(&cn) {
                    return true;
                }
                if constraint == "Blob" {
                    return true; // All Buf/Blob types are Blob (Buf inherits Blob)
                }
            }
            // blob8/buf8 aliases: blob8 == Blob[uint8], etc.
            if crate::runtime::utils::is_buf_or_blob_class(constraint)
                && crate::runtime::utils::is_buf_or_blob_class(&cn)
            {
                let nc = crate::runtime::utils::normalize_buf_type_name(constraint);
                let nv = crate::runtime::utils::normalize_buf_type_name(&cn);
                if nc == nv {
                    return true;
                }
                if nc.starts_with("Blob") && nv.starts_with("Buf") {
                    let ci = nc.split_once('[').map(|(_, i)| i.trim_end_matches(']'));
                    let vi = nv.split_once('[').map(|(_, i)| i.trim_end_matches(']'));
                    if ci.is_none() || ci == vi {
                        return true;
                    }
                }
            }
            // Check parent classes of the instance
            if let Some(class_def) = self.classes.get(&class_name.resolve()) {
                for parent in class_def.parents.clone() {
                    if Self::type_matches(constraint, &parent) {
                        return true;
                    }
                }
            }
            // Check MRO (handles built-in type hierarchies like Match -> Capture -> Cool)
            let mro = self.class_mro(&class_name.resolve());
            if mro
                .iter()
                .any(|parent| Self::type_matches(constraint, parent))
            {
                return true;
            }
            // Check composed roles for the instance's class (and its MRO)
            if self.roles.contains_key(constraint) {
                let mro = self.class_mro(&class_name.resolve());
                let mut role_stack: Vec<String> = Vec::new();
                let mut seen_roles = HashSet::new();
                for cn in &mro {
                    if let Some(composed) = self.class_composed_roles.get(cn.as_str()) {
                        for cr in composed {
                            let base = cr.split_once('[').map(|(b, _)| b).unwrap_or(cr.as_str());
                            role_stack.push(base.to_string());
                        }
                    }
                }
                while let Some(role_name) = role_stack.pop() {
                    if !seen_roles.insert(role_name.clone()) {
                        continue;
                    }
                    if role_name == constraint {
                        return true;
                    }
                    if let Some(rparents) = self.role_parents.get(&role_name) {
                        for rp in rparents {
                            let rp_base = rp.split_once('[').map(|(b, _)| b).unwrap_or(rp.as_str());
                            if self.roles.contains_key(rp_base) {
                                role_stack.push(rp_base.to_string());
                            }
                        }
                    }
                }
            }
        }
        // Mixin allomorphic types: check both inner type and mixin type keys
        if let Value::Mixin(inner, mixins) = value {
            if let Some((constraint_base, constraint_args)) =
                Self::parse_parametric_type_name(constraint)
            {
                let expected_args = constraint_args
                    .iter()
                    .map(|arg| self.type_arg_value_from_name(arg))
                    .collect::<Vec<_>>();
                for (key, mixin_value) in mixins.iter() {
                    let Some(actual_base) = key.strip_prefix("__mutsu_role_typeargs__") else {
                        continue;
                    };
                    let Value::Array(actual_args, ..) = mixin_value else {
                        continue;
                    };
                    let comparable_actual_args = if actual_base == constraint_base {
                        actual_args.as_ref().clone()
                    } else if let Some(parent_args) =
                        self.role_parent_args_for(actual_base, actual_args, &constraint_base)
                    {
                        parent_args
                    } else if self.role_is_subtype(actual_base, &constraint_base) {
                        actual_args.as_ref().clone()
                    } else {
                        continue;
                    };
                    if comparable_actual_args.len() == expected_args.len()
                        && comparable_actual_args.iter().zip(expected_args.iter()).all(
                            |(actual, expected)| self.parametric_arg_subtypes(actual, expected),
                        )
                    {
                        return true;
                    }
                }
            }
            // Check allomorphic type names first (IntStr, NumStr, RatStr, ComplexStr, Allomorph)
            if value.isa_check(constraint) {
                return true;
            }
            if self.type_matches_value(constraint, inner) {
                return true;
            }
            if mixins.contains_key(constraint) {
                return true;
            }
        }
        // For Package (type object) values, use the package name as the type
        // so that e.g. Junction (which is Mu, not Any) is correctly rejected
        // when the constraint is Any.
        if let Value::Package(package_name) = value {
            let resolved = package_name.resolve();
            return Self::type_matches(constraint, &resolved);
        }
        let value_type = crate::runtime::value_type_name(value);
        Self::type_matches(constraint, value_type)
    }
}
