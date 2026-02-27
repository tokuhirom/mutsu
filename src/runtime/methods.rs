use super::*;
use crate::ast::CallArg;
use crate::value::signature::{
    make_params_value_from_param_defs, make_signature_value, param_defs_to_sig_info,
};

impl Interpreter {
    pub(crate) fn auto_signature_uses(stmts: &[Stmt]) -> (bool, bool) {
        fn scan_stmt(stmt: &Stmt, positional: &mut bool, named: &mut bool) {
            match stmt {
                Stmt::Expr(e) | Stmt::Return(e) | Stmt::Die(e) | Stmt::Fail(e) | Stmt::Take(e) => {
                    scan_expr(e, positional, named);
                }
                Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => {
                    scan_expr(expr, positional, named);
                }
                Stmt::Call { args, .. } => {
                    for arg in args {
                        match arg {
                            CallArg::Positional(e) | CallArg::Slip(e) | CallArg::Invocant(e) => {
                                scan_expr(e, positional, named)
                            }
                            CallArg::Named { value: Some(e), .. } => {
                                scan_expr(e, positional, named)
                            }
                            CallArg::Named { value: None, .. } => {}
                        }
                    }
                }
                Stmt::Say(es) | Stmt::Print(es) | Stmt::Note(es) => {
                    for e in es {
                        scan_expr(e, positional, named);
                    }
                }
                Stmt::If {
                    cond,
                    then_branch,
                    else_branch,
                    ..
                } => {
                    scan_expr(cond, positional, named);
                    for s in then_branch {
                        scan_stmt(s, positional, named);
                    }
                    for s in else_branch {
                        scan_stmt(s, positional, named);
                    }
                }
                Stmt::While { cond, body, .. } => {
                    scan_expr(cond, positional, named);
                    for s in body {
                        scan_stmt(s, positional, named);
                    }
                }
                Stmt::For { iterable, body, .. } => {
                    scan_expr(iterable, positional, named);
                    for s in body {
                        scan_stmt(s, positional, named);
                    }
                }
                Stmt::Loop { body, .. }
                | Stmt::React { body }
                | Stmt::Block(body)
                | Stmt::SyntheticBlock(body)
                | Stmt::Default(body)
                | Stmt::Catch(body)
                | Stmt::Control(body)
                | Stmt::RoleDecl { body, .. }
                | Stmt::Phaser { body, .. } => {
                    for s in body {
                        scan_stmt(s, positional, named);
                    }
                }
                Stmt::Whenever { supply, body, .. } => {
                    scan_expr(supply, positional, named);
                    for s in body {
                        scan_stmt(s, positional, named);
                    }
                }
                Stmt::Given { topic, body } => {
                    scan_expr(topic, positional, named);
                    for s in body {
                        scan_stmt(s, positional, named);
                    }
                }
                Stmt::When { cond, body } => {
                    scan_expr(cond, positional, named);
                    for s in body {
                        scan_stmt(s, positional, named);
                    }
                }
                Stmt::Let { value, index, .. } => {
                    if let Some(v) = value {
                        scan_expr(v, positional, named);
                    }
                    if let Some(i) = index {
                        scan_expr(i, positional, named);
                    }
                }
                Stmt::TempMethodAssign {
                    method_args, value, ..
                } => {
                    for a in method_args {
                        scan_expr(a, positional, named);
                    }
                    scan_expr(value, positional, named);
                }
                Stmt::SubsetDecl {
                    predicate: Some(p), ..
                } => {
                    scan_expr(p, positional, named);
                }
                _ => {}
            }
        }

        fn scan_expr(expr: &Expr, positional: &mut bool, named: &mut bool) {
            match expr {
                Expr::ArrayVar(name) if name == "_" => *positional = true,
                Expr::HashVar(name) if name == "_" => *named = true,
                Expr::Binary { left, right, .. }
                | Expr::HyperOp { left, right, .. }
                | Expr::MetaOp { left, right, .. } => {
                    scan_expr(left, positional, named);
                    scan_expr(right, positional, named);
                }
                Expr::Unary { expr, .. }
                | Expr::PostfixOp { expr, .. }
                | Expr::AssignExpr { expr, .. }
                | Expr::ZenSlice(expr)
                | Expr::Reduction { expr, .. } => scan_expr(expr, positional, named),
                Expr::Exists { target, arg, .. } => {
                    scan_expr(target, positional, named);
                    if let Some(a) = arg {
                        scan_expr(a, positional, named);
                    }
                }
                Expr::MethodCall { target, args, .. }
                | Expr::DynamicMethodCall { target, args, .. }
                | Expr::HyperMethodCall { target, args, .. } => {
                    scan_expr(target, positional, named);
                    for a in args {
                        scan_expr(a, positional, named);
                    }
                }
                Expr::Call { args, .. } => {
                    for a in args {
                        scan_expr(a, positional, named);
                    }
                }
                Expr::CallOn { target, args } => {
                    scan_expr(target, positional, named);
                    for a in args {
                        scan_expr(a, positional, named);
                    }
                }
                Expr::Index { target, index } => {
                    scan_expr(target, positional, named);
                    scan_expr(index, positional, named);
                }
                Expr::Ternary {
                    cond,
                    then_expr,
                    else_expr,
                } => {
                    scan_expr(cond, positional, named);
                    scan_expr(then_expr, positional, named);
                    scan_expr(else_expr, positional, named);
                }
                Expr::ArrayLiteral(es)
                | Expr::BracketArray(es)
                | Expr::StringInterpolation(es)
                | Expr::CaptureLiteral(es) => {
                    for e in es {
                        scan_expr(e, positional, named);
                    }
                }
                Expr::InfixFunc { left, right, .. } => {
                    scan_expr(left, positional, named);
                    for e in right {
                        scan_expr(e, positional, named);
                    }
                }
                Expr::Block(stmts)
                | Expr::AnonSub { body: stmts, .. }
                | Expr::AnonSubParams { body: stmts, .. }
                | Expr::Gather(stmts) => {
                    for s in stmts {
                        scan_stmt(s, positional, named);
                    }
                }
                Expr::DoBlock { body, .. } => {
                    for s in body {
                        scan_stmt(s, positional, named);
                    }
                }
                Expr::DoStmt(stmt) => scan_stmt(stmt, positional, named),
                Expr::Lambda { body, .. } => {
                    for s in body {
                        scan_stmt(s, positional, named);
                    }
                }
                Expr::Try { body, catch } => {
                    for s in body {
                        scan_stmt(s, positional, named);
                    }
                    if let Some(c) = catch {
                        for s in c {
                            scan_stmt(s, positional, named);
                        }
                    }
                }
                Expr::IndirectCodeLookup { package, .. } => scan_expr(package, positional, named),
                Expr::Hash(pairs) => {
                    for (_, value) in pairs {
                        if let Some(v) = value {
                            scan_expr(v, positional, named);
                        }
                    }
                }
                _ => {}
            }
        }

        let mut positional = false;
        let mut named = false;
        for stmt in stmts {
            scan_stmt(stmt, &mut positional, &mut named);
        }
        (positional, named)
    }

    fn assumed_signature_param_defs(
        data: &crate::value::SubData,
        assumed_positional: &[Value],
        assumed_named: &std::collections::HashMap<String, Value>,
    ) -> Option<Vec<ParamDef>> {
        if data.param_defs.is_empty() {
            return None;
        }
        let mut param_defs = data.param_defs.clone();
        // Build type capture mappings from assumed positional args
        let mut type_captures: std::collections::HashMap<String, String> =
            std::collections::HashMap::new();
        {
            let mut pos_idx = 0usize;
            for pd in &param_defs {
                if !pd.named && !pd.slurpy {
                    if pos_idx < assumed_positional.len() {
                        if let Some(tc) = &pd.type_constraint
                            && let Some(capture_name) = tc.strip_prefix("::")
                        {
                            let resolved_type = crate::runtime::utils::value_type_name(
                                &assumed_positional[pos_idx],
                            )
                            .to_string();
                            type_captures.insert(capture_name.to_string(), resolved_type);
                        }
                        pos_idx += 1;
                    } else {
                        break;
                    }
                }
            }
        }
        let mut to_consume = assumed_positional.len();
        param_defs.retain(|pd| {
            if to_consume > 0 && !pd.named && !pd.slurpy {
                to_consume -= 1;
                false
            } else {
                true
            }
        });
        // Apply type capture resolution to remaining params
        if !type_captures.is_empty() {
            for pd in &mut param_defs {
                if let Some(tc) = &pd.type_constraint
                    && let Some(resolved) = type_captures.get(tc.as_str())
                {
                    pd.type_constraint = Some(resolved.clone());
                }
            }
        }
        // When .assuming() is used with named args, all named params lose their
        // defaults and where constraints in the signature display.
        // Assumed params additionally become non-required.
        if !assumed_named.is_empty() {
            for pd in &mut param_defs {
                if pd.named {
                    pd.default = None;
                    pd.where_constraint = None;
                    if assumed_named.contains_key(&pd.name) {
                        pd.required = false;
                    }
                }
            }
        }
        Some(param_defs)
    }

    fn collect_named_param_keys(
        param_defs: &[ParamDef],
        out: &mut std::collections::HashSet<String>,
    ) {
        for pd in param_defs {
            if pd.named {
                if pd.name == "__subsig__" {
                    if let Some(key) = &pd.type_constraint {
                        out.insert(key.clone());
                    }
                } else {
                    out.insert(pd.name.clone());
                }
            }
            if let Some(sub) = &pd.sub_signature {
                Self::collect_named_param_keys(sub, out);
            }
        }
    }

    fn has_named_slurpy_param(param_defs: &[ParamDef]) -> bool {
        for pd in param_defs {
            if pd.slurpy && pd.name.starts_with('%') {
                return true;
            }
            if let Some(sub) = &pd.sub_signature
                && Self::has_named_slurpy_param(sub)
            {
                return true;
            }
        }
        false
    }

    fn capture_to_call_args(value: &Value) -> Vec<Value> {
        match value {
            Value::Capture { positional, named } => {
                let mut args = positional.clone();
                for (k, v) in named {
                    args.push(Value::Pair(k.clone(), Box::new(v.clone())));
                }
                args
            }
            other => vec![other.clone()],
        }
    }

    fn candidate_matches_call_args(&mut self, candidate: &Value, args: &[Value]) -> bool {
        match candidate {
            Value::Sub(data) => {
                if data.empty_sig && !args.is_empty() {
                    return false;
                }
                if data.param_defs.is_empty() && !data.params.is_empty() {
                    if args.iter().any(|arg| {
                        matches!(
                            arg,
                            Value::Pair(key, _) if key != "__mutsu_test_callsite_line"
                        )
                    }) {
                        return false;
                    }
                    let positional = args
                        .iter()
                        .filter(|arg| !matches!(arg, Value::Pair(_, _) | Value::ValuePair(_, _)))
                        .count();
                    return positional == data.params.len();
                }
                self.method_args_match(args, &data.param_defs)
            }
            Value::WeakSub(weak) => weak
                .upgrade()
                .is_some_and(|strong| self.method_args_match(args, &strong.param_defs)),
            Value::Routine { name, .. } => self.resolve_function_with_types(name, args).is_some(),
            _ => false,
        }
    }

    fn routine_candidate_subs(&self, package: &str, name: &str) -> Vec<Value> {
        let exact_local = format!("{package}::{name}");
        let exact_global = format!("GLOBAL::{name}");
        let prefix_local = format!("{package}::{name}/");
        let prefix_global = format!("GLOBAL::{name}/");
        let mut seen = std::collections::HashSet::new();
        let mut out = Vec::new();
        for (key, def) in &self.functions {
            if key == &exact_local
                || key == &exact_global
                || key.starts_with(&prefix_local)
                || key.starts_with(&prefix_global)
            {
                let fp =
                    crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body);
                if seen.insert(fp) {
                    out.push(Value::make_sub(
                        def.package.clone(),
                        def.name.clone(),
                        def.params.clone(),
                        def.param_defs.clone(),
                        def.body.clone(),
                        def.is_rw,
                        self.env.clone(),
                    ));
                }
            }
        }
        out
    }

    fn sub_signature_value(&self, data: &crate::value::SubData) -> Value {
        let param_defs =
            Self::assumed_signature_param_defs(data, &data.assumed_positional, &data.assumed_named)
                .unwrap_or_else(|| {
                    if !data.params.is_empty() {
                        data.params
                            .iter()
                            .map(|name| ParamDef {
                                name: name.clone(),
                                default: None,
                                multi_invocant: true,
                                required: false,
                                named: false,
                                slurpy: false,
                                double_slurpy: false,
                                sigilless: false,
                                type_constraint: None,
                                literal_value: None,
                                sub_signature: None,
                                where_constraint: None,
                                traits: Vec::new(),
                                optional_marker: false,
                                outer_sub_signature: None,
                                code_signature: None,
                                is_invocant: false,
                                shape_constraints: None,
                            })
                            .collect()
                    } else {
                        let (use_positional, use_named) = Self::auto_signature_uses(&data.body);
                        let mut defs = Vec::new();
                        if use_positional {
                            defs.push(ParamDef {
                                name: "@_".to_string(),
                                default: None,
                                multi_invocant: true,
                                required: false,
                                named: false,
                                slurpy: true,
                                double_slurpy: false,
                                sigilless: false,
                                type_constraint: None,
                                literal_value: None,
                                sub_signature: None,
                                where_constraint: None,
                                traits: Vec::new(),
                                optional_marker: false,
                                outer_sub_signature: None,
                                code_signature: None,
                                is_invocant: false,
                                shape_constraints: None,
                            });
                        }
                        if use_named {
                            defs.push(ParamDef {
                                name: "%_".to_string(),
                                default: None,
                                multi_invocant: true,
                                required: false,
                                named: false,
                                slurpy: true,
                                double_slurpy: false,
                                sigilless: false,
                                type_constraint: None,
                                literal_value: None,
                                sub_signature: None,
                                where_constraint: None,
                                traits: Vec::new(),
                                optional_marker: false,
                                outer_sub_signature: None,
                                code_signature: None,
                                is_invocant: false,
                                shape_constraints: None,
                            });
                        }
                        defs
                    }
                });
        let return_type = data.env.get("__mutsu_return_type").and_then(|v| match v {
            Value::Str(s) => Some(s.clone()),
            _ => None,
        });
        let info = param_defs_to_sig_info(&param_defs, return_type);
        make_signature_value(info)
    }

    fn shaped_dims_from_new_args(&self, args: &[Value]) -> Option<Vec<usize>> {
        let shape_val = args.iter().find_map(|arg| match arg {
            Value::Pair(name, value) if name == "shape" => Some(value.as_ref().clone()),
            _ => None,
        })?;
        let dims_vals = match shape_val {
            Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => items.to_vec(),
            Value::Int(i) => vec![Value::Int(i)],
            Value::Package(ref name) => {
                // Enum type as shape: use the number of enum variants
                if let Some(variants) = self.enum_types.get(name) {
                    vec![Value::Int(variants.len() as i64)]
                } else if name == "Bool" {
                    // Bool is a builtin enum with 2 values (False, True)
                    vec![Value::Int(2)]
                } else {
                    return None;
                }
            }
            _ => return None,
        };
        let mut dims = Vec::with_capacity(dims_vals.len());
        for dim in &dims_vals {
            let n = match dim {
                Value::Int(i) if *i >= 0 => *i as usize,
                Value::Num(f) if *f >= 0.0 => *f as usize,
                Value::Package(name) => {
                    if let Some(variants) = self.enum_types.get(name) {
                        variants.len()
                    } else if name == "Bool" {
                        2
                    } else {
                        return None;
                    }
                }
                _ => return None,
            };
            dims.push(n);
        }
        if dims.is_empty() { None } else { Some(dims) }
    }

    fn make_shaped_array(dims: &[usize]) -> Value {
        if dims.is_empty() {
            return Value::Nil;
        }
        let len = dims[0];
        if dims.len() == 1 {
            let value = Value::real_array(vec![Value::Nil; len]);
            crate::runtime::utils::mark_shaped_array(&value, Some(dims));
            return value;
        }
        let child = Self::make_shaped_array(&dims[1..]);
        crate::runtime::utils::mark_shaped_array(&child, Some(&dims[1..]));
        let value = Value::real_array((0..len).map(|_| child.clone()).collect());
        crate::runtime::utils::mark_shaped_array(&value, Some(dims));
        value
    }

    fn infer_array_shape(value: &Value) -> Option<Vec<usize>> {
        if let Some(shape) = crate::runtime::utils::shaped_array_shape(value) {
            return Some(shape);
        }
        let Value::Array(items, ..) = value else {
            return None;
        };
        let mut shape = vec![items.len()];
        let mut current = items.first().cloned();
        while let Some(Value::Array(inner, ..)) = current {
            shape.push(inner.len());
            current = inner.first().cloned();
        }
        Some(shape)
    }

    fn parse_parametric_type_name(name: &str) -> Option<(String, Vec<String>)> {
        let base_end = name.find('[')?;
        if !name.ends_with(']') {
            return None;
        }
        let base = name[..base_end].trim().to_string();
        let inner = &name[base_end + 1..name.len() - 1];
        let args = inner
            .split(',')
            .map(|part| part.trim())
            .filter(|part| !part.is_empty())
            .map(ToOwned::to_owned)
            .collect::<Vec<_>>();
        Some((base, args))
    }

    pub(crate) fn call_method_with_values(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &target
            && class_name == "Format"
        {
            let fmt = attributes
                .get("format")
                .map(Value::to_string_value)
                .unwrap_or_default();
            match method {
                "CALL-ME" => {
                    return Ok(Value::Str(super::sprintf::format_sprintf_args(&fmt, &args)));
                }
                "Str" | "gist" => return Ok(Value::Str(fmt)),
                _ => {}
            }
        }
        if matches!(method, "max" | "min" | "lines")
            && matches!(&target, Value::Package(name) if name == "Supply")
        {
            return Err(RuntimeError::new(format!(
                "Cannot call .{} on a Supply type object",
                method
            )));
        }
        if let Value::Array(items, is_array) = &target {
            match (method, args.as_slice()) {
                ("EXISTS-POS", [idx]) => {
                    let index = match idx {
                        Value::Int(i) if *i >= 0 => Some(*i as usize),
                        Value::Num(f) if *f >= 0.0 => Some(*f as usize),
                        _ => None,
                    };
                    return Ok(Value::Bool(index.is_some_and(|i| i < items.len())));
                }
                ("ASSIGN-POS", [idx, value]) => {
                    let index = match idx {
                        Value::Int(i) if *i >= 0 => Some(*i as usize),
                        Value::Num(f) if *f >= 0.0 => Some(*f as usize),
                        _ => None,
                    };
                    let Some(index) = index else {
                        return Ok(Value::Nil);
                    };

                    if !matches!(value, Value::Nil)
                        && let Some((var_name, constraint)) =
                            self.env.iter().find_map(|(name, bound)| {
                                if let Value::Array(existing, ..) = bound
                                    && std::sync::Arc::ptr_eq(existing, items)
                                    && let Some(constraint) = self.var_type_constraint(name)
                                {
                                    return Some((name.clone(), constraint));
                                }
                                None
                            })
                        && !self.type_matches_value(&constraint, value)
                    {
                        return Err(RuntimeError::new(format!(
                            "X::TypeCheck::Assignment: Type check failed in assignment to '{}'; expected {}, got {}",
                            var_name,
                            constraint,
                            crate::runtime::utils::value_type_name(value)
                        )));
                    }

                    let mut updated = items.to_vec();
                    if index >= updated.len() {
                        updated.resize(index + 1, Value::Package("Any".to_string()));
                    }
                    updated[index] = value.clone();
                    self.overwrite_array_bindings_by_identity(
                        items,
                        Value::Array(std::sync::Arc::new(updated), *is_array),
                    );
                    return Ok(value.clone());
                }
                ("BIND-POS", [_, _]) => {
                    return Err(RuntimeError::new("Cannot bind to a natively typed array"));
                }
                ("DELETE-POS", [_]) => {
                    return Err(RuntimeError::new(
                        "Cannot delete from a natively typed array",
                    ));
                }
                ("clone", _) => {
                    let cloned = items.to_vec();
                    return Ok(Value::Array(Arc::new(cloned), *is_array));
                }
                _ => {}
            }
        }

        if let Value::Mixin(inner, mixins) = &target {
            if args.is_empty() {
                if let Some(mixin_val) = mixins.get(method) {
                    return Ok(mixin_val.clone());
                }
                for mixin_val in mixins.values() {
                    if let Value::Enum { enum_type, key, .. } = mixin_val {
                        if method == key {
                            return Ok(Value::Bool(true));
                        }
                        if let Some(variants) = self.enum_types.get(enum_type)
                            && variants.iter().any(|(variant, _)| variant == method)
                        {
                            return Ok(Value::Bool(false));
                        }
                    }
                }
            }
            let mut role_names: Vec<String> = mixins
                .iter()
                .filter_map(|(key, value)| {
                    key.strip_prefix("__mutsu_role__")
                        .and_then(|name| value.truthy().then_some(name.to_string()))
                })
                .collect();
            role_names.sort();
            let mut role_has_method = false;
            for role_name in role_names {
                let Some(role) = self.roles.get(&role_name).cloned() else {
                    continue;
                };
                let Some(overloads) = role.methods.get(method).cloned() else {
                    continue;
                };
                role_has_method = true;
                for def in overloads {
                    if def.is_private || !self.method_args_match(&args, &def.param_defs) {
                        continue;
                    }
                    let role_attrs: HashMap<String, Value> = mixins
                        .iter()
                        .filter_map(|(key, value)| {
                            key.strip_prefix("__mutsu_attr__")
                                .map(|attr| (attr.to_string(), value.clone()))
                        })
                        .collect();
                    let role_param_bindings: Vec<(String, Value)> = mixins
                        .iter()
                        .filter_map(|(key, value)| {
                            key.strip_prefix("__mutsu_role_param__")
                                .map(|name| (name.to_string(), value.clone()))
                        })
                        .collect();
                    let mut saved_role_params: Vec<(String, Option<Value>)> = Vec::new();
                    for (name, value) in &role_param_bindings {
                        saved_role_params.push((name.clone(), self.env.get(name).cloned()));
                        self.env.insert(name.clone(), value.clone());
                    }
                    let method_result = self.run_instance_method_resolved(
                        &role_name,
                        &role_name,
                        def,
                        role_attrs,
                        args,
                        Some(target.clone()),
                    );
                    for (name, previous) in saved_role_params {
                        if let Some(prev) = previous {
                            self.env.insert(name, prev);
                        } else {
                            self.env.remove(&name);
                        }
                    }
                    let (result, _updated) = method_result?;
                    return Ok(result);
                }
            }
            if role_has_method {
                return Err(RuntimeError::new(format!(
                    "No matching candidates for method: {}",
                    method
                )));
            }
            if method == "can" && args.len() == 1 {
                let method_name = args[0].to_string_value();
                if mixins.contains_key(&method_name)
                    || mixins.contains_key(&format!("__mutsu_attr__{}", method_name))
                {
                    return Ok(Value::Bool(true));
                }
                for role_name in mixins.keys().filter_map(|key| {
                    key.strip_prefix("__mutsu_role__")
                        .map(|name| name.to_string())
                }) {
                    if self
                        .roles
                        .get(&role_name)
                        .is_some_and(|role| role.methods.contains_key(&method_name))
                    {
                        return Ok(Value::Bool(true));
                    }
                }
                return self.call_method_with_values((**inner).clone(), method, args);
            }
            if method == "does" && args.len() == 1 {
                let does = match &args[0] {
                    Value::Enum {
                        enum_type,
                        key: probe_key,
                        ..
                    } => matches!(
                        mixins.get(enum_type),
                        Some(Value::Enum { key, .. }) if key == probe_key
                    ),
                    Value::Package(name) | Value::Str(name) => {
                        mixins.contains_key(name)
                            || mixins.contains_key(&format!("__mutsu_role__{}", name))
                            || inner.does_check(name)
                    }
                    other => inner.does_check(&other.to_string_value()),
                };
                return Ok(Value::Bool(does));
            }
        }

        // Role type-object method punning: calling a role method on the type object
        // first instantiates the role, then dispatches the method on that instance.
        if method != "new" {
            if let Value::Package(role_name) = &target
                && let Some(role) = self.roles.get(role_name)
            {
                let is_public_attr_accessor = args.is_empty()
                    && role
                        .attributes
                        .iter()
                        .any(|(attr_name, is_public, _, _)| *is_public && attr_name == method);
                if role.methods.contains_key(method) || is_public_attr_accessor {
                    let instance = self.dispatch_new(target.clone(), Vec::new())?;
                    return self.call_method_with_values(instance, method, args);
                }
            } else if let Value::ParametricRole { base_name, .. } = &target
                && let Some(role) = self.roles.get(base_name)
            {
                let is_public_attr_accessor = args.is_empty()
                    && role
                        .attributes
                        .iter()
                        .any(|(attr_name, is_public, _, _)| *is_public && attr_name == method);
                if role.methods.contains_key(method) || is_public_attr_accessor {
                    let instance = self.dispatch_new(target.clone(), Vec::new())?;
                    return self.call_method_with_values(instance, method, args);
                }
            }
        }

        // Skip native fast path for pseudo-methods when called with quoted syntax
        let skip_pseudo = self
            .skip_pseudo_method_native
            .as_ref()
            .is_some_and(|m| m == method);
        if skip_pseudo {
            self.skip_pseudo_method_native = None;
        }
        let is_pseudo_method = matches!(
            method,
            "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
        );
        let bypass_native_fastpath = skip_pseudo
            || (matches!(method, "max" | "min")
                && matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply"))
            || (method == "Supply"
                && matches!(&target, Value::Instance { class_name, .. } if class_name == "Supplier"))
            || (!is_pseudo_method
                && matches!(&target, Value::Instance { class_name, .. } if self.has_user_method(class_name, method)));
        let native_result = if bypass_native_fastpath {
            None
        } else {
            match args.as_slice() {
                [] => crate::builtins::native_method_0arg(&target, method),
                [a] => crate::builtins::native_method_1arg(&target, method, a),
                [a, b] => crate::builtins::native_method_2arg(&target, method, a, b),
                _ => None,
            }
        };
        if let Some(result) = native_result {
            return result;
        }

        // Force LazyList and re-dispatch as Seq for methods that need element access
        if let Value::LazyList(ll) = &target
            && matches!(method, "elems" | "list" | "Array" | "Numeric" | "Int")
        {
            let items = self.force_lazy_list_bridge(ll)?;
            let seq = Value::Seq(std::sync::Arc::new(items));
            return self.call_method_with_values(seq, method, args);
        }

        // Callable introspection and wrappers for routine handles
        if let Value::Routine {
            ref name,
            ref package,
            ..
        } = target
        {
            if method == "assuming" {
                // Create a wrapper Sub that delegates to the multi-dispatch routine
                let mut sub_data = crate::value::SubData {
                    package: package.clone(),
                    name: name.clone(),
                    params: Vec::new(),
                    param_defs: Vec::new(),
                    body: vec![],
                    is_rw: false,
                    env: self.env().clone(),
                    assumed_positional: Vec::new(),
                    assumed_named: std::collections::HashMap::new(),
                    id: crate::value::next_instance_id(),
                    empty_sig: false,
                };
                // Store the routine name so call_sub_value can dispatch
                sub_data
                    .env
                    .insert("__mutsu_routine_name".to_string(), Value::Str(name.clone()));
                // Apply assumed args
                for arg in &args {
                    if let Value::Pair(key, boxed) = arg {
                        if key == "__mutsu_test_callsite_line" {
                            continue;
                        }
                        sub_data.assumed_named.insert(key.clone(), *boxed.clone());
                    } else {
                        sub_data.assumed_positional.push(arg.clone());
                    }
                }
                return Ok(Value::Sub(std::sync::Arc::new(sub_data)));
            }
            if method == "candidates" && args.is_empty() {
                return Ok(Value::array(self.routine_candidate_subs(package, name)));
            }
            if method == "cando" && args.len() == 1 {
                let call_args = Self::capture_to_call_args(&args[0]);
                let matching = self
                    .resolve_all_matching_candidates(name, &call_args)
                    .into_iter()
                    .map(|def| {
                        Value::make_sub(
                            def.package,
                            def.name,
                            def.params,
                            def.param_defs,
                            def.body,
                            def.is_rw,
                            self.env.clone(),
                        )
                    })
                    .collect();
                return Ok(Value::array(matching));
            }
            if method == "signature" && args.is_empty() {
                let candidates = self.routine_candidate_subs(package, name);
                if candidates.is_empty() {
                    let (params, param_defs) = self.callable_signature(&target);
                    let defs = if !param_defs.is_empty() {
                        param_defs
                    } else {
                        params
                            .into_iter()
                            .map(|name| ParamDef {
                                name,
                                default: None,
                                multi_invocant: true,
                                required: false,
                                named: false,
                                slurpy: false,
                                double_slurpy: false,
                                sigilless: false,
                                type_constraint: None,
                                literal_value: None,
                                sub_signature: None,
                                where_constraint: None,
                                traits: Vec::new(),
                                optional_marker: false,
                                outer_sub_signature: None,
                                code_signature: None,
                                is_invocant: false,
                                shape_constraints: None,
                            })
                            .collect()
                    };
                    let info = param_defs_to_sig_info(&defs, None);
                    return Ok(make_signature_value(info));
                }
                if candidates.len() == 1 {
                    return self.call_method_with_values(
                        candidates[0].clone(),
                        "signature",
                        Vec::new(),
                    );
                }
                let mut signatures = Vec::new();
                for candidate in candidates {
                    signatures.push(self.call_method_with_values(
                        candidate,
                        "signature",
                        Vec::new(),
                    )?);
                }
                return Ok(Value::Junction {
                    kind: JunctionKind::Any,
                    values: std::sync::Arc::new(signatures),
                });
            }
            if method == "can" {
                let method_name = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let can = matches!(
                    method_name.as_str(),
                    "assuming"
                        | "signature"
                        | "candidates"
                        | "cando"
                        | "of"
                        | "name"
                        | "raku"
                        | "perl"
                        | "Str"
                        | "gist"
                        | "can"
                );
                return Ok(Value::Bool(can));
            }
        }
        if let Value::Sub(data) = &target {
            if method == "nextwith" {
                // Tail-style dispatch: call target with caller frame and return from current frame.
                let saved_env = self.env.clone();
                if let Some(parent_env) = self.caller_env_stack.last().cloned() {
                    self.env = parent_env;
                }
                let call_result = self.call_sub_value(target.clone(), args, false);
                self.env = saved_env;
                let value = match call_result {
                    Ok(v) => v,
                    Err(e) if e.return_value.is_some() => e.return_value.unwrap(),
                    Err(e) => return Err(e),
                };
                return Err(RuntimeError {
                    return_value: Some(value),
                    ..RuntimeError::new("")
                });
            }
            if method == "assuming" {
                let mut next = (**data).clone();
                let make_failure =
                    |sub_data: &crate::value::SubData, expected: Value, symbol: String| {
                        let mut ex_attrs = std::collections::HashMap::new();
                        ex_attrs.insert("expected".to_string(), expected.clone());
                        ex_attrs.insert("symbol".to_string(), Value::Str(symbol));
                        ex_attrs.insert(
                            "payload".to_string(),
                            Value::Str(expected.to_string_value()),
                        );
                        let exception =
                            Value::make_instance("X::TypeCheck::Binding".to_string(), ex_attrs);
                        let mut failure_attrs = std::collections::HashMap::new();
                        failure_attrs.insert("exception".to_string(), exception);
                        failure_attrs.insert("handled".to_string(), Value::Bool(false));
                        let failure = Value::make_instance("Failure".to_string(), failure_attrs);
                        let mut mixins = std::collections::HashMap::new();
                        mixins.insert("Failure".to_string(), failure);
                        Value::Mixin(
                            Box::new(Value::Sub(std::sync::Arc::new(sub_data.clone()))),
                            mixins,
                        )
                    };
                let mut incoming_named = std::collections::HashMap::new();
                for arg in args {
                    if let Value::Pair(key, boxed) = arg {
                        if key == "__mutsu_test_callsite_line" {
                            continue;
                        }
                        incoming_named.insert(key.clone(), *boxed.clone());
                        next.assumed_named.insert(key, *boxed);
                    } else {
                        next.assumed_positional.push(arg);
                    }
                }
                if next.param_defs.is_empty() && !incoming_named.is_empty() {
                    return Ok(make_failure(
                        &next,
                        Value::Str("Unexpected".to_string()),
                        String::new(),
                    ));
                }
                let mut known_named: std::collections::HashSet<String> =
                    std::collections::HashSet::new();
                Self::collect_named_param_keys(&next.param_defs, &mut known_named);
                let allows_extra_named = Self::has_named_slurpy_param(&next.param_defs);
                for name in incoming_named.keys() {
                    if !allows_extra_named && !known_named.contains(name) {
                        return Ok(make_failure(
                            &next,
                            Value::Str("Unexpected".to_string()),
                            String::new(),
                        ));
                    }
                }
                for pd in next.param_defs.iter().filter(|pd| pd.named) {
                    if let Some(value) = incoming_named.get(&pd.name)
                        && let Some(constraint) = &pd.type_constraint
                        && !self.type_matches_value(constraint, value)
                    {
                        return Ok(make_failure(
                            &next,
                            Value::Package("X::TypeCheck::Assignment".to_string()),
                            format!("${}", pd.name),
                        ));
                    }
                }
                return Ok(Value::Sub(std::sync::Arc::new(next)));
            }
            if method == "candidates" && args.is_empty() {
                return Ok(Value::array(vec![target.clone()]));
            }
            if method == "cando" && args.len() == 1 {
                let call_args = Self::capture_to_call_args(&args[0]);
                let matches = if self.candidate_matches_call_args(&target, &call_args) {
                    vec![target.clone()]
                } else {
                    Vec::new()
                };
                return Ok(Value::array(matches));
            }
            if method == "signature" && args.is_empty() {
                return Ok(self.sub_signature_value(data));
            }
            if method == "of" && args.is_empty() {
                let type_name = self
                    .callable_return_type(&target)
                    .unwrap_or_else(|| "Mu".to_string());
                return Ok(Value::Package(type_name));
            }
            if method == "can" {
                let method_name = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let can = matches!(
                    method_name.as_str(),
                    "assuming"
                        | "signature"
                        | "candidates"
                        | "cando"
                        | "of"
                        | "name"
                        | "raku"
                        | "perl"
                        | "Str"
                        | "gist"
                        | "can"
                );
                return Ok(Value::Bool(can));
            }
        }
        if let Value::WeakSub(weak) = &target
            && let Some(strong) = weak.upgrade()
        {
            return self.call_method_with_values(Value::Sub(strong), method, args);
        }

        if method == "join"
            && let Value::LazyList(list) = &target
        {
            let items = self.force_lazy_list_bridge(list)?;
            return self.call_method_with_values(Value::real_array(items), method, args);
        }

        if let Some(meta_method) = method.strip_prefix('^')
            && meta_method != "name"
        {
            let how = self.call_method_with_values(target.clone(), "HOW", vec![])?;
            let mut how_args = Vec::with_capacity(args.len() + 1);
            how_args.push(target.clone());
            how_args.extend(args.clone());
            return self.call_method_with_values(how, meta_method, how_args);
        }

        if let Value::Instance { class_name, .. } = &target
            && (class_name == "Perl6::Metamodel::ClassHOW"
                || class_name == "Perl6::Metamodel::CurriedRoleHOW"
                || class_name == "Perl6::Metamodel::ParametricRoleGroupHOW")
            && matches!(
                method,
                "can"
                    | "isa"
                    | "lookup"
                    | "find_method"
                    | "add_method"
                    | "archetypes"
                    | "name"
                    | "ver"
                    | "auth"
                    | "mro"
                    | "mro_unhidden"
                    | "methods"
                    | "concretization"
                    | "curried_role"
            )
        {
            let mut how_args = args.to_vec();
            if let Value::Instance { attributes, .. } = &target
                && !matches!(
                    how_args.first(),
                    Some(Value::Package(_))
                        | Some(Value::Instance { .. })
                        | Some(Value::ParametricRole { .. })
                )
                && let Some(Value::Str(type_name)) = attributes.get("name")
            {
                how_args.insert(0, Value::Package(type_name.clone()));
            }
            return self.dispatch_classhow_method(method, how_args);
        }

        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &target
            && class_name == "Perl6::Metamodel::Archetypes"
            && method == "composable"
            && args.is_empty()
        {
            return Ok(attributes
                .get("composable")
                .cloned()
                .unwrap_or(Value::Bool(false)));
        }

        // CREATE method: allocate a bare instance without BUILD
        if method == "CREATE" && args.is_empty() {
            match &target {
                Value::CustomType {
                    how,
                    repr,
                    name,
                    id,
                    ..
                } => {
                    return Ok(Value::CustomTypeInstance {
                        type_id: *id,
                        how: how.clone(),
                        repr: repr.clone(),
                        type_name: name.clone(),
                        attributes: std::sync::Arc::new(HashMap::new()),
                        id: crate::value::next_instance_id(),
                    });
                }
                Value::Package(class_name) => {
                    return Ok(Value::make_instance(class_name.clone(), HashMap::new()));
                }
                _ => {}
            }
        }

        // Custom type method dispatch: delegate to HOW.find_method
        // Skip pseudo-methods (HOW, WHAT, DEFINITE, REPR, etc.) which are handled natively.
        if !matches!(
            method,
            "HOW"
                | "WHAT"
                | "WHO"
                | "WHY"
                | "WHICH"
                | "WHERE"
                | "DEFINITE"
                | "VAR"
                | "REPR"
                | "Str"
                | "Stringy"
                | "gist"
                | "raku"
                | "perl"
                | "say"
                | "print"
                | "put"
                | "note"
                | "new"
        ) && let Value::CustomType { ref how, .. } | Value::CustomTypeInstance { ref how, .. } =
            target
        {
            let how_clone = *how.clone();
            let found = self.call_method_with_values(
                how_clone,
                "find_method",
                vec![target.clone(), Value::Str(method.to_string())],
            );
            if let Ok(callable) = found
                && !matches!(callable, Value::Nil)
            {
                return self.eval_call_on_value(callable, vec![target.clone()]);
            }
        }

        // Primary method dispatch by name
        match method {
            "new" if matches!(&target, Value::Package(name) if name == "Failure") => {
                let mut attrs = std::collections::HashMap::new();
                attrs.insert(
                    "exception".to_string(),
                    args.first().cloned().unwrap_or(Value::Nil),
                );
                attrs.insert("handled".to_string(), Value::Bool(false));
                return Ok(Value::make_instance("Failure".to_string(), attrs));
            }
            "handled"
                if args.is_empty()
                    && matches!(&target, Value::Instance { class_name, .. } if class_name == "Failure") =>
            {
                if let Value::Instance { attributes, .. } = &target {
                    return Ok(attributes
                        .get("handled")
                        .cloned()
                        .unwrap_or(Value::Bool(false)));
                }
            }
            "are" => {
                return self.dispatch_are(target, &args);
            }
            "say" if args.is_empty() => {
                let s = format!("{}\n", crate::runtime::gist_value(&target));
                self.emit_output(&s);
                return Ok(Value::Nil);
            }
            "print" if args.is_empty() => {
                self.emit_output(&target.to_string_value());
                return Ok(Value::Nil);
            }
            "put" if args.is_empty() => {
                let s = format!("{}\n", crate::runtime::gist_value(&target));
                self.emit_output(&s);
                return Ok(Value::Nil);
            }
            "shape" if args.is_empty() => {
                if let Some(shape) = Self::infer_array_shape(&target) {
                    return Ok(Value::array(
                        shape.into_iter().map(|n| Value::Int(n as i64)).collect(),
                    ));
                }
            }
            "default" if args.is_empty() => {
                if matches!(target, Value::Array(..)) {
                    return Ok(Value::Package("Any".to_string()));
                }
            }
            "note" if args.is_empty() => {
                let content = format!("{}\n", self.render_gist_value(&target));
                self.write_to_named_handle("$*ERR", &content, false)?;
                return Ok(Value::Nil);
            }
            "return-rw" if args.is_empty() => {
                return Ok(target);
            }
            "encode" => {
                let encoding = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "utf-8".to_string());
                let input = target.to_string_value();
                let translated = self.translate_newlines_for_encode(&input);
                let bytes = self.encode_with_encoding(&translated, &encoding)?;
                let bytes_vals: Vec<Value> =
                    bytes.into_iter().map(|b| Value::Int(b as i64)).collect();
                let mut attrs = HashMap::new();
                attrs.insert("bytes".to_string(), Value::array(bytes_vals));
                return Ok(Value::make_instance("Buf".to_string(), attrs));
            }
            "decode" => {
                if let Value::Instance {
                    class_name,
                    attributes,
                    ..
                } = &target
                    && (class_name == "Buf" || class_name == "Blob")
                {
                    let encoding = args
                        .first()
                        .map(|v| v.to_string_value())
                        .unwrap_or_else(|| "utf-8".to_string());
                    let bytes = if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                        items
                            .iter()
                            .map(|v| match v {
                                Value::Int(i) => *i as u8,
                                _ => 0,
                            })
                            .collect()
                    } else {
                        Vec::new()
                    };
                    let decoded = self.decode_with_encoding(&bytes, &encoding)?;
                    let normalized = self.translate_newlines_for_decode(&decoded);
                    return Ok(Value::Str(normalized));
                }
            }
            "polymod" => {
                return self.method_polymod(&target, &args);
            }
            "VAR" if args.is_empty() => {
                // Non-container .VAR is identity. Container variables are handled in
                // call_method_mut_with_values via target variable metadata.
                return Ok(target);
            }
            "does" if args.len() == 1 => {
                let role_name = match &args[0] {
                    Value::Package(name) | Value::Str(name) => name.clone(),
                    _ => return Ok(Value::Bool(false)),
                };
                return Ok(Value::Bool(target.does_check(&role_name)));
            }
            "start" => {
                if let Some(cls) = self.promise_class_name(&target) {
                    let block = args.into_iter().next().unwrap_or(Value::Nil);
                    let promise = SharedPromise::new_with_class(cls);
                    let ret = Value::Promise(promise.clone());
                    let mut thread_interp = self.clone_for_thread();
                    std::thread::spawn(move || {
                        match thread_interp.call_sub_value(block, vec![], false) {
                            Ok(result) => {
                                let output = std::mem::take(&mut thread_interp.output);
                                let stderr = std::mem::take(&mut thread_interp.stderr_output);
                                promise.keep(result, output, stderr);
                            }
                            Err(e) => {
                                let output = std::mem::take(&mut thread_interp.output);
                                let stderr = std::mem::take(&mut thread_interp.stderr_output);
                                let error_val = if let Some(ex) = e.exception {
                                    *ex
                                } else {
                                    Value::Str(e.message)
                                };
                                promise.break_with(error_val, output, stderr);
                            }
                        }
                    });
                    return Ok(ret);
                }
            }
            "in" => {
                if let Some(cls) = self.promise_class_name(&target) {
                    let secs = args.first().map(|v| v.to_f64()).unwrap_or(0.0).max(0.0);
                    let promise = SharedPromise::new_with_class(cls);
                    let ret = Value::Promise(promise.clone());
                    std::thread::spawn(move || {
                        if secs > 0.0 {
                            std::thread::sleep(Duration::from_secs_f64(secs));
                        }
                        promise.keep(Value::Bool(true), String::new(), String::new());
                    });
                    return Ok(ret);
                }
            }
            "THREAD" => {
                if let Value::Junction { values, .. } = &target {
                    let code = args.first().cloned().unwrap_or(Value::Nil);
                    for value in values.iter() {
                        self.call_sub_value(code.clone(), vec![value.clone()], false)?;
                    }
                    return Ok(Value::Nil);
                }
            }
            "at" => {
                if let Some(cls) = self.promise_class_name(&target) {
                    let at_time = args.first().map(|v| v.to_f64()).unwrap_or(0.0);
                    let now = crate::value::current_time_secs_f64();
                    let delay = (at_time - now).max(0.0);
                    let promise = SharedPromise::new_with_class(cls);
                    let ret = Value::Promise(promise.clone());
                    std::thread::spawn(move || {
                        if delay > 0.0 {
                            std::thread::sleep(Duration::from_secs_f64(delay));
                        }
                        promise.keep(Value::Bool(true), String::new(), String::new());
                    });
                    return Ok(ret);
                }
            }
            "kept" => {
                if let Some(cls) = self.promise_class_name(&target) {
                    let value = args.into_iter().next().unwrap_or(Value::Bool(true));
                    let promise = SharedPromise::new_with_class(cls);
                    promise.keep(value, String::new(), String::new());
                    return Ok(Value::Promise(promise));
                }
            }
            "broken" => {
                if let Some(cls) = self.promise_class_name(&target) {
                    let reason_val = args
                        .into_iter()
                        .next()
                        .unwrap_or_else(|| Value::Str("Died".to_string()));
                    let promise = SharedPromise::new_with_class(cls);
                    promise.break_with(reason_val, String::new(), String::new());
                    return Ok(Value::Promise(promise));
                }
            }
            "allof" => {
                if let Some(cls) = self.promise_class_name(&target) {
                    let promise = SharedPromise::new_with_class(cls);
                    let ret = Value::Promise(promise.clone());
                    let mut promises = Vec::new();
                    for arg in &args {
                        match arg {
                            Value::Promise(p) => promises.push(p.clone()),
                            Value::Array(arr, ..) => {
                                for elem in arr.iter() {
                                    if let Value::Promise(p) = elem {
                                        promises.push(p.clone());
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                    std::thread::spawn(move || {
                        for p in &promises {
                            p.wait();
                        }
                        promise.keep(Value::Bool(true), String::new(), String::new());
                    });
                    return Ok(ret);
                }
            }
            "anyof" => {
                if let Some(cls) = self.promise_class_name(&target) {
                    let promise = SharedPromise::new_with_class(cls);
                    let ret = Value::Promise(promise.clone());
                    let mut promises = Vec::new();
                    for arg in &args {
                        match arg {
                            Value::Promise(p) => promises.push(p.clone()),
                            Value::Array(arr, ..) => {
                                for elem in arr.iter() {
                                    if let Value::Promise(p) = elem {
                                        promises.push(p.clone());
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                    std::thread::spawn(move || {
                        // Poll until any promise resolves
                        loop {
                            for p in &promises {
                                if p.status() != "Planned" {
                                    promise.keep(Value::Bool(true), String::new(), String::new());
                                    return;
                                }
                            }
                            std::thread::sleep(Duration::from_millis(1));
                        }
                    });
                    return Ok(ret);
                }
            }
            "WHAT" if args.is_empty() => {
                if let Some(info) = self.container_type_metadata(&target) {
                    if let Some(declared) = info.declared_type {
                        return Ok(Value::Package(declared));
                    }
                    match &target {
                        Value::Array(_, _) => {
                            return Ok(Value::Package(format!("Array[{}]", info.value_type)));
                        }
                        Value::Hash(_) => {
                            let name = if let Some(key_type) = info.key_type {
                                format!("Hash[{},{}]", info.value_type, key_type)
                            } else {
                                format!("Hash[{}]", info.value_type)
                            };
                            return Ok(Value::Package(name));
                        }
                        _ => {}
                    }
                }
                let type_name = match &target {
                    Value::Int(_) => "Int",
                    Value::BigInt(_) => "Int",
                    Value::Num(_) => "Num",
                    Value::Str(_) => "Str",
                    Value::Bool(_) => "Bool",
                    Value::Range(_, _) => "Range",
                    Value::RangeExcl(_, _)
                    | Value::RangeExclStart(_, _)
                    | Value::RangeExclBoth(_, _)
                    | Value::GenericRange { .. } => "Range",
                    Value::Array(_, true) => "Array",
                    Value::Array(_, false) => "List",
                    Value::LazyList(_) => "Seq",
                    Value::Hash(_) => "Hash",
                    Value::Rat(_, _) => "Rat",
                    Value::FatRat(_, _) => "FatRat",
                    Value::BigRat(_, _) => "Rat",
                    Value::Complex(_, _) => "Complex",
                    Value::Set(_) => "Set",
                    Value::Bag(_) => "Bag",
                    Value::Mix(_) => "Mix",
                    Value::Pair(_, _) | Value::ValuePair(_, _) => "Pair",
                    Value::Enum { enum_type, .. } => enum_type.as_str(),
                    Value::Nil => "Any",
                    Value::Package(name) => name.as_str(),
                    Value::Routine { is_regex: true, .. } => "Regex",
                    Value::Routine { .. } => "Sub",
                    Value::Sub(data) => match data.env.get("__mutsu_callable_type") {
                        Some(Value::Str(kind)) if kind == "Method" => "Method",
                        _ => "Sub",
                    },
                    Value::WeakSub(_) => "Sub",
                    Value::CompUnitDepSpec { .. } => "CompUnit::DependencySpecification",
                    Value::Instance { class_name, .. } => class_name.as_str(),
                    Value::Junction { .. } => "Junction",
                    Value::Regex(_) | Value::RegexWithAdverbs { .. } => "Regex",
                    Value::Version { .. } => "Version",
                    Value::Slip(_) => "Slip",
                    Value::Seq(_) => "Seq",
                    Value::Promise(_) => "Promise",
                    Value::Channel(_) => "Channel",
                    Value::Whatever => "Whatever",
                    Value::HyperWhatever => "HyperWhatever",
                    Value::Capture { .. } => "Capture",
                    Value::Uni { form, .. } => form.as_str(),
                    Value::Mixin(inner, _) => {
                        return self.call_method_with_values(*inner.clone(), "WHAT", args.clone());
                    }
                    Value::Proxy { .. } => "Proxy",
                    Value::CustomType { name, .. } => {
                        return Ok(Value::Package(name.clone()));
                    }
                    Value::CustomTypeInstance { type_name: tn, .. } => tn.as_str(),
                    Value::ParametricRole {
                        base_name,
                        type_args,
                    } => {
                        let args_str: Vec<String> = type_args
                            .iter()
                            .map(|a| match a {
                                Value::Package(n) => n.clone(),
                                Value::ParametricRole { .. } => {
                                    // Recursively get the WHAT name for nested parametric roles
                                    if let Ok(Value::Package(n)) =
                                        self.call_method_with_values(a.clone(), "WHAT", Vec::new())
                                    {
                                        // Strip surrounding parens from (Name)
                                        n.trim_start_matches('(').trim_end_matches(')').to_string()
                                    } else {
                                        a.to_string_value()
                                    }
                                }
                                _ => a.to_string_value(),
                            })
                            .collect();
                        let name = format!("{}[{}]", base_name, args_str.join(","));
                        return Ok(Value::Package(name));
                    }
                };
                let visible_type_name = if crate::value::is_internal_anon_type_name(type_name) {
                    ""
                } else {
                    type_name
                };
                return Ok(Value::Package(visible_type_name.to_string()));
            }
            "HOW" => {
                if !args.is_empty() {
                    return Err(RuntimeError::new(
                        "X::Syntax::Argument::MOPMacro: HOW does not take arguments",
                    ));
                }
                // Return custom HOW for CustomType/CustomTypeInstance
                // Check rebless map first for reblessed instances
                if let Value::CustomTypeInstance { id, .. } = &target
                    && let Some(new_how) = self.rebless_map.get(id).cloned()
                {
                    return Ok(new_how);
                }
                if let Value::CustomType { ref how, .. }
                | Value::CustomTypeInstance { ref how, .. } = target
                {
                    return Ok(*how.clone());
                }
                // Return CurriedRoleHOW for parameterized roles
                if let Value::ParametricRole {
                    base_name,
                    type_args,
                } = &target
                {
                    let args_str = type_args
                        .iter()
                        .map(|v| match v {
                            Value::Package(n) => n.clone(),
                            other => other.to_string_value(),
                        })
                        .collect::<Vec<_>>()
                        .join(",");
                    let full_name = format!("{}[{}]", base_name, args_str);
                    let mut attrs = HashMap::new();
                    attrs.insert("name".to_string(), Value::Str(full_name));
                    return Ok(Value::make_instance(
                        "Perl6::Metamodel::CurriedRoleHOW".to_string(),
                        attrs,
                    ));
                }
                // Return a meta-object (ClassHOW) for any value
                let type_name = match &target {
                    Value::Package(name) => name.clone(),
                    Value::Instance { class_name, .. } => class_name.clone(),
                    _ => {
                        // Get type name via WHAT logic
                        let tn = match &target {
                            Value::Int(_) | Value::BigInt(_) => "Int",
                            Value::Num(_) => "Num",
                            Value::Str(_) => "Str",
                            Value::Bool(_) => "Bool",
                            Value::Hash(_) => "Hash",
                            Value::Array(_, true) => "Array",
                            Value::Array(_, false) => "List",
                            Value::Nil => "Any",
                            _ => "Mu",
                        };
                        tn.to_string()
                    }
                };
                // Use ParametricRoleGroupHOW for role type objects
                let how_name = if self.roles.contains_key(&type_name) && !type_name.contains('[') {
                    "Perl6::Metamodel::ParametricRoleGroupHOW"
                } else {
                    "Perl6::Metamodel::ClassHOW"
                };
                let mut attrs = HashMap::new();
                attrs.insert("name".to_string(), Value::Str(type_name));
                return Ok(Value::make_instance(how_name.to_string(), attrs));
            }
            "WHO" if args.is_empty() => {
                if let Value::Package(name) = &target {
                    return Ok(self.package_stash_value(name));
                }
                return Ok(Value::Hash(Arc::new(HashMap::new())));
            }
            "WHY" if args.is_empty() => {
                // Return declarator doc comment attached to this type/package/sub
                let keys: Vec<String> = match &target {
                    Value::Package(name) => vec![name.clone()],
                    Value::Instance { class_name, .. } => vec![class_name.clone()],
                    Value::Sub(sub_data) => {
                        let mut k = Vec::new();
                        if !sub_data.package.is_empty() && !sub_data.name.is_empty() {
                            k.push(format!("{}::{}", sub_data.package, sub_data.name));
                        }
                        if !sub_data.name.is_empty() {
                            k.push(sub_data.name.clone());
                        }
                        k
                    }
                    _ => vec![],
                };
                for key in keys {
                    if let Some(doc) = self.doc_comments.get(&key) {
                        return Ok(Value::Str(doc.clone()));
                    }
                }
                return Ok(Value::Nil);
            }
            "^name" if args.is_empty() => {
                return Ok(Value::Str(match &target {
                    Value::Package(name) => name.clone(),
                    Value::Instance { class_name, .. } => class_name.clone(),
                    Value::Promise(p) => p.class_name(),
                    Value::ParametricRole {
                        base_name,
                        type_args,
                    } => {
                        let args_str = type_args
                            .iter()
                            .map(|v| match v {
                                Value::Package(n) => n.clone(),
                                other => other.to_string_value(),
                            })
                            .collect::<Vec<_>>()
                            .join(",");
                        format!("{}[{}]", base_name, args_str)
                    }
                    other => value_type_name(other).to_string(),
                }));
            }
            "enums" => {
                if let Value::Str(type_name) = &target
                    && let Some(variants) = self.enum_types.get(type_name)
                {
                    let mut map = HashMap::new();
                    for (k, v) in variants {
                        map.insert(k.clone(), Value::Int(*v));
                    }
                    return Ok(Value::hash(map));
                }
            }
            "subparse" | "parse" | "parsefile" => {
                if let Value::Package(ref package_name) = target {
                    return self.dispatch_package_parse(package_name, method, &args);
                }
            }
            "match" => {
                if args.is_empty() {
                    return Ok(Value::Nil);
                }
                let text = target.to_string_value();
                let mut overlap = false;
                let mut anchored_pos: Option<usize> = None;
                let mut pattern_arg: Option<&Value> = None;
                for arg in &args {
                    if let Value::Pair(key, value) = arg {
                        if (key == "ov" || key == "overlap") && value.truthy() {
                            overlap = true;
                        } else if key == "p" || key == "pos" {
                            anchored_pos = Some(value.to_f64() as usize);
                        }
                        continue;
                    }
                    if pattern_arg.is_none() {
                        pattern_arg = Some(arg);
                    }
                }
                let Some(pattern) = pattern_arg else {
                    return Ok(Value::Nil);
                };
                return match pattern {
                    Value::Regex(pat) | Value::Str(pat) => {
                        if overlap {
                            let all = self.regex_match_all_with_captures(pat, &text);
                            if all.is_empty() {
                                return Ok(Value::Nil);
                            }
                            let mut best_by_start: std::collections::BTreeMap<
                                usize,
                                RegexCaptures,
                            > = std::collections::BTreeMap::new();
                            for capture in all {
                                let key = capture.from;
                                match best_by_start.get(&key) {
                                    Some(existing) if capture.to <= existing.to => {}
                                    _ => {
                                        best_by_start.insert(key, capture);
                                    }
                                }
                            }
                            let matches = best_by_start
                                .into_values()
                                .map(|c| {
                                    Value::make_match_object_with_captures(
                                        c.matched,
                                        c.from as i64,
                                        c.to as i64,
                                        &c.positional,
                                        &c.named,
                                    )
                                })
                                .collect::<Vec<_>>();
                            return Ok(Value::array(matches));
                        }
                        // Use anchored match if :p(N) or :pos(N) is specified
                        let captures = if let Some(pos) = anchored_pos {
                            self.regex_match_with_captures_at(pat, &text, pos)
                        } else {
                            self.regex_match_with_captures(pat, &text)
                        };
                        if let Some(captures) = captures {
                            let matched = captures.matched.clone();
                            let from = captures.from as i64;
                            let to = captures.to as i64;
                            for (i, v) in captures.positional.iter().enumerate() {
                                self.env.insert(i.to_string(), Value::Str(v.clone()));
                            }
                            // Execute code blocks from regex for side effects
                            self.execute_regex_code_blocks(&captures.code_blocks);
                            let match_obj = Value::make_match_object_full(
                                matched,
                                from,
                                to,
                                &captures.positional,
                                &captures.named,
                                &captures.named_subcaps,
                                Some(&text),
                            );
                            // Set named capture env vars from match object
                            if let Value::Instance { ref attributes, .. } = match_obj
                                && let Some(Value::Hash(named_hash)) = attributes.get("named")
                            {
                                for (k, v) in named_hash.iter() {
                                    self.env.insert(format!("<{}>", k), v.clone());
                                }
                            }
                            self.env.insert("/".to_string(), match_obj.clone());
                            Ok(match_obj)
                        } else {
                            Ok(Value::Nil)
                        }
                    }
                    _ => Ok(Value::Nil),
                };
            }
            "subst" => {
                return self.dispatch_subst(target, &args);
            }
            "comb" if args.len() == 1 => {
                let text = target.to_string_value();
                let pattern = match &args[0] {
                    Value::Regex(pat) => pat.clone(),
                    Value::Str(s) => s.clone(),
                    _ => args[0].to_string_value(),
                };
                let matches = self.regex_find_all(&pattern, &text);
                let chars: Vec<char> = text.chars().collect();
                let result: Vec<Value> = matches
                    .iter()
                    .map(|(start, end)| {
                        let s: String = chars[*start..*end].iter().collect();
                        Value::Str(s)
                    })
                    .collect();
                return Ok(Value::array(result));
            }
            "IO" if args.is_empty() => {
                let s = target.to_string_value();
                if s.contains('\0') {
                    return Err(RuntimeError::new(
                        "X::IO::Null: Found null byte in pathname",
                    ));
                }
                return Ok(self.make_io_path_instance(&s));
            }
            "contains" => {
                return self.dispatch_contains(target, &args);
            }
            "starts-with" => {
                return self.dispatch_starts_with(target, &args);
            }
            "ends-with" => {
                return self.dispatch_ends_with(target, &args);
            }
            "index" => {
                return self.dispatch_index(target, &args);
            }
            "substr-eq" => {
                return self.dispatch_substr_eq(target, &args);
            }
            "substr" => {
                return self.dispatch_substr(target, &args);
            }
            "trans" => {
                return self.dispatch_trans(target, &args);
            }
            // Trig methods on user-defined types: coerce via .Numeric or .Bridge
            "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "atan2" | "sec" | "cosec"
            | "cotan" | "asec" | "acosec" | "acotan" | "sinh" | "cosh" | "tanh" | "sech"
            | "cosech" | "cotanh" | "asinh" | "acosh" | "atanh" | "asech" | "acosech"
            | "acotanh"
                if matches!(target, Value::Instance { .. }) =>
            {
                // Try .Numeric first, then .Bridge
                let coerced = if let Ok(v) =
                    self.call_method_with_values(target.clone(), "Numeric", vec![])
                {
                    v
                } else if let Ok(v) = self.call_method_with_values(target.clone(), "Bridge", vec![])
                {
                    v
                } else {
                    return Err(RuntimeError::new(format!(
                        "Cannot coerce to numeric for {}",
                        method
                    )));
                };
                return self.call_method_with_values(coerced, method, args);
            }
            // .atan2(Instance) — coerce Instance arg
            "atan2" if args.len() == 1 && matches!(&args[0], Value::Instance { .. }) => {
                let coerced_arg = self
                    .call_method_with_values(args[0].clone(), "Numeric", vec![])
                    .or_else(|_| self.call_method_with_values(args[0].clone(), "Bridge", vec![]))?;
                return self.call_method_with_values(target, "atan2", vec![coerced_arg]);
            }
            "Seq" if args.is_empty() => {
                return Ok(match target {
                    Value::Seq(_) => target,
                    Value::Array(items, ..) => Value::Seq(items),
                    Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } if class_name == "Supply" => {
                        let values = if let Some(on_demand_cb) =
                            attributes.get("on_demand_callback")
                        {
                            let emitter = Value::make_instance("Supplier".to_string(), {
                                let mut a = HashMap::new();
                                a.insert("emitted".to_string(), Value::array(Vec::new()));
                                a.insert("done".to_string(), Value::Bool(false));
                                a
                            });
                            self.supply_emit_buffer.push(Vec::new());
                            let _ = self.call_sub_value(on_demand_cb.clone(), vec![emitter], false);
                            self.supply_emit_buffer.pop().unwrap_or_default()
                        } else if let Some(Value::Array(v, ..)) = attributes.get("values") {
                            v.to_vec()
                        } else {
                            Vec::new()
                        };
                        Value::Seq(std::sync::Arc::new(values))
                    }
                    Value::LazyList(ll) => {
                        let items = Self::value_to_list(&Value::LazyList(ll));
                        Value::Seq(std::sync::Arc::new(items))
                    }
                    other => Value::Seq(std::sync::Arc::new(vec![other])),
                });
            }
            "Set" | "SetHash" if args.is_empty() => {
                return self.dispatch_to_set(target);
            }
            "Bag" | "BagHash" if args.is_empty() => {
                return self.dispatch_to_bag(target);
            }
            "Mix" | "MixHash" if args.is_empty() => {
                return self.dispatch_to_mix(target);
            }
            "Map" | "Hash" if args.is_empty() => {
                return self.dispatch_to_hash(target);
            }
            "any" | "all" | "one" | "none" if args.is_empty() => {
                let kind = match method {
                    "any" => JunctionKind::Any,
                    "all" => JunctionKind::All,
                    "one" => JunctionKind::One,
                    _ => JunctionKind::None,
                };
                let values = Self::value_to_list(&target);
                return Ok(Value::junction(kind, values));
            }
            "iterator" if args.is_empty() => {
                if matches!(&target, Value::Instance { class_name, .. } if class_name == "Iterator")
                {
                    return Ok(target);
                }
                let items = crate::runtime::utils::value_to_list(&target);
                let mut attrs = HashMap::new();
                attrs.insert("items".to_string(), Value::array(items));
                attrs.insert("index".to_string(), Value::Int(0));
                return Ok(Value::make_instance("Iterator".to_string(), attrs));
            }
            "produce" => {
                let callable = args
                    .first()
                    .cloned()
                    .ok_or_else(|| RuntimeError::new("produce expects a callable"))?;
                if !matches!(
                    target,
                    Value::Array(_, _)
                        | Value::Seq(_)
                        | Value::Slip(_)
                        | Value::LazyList(_)
                        | Value::Range(_, _)
                        | Value::RangeExcl(_, _)
                        | Value::RangeExclStart(_, _)
                        | Value::RangeExclBoth(_, _)
                        | Value::GenericRange { .. }
                        | Value::Hash(_)
                ) {
                    return Ok(target);
                }
                return self.call_function("produce", vec![callable, target]);
            }
            "map" => {
                if let Value::Instance {
                    ref class_name,
                    ref attributes,
                    ..
                } = target
                    && class_name == "Supply"
                {
                    let mapper = args.first().cloned().unwrap_or(Value::Nil);
                    let source_values =
                        if let Some(on_demand_cb) = attributes.get("on_demand_callback") {
                            let emitter = Value::make_instance("Supplier".to_string(), {
                                let mut a = HashMap::new();
                                a.insert("emitted".to_string(), Value::array(Vec::new()));
                                a.insert("done".to_string(), Value::Bool(false));
                                a
                            });
                            self.supply_emit_buffer.push(Vec::new());
                            let _ = self.call_sub_value(on_demand_cb.clone(), vec![emitter], false);
                            self.supply_emit_buffer.pop().unwrap_or_default()
                        } else {
                            attributes
                                .get("values")
                                .and_then(|v| {
                                    if let Value::Array(items, ..) = v {
                                        Some(items.to_vec())
                                    } else {
                                        None
                                    }
                                })
                                .unwrap_or_default()
                        };

                    let mut mapped_values = Vec::with_capacity(source_values.len());
                    for value in source_values {
                        mapped_values.push(self.call_sub_value(
                            mapper.clone(),
                            vec![value],
                            true,
                        )?);
                    }

                    let mut attrs = HashMap::new();
                    attrs.insert("values".to_string(), Value::array(mapped_values));
                    attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    attrs.insert(
                        "live".to_string(),
                        attributes
                            .get("live")
                            .cloned()
                            .unwrap_or(Value::Bool(false)),
                    );
                    return Ok(Value::make_instance("Supply".to_string(), attrs));
                }
                let items = Self::value_to_list(&target);
                return self.eval_map_over_items(args.first().cloned(), items);
            }
            "max" | "min" => {
                if let Value::Instance { class_name, .. } = &target
                    && class_name == "Supply"
                {
                    return self.dispatch_supply_running_extrema(target, method, &args);
                }
            }
            "minpairs" | "maxpairs" if args.is_empty() => {
                return self.dispatch_minmaxpairs(target, method);
            }
            "sort" => {
                return self.dispatch_sort(target, &args);
            }
            "unique" => {
                return self.dispatch_unique(target, &args);
            }
            "collate" if args.is_empty() => {
                return self.dispatch_collate(target);
            }
            "take" if args.is_empty() => {
                self.take_value(target.clone());
                return Ok(target);
            }
            "rotor" => {
                return self.dispatch_rotor(target, &args);
            }
            "from-list" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "Supply"
                {
                    let mut values = Vec::new();
                    for arg in &args {
                        match arg {
                            Value::Array(items, false) => {
                                values.extend(items.iter().cloned());
                            }
                            Value::Range(..)
                            | Value::RangeExcl(..)
                            | Value::RangeExclStart(..)
                            | Value::RangeExclBoth(..)
                            | Value::GenericRange { .. } => {
                                values.extend(Self::value_to_list(arg));
                            }
                            Value::Slip(items) | Value::Seq(items) => {
                                values.extend(items.iter().cloned());
                            }
                            other => values.push(other.clone()),
                        }
                    }
                    let mut attrs = HashMap::new();
                    attrs.insert("values".to_string(), Value::array(values));
                    attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    attrs.insert("live".to_string(), Value::Bool(false));
                    return Ok(Value::make_instance("Supply".to_string(), attrs));
                }
            }
            "repository-for-name" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "CompUnit::RepositoryRegistry"
                {
                    let name = args.first().map(Value::to_string_value).unwrap_or_default();
                    if let Some(prefix) = name.strip_prefix("file#") {
                        let new_args = vec![Value::Pair(
                            "prefix".to_string(),
                            Box::new(Value::Str(prefix.to_string())),
                        )];
                        return self.call_method_with_values(
                            Value::Package("CompUnit::Repository::FileSystem".to_string()),
                            "new",
                            new_args,
                        );
                    }
                    return Ok(Value::Nil);
                }
            }
            "signal" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "Supply"
                {
                    let mut attrs = HashMap::new();
                    attrs.insert("values".to_string(), Value::array(Vec::new()));
                    attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    attrs.insert("live".to_string(), Value::Bool(true));
                    attrs.insert("signals".to_string(), Value::array(args.clone()));
                    return Ok(Value::make_instance("Supply".to_string(), attrs));
                }
            }
            "on-demand" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "Supply"
                {
                    let callback = args.first().cloned().unwrap_or(Value::Nil);
                    let mut attrs = HashMap::new();
                    attrs.insert("values".to_string(), Value::array(Vec::new()));
                    attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    attrs.insert("live".to_string(), Value::Bool(false));
                    attrs.insert("on_demand_callback".to_string(), callback);
                    return Ok(Value::make_instance("Supply".to_string(), attrs));
                }
                // on-demand called on a Supply instance should die
                if let Value::Instance { ref class_name, .. } = target
                    && class_name == "Supply"
                {
                    return Err(RuntimeError::new(
                        "Cannot call on-demand on a Supply instance",
                    ));
                }
            }
            "find" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "Encoding::Registry"
                {
                    return self.dispatch_encoding_registry_find(&args);
                }
            }
            "register" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "Encoding::Registry"
                {
                    return self.dispatch_encoding_registry_register(&args);
                }
            }
            "connect" => {
                if let Value::Package(ref class_name) = target
                    && class_name == "IO::Socket::INET"
                {
                    return self.dispatch_socket_connect(&args);
                }
            }
            "new" => {
                return self.dispatch_new(target, args);
            }
            "bless" => {
                // self.bless(:attr1($val1), :attr2($val2), ...)
                // Creates a new instance of the invocant's class with attributes from named args
                let class_name = match &target {
                    Value::Package(name) => name.clone(),
                    Value::Instance { class_name, .. } => class_name.clone(),
                    _ => {
                        return Err(RuntimeError::new(
                            "bless can only be called on a class or instance",
                        ));
                    }
                };
                // Initialize with default attribute values
                let mut attributes = HashMap::new();
                if self.classes.contains_key(&class_name) {
                    for (attr_name, _is_public, default, _is_rw) in
                        self.collect_class_attributes(&class_name)
                    {
                        let val = if let Some(expr) = default {
                            self.eval_block_value(&[Stmt::Expr(expr)])?
                        } else {
                            Value::Nil
                        };
                        attributes.insert(attr_name, val);
                    }
                }
                // Override with named args from bless call
                for arg in &args {
                    if let Value::Pair(key, value) = arg {
                        attributes.insert(key.clone(), *value.clone());
                    }
                }
                // Run BUILD/TWEAK if defined
                if self.class_has_method(&class_name, "BUILD") {
                    let (_v, updated) = self.run_instance_method(
                        &class_name,
                        attributes.clone(),
                        "BUILD",
                        Vec::new(),
                        Some(Value::make_instance(class_name.clone(), attributes.clone())),
                    )?;
                    attributes = updated;
                }
                if self.class_has_method(&class_name, "TWEAK") {
                    let (_v, updated) = self.run_instance_method(
                        &class_name,
                        attributes.clone(),
                        "TWEAK",
                        Vec::new(),
                        Some(Value::make_instance(class_name.clone(), attributes.clone())),
                    )?;
                    attributes = updated;
                }
                return Ok(Value::make_instance(class_name, attributes));
            }
            "now" if args.is_empty() => {
                if let Value::Package(ref class_name) = target
                    && class_name == "DateTime"
                {
                    let secs = crate::value::current_time_secs_f64();
                    let mut attrs = HashMap::new();
                    attrs.insert("epoch".to_string(), Value::Num(secs));
                    return Ok(Value::make_instance("DateTime".to_string(), attrs));
                }
            }
            "today" if args.is_empty() => {
                if let Value::Package(ref class_name) = target
                    && class_name == "Date"
                {
                    let secs = crate::value::current_time_secs_f64() as u64;
                    let days = secs / 86_400;
                    let mut attrs = HashMap::new();
                    attrs.insert("days".to_string(), Value::Int(days as i64));
                    return Ok(Value::make_instance("Date".to_string(), attrs));
                }
            }
            "grab" => {
                if let Value::Instance {
                    ref class_name,
                    ref attributes,
                    ..
                } = target
                    && class_name == "Supply"
                {
                    let values = match attributes.get("values") {
                        Some(Value::Array(items, ..)) => items.to_vec(),
                        _ => Vec::new(),
                    };
                    let func = args.first().cloned().unwrap_or(Value::Nil);
                    let values_list = Value::array(values);
                    let result = self.eval_call_on_value(func, vec![values_list])?;
                    let result_values = Self::value_to_list(&result);
                    let mut attrs = HashMap::new();
                    attrs.insert("values".to_string(), Value::array(result_values));
                    attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    attrs.insert("live".to_string(), Value::Bool(false));
                    return Ok(Value::make_instance("Supply".to_string(), attrs));
                }
                // Class-level Supply.grab should die
                if let Value::Package(ref class_name) = target
                    && class_name == "Supply"
                {
                    return Err(RuntimeError::new(
                        "Cannot call .grab on a Supply type object",
                    ));
                }
            }
            "skip" => {
                if let Value::Instance {
                    ref class_name,
                    ref attributes,
                    ..
                } = target
                    && class_name == "Supply"
                {
                    let n = if args.is_empty() {
                        1usize
                    } else {
                        let arg = &args[0];
                        match arg {
                            Value::Int(i) => *i as usize,
                            Value::Num(f) => *f as usize,
                            Value::Str(s) => s.parse::<usize>().map_err(|_| {
                                RuntimeError::new(format!(
                                    "X::Str::Numeric: Cannot convert string '{}' to a number",
                                    s
                                ))
                            })?,
                            _ => arg.to_f64() as usize,
                        }
                    };
                    let values = match attributes.get("values") {
                        Some(Value::Array(items, ..)) => {
                            items.iter().skip(n).cloned().collect::<Vec<_>>()
                        }
                        _ => Vec::new(),
                    };
                    let mut attrs = HashMap::new();
                    attrs.insert("values".to_string(), Value::array(values));
                    attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    attrs.insert("live".to_string(), Value::Bool(false));
                    return Ok(Value::make_instance("Supply".to_string(), attrs));
                }
            }
            "grep" => {
                return self.dispatch_grep(target, &args);
            }
            "first" if !args.is_empty() => {
                return self.dispatch_first(target, &args);
            }
            "tree" if !args.is_empty() => {
                return self.dispatch_tree(target, &args);
            }
            "values" if args.is_empty() => match target {
                Value::Capture { positional, named } => {
                    let mut vals = positional.clone();
                    vals.extend(named.values().cloned());
                    return Ok(Value::array(vals));
                }
                Value::Array(items, ..) => return Ok(Value::array(items.to_vec())),
                Value::Hash(map) => return Ok(Value::array(map.values().cloned().collect())),
                Value::Pair(_, value) => return Ok(Value::array(vec![*value.clone()])),
                Value::ValuePair(_, value) => return Ok(Value::array(vec![*value.clone()])),
                _ => return Ok(Value::array(Vec::new())),
            },
            "rotate" => {
                return self.dispatch_rotate(target, &args);
            }
            _ => {}
        }

        // Enum dispatch
        if let Value::Enum {
            enum_type,
            key,
            value,
            index,
        } = &target
        {
            match method {
                "key" => return Ok(Value::Str(key.clone())),
                "value" | "Int" | "Numeric" => return Ok(Value::Int(*value)),
                "WHAT" => return Ok(Value::Package(enum_type.clone())),
                "raku" | "perl" => {
                    return Ok(Value::Str(format!("{}::{}", enum_type, key)));
                }
                "gist" | "Str" => return Ok(Value::Str(key.clone())),
                "kv" => {
                    return Ok(Value::array(vec![
                        Value::Str(key.clone()),
                        Value::Int(*value),
                    ]));
                }
                "pair" => {
                    return Ok(Value::Pair(key.clone(), Box::new(Value::Int(*value))));
                }
                "pred" => {
                    if *index == 0 {
                        return Ok(Value::Nil);
                    }
                    if let Some(variants) = self.enum_types.get(enum_type)
                        && let Some((prev_key, prev_val)) = variants.get(index - 1)
                    {
                        return Ok(Value::Enum {
                            enum_type: enum_type.clone(),
                            key: prev_key.clone(),
                            value: *prev_val,
                            index: index - 1,
                        });
                    }
                    return Ok(Value::Nil);
                }
                "succ" => {
                    if let Some(variants) = self.enum_types.get(enum_type)
                        && let Some((next_key, next_val)) = variants.get(index + 1)
                    {
                        return Ok(Value::Enum {
                            enum_type: enum_type.clone(),
                            key: next_key.clone(),
                            value: *next_val,
                            index: index + 1,
                        });
                    }
                    return Ok(Value::Nil);
                }
                _ => {}
            }
        }

        // SharedPromise dispatch
        if let Value::Promise(ref shared) = target {
            return match method {
                "result" => {
                    let status = shared.status();
                    if status == "Broken" {
                        // .result on a Broken promise throws the cause as X::AdHoc
                        let (result, _, _) = shared.wait();
                        let msg = result.to_string_value();
                        let mut attrs = HashMap::new();
                        attrs.insert("payload".to_string(), Value::Str(msg.clone()));
                        attrs.insert("message".to_string(), Value::Str(msg.clone()));
                        let ex = Value::make_instance("X::AdHoc".to_string(), attrs);
                        let mut err = RuntimeError::new(msg);
                        err.exception = Some(Box::new(ex));
                        Err(err)
                    } else {
                        // Planned blocks, Kept returns value
                        let result = shared.result_blocking();
                        // Replay deferred taps for Proc::Async results
                        if let Value::Instance {
                            ref class_name,
                            ref attributes,
                            ..
                        } = result
                            && class_name == "Proc"
                        {
                            self.replay_proc_taps(attributes);
                        }
                        Ok(result)
                    }
                }
                "status" => Ok(Value::Str(shared.status())),
                "then" => {
                    let block = args.into_iter().next().unwrap_or(Value::Nil);
                    let orig = shared.clone();
                    let new_promise = SharedPromise::new_with_class(shared.class_name());
                    let ret = Value::Promise(new_promise.clone());
                    let mut thread_interp = self.clone_for_thread();
                    std::thread::spawn(move || {
                        let (result, output, stderr) = orig.wait();
                        match thread_interp.call_sub_value(block, vec![result], false) {
                            Ok(v) => {
                                let out = std::mem::take(&mut thread_interp.output);
                                let err = std::mem::take(&mut thread_interp.stderr_output);
                                new_promise.keep(
                                    v,
                                    format!("{}{}", output, out),
                                    format!("{}{}", stderr, err),
                                );
                            }
                            Err(e) => {
                                let out = std::mem::take(&mut thread_interp.output);
                                let err = std::mem::take(&mut thread_interp.stderr_output);
                                let error_val = if let Some(ex) = e.exception {
                                    *ex
                                } else {
                                    Value::Str(e.message)
                                };
                                new_promise.break_with(
                                    error_val,
                                    format!("{}{}", output, out),
                                    format!("{}{}", stderr, err),
                                );
                            }
                        }
                    });
                    Ok(ret)
                }
                "keep" => {
                    let value = args.into_iter().next().unwrap_or(Value::Bool(true));
                    if let Err(_status) = shared.try_keep(value) {
                        let mut attrs = HashMap::new();
                        attrs.insert(
                            "message".to_string(),
                            Value::Str(
                                "Access denied to keep/break this Promise; already vowed"
                                    .to_string(),
                            ),
                        );
                        let ex = Value::make_instance("X::Promise::Vowed".to_string(), attrs);
                        let mut err = RuntimeError::new(
                            "Access denied to keep/break this Promise; already vowed".to_string(),
                        );
                        err.exception = Some(Box::new(ex));
                        Err(err)
                    } else {
                        Ok(Value::Nil)
                    }
                }
                "break" => {
                    let reason_val = args
                        .into_iter()
                        .next()
                        .unwrap_or_else(|| Value::Str("Died".to_string()));
                    if let Err(_status) = shared.try_break(reason_val) {
                        let mut attrs = HashMap::new();
                        attrs.insert(
                            "message".to_string(),
                            Value::Str(
                                "Access denied to keep/break this Promise; already vowed"
                                    .to_string(),
                            ),
                        );
                        let ex = Value::make_instance("X::Promise::Vowed".to_string(), attrs);
                        let mut err = RuntimeError::new(
                            "Access denied to keep/break this Promise; already vowed".to_string(),
                        );
                        err.exception = Some(Box::new(ex));
                        Err(err)
                    } else {
                        Ok(Value::Nil)
                    }
                }
                "cause" => {
                    let status = shared.status();
                    if status != "Broken" {
                        let mut attrs = HashMap::new();
                        attrs.insert("status".to_string(), Value::Str(status.clone()));
                        attrs.insert(
                            "message".to_string(),
                            Value::Str(format!(
                                "Can only call '.cause' on a broken promise (status: {})",
                                status
                            )),
                        );
                        let ex = Value::make_instance(
                            "X::Promise::CauseOnlyValidOnBroken".to_string(),
                            attrs,
                        );
                        let mut err = RuntimeError::new(format!(
                            "Can only call '.cause' on a broken promise (status: {})",
                            status
                        ));
                        err.exception = Some(Box::new(ex));
                        Err(err)
                    } else {
                        // Broken
                        let (result, _, _) = shared.wait();
                        // Wrap in X::AdHoc if it's a plain string
                        let cause = match &result {
                            Value::Instance { class_name, .. }
                                if class_name.contains("Exception")
                                    || class_name.starts_with("X::") =>
                            {
                                result
                            }
                            _ => {
                                let mut attrs = HashMap::new();
                                attrs.insert(
                                    "payload".to_string(),
                                    Value::Str(result.to_string_value()),
                                );
                                attrs.insert(
                                    "message".to_string(),
                                    Value::Str(result.to_string_value()),
                                );
                                Value::make_instance("X::AdHoc".to_string(), attrs)
                            }
                        };
                        Ok(cause)
                    }
                }
                "Bool" => Ok(Value::Bool(shared.is_resolved())),
                "vow" => {
                    // Return a simple Vow object
                    let mut attrs = HashMap::new();
                    attrs.insert("promise".to_string(), target.clone());
                    Ok(Value::make_instance("Promise::Vow".to_string(), attrs))
                }
                "WHAT" => Ok(Value::Package(shared.class_name())),
                "raku" | "perl" => Ok(Value::Str(format!(
                    "Promise.new(status => {})",
                    shared.status()
                ))),
                "Str" | "gist" => Ok(Value::Str(format!("Promise({})", shared.status()))),
                "isa" => {
                    let target_name = match args.first().cloned().unwrap_or(Value::Nil) {
                        Value::Package(name) => name,
                        Value::Str(name) => name,
                        other => other.to_string_value(),
                    };
                    let cn = shared.class_name();
                    let is_match = cn == target_name
                        || target_name == "Promise"
                        || target_name == "Any"
                        || target_name == "Mu"
                        || self.class_mro(&cn).contains(&target_name);
                    Ok(Value::Bool(is_match))
                }
                _ => Err(RuntimeError::new(format!(
                    "No method '{}' on Promise",
                    method
                ))),
            };
        }

        // SharedChannel dispatch
        if let Value::Channel(ref ch) = target {
            return match method {
                "send" => {
                    let value = args.into_iter().next().unwrap_or(Value::Nil);
                    ch.send(value);
                    Ok(Value::Nil)
                }
                "receive" => Ok(ch.receive()),
                "poll" => Ok(ch.poll().unwrap_or(Value::Nil)),
                "close" => {
                    ch.close();
                    Ok(Value::Nil)
                }
                "list" | "List" | "Seq" => {
                    // Drain the channel into a list (blocks until closed)
                    let mut items = Vec::new();
                    loop {
                        let val = ch.receive();
                        if val == Value::Nil && ch.closed() {
                            break;
                        }
                        items.push(val);
                    }
                    Ok(Value::array(items))
                }
                "closed" => Ok(Value::Bool(ch.closed())),
                "Bool" => Ok(Value::Bool(true)),
                "WHAT" => Ok(Value::Package("Channel".to_string())),
                "Str" | "gist" => Ok(Value::Str("Channel".to_string())),
                _ => Err(RuntimeError::new(format!(
                    "No method '{}' on Channel",
                    method
                ))),
            };
        }

        if let Value::Mixin(inner, mixins) = &target {
            if args.is_empty() {
                let attr_key = format!("__mutsu_attr__{}", method);
                if let Some(value) = mixins.get(&attr_key) {
                    return Ok(value.clone());
                }
            }
            return self.call_method_with_values(*inner.clone(), method, args);
        }

        // Instance dispatch
        if let Value::Instance {
            class_name,
            attributes,
            id: target_id,
        } = &target
        {
            if let Some(private_method_name) = method.strip_prefix('!')
                && let Some((resolved_owner, method_def)) =
                    self.resolve_private_method_any_owner(class_name, private_method_name, &args)
            {
                let (result, _updated) = self.run_instance_method_resolved(
                    class_name,
                    &resolved_owner,
                    method_def,
                    (**attributes).clone(),
                    args,
                    Some(target.clone()),
                )?;
                return Ok(result);
            }

            if let Some((owner_class, private_method_name)) = method.split_once("::")
                && let Some((resolved_owner, method_def)) = self.resolve_private_method_with_owner(
                    class_name,
                    owner_class,
                    private_method_name,
                    &args,
                )
            {
                let caller_class = self.method_class_stack.last().cloned();
                let caller_allowed = caller_class.as_deref() == Some(resolved_owner.as_str())
                    || self
                        .class_trusts
                        .get(&resolved_owner)
                        .is_some_and(|trusted| {
                            caller_class
                                .as_ref()
                                .is_some_and(|caller| trusted.contains(caller))
                        });
                if !caller_allowed {
                    return Err(RuntimeError::new("X::Method::Private::Permission"));
                }
                let (result, updated) = self.run_instance_method_resolved(
                    class_name,
                    &resolved_owner,
                    method_def,
                    (**attributes).clone(),
                    args,
                    Some(target.clone()),
                )?;
                self.overwrite_instance_bindings_by_identity(class_name, *target_id, updated);
                return Ok(result);
            }

            // IO::Spec methods
            if class_name == "IO::Spec" {
                match method {
                    "catdir" => {
                        let parts: Vec<String> = args
                            .iter()
                            .map(|a| {
                                if let Value::Array(items, ..) = a {
                                    items
                                        .iter()
                                        .map(|v| v.to_string_value())
                                        .collect::<Vec<_>>()
                                        .join("/")
                                } else {
                                    a.to_string_value()
                                }
                            })
                            .collect();
                        let joined = parts.join("/");
                        return Ok(Value::Str(joined));
                    }
                    "catfile" => {
                        let parts: Vec<String> = args
                            .iter()
                            .map(|a| {
                                if let Value::Array(items, ..) = a {
                                    items
                                        .iter()
                                        .map(|v| v.to_string_value())
                                        .collect::<Vec<_>>()
                                        .join("/")
                                } else {
                                    a.to_string_value()
                                }
                            })
                            .collect();
                        let joined = parts.join("/");
                        return Ok(Value::Str(joined));
                    }
                    "catpath" => {
                        let vol = args
                            .first()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let dir = args.get(1).map(|v| v.to_string_value()).unwrap_or_default();
                        let file = args.get(2).map(|v| v.to_string_value()).unwrap_or_default();
                        let mut result = String::new();
                        if !vol.is_empty() {
                            result.push_str(&vol);
                        }
                        if !dir.is_empty() {
                            if !result.is_empty() && !result.ends_with('/') {
                                result.push('/');
                            }
                            result.push_str(&dir);
                        }
                        if !file.is_empty() {
                            if !result.is_empty() && !result.ends_with('/') {
                                result.push('/');
                            }
                            result.push_str(&file);
                        }
                        return Ok(Value::Str(result));
                    }
                    _ => {}
                }
            }
            if class_name == "CompUnit::Repository::FileSystem" {
                match method {
                    "install" => {
                        return Err(RuntimeError::new("Cannot install on CUR::FileSystem"));
                    }
                    "need" => {
                        let short_name = match args.first() {
                            Some(Value::CompUnitDepSpec { short_name }) => short_name.clone(),
                            _ => return Ok(Value::Nil),
                        };
                        let prefix = attributes
                            .get("prefix")
                            .map(Value::to_string_value)
                            .unwrap_or_default();
                        let canonical_prefix = std::fs::canonicalize(&prefix)
                            .unwrap_or_else(|_| std::path::PathBuf::from(&prefix))
                            .to_string_lossy()
                            .to_string();
                        let cache_key =
                            format!("__mutsu_compunit::{}::{}", canonical_prefix, short_name);
                        if let Some(existing) = self.env.get(&cache_key).cloned() {
                            return Ok(existing);
                        }
                        let relative = short_name.replace("::", "/");
                        let mut found = None;
                        for ext in [".rakumod", ".pm6", ".raku", ".pm"] {
                            let candidate = std::path::Path::new(&canonical_prefix)
                                .join(format!("{relative}{ext}"));
                            if candidate.exists() {
                                found = Some(candidate);
                                break;
                            }
                        }
                        if found.is_none() {
                            return Ok(Value::Nil);
                        }
                        let mut attrs = HashMap::new();
                        attrs.insert("from".to_string(), Value::Str("Raku".to_string()));
                        attrs.insert("short-name".to_string(), Value::Str(short_name));
                        attrs.insert("precompiled".to_string(), Value::Bool(false));
                        let compunit = Value::make_instance("CompUnit".to_string(), attrs);
                        self.env.insert(cache_key, compunit.clone());
                        return Ok(compunit);
                    }
                    _ => {}
                }
            }
            if method == "can" {
                let method_name = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                if method_name.is_empty() {
                    return Ok(Value::Bool(false));
                }
                let can = self.class_has_method(class_name, &method_name)
                    || self.has_user_method(class_name, &method_name)
                    || matches!(
                        method_name.as_str(),
                        "isa" | "gist" | "raku" | "perl" | "name" | "clone"
                    );
                return Ok(Value::Bool(can));
            }
            if self.is_native_method(class_name, method) {
                return self.call_native_instance_method(class_name, attributes, method, args);
            }
            if method == "isa" {
                let target_name = match args.first().cloned().unwrap_or(Value::Nil) {
                    Value::Package(name) => name,
                    Value::Str(name) => name,
                    Value::Instance { class_name, .. } => class_name,
                    other => other.to_string_value(),
                };
                return Ok(Value::Bool(
                    self.class_mro(class_name).contains(&target_name),
                ));
            }
            if method == "gist"
                && args.is_empty()
                && (class_name.starts_with("X::")
                    || class_name == "Exception"
                    || class_name.ends_with("Exception"))
                && let Some(msg) = attributes.get("message")
            {
                return Ok(Value::Str(msg.to_string_value()));
            }
            if (method == "raku" || method == "perl") && args.is_empty() {
                if class_name == "ObjAt" {
                    let which = attributes
                        .get("WHICH")
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    return Ok(Value::Str(format!("ObjAt.new(\"{}\")", which)));
                }
                return Ok(Value::Str(format!("{}.new()", class_name)));
            }
            if method == "name" && args.is_empty() {
                return Ok(attributes.get("name").cloned().unwrap_or(Value::Nil));
            }
            if method == "clone" {
                let mut attrs: HashMap<String, Value> = (**attributes).clone();
                for arg in &args {
                    if let Value::Pair(key, boxed) = arg {
                        attrs.insert(key.clone(), *boxed.clone());
                    }
                }
                return Ok(Value::make_instance(class_name.clone(), attrs));
            }
            // User-defined methods take priority over auto-generated accessors
            if self.has_user_method(class_name, method) {
                let (result, updated) = self.run_instance_method(
                    class_name,
                    (**attributes).clone(),
                    method,
                    args,
                    Some(target.clone()),
                )?;
                self.overwrite_instance_bindings_by_identity(
                    class_name,
                    *target_id,
                    updated.clone(),
                );
                // Auto-FETCH if the method returned a Proxy
                if let Value::Proxy { ref fetcher, .. } = result {
                    return self.proxy_fetch(fetcher, None, class_name, &updated, *target_id);
                }
                return Ok(result);
            }
            // Fallback: auto-generated accessor for public attributes
            if args.is_empty() {
                let class_attrs = self.collect_class_attributes(class_name);
                if class_attrs.is_empty() {
                    if let Some(val) = attributes.get(method) {
                        return Ok(val.clone());
                    }
                } else {
                    for (attr_name, is_public, _, _) in &class_attrs {
                        if *is_public && attr_name == method {
                            return Ok(attributes.get(method).cloned().unwrap_or(Value::Nil));
                        }
                    }
                }
            }
        }

        // Package (type object) dispatch — private method call
        if let Value::Package(ref name) = target {
            if let Some(private_method_name) = method.strip_prefix('!')
                && let Some((resolved_owner, method_def)) =
                    self.resolve_private_method_any_owner(name, private_method_name, &args)
            {
                let attrs = HashMap::new();
                let (result, _updated) = self.run_instance_method_resolved(
                    name,
                    &resolved_owner,
                    method_def,
                    attrs,
                    args,
                    Some(target.clone()),
                )?;
                return Ok(result);
            }
            // Package (type object) dispatch — check user-defined methods
            if self.has_user_method(name, method) {
                let attrs = HashMap::new();
                let (result, _updated) =
                    self.run_instance_method(name, attrs, method, args, None)?;
                return Ok(result);
            }
        }

        // Value-type dispatch for user-defined methods (e.g. `augment class Array/Hash/List`).
        // Non-instance values still need to find methods declared on their type object.
        if !matches!(target, Value::Instance { .. } | Value::Package(_)) {
            let class_name = crate::runtime::utils::value_type_name(&target);
            let dispatch_class = if self.has_user_method(class_name, method) {
                Some(class_name)
            } else if matches!(target, Value::Array(_, false))
                && self.has_user_method("Array", method)
            {
                // @-sigiled values are list-like internally, but augmenting Array methods
                // should still apply to them.
                Some("Array")
            } else {
                None
            };
            if let Some(dispatch_class) = dispatch_class {
                let attrs = HashMap::new();
                let (result, _updated) = self.run_instance_method(
                    dispatch_class,
                    attrs,
                    method,
                    args,
                    Some(target.clone()),
                )?;
                return Ok(result);
            }
        }

        // .can for Package values
        if method == "can"
            && !args.is_empty()
            && let Value::Package(ref class_name) = target
        {
            let method_name = args[0].to_string_value();
            if (self.class_has_method(class_name, &method_name)
                || self.has_user_method(class_name, &method_name))
                && let Some(val) = self.classhow_find_method(&target, &method_name)
            {
                return Ok(Value::array(vec![val]));
            }
            return Ok(Value::array(Vec::new()));
        }

        // Wildcard delegation (`handles *`) and FALLBACK method dispatch.
        // Dispatch order: wildcard delegation → FALLBACK → built-in fallbacks → error.
        {
            let fallback_class = match &target {
                Value::Instance { class_name, .. } => Some(class_name.clone()),
                Value::Package(name) => Some(name.clone()),
                _ => None,
            };
            if let Some(ref class_name) = fallback_class {
                // Try wildcard delegation: forward to delegate attribute's object
                let wildcard_attrs = self.collect_wildcard_handles(class_name);
                if let Value::Instance { attributes, .. } = &target {
                    for attr_var in &wildcard_attrs {
                        let attr_key = attr_var.trim_start_matches('!').trim_start_matches('.');
                        if let Some(delegate) = attributes.get(attr_key) {
                            // Try calling the method on the delegate; if it succeeds, return
                            match self.call_method_with_values(
                                delegate.clone(),
                                method,
                                args.clone(),
                            ) {
                                Ok(val) => return Ok(val),
                                Err(_) => continue, // delegate doesn't handle it either
                            }
                        }
                    }
                }

                // Try user-defined FALLBACK method
                if method != "FALLBACK" && self.has_user_method(class_name, "FALLBACK") {
                    let mut fallback_args = vec![Value::Str(method.to_string())];
                    fallback_args.extend(args);
                    return self.call_method_with_values(target, "FALLBACK", fallback_args);
                }
            }
        }

        // Fallback methods
        match method {
            "DUMP" if args.is_empty() => match target {
                Value::Package(name) => Ok(Value::Str(format!("{}()", name))),
                other => Ok(Value::Str(other.to_string_value())),
            },
            "gist" if args.is_empty() => match target {
                Value::Package(name) => {
                    if crate::value::is_internal_anon_type_name(&name) {
                        return Ok(Value::Str("()".to_string()));
                    }
                    let short = name.split("::").last().unwrap_or(&name);
                    Ok(Value::Str(format!("({})", short)))
                }
                other => Ok(Value::Str(other.to_string_value())),
            },
            "WHERE" if args.is_empty() => {
                if let Value::Package(name) | Value::Str(name) = target {
                    if !self.roles.contains_key(&name) {
                        return Err(RuntimeError::new(format!(
                            "X::Method::NotFound: Unknown method value dispatch (fallback disabled): {}",
                            method
                        )));
                    }
                    Ok(Value::Str(format!("{}|type-object", name)))
                } else {
                    Err(RuntimeError::new(format!(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): {}",
                        method
                    )))
                }
            }
            "raku" | "perl" if args.is_empty() => match target {
                Value::Package(name) => Ok(Value::Str(name.clone())),
                other => Ok(Value::Str(other.to_string_value())),
            },
            "name" if args.is_empty() => match target {
                Value::Routine { name, .. } => Ok(Value::Str(name)),
                Value::Package(name) => Ok(Value::Str(name)),
                Value::Str(name) => Ok(Value::Str(name)),
                Value::Sub(data) => Ok(Value::Str(data.name.clone())),
                _ => Ok(Value::Nil),
            },
            "REPR" if args.is_empty() => match target {
                Value::CustomType { repr, .. } | Value::CustomTypeInstance { repr, .. } => {
                    Ok(Value::Str(repr))
                }
                Value::Package(name) if self.classes.contains_key(&name) => {
                    Ok(Value::Str("P6opaque".to_string()))
                }
                _ => Ok(Value::Str("P6opaque".to_string())),
            },
            "Str" | "Stringy" if args.is_empty() => match target {
                Value::Package(_) => Ok(Value::Str(String::new())),
                _ => Ok(Value::Str(target.to_string_value())),
            },
            "Numeric" | "Real" if args.is_empty() => {
                if let Value::Package(name) | Value::Str(name) = target {
                    if self.roles.contains_key(&name) {
                        Ok(Value::Int(0))
                    } else {
                        Err(RuntimeError::new(format!(
                            "X::Method::NotFound: Unknown method value dispatch (fallback disabled): {}",
                            method
                        )))
                    }
                } else {
                    Err(RuntimeError::new(format!(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): {}",
                        method
                    )))
                }
            }
            "EVAL" if args.is_empty() => match target {
                Value::Str(code) => self.call_function("EVAL", vec![Value::Str(code)]),
                _ => Err(RuntimeError::new(
                    "X::Method::NotFound: Unknown method value dispatch (fallback disabled): EVAL",
                )),
            },
            // Metamodel::*HOW methods
            "new_type" if matches!(&target, Value::Package(n) if n.starts_with("Metamodel::")) => {
                // Metamodel::PackageHOW.new_type(name => 'Foo')
                // Returns a type object (Package) with the given name
                let name = args
                    .iter()
                    .find_map(|a| {
                        if let Value::Pair(k, v) = a {
                            if k == "name" {
                                Some(v.to_string_value())
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                    .unwrap_or_else(|| "Anon".to_string());
                Ok(Value::Package(name))
            }
            // Metamodel::Primitives static methods
            _ if matches!(&target, Value::Package(n) if n == "Metamodel::Primitives") => {
                self.metamodel_primitives_dispatch(method, args)
            }
            _ => {
                // Method calls on callables compose by applying the method to the
                // callable's return value, e.g. `(*-*).abs`.
                if matches!(&target, Value::Sub(_) | Value::WeakSub(_)) {
                    use crate::ast::{Expr, Stmt};
                    use std::sync::atomic::{AtomicU64, Ordering};

                    static COMPOSE_METHOD_ID: AtomicU64 = AtomicU64::new(2_000_000);

                    let callable = match target {
                        Value::Sub(data) => Value::Sub(data),
                        Value::WeakSub(weak) => weak
                            .upgrade()
                            .map(Value::Sub)
                            .ok_or_else(|| RuntimeError::new("Callable has been freed"))?,
                        _ => Value::Nil,
                    };
                    let params = match &callable {
                        Value::Sub(data) if !data.params.is_empty() => data.params.clone(),
                        _ => vec!["_".to_string()],
                    };

                    let mut env = HashMap::new();
                    env.insert("__method_compose_target__".to_string(), callable);
                    let call_args = params.iter().cloned().map(Expr::Var).collect();
                    let method_args = args.into_iter().map(Expr::Literal).collect();
                    let body = vec![Stmt::Expr(Expr::MethodCall {
                        target: Box::new(Expr::Call {
                            name: "__method_compose_target__".to_string(),
                            args: call_args,
                        }),
                        name: method.to_string(),
                        args: method_args,
                        modifier: None,
                        quoted: false,
                    })];
                    let id = COMPOSE_METHOD_ID.fetch_add(1, Ordering::Relaxed);
                    return Ok(Value::make_sub_with_id(
                        String::new(),
                        format!("<composed-method:{}>", method),
                        params,
                        Vec::new(),
                        body,
                        false,
                        env,
                        id,
                    ));
                }

                Err(RuntimeError::new(format!(
                    "X::Method::NotFound: Unknown method value dispatch (fallback disabled): {}",
                    method
                )))
            }
        }
    }

    fn translate_newlines_for_encode(&self, input: &str) -> String {
        match self.newline_mode {
            NewlineMode::Lf => input.to_string(),
            NewlineMode::Cr => input.replace('\n', "\r"),
            NewlineMode::Crlf => input.replace('\n', "\r\n"),
        }
    }

    fn translate_newlines_for_decode(&self, input: &str) -> String {
        match self.newline_mode {
            NewlineMode::Lf => input.to_string(),
            NewlineMode::Cr => input.replace('\r', "\n"),
            NewlineMode::Crlf => input.replace("\r\n", "\n"),
        }
    }

    fn encode_with_encoding(
        &self,
        input: &str,
        encoding_name: &str,
    ) -> Result<Vec<u8>, RuntimeError> {
        let encoding = self
            .find_encoding(encoding_name)
            .map(|e| e.name.as_str())
            .unwrap_or(encoding_name)
            .to_lowercase();

        match encoding.as_str() {
            "ascii" => Ok(input
                .chars()
                .map(|c| if (c as u32) <= 0x7F { c as u8 } else { b'?' })
                .collect()),
            "iso-8859-1" => Ok(input
                .chars()
                .map(|c| if (c as u32) <= 0xFF { c as u8 } else { b'?' })
                .collect()),
            "utf-16" | "utf-16le" => {
                Ok(input.encode_utf16().flat_map(|u| u.to_le_bytes()).collect())
            }
            "utf-16be" => Ok(input.encode_utf16().flat_map(|u| u.to_be_bytes()).collect()),
            _ => {
                if let Some(enc) = Self::lookup_encoding_rs_codec(&encoding) {
                    if matches!(encoding.as_str(), "windows-1251" | "windows-1252") {
                        return Ok(Self::encode_single_byte_with_encoding_rs(input, enc));
                    }
                    let (encoded, _used_encoding, _had_errors) = enc.encode(input);
                    return Ok(encoded.into_owned());
                }
                Ok(input.as_bytes().to_vec())
            }
        }
    }

    fn decode_with_encoding(
        &self,
        bytes: &[u8],
        encoding_name: &str,
    ) -> Result<String, RuntimeError> {
        let encoding = self
            .find_encoding(encoding_name)
            .map(|e| e.name.as_str())
            .unwrap_or(encoding_name)
            .to_lowercase();

        match encoding.as_str() {
            "ascii" => Ok(bytes
                .iter()
                .map(|b| if *b <= 0x7F { *b as char } else { '\u{FFFD}' })
                .collect()),
            "iso-8859-1" => Ok(bytes.iter().map(|b| *b as char).collect()),
            "utf-16" | "utf-16le" => {
                if !bytes.len().is_multiple_of(2) {
                    return Err(RuntimeError::new("Invalid utf-16 byte length"));
                }
                let units: Vec<u16> = bytes
                    .chunks_exact(2)
                    .map(|chunk| u16::from_le_bytes([chunk[0], chunk[1]]))
                    .collect();
                Ok(String::from_utf16_lossy(&units))
            }
            "utf-16be" => {
                if !bytes.len().is_multiple_of(2) {
                    return Err(RuntimeError::new("Invalid utf-16be byte length"));
                }
                let units: Vec<u16> = bytes
                    .chunks_exact(2)
                    .map(|chunk| u16::from_be_bytes([chunk[0], chunk[1]]))
                    .collect();
                Ok(String::from_utf16_lossy(&units))
            }
            _ => {
                if let Some(enc) = Self::lookup_encoding_rs_codec(&encoding) {
                    let (decoded, _used_encoding, _had_errors) = enc.decode(bytes);
                    return Ok(decoded.into_owned());
                }
                Ok(String::from_utf8_lossy(bytes).into_owned())
            }
        }
    }

    fn lookup_encoding_rs_codec(encoding: &str) -> Option<&'static encoding_rs::Encoding> {
        let label = match encoding {
            "windows-932" => "shift_jis",
            _ => encoding,
        };
        encoding_rs::Encoding::for_label(label.as_bytes())
    }

    fn encode_single_byte_with_encoding_rs(
        input: &str,
        enc: &'static encoding_rs::Encoding,
    ) -> Vec<u8> {
        let mut reverse = HashMap::with_capacity(256);
        for b in 0u8..=255 {
            let one = [b];
            let (decoded, _used_encoding, _had_errors) = enc.decode(&one);
            let mut chars = decoded.chars();
            if let (Some(ch), None) = (chars.next(), chars.next()) {
                reverse.insert(ch, b);
            }
        }
        input
            .chars()
            .map(|ch| reverse.get(&ch).copied().unwrap_or(b'?'))
            .collect()
    }

    fn dispatch_are(&mut self, target: Value, args: &[Value]) -> Result<Value, RuntimeError> {
        let values = Self::value_to_list(&target);
        match args {
            [] => {
                if values.is_empty() {
                    return Ok(Value::Nil);
                }
                let candidates = self.are_candidate_type_names(&values);
                for candidate in candidates {
                    if values
                        .iter()
                        .all(|value| self.are_value_matches_type(value, &candidate))
                    {
                        return Ok(Value::Package(candidate));
                    }
                }
                Ok(Value::Package("Any".to_string()))
            }
            [expected] => {
                let expected_type = self.are_expected_type_name(expected);
                for (idx, value) in values.iter().enumerate() {
                    if !self.are_value_matches_type(value, &expected_type) {
                        let actual = Self::are_actual_type_name(value);
                        let message = if values.len() == 1 {
                            format!("Expected '{}' but got '{}'", expected_type, actual)
                        } else {
                            format!(
                                "Expected '{}' but got '{}' in element {}",
                                expected_type, actual, idx
                            )
                        };
                        return Err(RuntimeError::new(message));
                    }
                }
                Ok(Value::Bool(true))
            }
            _ => Err(RuntimeError::new(
                "Method 'are' accepts zero or one argument",
            )),
        }
    }

    fn are_candidate_type_names(&mut self, values: &[Value]) -> Vec<String> {
        let mut names = Vec::new();
        for value in values {
            for candidate in self.are_specific_candidate_type_names(value) {
                if !names.contains(&candidate) {
                    names.push(candidate);
                }
            }
        }
        for fallback in ["Dateish", "Real", "Numeric", "Cool", "Any"] {
            let fallback = fallback.to_string();
            if !names.contains(&fallback) {
                names.push(fallback);
            }
        }
        names
    }

    fn are_specific_candidate_type_names(&mut self, value: &Value) -> Vec<String> {
        match value {
            Value::Package(name) => vec![name.clone()],
            Value::Instance { class_name, .. } => self.class_mro(class_name),
            _ => vec![crate::runtime::utils::value_type_name(value).to_string()],
        }
    }

    fn are_expected_type_name(&self, value: &Value) -> String {
        match value {
            Value::Package(name) => name.clone(),
            Value::Str(name) => name.clone(),
            Value::Instance { class_name, .. } => class_name.clone(),
            _ => value.to_string_value(),
        }
    }

    fn are_value_matches_type(&mut self, value: &Value, expected_type: &str) -> bool {
        match value {
            Value::Package(actual_type) => {
                actual_type == expected_type || expected_type == "Any" || expected_type == "Mu"
            }
            _ => self.type_matches_value(expected_type, value),
        }
    }

    fn are_actual_type_name(value: &Value) -> String {
        match value {
            Value::Package(name) => name.clone(),
            Value::Instance { class_name, .. } => class_name.clone(),
            _ => crate::runtime::utils::value_type_name(value).to_string(),
        }
    }

    fn dispatch_package_parse(
        &mut self,
        package_name: &str,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut source_arg: Option<String> = None;
        let mut start_rule = "TOP".to_string();
        let mut rule_args: Vec<Value> = Vec::new();
        for arg in args {
            if let Value::Pair(key, value) = arg {
                if key == "rule" || key == "token" {
                    start_rule = value.to_string_value();
                } else if key == "args" {
                    // :args(\(42)) passes a Capture; :args(42,) passes an Array
                    match value.as_ref() {
                        Value::Capture {
                            positional,
                            named: _,
                        } => {
                            rule_args = positional.clone();
                        }
                        Value::Array(arr, _) => {
                            rule_args = arr.as_ref().clone();
                        }
                        other => {
                            rule_args = vec![other.clone()];
                        }
                    }
                }
                continue;
            }
            if source_arg.is_none() {
                source_arg = Some(arg.to_string_value());
            }
        }
        let Some(source_text) = source_arg else {
            return Err(RuntimeError::new(
                "Too few positionals passed; expected 2 arguments but got 1",
            ));
        };
        let text = if method == "parsefile" {
            match std::fs::read_to_string(&source_text) {
                Ok(contents) => contents,
                Err(err) => return Err(RuntimeError::new(err.to_string())),
            }
        } else {
            source_text
        };

        let saved_package = self.current_package.clone();
        let saved_topic = self.env.get("_").cloned();
        let saved_made = self.env.get("made").cloned();
        self.env.remove("made");
        self.current_package = package_name.to_string();
        let has_start_rule =
            self.resolve_token_defs(&start_rule).is_some() || self.has_proto_token(&start_rule);
        if !has_start_rule {
            self.current_package = saved_package;
            if let Some(old_topic) = saved_topic {
                self.env.insert("_".to_string(), old_topic);
            } else {
                self.env.remove("_");
            }
            return Err(RuntimeError::new(format!(
                "X::Method::NotFound: Unknown method value dispatch (fallback disabled): {}",
                method
            )));
        }
        self.env.insert("_".to_string(), Value::Str(text.clone()));
        let result = (|| -> Result<Value, RuntimeError> {
            let pattern = match self.eval_token_call_values(&start_rule, &rule_args) {
                Ok(Some(pattern)) => pattern,
                Ok(None) => {
                    self.env.insert("/".to_string(), Value::Nil);
                    return Ok(self.parse_failure_for_pattern(&text, None));
                }
                Err(err)
                    if err
                        .message
                        .contains("No matching candidates for proto token") =>
                {
                    self.env.insert("/".to_string(), Value::Nil);
                    return Ok(self.parse_failure_for_pattern(&text, None));
                }
                Err(err) => return Err(err),
            };

            // Bind rule args to the env so code assertions { ... } can access them
            if !rule_args.is_empty()
                && let Some(def) = self
                    .resolve_token_defs(&start_rule)
                    .and_then(|defs| defs.into_iter().next())
            {
                let _ = self.bind_function_args_values(&def.param_defs, &def.params, &rule_args);
            }

            let Some(captures) = self.regex_match_with_captures(&pattern, &text) else {
                self.env.insert("/".to_string(), Value::Nil);
                return Ok(self.parse_failure_for_pattern(&text, Some(&pattern)));
            };
            self.execute_regex_code_blocks(&captures.code_blocks);
            if captures.from != 0 {
                self.env.insert("/".to_string(), Value::Nil);
                return Ok(self.parse_failure_for_pattern(&text, Some(&pattern)));
            }
            if (method == "parse" || method == "parsefile") && captures.to != text.chars().count() {
                self.env.insert("/".to_string(), Value::Nil);
                return Ok(self.make_parse_failure_value(&text, captures.to));
            }
            for (i, v) in captures.positional.iter().enumerate() {
                self.env.insert(i.to_string(), Value::Str(v.clone()));
            }
            let match_obj = Value::make_match_object_full(
                captures.matched,
                captures.from as i64,
                captures.to as i64,
                &captures.positional,
                &captures.named,
                &captures.named_subcaps,
                Some(&text),
            );
            let match_obj = if let Value::Instance {
                class_name,
                attributes,
                ..
            } = &match_obj
            {
                let mut attrs = attributes.as_ref().clone();
                if let Some(ast) = self.env.get("made").cloned() {
                    attrs.insert("ast".to_string(), ast);
                }
                Value::make_instance(class_name.clone(), attrs)
            } else {
                match_obj
            };
            // Set named capture env vars from match object
            if let Value::Instance { attributes, .. } = &match_obj
                && let Some(Value::Hash(named_hash)) = attributes.get("named")
            {
                for (k, v) in named_hash.iter() {
                    self.env.insert(format!("<{}>", k), v.clone());
                }
            }
            self.env.insert("/".to_string(), match_obj.clone());
            Ok(match_obj)
        })();

        self.current_package = saved_package;
        if let Some(old_topic) = saved_topic {
            self.env.insert("_".to_string(), old_topic);
        } else {
            self.env.remove("_");
        }
        if let Some(old_made) = saved_made {
            self.env.insert("made".to_string(), old_made);
        } else {
            self.env.remove("made");
        }
        result
    }

    fn parse_failure_for_pattern(&mut self, text: &str, pattern: Option<&str>) -> Value {
        let best_end = pattern
            .map(|pat| self.longest_complete_prefix_end(pat, text))
            .unwrap_or(0);
        self.make_parse_failure_value(text, best_end)
    }

    fn longest_complete_prefix_end(&mut self, pattern: &str, text: &str) -> usize {
        let chars: Vec<char> = text.chars().collect();
        for end in (0..=chars.len()).rev() {
            let prefix: String = chars[..end].iter().collect();
            if let Some(captures) = self.regex_match_with_captures(pattern, &prefix)
                && captures.from == 0
                && captures.to == end
            {
                return end;
            }
        }
        0
    }

    fn make_parse_failure_value(&self, text: &str, best_end: usize) -> Value {
        let chars: Vec<char> = text.chars().collect();
        let pos = best_end.saturating_sub(1).min(chars.len());
        let line_start = chars[..pos]
            .iter()
            .rposition(|ch| *ch == '\n')
            .map(|idx| idx + 1)
            .unwrap_or(0);
        let line_end = chars[pos..]
            .iter()
            .position(|ch| *ch == '\n')
            .map(|offset| pos + offset)
            .unwrap_or(chars.len());
        let pre: String = chars[line_start..pos].iter().collect();
        let post = if pos >= chars.len() || chars[pos] == '\n' {
            "<EOL>".to_string()
        } else {
            chars[pos..line_end].iter().collect()
        };
        let line = chars[..pos].iter().filter(|ch| **ch == '\n').count() as i64 + 1;

        let mut ex_attrs = HashMap::new();
        ex_attrs.insert("reason".to_string(), Value::Str("unknown".to_string()));
        ex_attrs.insert("filename".to_string(), Value::Str("<anon>".to_string()));
        ex_attrs.insert("pos".to_string(), Value::Int(pos as i64));
        ex_attrs.insert("line".to_string(), Value::Int(line));
        ex_attrs.insert("pre".to_string(), Value::Str(pre));
        ex_attrs.insert("post".to_string(), Value::Str(post));
        ex_attrs.insert("highexpect".to_string(), Value::array(Vec::new()));
        let exception = Value::make_instance("X::Syntax::Confused".to_string(), ex_attrs);

        let mut failure_attrs = HashMap::new();
        failure_attrs.insert("exception".to_string(), exception);
        failure_attrs.insert("handled".to_string(), Value::Bool(false));
        Value::make_instance("Failure".to_string(), failure_attrs)
    }

    fn classhow_lookup(&self, invocant: &Value, method_name: &str) -> Option<Value> {
        let Value::Package(class_name) = invocant else {
            return None;
        };
        let class_def = self.classes.get(class_name)?;
        let defs = class_def.methods.get(method_name)?;
        let def = defs.first()?;
        Some(Value::make_sub(
            class_name.clone(),
            method_name.to_string(),
            def.params.clone(),
            def.param_defs.clone(),
            def.body.clone(),
            def.is_rw,
            HashMap::new(),
        ))
    }

    fn classhow_find_method(&self, invocant: &Value, method_name: &str) -> Option<Value> {
        if matches!(
            method_name,
            "name"
                | "ver"
                | "auth"
                | "mro"
                | "mro_unhidden"
                | "archetypes"
                | "isa"
                | "can"
                | "lookup"
                | "find_method"
                | "add_method"
                | "methods"
                | "concretization"
                | "curried_role"
        ) {
            return Some(Value::Str(method_name.to_string()));
        }
        if let Some(value) = self.classhow_lookup(invocant, method_name) {
            return Some(value);
        }
        // CREATE is a built-in method on all types
        if method_name == "CREATE" {
            return Some(Value::Routine {
                package: "Mu".to_string(),
                name: "CREATE".to_string(),
                is_regex: false,
            });
        }
        if let Value::Package(class_name) = invocant
            && let Some(class_def) = self.classes.get(class_name)
            && class_def.native_methods.contains(method_name)
        {
            return Some(Value::Str(method_name.to_string()));
        }
        None
    }

    fn dispatch_classhow_method(
        &mut self,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "name" if args.len() == 1 => Ok(Value::Str(match &args[0] {
                Value::Package(name) => name.clone(),
                Value::Instance { class_name, .. } => class_name.clone(),
                Value::ParametricRole {
                    base_name,
                    type_args,
                } => {
                    let args_str = type_args
                        .iter()
                        .map(|v| match v {
                            Value::Package(n) => n.clone(),
                            other => other.to_string_value(),
                        })
                        .collect::<Vec<_>>()
                        .join(",");
                    format!("{}[{}]", base_name, args_str)
                }
                other => value_type_name(other).to_string(),
            })),
            "ver" if args.len() == 1 => {
                let invocant_name = match &args[0] {
                    Value::Package(name) => name.clone(),
                    Value::Instance { class_name, .. } => class_name.clone(),
                    _ => {
                        return Err(RuntimeError::new(
                            "X::Method::NotFound: Unknown method value dispatch (fallback disabled): ver",
                        ));
                    }
                };
                let Some(meta) = self.type_metadata.get(&invocant_name) else {
                    if invocant_name == "Grammar" {
                        return Ok(Self::version_from_value(Value::Str("6.e".to_string())));
                    }
                    return Err(RuntimeError::new(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): ver",
                    ));
                };
                let Some(value) = meta.get("ver").cloned() else {
                    if invocant_name == "Grammar" {
                        return Ok(Self::version_from_value(Value::Str("6.e".to_string())));
                    }
                    return Err(RuntimeError::new(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): ver",
                    ));
                };
                Ok(Self::version_from_value(value))
            }
            "auth" if args.len() == 1 => {
                let invocant_name = match &args[0] {
                    Value::Package(name) => name.clone(),
                    Value::Instance { class_name, .. } => class_name.clone(),
                    _ => {
                        return Err(RuntimeError::new(
                            "X::Method::NotFound: Unknown method value dispatch (fallback disabled): auth",
                        ));
                    }
                };
                let Some(meta) = self.type_metadata.get(&invocant_name) else {
                    return Err(RuntimeError::new(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): auth",
                    ));
                };
                let Some(value) = meta.get("auth").cloned() else {
                    return Err(RuntimeError::new(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): auth",
                    ));
                };
                Ok(Value::Str(value.to_string_value()))
            }
            "isa" if args.len() == 2 => {
                let Value::Package(class_name) = &args[0] else {
                    return Ok(Value::Bool(false));
                };
                let Value::Package(other_name) = &args[1] else {
                    return Ok(Value::Bool(false));
                };
                let is_same = class_name == other_name;
                if is_same {
                    return Ok(Value::Bool(true));
                }
                let mro = self.class_mro(class_name);
                Ok(Value::Bool(mro.iter().any(|p| p == other_name)))
            }
            "mro" if !args.is_empty() => {
                let mut include_roles = false;
                let mut include_concretizations = false;
                for arg in &args[1..] {
                    match arg {
                        Value::Pair(k, v) if k == "roles" => {
                            include_roles = v.truthy();
                        }
                        Value::Pair(k, v) if k == "concretizations" => {
                            include_concretizations = v.truthy();
                        }
                        _ => {}
                    }
                }
                if include_roles || include_concretizations {
                    let mro = self.classhow_mro_with_roles(&args[0], include_concretizations);
                    Ok(Value::array(mro))
                } else {
                    let mro = self.classhow_mro_names(&args[0]);
                    Ok(Value::array(
                        mro.into_iter().map(Value::Package).collect::<Vec<_>>(),
                    ))
                }
            }
            "archetypes" if !args.is_empty() => {
                let invocant_name = match &args[0] {
                    Value::Package(name) => name.clone(),
                    Value::Instance { class_name, .. } => class_name.clone(),
                    _ => value_type_name(&args[0]).to_string(),
                };
                let base_name = invocant_name
                    .split_once('[')
                    .map(|(base, _)| base)
                    .unwrap_or(invocant_name.as_str());
                let mut attrs = HashMap::new();
                attrs.insert(
                    "composable".to_string(),
                    Value::Bool(self.roles.contains_key(base_name)),
                );
                Ok(Value::make_instance(
                    "Perl6::Metamodel::Archetypes".to_string(),
                    attrs,
                ))
            }
            "mro_unhidden" if !args.is_empty() => {
                let mut include_roles = false;
                let mut include_concretizations = false;
                for arg in &args[1..] {
                    match arg {
                        Value::Pair(k, v) if k == "roles" => {
                            include_roles = v.truthy();
                        }
                        Value::Pair(k, v) if k == "concretizations" => {
                            include_concretizations = v.truthy();
                        }
                        _ => {}
                    }
                }
                if include_roles || include_concretizations {
                    let mro = self.classhow_mro_with_roles(&args[0], include_concretizations);
                    let filtered = self.filter_mro_unhidden(&args[0], mro);
                    Ok(Value::array(filtered))
                } else {
                    let mro = self.classhow_mro_unhidden_names(&args[0]);
                    Ok(Value::array(
                        mro.into_iter().map(Value::Package).collect::<Vec<_>>(),
                    ))
                }
            }
            "can" if args.len() >= 2 => {
                let invocant = &args[0];
                let method_name = args[1].to_string_value();
                if let Some(value) = self.classhow_find_method(invocant, &method_name) {
                    return Ok(Value::array(vec![value]));
                }
                Ok(Value::array(Vec::new()))
            }
            "lookup" if args.len() >= 2 => {
                let invocant = &args[0];
                let method_name = args[1].to_string_value();
                Ok(self
                    .classhow_lookup(invocant, &method_name)
                    .unwrap_or(Value::Nil))
            }
            "find_method" if args.len() >= 2 => {
                let invocant = &args[0];
                let method_name = args[1].to_string_value();
                Ok(self
                    .classhow_find_method(invocant, &method_name)
                    .unwrap_or(Value::Nil))
            }
            "add_method" if args.len() >= 3 => {
                let class_name = match &args[0] {
                    Value::Package(name) => name.clone(),
                    Value::Str(name) => name.clone(),
                    _ => {
                        return Err(RuntimeError::new("add_method target must be a type object"));
                    }
                };
                let method_name = args[1].to_string_value();
                let method_value = args[2].clone();
                let Value::Sub(sub_data) = method_value else {
                    return Ok(Value::Nil);
                };
                let def = MethodDef {
                    params: sub_data.params.clone(),
                    param_defs: sub_data
                        .params
                        .iter()
                        .map(|name| ParamDef {
                            name: name.clone(),
                            default: None,
                            multi_invocant: true,
                            required: false,
                            named: false,
                            slurpy: false,
                            double_slurpy: false,
                            sigilless: false,
                            type_constraint: None,
                            literal_value: None,
                            sub_signature: None,
                            where_constraint: None,
                            traits: Vec::new(),
                            optional_marker: false,
                            outer_sub_signature: None,
                            code_signature: None,
                            is_invocant: false,
                            shape_constraints: None,
                        })
                        .collect(),
                    body: sub_data.body.clone(),
                    is_rw: false,
                    is_private: false,
                    return_type: None,
                };
                if let Some(class_def) = self.classes.get_mut(&class_name) {
                    class_def.methods.insert(method_name, vec![def]);
                    return Ok(Value::Nil);
                }
                Err(RuntimeError::new(format!(
                    "Unknown class for add_method: {}",
                    class_name
                )))
            }
            "methods" if !args.is_empty() => self.dispatch_classhow_methods(&args),
            "concretization" if args.len() >= 2 => {
                let class_name = match &args[0] {
                    Value::Package(name) => name.clone(),
                    Value::Instance { class_name, .. } => class_name.clone(),
                    other => value_type_name(other).to_string(),
                };
                let role_name = match &args[1] {
                    Value::Package(name) => name.clone(),
                    Value::ParametricRole {
                        base_name,
                        type_args,
                    } => {
                        let args_str = type_args
                            .iter()
                            .map(|v| match v {
                                Value::Package(n) => n.clone(),
                                other => other.to_string_value(),
                            })
                            .collect::<Vec<_>>()
                            .join(",");
                        format!("{}[{}]", base_name, args_str)
                    }
                    _ => args[1].to_string_value(),
                };
                let base_role_name = role_name
                    .split_once('[')
                    .map(|(b, _)| b)
                    .unwrap_or(role_name.as_str());
                // Check for :local named arg
                let local_only = args[2..]
                    .iter()
                    .any(|a| matches!(a, Value::Pair(k, v) if k == "local" && v.truthy()));
                // Check direct composed roles and transitive sub-roles
                let check_transitive = |class_composed: &HashMap<String, Vec<String>>,
                                        role_parents: &HashMap<String, Vec<String>>,
                                        cn: &str|
                 -> Option<Value> {
                    let composed = class_composed.get(cn).cloned().unwrap_or_default();
                    // Check direct matches
                    for cr in &composed {
                        let cr_base = cr.split_once('[').map(|(b, _)| b).unwrap_or(cr.as_str());
                        if *cr == role_name || cr_base == base_role_name {
                            return Some(Value::Package(cr_base.to_string()));
                        }
                    }
                    // Check transitive sub-roles
                    let mut stack: Vec<String> = composed
                        .iter()
                        .map(|cr| {
                            cr.split_once('[')
                                .map(|(b, _)| b)
                                .unwrap_or(cr.as_str())
                                .to_string()
                        })
                        .collect();
                    let mut seen = std::collections::HashSet::new();
                    while let Some(rn) = stack.pop() {
                        if !seen.insert(rn.clone()) {
                            continue;
                        }
                        if let Some(rp) = role_parents.get(&rn) {
                            for p in rp {
                                let p_base =
                                    p.split_once('[').map(|(b, _)| b).unwrap_or(p.as_str());
                                if p_base == base_role_name || *p == role_name {
                                    return Some(Value::Package(p_base.to_string()));
                                }
                                stack.push(p_base.to_string());
                            }
                        }
                    }
                    None
                };
                if let Some(result) =
                    check_transitive(&self.class_composed_roles, &self.role_parents, &class_name)
                {
                    return Ok(result);
                }
                if !local_only {
                    let mro = self.class_mro(&class_name);
                    for cn in &mro[1..] {
                        if let Some(result) =
                            check_transitive(&self.class_composed_roles, &self.role_parents, cn)
                        {
                            return Ok(result);
                        }
                    }
                }
                Err(RuntimeError::new(format!(
                    "No concretization of {} found for {}",
                    role_name, class_name
                )))
            }
            "curried_role" if !args.is_empty() => {
                // For a parameterized role like R[Int], return the base role R
                match &args[0] {
                    Value::ParametricRole { base_name, .. } => {
                        Ok(Value::Package(base_name.clone()))
                    }
                    Value::Package(name) => {
                        let base = name
                            .split_once('[')
                            .map(|(b, _)| b)
                            .unwrap_or(name.as_str());
                        Ok(Value::Package(base.to_string()))
                    }
                    _ => {
                        let s = args[0].to_string_value();
                        let base = s.split_once('[').map(|(b, _)| b).unwrap_or(s.as_str());
                        Ok(Value::Package(base.to_string()))
                    }
                }
            }
            _ => Err(RuntimeError::new(format!(
                "X::Method::NotFound: Unknown method value dispatch (fallback disabled): {}",
                method
            ))),
        }
    }

    fn classhow_mro_names(&mut self, invocant: &Value) -> Vec<String> {
        let class_name = match invocant {
            Value::Package(name) => name.clone(),
            Value::Instance { class_name, .. } => class_name.clone(),
            other => value_type_name(other).to_string(),
        };
        let mut mro = if self.classes.contains_key(&class_name) {
            self.class_mro(&class_name)
        } else {
            vec![class_name.clone()]
        };
        if self.package_looks_like_grammar(&class_name) {
            for parent in ["Grammar", "Match", "Capture", "Cool", "Any", "Mu"] {
                if !mro.iter().any(|name| name == parent) {
                    mro.push(parent.to_string());
                }
            }
            return mro;
        }
        if class_name != "Mu" && !mro.iter().any(|name| name == "Any") {
            mro.push("Any".to_string());
        }
        if !mro.iter().any(|name| name == "Mu") {
            mro.push("Mu".to_string());
        }
        mro
    }

    /// Build MRO with roles interleaved (for :roles or :concretizations).
    /// For each class in the MRO, insert its composed roles right after it.
    /// For :roles mode, use base role names. For :concretizations, use as-is.
    fn classhow_mro_with_roles(&mut self, invocant: &Value, _concretizations: bool) -> Vec<Value> {
        let class_name = match invocant {
            Value::Package(name) => name.clone(),
            Value::Instance { class_name, .. } => class_name.clone(),
            other => value_type_name(other).to_string(),
        };
        let base_mro = self.classhow_mro_names(invocant);
        let mut result: Vec<Value> = Vec::new();
        let mut seen: HashSet<String> = HashSet::new();

        for entry in &base_mro {
            // Check if this entry is a role (in the parents list because of `does`)
            let base_entry = entry
                .split_once('[')
                .map(|(b, _)| b)
                .unwrap_or(entry.as_str());
            if self.roles.contains_key(base_entry)
                && entry != "Any"
                && entry != "Mu"
                && entry != &class_name
            {
                // This is a role in the class's parent list - include it with base name
                if seen.insert(base_entry.to_string()) {
                    result.push(Value::Package(base_entry.to_string()));
                    // Also add the role's parent classes that aren't already in base MRO
                    self.add_role_parents_to_mro(base_entry, &base_mro, &mut result, &mut seen);
                }
            } else {
                // This is a class
                if seen.insert(entry.clone()) {
                    result.push(Value::Package(entry.clone()));
                    // Insert composed roles for this class
                    let composed = self
                        .class_composed_roles
                        .get(entry)
                        .cloned()
                        .unwrap_or_default();
                    for role_name in &composed {
                        let base_role = role_name
                            .split_once('[')
                            .map(|(b, _)| b)
                            .unwrap_or(role_name.as_str());
                        if seen.insert(base_role.to_string()) {
                            result.push(Value::Package(base_role.to_string()));
                            // Add role's sub-roles (from `does` inside the role)
                            self.add_role_parents_to_mro(
                                base_role,
                                &base_mro,
                                &mut result,
                                &mut seen,
                            );
                        }
                    }
                }
            }
        }
        result
    }

    /// Add a role's parent roles/classes to the MRO result.
    fn add_role_parents_to_mro(
        &self,
        role_name: &str,
        _base_mro: &[String],
        result: &mut Vec<Value>,
        seen: &mut HashSet<String>,
    ) {
        if let Some(parents) = self.role_parents.get(role_name) {
            for parent in parents {
                let base = parent
                    .split_once('[')
                    .map(|(b, _)| b)
                    .unwrap_or(parent.as_str());
                if self.roles.contains_key(base) && seen.insert(base.to_string()) {
                    result.push(Value::Package(base.to_string()));
                    self.add_role_parents_to_mro(base, _base_mro, result, seen);
                }
                // Parent classes from roles are handled by the class MRO
            }
        }
    }

    /// Filter MRO to remove hidden classes and their associated roles.
    fn filter_mro_unhidden(&self, invocant: &Value, mro: Vec<Value>) -> Vec<Value> {
        let class_name = match invocant {
            Value::Package(name) => name.clone(),
            Value::Instance { class_name, .. } => class_name.clone(),
            other => value_type_name(other).to_string(),
        };
        // Collect hidden parent names for this class
        let hidden_parents: HashSet<String> = self
            .hidden_defer_parents
            .get(&class_name)
            .cloned()
            .unwrap_or_default();
        // Also collect classes marked `is hidden`
        let mut hidden_set: HashSet<String> = HashSet::new();
        for hp in &hidden_parents {
            hidden_set.insert(hp.clone());
        }
        for hc in &self.hidden_classes {
            hidden_set.insert(hc.clone());
        }
        if hidden_set.is_empty() {
            return mro;
        }
        // Build set of all entries to hide: hidden classes + their composed roles
        let mut to_hide: HashSet<String> = HashSet::new();
        for hidden in &hidden_set {
            to_hide.insert(hidden.clone());
            // Also add the base name (strip type params)
            let hidden_base = hidden
                .split_once('[')
                .map(|(b, _)| b)
                .unwrap_or(hidden.as_str());
            to_hide.insert(hidden_base.to_string());
            // Also hide roles composed by the hidden class (try both full and base name)
            let composed_full = self.class_composed_roles.get(hidden.as_str()).cloned();
            let composed_base = self.class_composed_roles.get(hidden_base).cloned();
            let composed = composed_full.or(composed_base).unwrap_or_default();
            for role in &composed {
                let base = role
                    .split_once('[')
                    .map(|(b, _)| b)
                    .unwrap_or(role.as_str());
                to_hide.insert(base.to_string());
                // And roles composed by those roles
                self.collect_hidden_roles(base, &mut to_hide);
            }
            // Also check role_parents for the hidden entry (in case it's a punned role)
            self.collect_hidden_roles(hidden_base, &mut to_hide);
        }
        mro.into_iter()
            .filter(|v| {
                if let Value::Package(name) = v {
                    !to_hide.contains(name)
                } else {
                    true
                }
            })
            .collect()
    }

    fn collect_hidden_roles(&self, role_name: &str, to_hide: &mut HashSet<String>) {
        if let Some(parents) = self.role_parents.get(role_name) {
            for parent in parents {
                let base = parent
                    .split_once('[')
                    .map(|(b, _)| b)
                    .unwrap_or(parent.as_str());
                if self.roles.contains_key(base) && to_hide.insert(base.to_string()) {
                    self.collect_hidden_roles(base, to_hide);
                }
            }
        }
    }

    /// MRO without hidden classes (no roles)
    fn classhow_mro_unhidden_names(&mut self, invocant: &Value) -> Vec<String> {
        let class_name = match invocant {
            Value::Package(name) => name.clone(),
            Value::Instance { class_name, .. } => class_name.clone(),
            other => value_type_name(other).to_string(),
        };
        let mro = self.classhow_mro_names(invocant);
        let hidden_parents: HashSet<String> = self
            .hidden_defer_parents
            .get(&class_name)
            .cloned()
            .unwrap_or_default();
        let mut hidden_set: HashSet<String> = HashSet::new();
        for hp in &hidden_parents {
            hidden_set.insert(hp.clone());
        }
        for hc in &self.hidden_classes {
            hidden_set.insert(hc.clone());
        }
        if hidden_set.is_empty() {
            return mro;
        }
        mro.into_iter()
            .filter(|name| !hidden_set.contains(name))
            .collect()
    }

    fn package_looks_like_grammar(&self, package_name: &str) -> bool {
        let prefix = format!("{package_name}::");
        self.token_defs.keys().any(|key| key.starts_with(&prefix))
    }

    fn dispatch_classhow_methods(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let invocant = &args[0];
        let class_name = match invocant {
            Value::Package(name) => name.clone(),
            Value::Instance { class_name, .. } => class_name.clone(),
            other => value_type_name(other).to_string(),
        };

        // Parse named arguments
        let mut local = false;
        let mut all = false;
        let mut private = false;
        let mut tree = false;
        for arg in &args[1..] {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "local" => local = value.truthy(),
                    "all" => all = value.truthy(),
                    "private" => private = value.truthy(),
                    "tree" => tree = value.truthy(),
                    _ => {}
                }
            }
        }

        if tree {
            return self.classhow_methods_tree(&class_name, private);
        }

        let mut result = Vec::new();

        if local {
            // Only methods defined directly on this class
            self.collect_class_methods(&class_name, private, &mut result);
        } else {
            // Walk MRO (already includes the class itself)
            let mro = self.class_mro(&class_name);

            for cn in &mro {
                if !all && (cn == "Any" || cn == "Mu") {
                    continue;
                }
                self.collect_class_methods(cn, private, &mut result);
            }

            // For built-in types that don't have class defs, add known methods
            if result.is_empty() || !self.classes.contains_key(&class_name) {
                self.collect_builtin_type_methods(&class_name, &mut result);
                if all {
                    self.collect_builtin_type_methods("Any", &mut result);
                    self.collect_builtin_type_methods("Mu", &mut result);
                }
            }
        }

        Ok(Value::array(result))
    }

    fn collect_builtin_type_methods(&self, type_name: &str, result: &mut Vec<Value>) {
        let methods: &[&str] = match type_name {
            "Str" => &[
                "chars",
                "codes",
                "comb",
                "chomp",
                "chop",
                "contains",
                "ends-with",
                "fc",
                "flip",
                "index",
                "indices",
                "lc",
                "lines",
                "match",
                "ords",
                "pred",
                "rindex",
                "split",
                "starts-with",
                "substr",
                "succ",
                "tc",
                "trim",
                "trim-leading",
                "trim-trailing",
                "uc",
                "words",
                "wordcase",
                "NFC",
                "NFD",
                "NFKC",
                "NFKD",
                "encode",
                "IO",
                "Numeric",
                "Int",
                "Num",
                "Rat",
                "Bool",
                "Str",
                "gist",
                "raku",
                "elems",
                "fmt",
            ],
            "Int" | "Num" | "Rat" | "Complex" => &[
                "abs", "ceiling", "floor", "round", "sign", "sqrt", "log", "log10", "exp", "roots",
                "is-prime", "chr", "base", "polymod", "pred", "succ", "Numeric", "Int", "Num",
                "Rat", "Bool", "Str", "gist", "raku",
            ],
            "List" | "Array" => &[
                "elems",
                "end",
                "keys",
                "values",
                "kv",
                "pairs",
                "antipairs",
                "join",
                "map",
                "grep",
                "first",
                "sort",
                "reverse",
                "rotate",
                "unique",
                "repeated",
                "squish",
                "flat",
                "eager",
                "lazy",
                "head",
                "tail",
                "skip",
                "push",
                "pop",
                "shift",
                "unshift",
                "splice",
                "append",
                "prepend",
                "classify",
                "categorize",
                "min",
                "max",
                "minmax",
                "sum",
                "pick",
                "roll",
                "permutations",
                "combinations",
                "rotor",
                "batch",
                "produce",
                "reduce",
                "Bool",
                "Str",
                "gist",
                "raku",
                "Numeric",
                "Int",
                "Array",
                "List",
            ],
            "Hash" => &[
                "elems",
                "keys",
                "values",
                "kv",
                "pairs",
                "antipairs",
                "push",
                "append",
                "classify-list",
                "categorize-list",
                "Bool",
                "Str",
                "gist",
                "raku",
                "Numeric",
                "Int",
            ],
            "Bool" => &[
                "pred", "succ", "pick", "roll", "Numeric", "Int", "Num", "Rat", "Bool", "Str",
                "gist", "raku",
            ],
            "Range" => &[
                "min", "max", "bounds", "elems", "list", "flat", "reverse", "pick", "roll", "sum",
                "rand", "minmax", "infinite", "is-int", "Bool", "Str", "gist", "raku", "Numeric",
                "Int",
            ],
            "Sub" | "Method" | "Block" | "Routine" | "Code" => &[
                "name",
                "signature",
                "arity",
                "count",
                "of",
                "returns",
                "Bool",
                "Str",
                "gist",
                "raku",
            ],
            "Signature" => &[
                "params", "arity", "count", "returns", "Bool", "Str", "gist", "raku",
            ],
            "Any" => &[
                "say",
                "put",
                "print",
                "note",
                "so",
                "not",
                "defined",
                "WHAT",
                "WHERE",
                "HOW",
                "WHY",
                "iterator",
                "flat",
                "eager",
                "lazy",
                "map",
                "grep",
                "first",
                "sort",
                "reverse",
                "unique",
                "repeated",
                "squish",
                "head",
                "tail",
                "skip",
                "min",
                "max",
                "minmax",
                "elems",
                "end",
                "keys",
                "values",
                "kv",
                "pairs",
                "antipairs",
                "classify",
                "categorize",
                "join",
                "pick",
                "roll",
                "sum",
                "reduce",
                "produce",
                "rotor",
                "batch",
                "Bool",
                "Str",
                "gist",
                "raku",
                "Numeric",
                "Int",
            ],
            "Mu" => &[
                "defined", "WHAT", "WHERE", "HOW", "WHY", "WHICH", "Bool", "Str", "gist", "raku",
                "clone", "new",
            ],
            _ => &[],
        };
        for name in methods {
            if !result.iter().any(|v| {
                if let Value::Instance { attributes, .. } = v {
                    attributes
                        .get("name")
                        .map(|n| n.to_string_value())
                        .as_deref()
                        == Some(name)
                } else {
                    false
                }
            }) {
                result.push(self.make_native_method_object(name));
            }
        }
    }

    fn collect_class_methods(
        &self,
        class_name: &str,
        include_private: bool,
        result: &mut Vec<Value>,
    ) {
        if let Some(class_def) = self.classes.get(class_name) {
            // First add accessor methods for public attributes (in order)
            for (attr_name, is_public, ..) in &class_def.attributes {
                if *is_public && !class_def.methods.contains_key(attr_name) {
                    result.push(self.make_native_method_object(attr_name));
                }
            }
            // Then add explicit methods
            for (method_name, overloads) in &class_def.methods {
                if overloads.is_empty() {
                    continue;
                }
                // Skip private methods unless :private
                let first = &overloads[0];
                if first.is_private && !include_private {
                    continue;
                }
                let is_multi = overloads.len() > 1;
                let return_type = first.return_type.clone();
                let method_obj = self.make_method_object(method_name, first, is_multi, return_type);
                result.push(method_obj);
            }
            // Also include native (built-in) methods
            for native_name in &class_def.native_methods {
                let method_obj = self.make_native_method_object(native_name);
                result.push(method_obj);
            }
        }
    }

    fn make_native_method_object(&self, name: &str) -> Value {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("name".to_string(), Value::Str(name.to_string()));
        attrs.insert("is_dispatcher".to_string(), Value::Bool(false));
        let sig_attrs = {
            let mut sa = std::collections::HashMap::new();
            sa.insert("params".to_string(), Value::array(Vec::new()));
            sa
        };
        attrs.insert(
            "signature".to_string(),
            Value::make_instance("Signature".to_string(), sig_attrs),
        );
        attrs.insert("returns".to_string(), Value::Package("Mu".to_string()));
        attrs.insert("of".to_string(), Value::Package("Mu".to_string()));
        Value::make_instance("Method".to_string(), attrs)
    }

    fn make_method_object(
        &self,
        name: &str,
        method_def: &MethodDef,
        is_dispatcher: bool,
        return_type: Option<String>,
    ) -> Value {
        let mut attrs = std::collections::HashMap::new();

        // Store the display name (with ! prefix for private methods)
        let display_name = if method_def.is_private {
            format!("!{}", name)
        } else {
            name.to_string()
        };
        attrs.insert("name".to_string(), Value::Str(display_name));
        attrs.insert("is_dispatcher".to_string(), Value::Bool(is_dispatcher));

        // Build a Signature object for this method
        let param_defs = &method_def.param_defs;
        let sig_attrs = {
            let mut sa = std::collections::HashMap::new();
            sa.insert(
                "params".to_string(),
                make_params_value_from_param_defs(param_defs),
            );
            sa
        };
        attrs.insert(
            "signature".to_string(),
            Value::make_instance("Signature".to_string(), sig_attrs),
        );

        // Return type
        let rt = return_type.unwrap_or_else(|| "Mu".to_string());
        attrs.insert("returns".to_string(), Value::Package(rt.clone()));
        attrs.insert("of".to_string(), Value::Package(rt));

        Value::make_instance("Method".to_string(), attrs)
    }

    fn classhow_methods_tree(
        &self,
        class_name: &str,
        include_private: bool,
    ) -> Result<Value, RuntimeError> {
        let mut result = Vec::new();

        // First: own methods
        self.collect_class_methods(class_name, include_private, &mut result);

        // Then: each parent's tree as a nested array
        if let Some(class_def) = self.classes.get(class_name) {
            for parent in &class_def.parents {
                let subtree = self.classhow_methods_tree(parent, include_private)?;
                result.push(subtree);
            }
        }

        Ok(Value::array(result))
    }

    fn dispatch_subst(&mut self, target: Value, args: &[Value]) -> Result<Value, RuntimeError> {
        let text = target.to_string_value();
        let mut positional: Vec<Value> = Vec::new();
        let mut global = false;
        for arg in args {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "g" | "global" => global = value.truthy(),
                    _ => {}
                }
            } else {
                positional.push(arg.clone());
            }
        }
        let pattern = positional
            .first()
            .ok_or_else(|| RuntimeError::new("subst requires a pattern argument"))?;
        let replacement_val = positional.get(1).cloned();
        let is_closure = matches!(
            replacement_val,
            Some(Value::Sub(_)) | Some(Value::WeakSub(_))
        );
        let replacement_str = if is_closure {
            String::new()
        } else {
            replacement_val
                .as_ref()
                .map(|v| v.to_string_value())
                .unwrap_or_default()
        };

        match pattern {
            Value::Regex(pat) => {
                let matches = self.regex_find_all(pat, &text);
                if matches.is_empty() {
                    return Ok(Value::Str(text));
                }
                let chars: Vec<char> = text.chars().collect();
                if global {
                    let mut result = String::new();
                    let mut last_end = 0;
                    for (start, end) in &matches {
                        let prefix: String = chars[last_end..*start].iter().collect();
                        result.push_str(&prefix);
                        let matched_text: String = chars[*start..*end].iter().collect();
                        let repl = self.eval_subst_replacement(
                            &replacement_val,
                            is_closure,
                            &replacement_str,
                            &matched_text,
                        )?;
                        result.push_str(&repl);
                        last_end = *end;
                    }
                    let suffix: String = chars[last_end..].iter().collect();
                    result.push_str(&suffix);
                    Ok(Value::Str(result))
                } else {
                    // Replace first match only
                    if let Some((start, end)) = matches.first() {
                        let prefix: String = chars[..*start].iter().collect();
                        let suffix: String = chars[*end..].iter().collect();
                        let matched_text: String = chars[*start..*end].iter().collect();
                        let repl = self.eval_subst_replacement(
                            &replacement_val,
                            is_closure,
                            &replacement_str,
                            &matched_text,
                        )?;
                        Ok(Value::Str(format!("{}{}{}", prefix, repl, suffix)))
                    } else {
                        Ok(Value::Str(text))
                    }
                }
            }
            Value::Str(pat) => {
                if global {
                    Ok(Value::Str(text.replace(pat, &replacement_str)))
                } else {
                    Ok(Value::Str(text.replacen(pat, &replacement_str, 1)))
                }
            }
            _ => {
                let pat_str = pattern.to_string_value();
                if global {
                    Ok(Value::Str(text.replace(&pat_str, &replacement_str)))
                } else {
                    Ok(Value::Str(text.replacen(&pat_str, &replacement_str, 1)))
                }
            }
        }
    }

    /// Evaluate a subst replacement — either a static string or a closure call.
    fn eval_subst_replacement(
        &mut self,
        replacement_val: &Option<Value>,
        is_closure: bool,
        replacement_str: &str,
        matched_text: &str,
    ) -> Result<String, RuntimeError> {
        if !is_closure {
            return Ok(replacement_str.to_string());
        }
        let sub_data = match replacement_val {
            Some(Value::Sub(data)) => data.clone(),
            Some(Value::WeakSub(weak)) => weak
                .upgrade()
                .ok_or_else(|| RuntimeError::new("subst closure has been garbage collected"))?,
            _ => return Ok(replacement_str.to_string()),
        };
        let saved = self.env.clone();
        // Set up closure environment
        for (k, v) in &sub_data.env {
            self.env.insert(k.clone(), v.clone());
        }
        // Set $/ to the matched text
        let match_val = Value::Str(matched_text.to_string());
        self.env.insert("/".to_string(), match_val.clone());
        self.env.insert("$_".to_string(), match_val);
        let result = self.eval_block_value(&sub_data.body).unwrap_or(Value::Nil);
        self.env = saved;
        Ok(result.to_string_value())
    }

    fn dispatch_contains(&self, target: Value, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut positional: Vec<Value> = Vec::new();
        let mut ignore_case = false;
        for arg in args {
            if let Value::Pair(key, value) = arg {
                if matches!(key.as_str(), "i" | "ignorecase" | "m" | "ignoremark") {
                    ignore_case = value.truthy();
                }
            } else {
                positional.push(arg.clone());
            }
        }
        let needle = positional
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let start = if let Some(pos) = positional.get(1) {
            match pos {
                Value::Int(i) => *i,
                Value::Num(f) => *f as i64,
                Value::Str(s) => s.parse::<i64>().unwrap_or(0),
                Value::BigInt(b) => {
                    if b > &num_bigint::BigInt::from(i64::MAX) {
                        return Err(RuntimeError::new("X::OutOfRange"));
                    }
                    b.to_string().parse::<i64>().unwrap_or(0)
                }
                _ => 0,
            }
        } else {
            0
        };
        let text = target.to_string_value();
        let len = text.chars().count() as i64;
        if start < 0 || start > len {
            return Err(RuntimeError::new("X::OutOfRange"));
        }
        let hay: String = text.chars().skip(start as usize).collect();
        let ok = if ignore_case {
            hay.to_lowercase().contains(&needle.to_lowercase())
        } else {
            hay.contains(&needle)
        };
        Ok(Value::Bool(ok))
    }

    fn dispatch_starts_with(&self, target: Value, args: &[Value]) -> Result<Value, RuntimeError> {
        self.dispatch_prefix_suffix_check(target, args, true)
    }

    fn dispatch_ends_with(&self, target: Value, args: &[Value]) -> Result<Value, RuntimeError> {
        self.dispatch_prefix_suffix_check(target, args, false)
    }

    fn dispatch_prefix_suffix_check(
        &self,
        target: Value,
        args: &[Value],
        is_prefix: bool,
    ) -> Result<Value, RuntimeError> {
        let method_name = if is_prefix {
            "starts-with"
        } else {
            "ends-with"
        };
        // Separate positional and named args first
        let mut positional: Vec<Value> = Vec::new();
        let mut ignore_case = false;
        let mut ignore_mark = false;
        for arg in args {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "i" | "ignorecase" => ignore_case = value.truthy(),
                    "m" | "ignoremark" => ignore_mark = value.truthy(),
                    _ => {}
                }
            } else {
                positional.push(arg.clone());
            }
        }
        // Type objects (Package) as needle should throw
        if let Some(Value::Package(type_name)) = positional.first() {
            return Err(RuntimeError::new(format!(
                "Cannot resolve caller {}({}:U)",
                method_name, type_name
            )));
        }
        let needle = positional
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let text = target.to_string_value();

        let ok = match (ignore_case, ignore_mark) {
            (false, false) => {
                if is_prefix {
                    text.starts_with(needle.as_str())
                } else {
                    text.ends_with(needle.as_str())
                }
            }
            (true, false) => {
                let t = text.to_lowercase();
                let n = needle.to_lowercase();
                if is_prefix {
                    t.starts_with(n.as_str())
                } else {
                    t.ends_with(n.as_str())
                }
            }
            (false, true) => {
                let t = self.strip_marks(&text);
                let n = self.strip_marks(&needle);
                if is_prefix {
                    t.starts_with(n.as_str())
                } else {
                    t.ends_with(n.as_str())
                }
            }
            (true, true) => {
                let t = self.strip_marks(&text).to_lowercase();
                let n = self.strip_marks(&needle).to_lowercase();
                if is_prefix {
                    t.starts_with(n.as_str())
                } else {
                    t.ends_with(n.as_str())
                }
            }
        };
        Ok(Value::Bool(ok))
    }

    fn dispatch_index(&self, target: Value, args: &[Value]) -> Result<Value, RuntimeError> {
        // Type objects (Package) as needle are not allowed
        if let Some(Value::Package(type_name)) = args.first() {
            return Err(RuntimeError::new(format!(
                "Cannot resolve caller index({}:U)",
                type_name
            )));
        }
        let mut positional: Vec<Value> = Vec::new();
        let mut ignore_case = false;
        let mut ignore_mark = false;
        for arg in args {
            if let Value::Pair(key, value) = arg {
                match key.as_str() {
                    "i" | "ignorecase" => ignore_case = value.truthy(),
                    "m" | "ignoremark" => ignore_mark = value.truthy(),
                    _ => {}
                }
            } else {
                positional.push(arg.clone());
            }
        }
        // Handle list of needles: \(<a o>) passes an Array as first arg
        let needles: Vec<String> = if let Some(Value::Array(items, ..)) = positional.first() {
            items.iter().map(|v| v.to_string_value()).collect()
        } else {
            vec![
                positional
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default(),
            ]
        };
        let start = if let Some(pos) = positional.get(1) {
            self.value_to_position(pos)?
        } else {
            0
        };
        let text = target.to_string_value();
        let len = text.chars().count() as i64;
        if start < 0 {
            return Err(RuntimeError::new("X::OutOfRange"));
        }
        if start > len {
            return Ok(Value::Nil);
        }
        let hay: String = text.chars().skip(start as usize).collect();
        let mut best: Option<usize> = None;
        for needle in &needles {
            let pos = if ignore_case && ignore_mark {
                self.index_ignorecase_ignoremark(&hay, needle)
            } else if ignore_case {
                self.index_ignorecase(&hay, needle)
            } else if ignore_mark {
                self.index_ignoremark(&hay, needle)
            } else {
                hay.find(needle.as_str()).map(|p| hay[..p].chars().count())
            };
            if let Some(char_pos) = pos {
                best = Some(match best {
                    Some(prev) => prev.min(char_pos),
                    None => char_pos,
                });
            }
        }
        match best {
            Some(char_pos) => Ok(Value::Int(char_pos as i64 + start)),
            None => Ok(Value::Nil),
        }
    }

    fn index_ignorecase(&self, hay: &str, needle: &str) -> Option<usize> {
        let hay_lower = hay.to_lowercase();
        let needle_lower = needle.to_lowercase();
        hay_lower
            .find(&needle_lower)
            .map(|byte_pos| hay_lower[..byte_pos].chars().count())
    }

    fn index_ignoremark(&self, hay: &str, needle: &str) -> Option<usize> {
        let hay_stripped = self.strip_marks(hay);
        let needle_stripped = self.strip_marks(needle);
        hay_stripped
            .find(&needle_stripped)
            .map(|byte_pos| hay_stripped[..byte_pos].chars().count())
    }

    fn index_ignorecase_ignoremark(&self, hay: &str, needle: &str) -> Option<usize> {
        let hay_stripped = self.strip_marks(hay).to_lowercase();
        let needle_stripped = self.strip_marks(needle).to_lowercase();
        hay_stripped
            .find(&needle_stripped)
            .map(|byte_pos| hay_stripped[..byte_pos].chars().count())
    }

    fn strip_marks(&self, s: &str) -> String {
        use unicode_normalization::UnicodeNormalization;
        s.nfd()
            .filter(|c| !unicode_normalization::char::is_combining_mark(*c))
            .collect()
    }

    fn value_to_position(&self, pos: &Value) -> Result<i64, RuntimeError> {
        match pos {
            Value::Int(i) => Ok(*i),
            Value::Num(f) => {
                if f.abs() > i64::MAX as f64 {
                    Err(RuntimeError::new("X::OutOfRange"))
                } else {
                    Ok(*f as i64)
                }
            }
            Value::Str(s) => Ok(s.parse::<i64>().unwrap_or(0)),
            Value::BigInt(b) => {
                if b > &num_bigint::BigInt::from(i64::MAX) {
                    Err(RuntimeError::new("X::OutOfRange"))
                } else {
                    Ok(b.to_string().parse::<i64>().unwrap_or(0))
                }
            }
            _ => Ok(0),
        }
    }

    fn dispatch_substr_eq(&self, target: Value, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Err(RuntimeError::new(
                "Too few positionals passed to 'substr-eq'",
            ));
        }
        let text = target.to_string_value();
        let needle = args[0].to_string_value();
        let start = if let Some(pos) = args.get(1) {
            self.value_to_position(pos)?
        } else {
            0
        };
        let len = text.chars().count() as i64;
        if start < 0 || start > len {
            return Err(RuntimeError::new("X::OutOfRange"));
        }
        let substr: String = text
            .chars()
            .skip(start as usize)
            .take(needle.len())
            .collect();
        Ok(Value::Bool(substr == needle))
    }

    fn dispatch_substr(&mut self, target: Value, args: &[Value]) -> Result<Value, RuntimeError> {
        let s = target.to_string_value();
        let chars: Vec<char> = s.chars().collect();
        let total_len = chars.len();

        // First arg: start position
        let start = if let Some(pos) = args.first() {
            match pos {
                Value::Int(i) => {
                    let i = *i;
                    if i < 0 {
                        (total_len as i64 + i).max(0) as usize
                    } else {
                        i as usize
                    }
                }
                other => other.to_string_value().parse::<i64>().unwrap_or(0).max(0) as usize,
            }
        } else {
            0
        };

        // Second arg: length (can be Int, WhateverCode/Sub, or absent)
        let end = if let Some(len_val) = args.get(1) {
            match len_val {
                Value::Int(i) => {
                    let len = (*i).max(0) as usize;
                    (start + len).min(total_len)
                }
                Value::Sub { .. } => {
                    // WhateverCode: call with remaining length to get actual length
                    let remaining = if start <= total_len {
                        (total_len - start) as i64
                    } else {
                        0
                    };
                    let result =
                        self.eval_call_on_value(len_val.clone(), vec![Value::Int(remaining)])?;
                    let len = match &result {
                        Value::Int(i) => (*i).max(0) as usize,
                        _ => 0,
                    };
                    (start + len).min(total_len)
                }
                _ => total_len, // default: take rest
            }
        } else {
            total_len // no length: take rest
        };

        let start = start.min(total_len);
        Ok(Value::Str(chars[start..end].iter().collect()))
    }

    fn dispatch_to_set(&self, target: Value) -> Result<Value, RuntimeError> {
        let mut elems = HashSet::new();
        match target {
            Value::Set(_) => return Ok(target),
            Value::Array(items, ..) => {
                for item in items.iter() {
                    elems.insert(item.to_string_value());
                }
            }
            Value::Hash(items) => {
                for (k, v) in items.iter() {
                    if v.truthy() {
                        elems.insert(k.clone());
                    }
                }
            }
            Value::Bag(b) => {
                for k in b.keys() {
                    elems.insert(k.clone());
                }
            }
            Value::Mix(m) => {
                for k in m.keys() {
                    elems.insert(k.clone());
                }
            }
            other => {
                elems.insert(other.to_string_value());
            }
        }
        Ok(Value::set(elems))
    }

    fn dispatch_to_bag(&self, target: Value) -> Result<Value, RuntimeError> {
        let mut counts: HashMap<String, i64> = HashMap::new();
        match target {
            Value::Bag(_) => return Ok(target),
            Value::Array(items, ..) => {
                for item in items.iter() {
                    *counts.entry(item.to_string_value()).or_insert(0) += 1;
                }
            }
            Value::Set(s) => {
                for k in s.iter() {
                    counts.insert(k.clone(), 1);
                }
            }
            Value::Mix(m) => {
                for (k, v) in m.iter() {
                    counts.insert(k.clone(), *v as i64);
                }
            }
            other => {
                counts.insert(other.to_string_value(), 1);
            }
        }
        Ok(Value::bag(counts))
    }

    fn dispatch_to_mix(&self, target: Value) -> Result<Value, RuntimeError> {
        let mut weights: HashMap<String, f64> = HashMap::new();
        match target {
            Value::Mix(_) => return Ok(target),
            Value::Array(items, ..) => {
                for item in items.iter() {
                    match item {
                        Value::Pair(k, v) => {
                            let w = match v.as_ref() {
                                Value::Int(i) => *i as f64,
                                Value::Num(n) => *n,
                                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                                _ => 1.0,
                            };
                            *weights.entry(k.clone()).or_insert(0.0) += w;
                        }
                        Value::ValuePair(k, v) => {
                            let w = match v.as_ref() {
                                Value::Int(i) => *i as f64,
                                Value::Num(n) => *n,
                                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                                _ => 1.0,
                            };
                            *weights.entry(k.to_string_value()).or_insert(0.0) += w;
                        }
                        _ => {
                            *weights.entry(item.to_string_value()).or_insert(0.0) += 1.0;
                        }
                    }
                }
            }
            Value::Set(s) => {
                for k in s.iter() {
                    weights.insert(k.clone(), 1.0);
                }
            }
            Value::Bag(b) => {
                for (k, v) in b.iter() {
                    weights.insert(k.clone(), *v as f64);
                }
            }
            other => {
                weights.insert(other.to_string_value(), 1.0);
            }
        }
        Ok(Value::mix(weights))
    }

    fn dispatch_to_hash(&self, target: Value) -> Result<Value, RuntimeError> {
        match target {
            Value::Hash(_) => Ok(target),
            Value::Array(items, ..) => {
                let mut map = HashMap::new();
                let mut iter = items.iter();
                while let Some(item) = iter.next() {
                    match item {
                        Value::Pair(k, v) => {
                            map.insert(k.clone(), *v.clone());
                        }
                        Value::ValuePair(k, v) => {
                            map.insert(k.to_string_value(), *v.clone());
                        }
                        other => {
                            let key = other.to_string_value();
                            let value = iter.next().cloned().unwrap_or(Value::Nil);
                            map.insert(key, value);
                        }
                    }
                }
                Ok(Value::hash(map))
            }
            Value::Seq(items) | Value::Slip(items) => {
                let mut map = HashMap::new();
                let mut iter = items.iter();
                while let Some(item) = iter.next() {
                    match item {
                        Value::Pair(k, v) => {
                            map.insert(k.clone(), *v.clone());
                        }
                        Value::ValuePair(k, v) => {
                            map.insert(k.to_string_value(), *v.clone());
                        }
                        other => {
                            let key = other.to_string_value();
                            let value = iter.next().cloned().unwrap_or(Value::Nil);
                            map.insert(key, value);
                        }
                    }
                }
                Ok(Value::hash(map))
            }
            Value::Set(s) => {
                let mut map = HashMap::new();
                for k in s.iter() {
                    map.insert(k.clone(), Value::Bool(true));
                }
                Ok(Value::hash(map))
            }
            Value::Bag(b) => {
                let mut map = HashMap::new();
                for (k, v) in b.iter() {
                    map.insert(k.clone(), Value::Int(*v));
                }
                Ok(Value::hash(map))
            }
            Value::Mix(m) => {
                let mut map = HashMap::new();
                for (k, v) in m.iter() {
                    map.insert(k.clone(), Value::Num(*v));
                }
                Ok(Value::hash(map))
            }
            Value::Instance {
                ref class_name,
                ref attributes,
                ..
            } if class_name == "Match" => {
                // %($/) returns the named captures hash
                if let Some(named) = attributes.get("named") {
                    Ok(named.clone())
                } else {
                    Ok(Value::hash(HashMap::new()))
                }
            }
            other => {
                let mut map = HashMap::new();
                map.insert(other.to_string_value(), Value::Bool(true));
                Ok(Value::hash(map))
            }
        }
    }

    fn dispatch_rotate(&self, target: Value, args: &[Value]) -> Result<Value, RuntimeError> {
        let items = match target {
            Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => items.to_vec(),
            Value::Capture { positional, .. } => positional,
            Value::LazyList(ll) => ll.cache.lock().unwrap().clone().unwrap_or_default(),
            _ => return Ok(Value::Nil),
        };
        if items.is_empty() {
            return Ok(Value::array(Vec::new()));
        }
        let len = items.len() as i64;
        let by = match args.first() {
            Some(Value::Int(i)) => *i,
            Some(Value::Num(n)) => *n as i64,
            Some(other) => other.to_string_value().parse::<i64>().unwrap_or(1),
            None => 1,
        };
        let shift = ((by % len) + len) % len;
        let mut out = vec![Value::Nil; items.len()];
        for (i, item) in items.into_iter().enumerate() {
            let dst = ((i as i64 + len - shift) % len) as usize;
            out[dst] = item;
        }
        Ok(Value::array(out))
    }

    fn dispatch_minmaxpairs(&mut self, target: Value, method: &str) -> Result<Value, RuntimeError> {
        if matches!(target, Value::Instance { .. })
            && let Ok(pairs) = self.call_method_with_values(target.clone(), "pairs", Vec::new())
        {
            return Ok(pairs);
        }
        let want_max = method == "maxpairs";
        let to_pairs = |items: &[Value]| -> Value {
            let mut best: Option<Value> = None;
            let mut out: Vec<Value> = Vec::new();
            for (idx, item) in items.iter().enumerate() {
                if matches!(item, Value::Nil) || matches!(item, Value::Package(n) if n == "Any") {
                    continue;
                }
                let ord = if let Some(current) = &best {
                    // Use `cmp` semantics: numeric comparison for numeric
                    // pairs, string comparison otherwise
                    match (item, current) {
                        (Value::Int(a), Value::Int(b)) => a.cmp(b),
                        (Value::Num(a), Value::Num(b)) => {
                            a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)
                        }
                        (Value::Int(a), Value::Num(b)) => (*a as f64)
                            .partial_cmp(b)
                            .unwrap_or(std::cmp::Ordering::Equal),
                        (Value::Num(a), Value::Int(b)) => a
                            .partial_cmp(&(*b as f64))
                            .unwrap_or(std::cmp::Ordering::Equal),
                        (Value::Rat(..), _) | (_, Value::Rat(..)) => {
                            if let (Some((an, ad)), Some((bn, bd))) = (
                                crate::runtime::to_rat_parts(item),
                                crate::runtime::to_rat_parts(current),
                            ) {
                                crate::runtime::compare_rat_parts((an, ad), (bn, bd))
                            } else {
                                item.to_string_value().cmp(&current.to_string_value())
                            }
                        }
                        _ => item.to_string_value().cmp(&current.to_string_value()),
                    }
                } else {
                    std::cmp::Ordering::Equal
                };
                let replace = best.is_none()
                    || (want_max && ord == std::cmp::Ordering::Greater)
                    || (!want_max && ord == std::cmp::Ordering::Less);
                if replace {
                    best = Some(item.clone());
                    out.clear();
                    out.push(Value::ValuePair(
                        Box::new(Value::Int(idx as i64)),
                        Box::new(item.clone()),
                    ));
                } else if ord == std::cmp::Ordering::Equal {
                    out.push(Value::ValuePair(
                        Box::new(Value::Int(idx as i64)),
                        Box::new(item.clone()),
                    ));
                }
            }
            Value::Seq(Arc::new(out))
        };
        Ok(match target {
            Value::Array(items, ..) => to_pairs(&items),
            other => Value::Seq(Arc::new(vec![Value::ValuePair(
                Box::new(Value::Int(0)),
                Box::new(other),
            )])),
        })
    }

    fn dispatch_supply_running_extrema(
        &mut self,
        target: Value,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let Value::Instance { attributes, .. } = target else {
            return Err(RuntimeError::new("Expected Supply instance"));
        };
        let values = if let Some(Value::Array(items, ..)) = attributes.get("values") {
            items.to_vec()
        } else {
            Vec::new()
        };
        let want_max = method == "max";

        let build_supply = |vals: Vec<Value>| {
            let mut attrs = HashMap::new();
            attrs.insert("values".to_string(), Value::array(vals));
            attrs.insert("taps".to_string(), Value::array(Vec::new()));
            attrs.insert("live".to_string(), Value::Bool(false));
            Value::make_instance("Supply".to_string(), attrs)
        };

        let compare_or_key_fn = args.first().cloned();
        if let Some(ref fn_val) = compare_or_key_fn
            && !matches!(
                fn_val,
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
            )
        {
            return Err(RuntimeError::new("must be code if specified"));
        }
        if values.is_empty() {
            return Ok(build_supply(Vec::new()));
        }

        let Some(compare_or_key_fn) = compare_or_key_fn else {
            let mut emitted = Vec::new();
            let mut best = values[0].clone();
            emitted.push(best.clone());
            for item in values.iter().skip(1) {
                let cmp = compare_values(item, &best);
                let is_better = if want_max { cmp > 0 } else { cmp < 0 };
                if is_better {
                    best = item.clone();
                    emitted.push(item.clone());
                }
            }
            return Ok(build_supply(emitted));
        };

        let is_binary_comparator =
            matches!(&compare_or_key_fn, Value::Sub(data) if data.params.len() >= 2);
        let mut emitted = Vec::new();
        let mut best = values[0].clone();
        emitted.push(best.clone());

        if is_binary_comparator {
            for item in values.into_iter().skip(1) {
                let cmp_val = self.call_sub_value(
                    compare_or_key_fn.clone(),
                    vec![item.clone(), best.clone()],
                    true,
                )?;
                let cmp = match cmp_val {
                    Value::Enum {
                        ref enum_type,
                        value,
                        ..
                    } if enum_type == "Order" => value,
                    other => {
                        let n = other.to_f64();
                        if n > 0.0 {
                            1
                        } else if n < 0.0 {
                            -1
                        } else {
                            0
                        }
                    }
                };
                let is_better = if want_max { cmp > 0 } else { cmp < 0 };
                if is_better {
                    best = item.clone();
                    emitted.push(item);
                }
            }
        } else {
            let mut best_key =
                self.call_sub_value(compare_or_key_fn.clone(), vec![best.clone()], true)?;
            for item in values.into_iter().skip(1) {
                let key =
                    self.call_sub_value(compare_or_key_fn.clone(), vec![item.clone()], true)?;
                let cmp = compare_values(&key, &best_key);
                let is_better = if want_max { cmp > 0 } else { cmp < 0 };
                if is_better {
                    best_key = key;
                    emitted.push(item);
                }
            }
        }

        Ok(build_supply(emitted))
    }

    pub(crate) fn dispatch_sort(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        match target {
            Value::Array(mut items, ..) => {
                let items_mut = Arc::make_mut(&mut items);
                // Handle Routine comparator (e.g. &[<=>], &infix:<+>)
                if let Some(comparator @ Value::Routine { .. }) = args.first().cloned() {
                    items_mut.sort_by(|a, b| {
                        let call_args = vec![a.clone(), b.clone()];
                        match self.eval_call_on_value(comparator.clone(), call_args) {
                            Ok(result) => match &result {
                                Value::Int(n) => n.cmp(&0),
                                Value::Enum {
                                    enum_type, value, ..
                                } if enum_type == "Order" => value.cmp(&0),
                                _ => std::cmp::Ordering::Equal,
                            },
                            Err(_) => std::cmp::Ordering::Equal,
                        }
                    });
                } else if let Some(Value::Sub(data)) = args.first().cloned() {
                    let is_key_extractor = data.params.len() <= 1;
                    if is_key_extractor {
                        items_mut.sort_by(|a, b| {
                            let saved = self.env.clone();
                            for (k, v) in &data.env {
                                self.env.insert(k.clone(), v.clone());
                            }
                            if let Some(p) = data.params.first() {
                                self.env.insert(p.clone(), a.clone());
                            }
                            self.env.insert("_".to_string(), a.clone());
                            let key_a = self.eval_block_value(&data.body).unwrap_or(Value::Nil);
                            self.env = saved.clone();
                            for (k, v) in &data.env {
                                self.env.insert(k.clone(), v.clone());
                            }
                            if let Some(p) = data.params.first() {
                                self.env.insert(p.clone(), b.clone());
                            }
                            self.env.insert("_".to_string(), b.clone());
                            let key_b = self.eval_block_value(&data.body).unwrap_or(Value::Nil);
                            self.env = saved;
                            compare_values(&key_a, &key_b).cmp(&0)
                        });
                    } else {
                        items_mut.sort_by(|a, b| {
                            let saved = self.env.clone();
                            for (k, v) in &data.env {
                                self.env.insert(k.clone(), v.clone());
                            }
                            if data.params.len() >= 2 {
                                self.env.insert(data.params[0].clone(), a.clone());
                                self.env.insert(data.params[1].clone(), b.clone());
                            } else if let Some(p) = data.params.first() {
                                self.env.insert(p.clone(), a.clone());
                            }
                            self.env.insert("_".to_string(), a.clone());
                            let result = self.eval_block_value(&data.body).unwrap_or(Value::Int(0));
                            self.env = saved;
                            match result {
                                Value::Int(n) => n.cmp(&0),
                                Value::Enum {
                                    enum_type, value, ..
                                } if enum_type == "Order" => value.cmp(&0),
                                _ => std::cmp::Ordering::Equal,
                            }
                        });
                    }
                } else {
                    items_mut.sort_by(|a, b| compare_values(a, b).cmp(&0));
                }
                Ok(Value::Array(items, false))
            }
            Value::Hash(map) => {
                // Convert hash to list of pairs, then sort
                let items: Vec<Value> = map
                    .iter()
                    .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                    .collect();
                self.dispatch_sort(Value::array(items), args)
            }
            other => Ok(other),
        }
    }

    fn dispatch_unique(&mut self, target: Value, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut as_func: Option<Value> = None;
        let mut with_func: Option<Value> = None;
        for arg in args {
            if let Value::Pair(key, value) = arg {
                if key == "as" && value.truthy() {
                    as_func = Some(value.as_ref().clone());
                    continue;
                }
                if key == "with" && value.truthy() {
                    with_func = Some(value.as_ref().clone());
                    continue;
                }
            }
        }

        let items: Vec<Value> = match target {
            Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => items.to_vec(),
            Value::LazyList(ll) => self.force_lazy_list_bridge(&ll)?,
            v @ (Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. }) => Self::value_to_list(&v),
            other => vec![other],
        };

        let mut seen_keys: Vec<Value> = Vec::new();
        let mut unique_items: Vec<Value> = Vec::new();
        for item in items {
            let key = if let Some(func) = as_func.clone() {
                self.call_sub_value(func, vec![item.clone()], true)?
            } else {
                item.clone()
            };

            let mut duplicate = false;
            for seen in &seen_keys {
                let is_same = if let Some(func) = with_func.clone() {
                    self.call_sub_value(func, vec![seen.clone(), key.clone()], true)?
                        .truthy()
                } else {
                    values_identical(seen, &key)
                };
                if is_same {
                    duplicate = true;
                    break;
                }
            }

            if !duplicate {
                seen_keys.push(key);
                unique_items.push(item);
            }
        }

        Ok(Value::array(unique_items))
    }

    fn dispatch_collate(&mut self, target: Value) -> Result<Value, RuntimeError> {
        fn case_profile(s: &str) -> Vec<u8> {
            s.chars()
                .map(|ch| {
                    if ch.is_lowercase() {
                        0
                    } else if ch.is_uppercase() {
                        1
                    } else {
                        2
                    }
                })
                .collect()
        }

        fn collate_sorted(values: Vec<Value>) -> Vec<Value> {
            let mut keyed: Vec<(String, Vec<u8>, String, Value)> = values
                .into_iter()
                .map(|value| {
                    let s = value.to_string_value();
                    (s.to_lowercase(), case_profile(&s), s.clone(), value)
                })
                .collect();
            keyed.sort_by(|a, b| {
                a.0.cmp(&b.0)
                    .then_with(|| a.1.cmp(&b.1))
                    .then_with(|| a.2.cmp(&b.2))
            });
            keyed.into_iter().map(|(_, _, _, value)| value).collect()
        }

        match target {
            Value::Package(class_name) if class_name == "Supply" => {
                Ok(Value::Seq(Arc::new(vec![Value::Package(class_name)])))
            }
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Supply" => {
                let values = match attributes.get("values") {
                    Some(Value::Array(items, ..)) => items.to_vec(),
                    _ => Vec::new(),
                };
                let mut attrs = HashMap::new();
                attrs.insert("values".to_string(), Value::array(collate_sorted(values)));
                attrs.insert("taps".to_string(), Value::array(Vec::new()));
                attrs.insert("live".to_string(), Value::Bool(false));
                Ok(Value::make_instance("Supply".to_string(), attrs))
            }
            Value::Array(items, ..) => Ok(Value::Seq(Arc::new(collate_sorted(items.to_vec())))),
            other => {
                let values = Self::value_to_list(&other);
                Ok(Value::Seq(Arc::new(collate_sorted(values))))
            }
        }
    }

    fn dispatch_new(&mut self, target: Value, args: Vec<Value>) -> Result<Value, RuntimeError> {
        if let Value::ParametricRole {
            base_name,
            type_args,
        } = &target
            && let Some(role) = self.roles.get(base_name).cloned()
        {
            let mut named_args: HashMap<String, Value> = HashMap::new();
            let mut positional_args: Vec<Value> = Vec::new();
            for arg in &args {
                if let Value::Pair(key, value) = arg {
                    named_args.insert(key.clone(), *value.clone());
                } else {
                    positional_args.push(arg.clone());
                }
            }

            let mut mixins = HashMap::new();
            mixins.insert(format!("__mutsu_role__{}", base_name), Value::Bool(true));
            mixins.insert(
                format!("__mutsu_role_typeargs__{}", base_name),
                Value::array(type_args.clone()),
            );
            if let Some(param_names) = self.role_type_params.get(base_name) {
                for (param_name, type_arg) in param_names.iter().zip(type_args.iter()) {
                    mixins.insert(
                        format!("__mutsu_role_param__{}", param_name),
                        type_arg.clone(),
                    );
                }
            }
            for (idx, (attr_name, _is_public, default_expr, _is_rw)) in
                role.attributes.iter().enumerate()
            {
                let value = if let Some(v) = named_args.get(attr_name) {
                    v.clone()
                } else if let Some(v) = positional_args.get(idx) {
                    v.clone()
                } else if let Some(expr) = default_expr {
                    self.eval_block_value(&[Stmt::Expr(expr.clone())])?
                } else {
                    Value::Nil
                };
                mixins.insert(format!("__mutsu_attr__{}", attr_name), value);
            }
            return Ok(Value::Mixin(
                Box::new(Value::make_instance(base_name.clone(), HashMap::new())),
                mixins,
            ));
        }

        if let Value::Package(class_name) = &target {
            let parametric = Self::parse_parametric_type_name(class_name);
            let (base_class_name, type_args) = if let Some((base, args)) = &parametric {
                (base.as_str(), Some(args.clone()))
            } else {
                (class_name.as_str(), None)
            };
            match base_class_name {
                "Array" | "List" | "Positional" | "array" => {
                    if let Some(dims) = self.shaped_dims_from_new_args(&args) {
                        // Check for :data argument to populate the shaped array
                        let data = args.iter().find_map(|arg| match arg {
                            Value::Pair(name, value) if name == "data" => {
                                Some(value.as_ref().clone())
                            }
                            _ => None,
                        });
                        let shaped = Self::make_shaped_array(&dims);
                        if let Some(data_val) = data {
                            // Populate the shaped array with data
                            let data_items = match data_val {
                                Value::Array(items, ..)
                                | Value::Seq(items)
                                | Value::Slip(items) => items.to_vec(),
                                other => vec![other],
                            };
                            if let Value::Array(ref items, is_arr) = shaped {
                                let mut new_items = items.as_ref().clone();
                                for (i, val) in data_items.into_iter().enumerate() {
                                    if i < new_items.len() {
                                        new_items[i] = val;
                                    }
                                }
                                let result = Value::Array(std::sync::Arc::new(new_items), is_arr);
                                crate::runtime::utils::mark_shaped_array(&result, Some(&dims));
                                return Ok(result);
                            }
                        }
                        return Ok(shaped);
                    }
                    let mut items = Vec::new();
                    for arg in &args {
                        match arg {
                            Value::Slip(vals) => items.extend(vals.iter().cloned()),
                            other => items.push(other.clone()),
                        }
                    }
                    // Type check for typed arrays (e.g. Array[Int].new(...))
                    // Skip for native types (int8, num32, etc.) which coerce rather than check
                    if let Some(ref ta) = type_args
                        && let Some(constraint) = ta.first()
                        && constraint.starts_with(char::is_uppercase)
                    {
                        for item in &items {
                            if !self.type_matches_value(constraint, item) {
                                let got_type = crate::value::what_type_name(item);
                                let got_repr = item.to_string_value();
                                let msg = format!(
                                    "Type check failed in assignment to ; expected {} but got {} ({})",
                                    constraint, got_type, got_repr,
                                );
                                let mut attrs = std::collections::HashMap::new();
                                attrs.insert("message".to_string(), Value::Str(msg.clone()));
                                attrs.insert(
                                    "operation".to_string(),
                                    Value::Str("assignment".to_string()),
                                );
                                attrs.insert("got".to_string(), item.clone());
                                attrs.insert(
                                    "expected".to_string(),
                                    Value::Package(constraint.clone()),
                                );
                                let ex = Value::make_instance(
                                    "X::TypeCheck::Assignment".to_string(),
                                    attrs,
                                );
                                let mut err = RuntimeError::new(msg);
                                err.exception = Some(Box::new(ex));
                                return Err(err);
                            }
                        }
                    }
                    let result = if matches!(base_class_name, "Array" | "array") {
                        Value::real_array(items)
                    } else {
                        Value::array(items)
                    };
                    // Register type metadata for typed arrays (e.g. Array[Int].new)
                    if let Some(ref ta) = type_args
                        && let Some(constraint) = ta.first()
                    {
                        let info = crate::runtime::ContainerTypeInfo {
                            value_type: constraint.clone(),
                            key_type: None,
                            declared_type: Some(class_name.clone()),
                        };
                        self.register_container_type_metadata(&result, info);
                    }
                    return Ok(result);
                }
                "Hash" | "Map" => {
                    let mut flat = Vec::new();
                    for arg in &args {
                        flat.extend(Self::value_to_list(arg));
                    }
                    let mut map = HashMap::new();
                    let mut iter = flat.into_iter();
                    while let Some(item) = iter.next() {
                        match item {
                            Value::Pair(k, v) => {
                                map.insert(k, *v);
                            }
                            Value::ValuePair(k, v) => {
                                map.insert(k.to_string_value(), *v);
                            }
                            other => {
                                let key = other.to_string_value();
                                let value = iter.next().unwrap_or(Value::Nil);
                                map.insert(key, value);
                            }
                        }
                    }
                    let result = Value::hash(map);
                    // Register type metadata for typed hashes (e.g. Hash[Int].new)
                    if let Some(ref ta) = type_args {
                        let value_type = ta.first().cloned().unwrap_or_default();
                        let key_type = ta.get(1).cloned();
                        let info = crate::runtime::ContainerTypeInfo {
                            value_type,
                            key_type,
                            declared_type: Some(class_name.clone()),
                        };
                        self.register_container_type_metadata(&result, info);
                    }
                    return Ok(result);
                }
                "Uni" => {
                    let codepoints: Vec<Value> = args
                        .iter()
                        .map(|a| match a {
                            Value::Int(i) => Value::Int(*i),
                            Value::Num(f) => Value::Int(*f as i64),
                            other => {
                                Value::Int(other.to_string_value().parse::<i64>().unwrap_or(0))
                            }
                        })
                        .collect();
                    return Ok(Value::array(codepoints));
                }
                "Seq" => {
                    // Seq.new(iterator) — pull all items from the iterator
                    if let Some(iterator) = args.first() {
                        let mut items = Vec::new();
                        loop {
                            let val =
                                self.call_method_with_values(iterator.clone(), "pull-one", vec![])?;
                            if matches!(&val, Value::Str(s) if s == "IterationEnd") {
                                break;
                            }
                            items.push(val);
                        }
                        return Ok(Value::Seq(std::sync::Arc::new(items)));
                    }
                    return Ok(Value::Seq(std::sync::Arc::new(Vec::new())));
                }
                "Version" => {
                    let arg = args.first().cloned().unwrap_or(Value::Nil);
                    return Ok(Self::version_from_value(arg));
                }
                "Duration" => {
                    let secs = args.first().map(to_float_value).unwrap_or(Some(0.0));
                    return Ok(Value::Num(secs.unwrap_or(0.0)));
                }
                "Promise" => {
                    return Ok(Value::Promise(SharedPromise::new()));
                }
                "Channel" => {
                    return Ok(Value::Channel(SharedChannel::new()));
                }
                "Stash" => {
                    // Stash is essentially a Hash but with type Stash
                    return Ok(Value::make_instance("Stash".to_string(), HashMap::new()));
                }
                "Supply" => return Ok(self.make_supply_instance()),
                "Supplier" => {
                    let mut attrs = HashMap::new();
                    attrs.insert("emitted".to_string(), Value::array(Vec::new()));
                    attrs.insert("done".to_string(), Value::Bool(false));
                    attrs.insert(
                        "supplier_id".to_string(),
                        Value::Int(super::native_methods::next_supplier_id() as i64),
                    );
                    return Ok(Value::make_instance(class_name.clone(), attrs));
                }
                "ThreadPoolScheduler" | "CurrentThreadScheduler" | "Tap" | "Cancellation" => {
                    return Ok(Value::make_instance(class_name.clone(), HashMap::new()));
                }
                "Proxy" => {
                    let mut fetcher = Value::Nil;
                    let mut storer = Value::Nil;
                    for arg in &args {
                        if let Value::Pair(key, value) = arg {
                            match key.as_str() {
                                "FETCH" => fetcher = *value.clone(),
                                "STORE" => storer = *value.clone(),
                                _ => {}
                            }
                        }
                    }
                    return Ok(Value::Proxy {
                        fetcher: Box::new(fetcher),
                        storer: Box::new(storer),
                    });
                }
                "CompUnit::DependencySpecification" => {
                    // Extract :short-name from named args
                    let mut short_name: Option<String> = None;
                    for arg in &args {
                        if let Value::Pair(key, value) = arg
                            && key == "short-name"
                        {
                            if let Value::Str(s) = value.as_ref() {
                                short_name = Some(s.clone());
                            } else {
                                return Err(RuntimeError::new(
                                    "CompUnit::DependencySpecification.new: :short-name must be a Str",
                                ));
                            }
                        }
                    }
                    let short_name = short_name.ok_or_else(|| {
                        RuntimeError::new(
                            "CompUnit::DependencySpecification.new: :short-name is required",
                        )
                    })?;
                    return Ok(Value::CompUnitDepSpec { short_name });
                }
                "CompUnit::Repository::FileSystem" => {
                    let mut prefix = ".".to_string();
                    for arg in &args {
                        if let Value::Pair(key, value) = arg
                            && key == "prefix"
                        {
                            prefix = value.to_string_value();
                        }
                    }
                    let prefix_path = if prefix.is_empty() { "." } else { &prefix };
                    let canonical_prefix = std::fs::canonicalize(prefix_path)
                        .unwrap_or_else(|_| std::path::PathBuf::from(prefix_path))
                        .to_string_lossy()
                        .to_string();
                    let cache_key = format!("__mutsu_repo_fs::{}", canonical_prefix);
                    if let Some(existing) = self.env.get(&cache_key).cloned() {
                        return Ok(existing);
                    }
                    let mut attrs = HashMap::new();
                    attrs.insert(
                        "prefix".to_string(),
                        self.make_io_path_instance(&canonical_prefix),
                    );
                    attrs.insert("short-id".to_string(), Value::Str("file".to_string()));
                    let repo = Value::make_instance(class_name.clone(), attrs);
                    self.env.insert(cache_key, repo.clone());
                    return Ok(repo);
                }
                "Proc::Async" => {
                    let mut positional = Vec::new();
                    let mut w_flag = false;
                    for arg in &args {
                        match arg {
                            Value::Pair(key, value) if key == "w" => {
                                w_flag = value.truthy();
                            }
                            _ => positional.push(arg.clone()),
                        }
                    }
                    let stdout_id = super::native_methods::next_supply_id();
                    let stderr_id = super::native_methods::next_supply_id();
                    let mut stdout_supply_attrs = HashMap::new();
                    stdout_supply_attrs.insert("values".to_string(), Value::array(Vec::new()));
                    stdout_supply_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    stdout_supply_attrs
                        .insert("supply_id".to_string(), Value::Int(stdout_id as i64));
                    let mut stderr_supply_attrs = HashMap::new();
                    stderr_supply_attrs.insert("values".to_string(), Value::array(Vec::new()));
                    stderr_supply_attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    stderr_supply_attrs
                        .insert("supply_id".to_string(), Value::Int(stderr_id as i64));

                    let mut attrs = HashMap::new();
                    attrs.insert("cmd".to_string(), Value::array(positional));
                    attrs.insert("started".to_string(), Value::Bool(false));
                    attrs.insert(
                        "stdout".to_string(),
                        Value::make_instance("Supply".to_string(), stdout_supply_attrs),
                    );
                    attrs.insert(
                        "stderr".to_string(),
                        Value::make_instance("Supply".to_string(), stderr_supply_attrs),
                    );
                    if w_flag {
                        attrs.insert("w".to_string(), Value::Bool(true));
                    }
                    return Ok(Value::make_instance(class_name.clone(), attrs));
                }
                "IO::Path" => {
                    let mut path = String::new();
                    let mut cwd_attr: Option<String> = None;
                    for arg in &args {
                        match arg {
                            Value::Pair(key, value) if key == "CWD" => {
                                cwd_attr = Some(value.to_string_value());
                            }
                            Value::Instance {
                                class_name,
                                attributes,
                                ..
                            } if path.is_empty() && class_name == "IO::Path" => {
                                path = attributes
                                    .get("path")
                                    .map(|v| v.to_string_value())
                                    .unwrap_or_default();
                                if cwd_attr.is_none() {
                                    cwd_attr = attributes.get("cwd").map(|v| v.to_string_value());
                                }
                            }
                            _ if path.is_empty() => {
                                path = arg.to_string_value();
                            }
                            _ => {}
                        }
                    }
                    if path.contains('\0') {
                        return Err(RuntimeError::new(
                            "X::IO::Null: Found null byte in pathname",
                        ));
                    }
                    let mut attrs = HashMap::new();
                    attrs.insert("path".to_string(), Value::Str(path));
                    if let Some(cwd) = cwd_attr {
                        attrs.insert("cwd".to_string(), Value::Str(cwd));
                    }
                    return Ok(Value::make_instance("IO::Path".to_string(), attrs));
                }
                "utf8" | "utf16" => {
                    let elems: Vec<Value> = args
                        .iter()
                        .flat_map(|a| match a {
                            Value::Int(i) => vec![Value::Int(*i)],
                            Value::Array(items, ..) => items.to_vec(),
                            Value::Range(start, end) => (*start..=*end).map(Value::Int).collect(),
                            Value::RangeExcl(start, end) => {
                                (*start..*end).map(Value::Int).collect()
                            }
                            _ => vec![],
                        })
                        .collect();
                    let mut attrs = HashMap::new();
                    attrs.insert("bytes".to_string(), Value::array(elems));
                    return Ok(Value::make_instance(class_name.clone(), attrs));
                }
                "Buf" | "buf8" | "Buf[uint8]" | "Blob" | "blob8" | "Blob[uint8]" | "buf16"
                | "buf32" | "buf64" | "blob16" | "blob32" | "blob64" => {
                    let byte_vals: Vec<Value> = args
                        .iter()
                        .flat_map(|a| match a {
                            Value::Int(i) => vec![Value::Int(*i)],
                            Value::Array(items, ..) => items.to_vec(),
                            Value::Seq(items) => items.to_vec(),
                            Value::Slip(items) => items.to_vec(),
                            Value::Range(start, end) => (*start..=*end).map(Value::Int).collect(),
                            Value::RangeExcl(start, end) => {
                                (*start..*end).map(Value::Int).collect()
                            }
                            Value::Instance {
                                class_name,
                                attributes,
                                ..
                            } if class_name == "Buf"
                                || class_name == "Blob"
                                || class_name.starts_with("Buf[")
                                || class_name.starts_with("Blob[")
                                || class_name.starts_with("buf")
                                || class_name.starts_with("blob") =>
                            {
                                if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                                    items.to_vec()
                                } else {
                                    Vec::new()
                                }
                            }
                            _ => vec![],
                        })
                        .collect();
                    let is_blob = class_name.starts_with("Blob") || class_name.starts_with("blob");
                    let type_name = if is_blob { "Blob" } else { "Buf" }.to_string();
                    let mut attrs = HashMap::new();
                    attrs.insert("bytes".to_string(), Value::array(byte_vals));
                    return Ok(Value::make_instance(type_name, attrs));
                }
                "Rat" => {
                    let a = match args.first() {
                        Some(v) => to_int(v),
                        None => 0,
                    };
                    let b = match args.get(1) {
                        Some(v) => to_int(v),
                        None => 1,
                    };
                    return Ok(make_rat(a, b));
                }
                "FatRat" => {
                    let a = match args.first() {
                        Some(v) => to_int(v),
                        None => 0,
                    };
                    let b = match args.get(1) {
                        Some(v) => to_int(v),
                        None => 1,
                    };
                    return Ok(Value::FatRat(a, b));
                }
                "Set" | "SetHash" => {
                    let mut elems = HashSet::new();
                    for arg in &args {
                        for item in Self::value_to_list(arg) {
                            elems.insert(item.to_string_value());
                        }
                    }
                    return Ok(Value::set(elems));
                }
                "Bag" | "BagHash" => {
                    let mut counts: HashMap<String, i64> = HashMap::new();
                    for arg in &args {
                        for item in Self::value_to_list(arg) {
                            *counts.entry(item.to_string_value()).or_insert(0) += 1;
                        }
                    }
                    return Ok(Value::bag(counts));
                }
                "Mix" | "MixHash" => {
                    let mut weights: HashMap<String, f64> = HashMap::new();
                    for arg in &args {
                        for item in Self::value_to_list(arg) {
                            *weights.entry(item.to_string_value()).or_insert(0.0) += 1.0;
                        }
                    }
                    return Ok(Value::mix(weights));
                }
                "Complex" => {
                    let re = match args.first() {
                        Some(Value::Int(i)) => *i as f64,
                        Some(Value::Num(f)) => *f,
                        _ => 0.0,
                    };
                    let im = match args.get(1) {
                        Some(Value::Int(i)) => *i as f64,
                        Some(Value::Num(f)) => *f,
                        _ => 0.0,
                    };
                    return Ok(Value::Complex(re, im));
                }
                "Backtrace" => {
                    let file = self
                        .env
                        .get("?FILE")
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let mut frame_attrs = HashMap::new();
                    frame_attrs.insert("file".to_string(), Value::Str(file));
                    let frame = Value::make_instance("Backtrace::Frame".to_string(), frame_attrs);
                    return Ok(Value::array(vec![frame]));
                }
                "Lock" | "Lock::Async" => {
                    let mut attrs = HashMap::new();
                    let lock_id = super::native_methods::next_lock_id() as i64;
                    attrs.insert("lock-id".to_string(), Value::Int(lock_id));
                    return Ok(Value::make_instance(class_name.clone(), attrs));
                }
                "Slip" => {
                    return Ok(Value::slip(args.clone()));
                }
                "Match" => {
                    // Match.new(:orig("..."), :from(N), :pos(N), :list(...), :hash(...))
                    let mut orig = String::new();
                    let mut from: i64 = 0;
                    let mut to: i64 = 0;
                    let mut list = Value::array(Vec::new());
                    let mut hash = Value::hash(HashMap::new());
                    for arg in &args {
                        if let Value::Pair(key, value) = arg {
                            match key.as_str() {
                                "orig" => orig = value.to_string_value(),
                                "from" => from = to_int(value),
                                "pos" | "to" => to = to_int(value),
                                "list" => list = *value.clone(),
                                "hash" => hash = *value.clone(),
                                _ => {}
                            }
                        }
                    }
                    // Compute matched string from orig[from..pos]
                    let matched: String = orig
                        .chars()
                        .skip(from as usize)
                        .take((to - from) as usize)
                        .collect();
                    let mut attrs = HashMap::new();
                    attrs.insert("str".to_string(), Value::Str(matched));
                    attrs.insert("from".to_string(), Value::Int(from));
                    attrs.insert("to".to_string(), Value::Int(to));
                    attrs.insert("orig".to_string(), Value::Str(orig));
                    // Convert list to positional captures
                    if let Value::Array(items, ..) = &list {
                        attrs.insert("list".to_string(), Value::array(items.to_vec()));
                    } else {
                        attrs.insert("list".to_string(), Value::array(Vec::new()));
                    }
                    // Convert hash (Map) to named captures
                    if let Value::Hash(map, ..) = &hash {
                        attrs.insert("named".to_string(), Value::hash(map.as_ref().clone()));
                    } else {
                        attrs.insert("named".to_string(), Value::hash(HashMap::new()));
                    }
                    return Ok(Value::make_instance("Match".to_string(), attrs));
                }
                // Types that cannot be instantiated with .new
                "HyperWhatever" | "Whatever" | "Junction" => {
                    return Err(RuntimeError::new(format!(
                        "X::Cannot::New: Cannot create new object of type {}",
                        class_name
                    )));
                }
                _ => {}
            }
            // Parametric package handling (e.g. Array[Int], Hash[Int,Str], A[Int]).
            if let Some(type_args) = type_args.as_ref() {
                if matches!(base_class_name, "Array" | "List" | "Positional" | "array") {
                    if let Some(dims) = self.shaped_dims_from_new_args(&args) {
                        let data = args.iter().find_map(|arg| match arg {
                            Value::Pair(name, value) if name == "data" => {
                                Some(value.as_ref().clone())
                            }
                            _ => None,
                        });
                        let shaped = Self::make_shaped_array(&dims);
                        let result = if let Some(data_val) = data {
                            let data_items = match data_val {
                                Value::Array(items, ..)
                                | Value::Seq(items)
                                | Value::Slip(items) => items.to_vec(),
                                other => vec![other],
                            };
                            if let Value::Array(ref items, is_arr) = shaped {
                                let mut new_items = items.as_ref().clone();
                                for (i, val) in data_items.into_iter().enumerate() {
                                    if i < new_items.len() {
                                        new_items[i] = val;
                                    }
                                }
                                let result = Value::Array(std::sync::Arc::new(new_items), is_arr);
                                crate::runtime::utils::mark_shaped_array(&result, Some(&dims));
                                result
                            } else {
                                shaped
                            }
                        } else {
                            shaped
                        };
                        let value_type = type_args
                            .first()
                            .cloned()
                            .unwrap_or_else(|| "Any".to_string());
                        self.register_container_type_metadata(
                            &result,
                            crate::runtime::ContainerTypeInfo {
                                value_type,
                                key_type: None,
                                declared_type: Some(class_name.clone()),
                            },
                        );
                        return Ok(result);
                    }
                    let mut items = Vec::new();
                    for arg in &args {
                        match arg {
                            Value::Slip(vals) => items.extend(vals.iter().cloned()),
                            other => items.push(other.clone()),
                        }
                    }
                    let result = if matches!(base_class_name, "Array" | "array") {
                        Value::real_array(items)
                    } else {
                        Value::array(items)
                    };
                    let value_type = type_args
                        .first()
                        .cloned()
                        .unwrap_or_else(|| "Any".to_string());
                    self.register_container_type_metadata(
                        &result,
                        crate::runtime::ContainerTypeInfo {
                            value_type,
                            key_type: None,
                            declared_type: Some(class_name.clone()),
                        },
                    );
                    return Ok(result);
                }
                if matches!(base_class_name, "Hash" | "Map") {
                    let mut flat = Vec::new();
                    for arg in &args {
                        flat.extend(Self::value_to_list(arg));
                    }
                    let mut map = HashMap::new();
                    let mut iter = flat.into_iter();
                    while let Some(item) = iter.next() {
                        match item {
                            Value::Pair(k, v) => {
                                map.insert(k, *v);
                            }
                            Value::ValuePair(k, v) => {
                                map.insert(k.to_string_value(), *v);
                            }
                            other => {
                                let key = other.to_string_value();
                                let value = iter.next().unwrap_or(Value::Nil);
                                map.insert(key, value);
                            }
                        }
                    }
                    let result = Value::hash(map);
                    let value_type = type_args
                        .first()
                        .cloned()
                        .unwrap_or_else(|| "Any".to_string());
                    let key_type = type_args.get(1).cloned();
                    self.register_container_type_metadata(
                        &result,
                        crate::runtime::ContainerTypeInfo {
                            value_type,
                            key_type,
                            declared_type: Some(class_name.clone()),
                        },
                    );
                    return Ok(result);
                }
            }
            if let Some(role) = self.roles.get(class_name).cloned() {
                let mut named_args: HashMap<String, Value> = HashMap::new();
                let mut positional_args: Vec<Value> = Vec::new();
                for arg in &args {
                    if let Value::Pair(key, value) = arg {
                        named_args.insert(key.clone(), *value.clone());
                    } else {
                        positional_args.push(arg.clone());
                    }
                }

                let mut mixins = HashMap::new();
                mixins.insert(format!("__mutsu_role__{}", class_name), Value::Bool(true));
                for (idx, (attr_name, _is_public, default_expr, _is_rw)) in
                    role.attributes.iter().enumerate()
                {
                    let value = if let Some(v) = named_args.get(attr_name) {
                        v.clone()
                    } else if let Some(v) = positional_args.get(idx) {
                        v.clone()
                    } else if let Some(expr) = default_expr {
                        self.eval_block_value(&[Stmt::Expr(expr.clone())])?
                    } else {
                        Value::Nil
                    };
                    mixins.insert(format!("__mutsu_attr__{}", attr_name), value);
                }
                return Ok(Value::Mixin(
                    Box::new(Value::make_instance(class_name.clone(), HashMap::new())),
                    mixins,
                ));
            }
            if self.classes.contains_key(class_name)
                || type_args
                    .as_ref()
                    .is_some_and(|_| self.classes.contains_key(base_class_name))
            {
                let class_key = if self.classes.contains_key(class_name) {
                    class_name.as_str()
                } else {
                    base_class_name
                };
                // Check for user-defined .new method first
                if self.has_user_method(class_key, "new") {
                    let empty_attrs = HashMap::new();
                    let (result, _updated) =
                        self.run_instance_method(class_name, empty_attrs, "new", args, None)?;
                    return Ok(result);
                }
                let mut attrs = HashMap::new();
                for (attr_name, _is_public, default, _is_rw) in
                    self.collect_class_attributes(class_key)
                {
                    let val = if let Some(expr) = default {
                        self.eval_block_value(&[Stmt::Expr(expr)])?
                    } else {
                        Value::Nil
                    };
                    attrs.insert(attr_name, val);
                }
                let class_mro = self.class_mro(class_key);
                for val in &args {
                    match val {
                        Value::Pair(k, v) => {
                            attrs.insert(k.clone(), *v.clone());
                        }
                        Value::Instance {
                            class_name: src_class,
                            attributes: src_attrs,
                            ..
                        } if class_mro.iter().any(|name| name == src_class) => {
                            for (attr, value) in src_attrs.iter() {
                                if attrs.contains_key(attr) {
                                    attrs.insert(attr.clone(), value.clone());
                                }
                            }
                        }
                        _ => {}
                    }
                }
                let class_def = self.classes.get(class_key);
                let has_direct_build = class_def.and_then(|def| def.methods.get("BUILD")).is_some();
                let has_direct_tweak = class_def.and_then(|def| def.methods.get("TWEAK")).is_some();
                if self.class_has_method(class_name, "BUILD") {
                    let build_args = if has_direct_build {
                        args.clone()
                    } else {
                        Vec::new()
                    };
                    let (_v, updated) = self.run_instance_method(
                        class_name,
                        attrs.clone(),
                        "BUILD",
                        build_args,
                        Some(Value::make_instance(class_name.clone(), attrs.clone())),
                    )?;
                    attrs = updated;
                }
                if self.class_has_method(class_name, "TWEAK") {
                    let tweak_args = if has_direct_tweak {
                        args.clone()
                    } else {
                        Vec::new()
                    };
                    let (_v, updated) = self.run_instance_method(
                        class_name,
                        attrs.clone(),
                        "TWEAK",
                        tweak_args,
                        Some(Value::make_instance(class_name.clone(), attrs.clone())),
                    )?;
                    attrs = updated;
                }
                let instance = Value::make_instance(class_name.clone(), attrs);
                if let Some(type_args) = type_args.as_ref() {
                    if self.class_mro(class_key).iter().any(|n| n == "Array") {
                        let value_type = type_args
                            .first()
                            .cloned()
                            .unwrap_or_else(|| "Any".to_string());
                        self.register_container_type_metadata(
                            &instance,
                            crate::runtime::ContainerTypeInfo {
                                value_type,
                                key_type: None,
                                declared_type: Some(class_name.clone()),
                            },
                        );
                    } else if self.class_mro(class_key).iter().any(|n| n == "Hash") {
                        let value_type = type_args
                            .first()
                            .cloned()
                            .unwrap_or_else(|| "Any".to_string());
                        let key_type = type_args.get(1).cloned();
                        self.register_container_type_metadata(
                            &instance,
                            crate::runtime::ContainerTypeInfo {
                                value_type,
                                key_type,
                                declared_type: Some(class_name.clone()),
                            },
                        );
                    }
                }
                return Ok(instance);
            }
        }
        // Fallback .new on basic types
        match target {
            Value::Package(name) if name == "CallFrame" => {
                // CallFrame.new(depth) — equivalent to callframe(depth)
                let depth = args
                    .first()
                    .and_then(|v| match v {
                        Value::Int(i) => Some(*i as usize),
                        Value::Num(f) => Some(*f as usize),
                        _ => None,
                    })
                    .unwrap_or(0);
                self.builtin_callframe(&args, depth)
            }
            Value::Package(name) => Err(RuntimeError::new(format!(
                "X::Method::NotFound: Unknown method value dispatch (fallback disabled): new on {}",
                name
            ))),
            Value::Str(_) => Ok(Value::Str(String::new())),
            Value::Int(_) => Ok(Value::Int(0)),
            Value::Num(_) => Ok(Value::Num(0.0)),
            Value::Bool(_) => Ok(Value::Bool(false)),
            Value::Nil => Ok(Value::Nil),
            _ => Err(RuntimeError::new(
                "X::Method::NotFound: Unknown method value dispatch (fallback disabled): new",
            )),
        }
    }

    fn dispatch_grep(&mut self, target: Value, args: &[Value]) -> Result<Value, RuntimeError> {
        match target {
            Value::Package(class_name) if class_name == "Supply" => Err(RuntimeError::new(
                "Cannot call .grep on a Supply type object",
            )),
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Supply" => {
                let source_values = if let Some(on_demand_cb) = attributes.get("on_demand_callback")
                {
                    let emitter = Value::make_instance("Supplier".to_string(), {
                        let mut a = HashMap::new();
                        a.insert("emitted".to_string(), Value::array(Vec::new()));
                        a.insert("done".to_string(), Value::Bool(false));
                        a
                    });
                    self.supply_emit_buffer.push(Vec::new());
                    let _ = self.call_sub_value(on_demand_cb.clone(), vec![emitter], false);
                    self.supply_emit_buffer.pop().unwrap_or_default()
                } else {
                    attributes
                        .get("values")
                        .and_then(|v| {
                            if let Value::Array(items, ..) = v {
                                Some(items.to_vec())
                            } else {
                                None
                            }
                        })
                        .unwrap_or_default()
                };
                let filtered = self.eval_grep_over_items(args.first().cloned(), source_values)?;
                let filtered_values = Self::value_to_list(&filtered);
                let mut attrs = HashMap::new();
                attrs.insert("values".to_string(), Value::array(filtered_values));
                attrs.insert("taps".to_string(), Value::array(Vec::new()));
                attrs.insert(
                    "live".to_string(),
                    attributes
                        .get("live")
                        .cloned()
                        .unwrap_or(Value::Bool(false)),
                );
                Ok(Value::make_instance("Supply".to_string(), attrs))
            }
            Value::Array(items, ..) => {
                self.eval_grep_over_items(args.first().cloned(), items.to_vec())
            }
            Value::Range(a, b) => {
                let items: Vec<Value> = (a..=b).map(Value::Int).collect();
                self.eval_grep_over_items(args.first().cloned(), items)
            }
            Value::RangeExcl(a, b) => {
                let items: Vec<Value> = (a..b).map(Value::Int).collect();
                self.eval_grep_over_items(args.first().cloned(), items)
            }
            Value::RangeExclStart(a, b) => {
                let items: Vec<Value> = (a + 1..=b).map(Value::Int).collect();
                self.eval_grep_over_items(args.first().cloned(), items)
            }
            Value::RangeExclBoth(a, b) => {
                let items: Vec<Value> = (a + 1..b).map(Value::Int).collect();
                self.eval_grep_over_items(args.first().cloned(), items)
            }
            Value::GenericRange { .. } => {
                let items = crate::runtime::utils::value_to_list(&target);
                self.eval_grep_over_items(args.first().cloned(), items)
            }
            other => Ok(other),
        }
    }

    fn dispatch_first(&mut self, target: Value, args: &[Value]) -> Result<Value, RuntimeError> {
        // Separate named args (Pairs) from positional args
        let mut positional = Vec::new();
        let mut has_neg_v = false;
        let mut has_end = false;
        let mut has_k = false;
        let mut has_kv = false;
        let mut has_p = false;
        for arg in args {
            match arg {
                Value::Pair(key, value) if key == "v" => {
                    if !value.truthy() {
                        has_neg_v = true;
                    }
                }
                Value::Pair(key, value) if key == "end" => {
                    if value.truthy() {
                        has_end = true;
                    }
                }
                Value::Pair(key, value) if key == "k" => {
                    has_k = value.truthy();
                }
                Value::Pair(key, value) if key == "kv" => {
                    has_kv = value.truthy();
                }
                Value::Pair(key, value) if key == "p" => {
                    has_p = value.truthy();
                }
                _ => positional.push(arg.clone()),
            }
        }
        if has_neg_v {
            return Err(RuntimeError::new(
                "Throwing `:!v` on first is not supported",
            ));
        }
        // Check for Bool matcher (X::Match::Bool)
        if matches!(positional.first(), Some(Value::Bool(_))) {
            let mut err = RuntimeError::new("Cannot use Bool as a matcher");
            err.exception = Some(Box::new(Value::make_instance(
                "X::Match::Bool".to_string(),
                std::collections::HashMap::new(),
            )));
            return Err(err);
        }
        let func = positional.first().cloned();
        let items = crate::runtime::utils::value_to_list(&target);
        if let Some((idx, value)) = self.find_first_match_over_items(func, &items, has_end)? {
            return Ok(super::builtins_collection::format_first_result(
                idx, value, has_k, has_kv, has_p,
            ));
        }
        Ok(Value::Nil)
    }

    /// `$n.polymod(@divisors)` — successive modular decomposition.
    fn method_polymod(&mut self, target: &Value, args: &[Value]) -> Result<Value, RuntimeError> {
        fn val_to_f64(v: &Value) -> f64 {
            match v {
                Value::Int(n) => *n as f64,
                Value::Num(n) => *n,
                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                Value::Bool(b) => {
                    if *b {
                        1.0
                    } else {
                        0.0
                    }
                }
                Value::Str(s) => s.parse::<f64>().unwrap_or(0.0),
                _ => 0.0,
            }
        }
        fn f64_to_val(n: f64) -> Value {
            if n.is_finite() && n == n.trunc() && n.abs() < i64::MAX as f64 {
                Value::Int(n as i64)
            } else {
                Value::Num(n)
            }
        }
        let mut n = val_to_f64(target);
        // Flatten args into a list of divisors
        let mut divisors = Vec::new();
        for arg in args {
            match arg {
                Value::Array(items, ..) => divisors.extend(items.iter().cloned()),
                _ => divisors.push(arg.clone()),
            }
        }
        let mut result = Vec::new();
        for d in &divisors {
            let d_val = val_to_f64(d);
            if d_val == 0.0 {
                result.push(f64_to_val(n));
                n = f64::INFINITY;
                continue;
            }
            let rem = n % d_val;
            let quot = ((n - rem) / d_val).trunc();
            result.push(f64_to_val(rem));
            n = quot;
            // Modulo 1 always yields remainder 0 and quotient = n; stop infinite loops
            if d_val == 1.0 {
                break;
            }
        }
        result.push(f64_to_val(n));
        Ok(Value::array(result))
    }

    fn dispatch_tree(&mut self, target: Value, args: &[Value]) -> Result<Value, RuntimeError> {
        // Non-iterable: .tree(anything) returns self
        let items = match &target {
            Value::Array(items, ..) => items.clone(),
            _ => return Ok(target),
        };

        let arg = &args[0];
        match arg {
            // .tree(0) — identity
            Value::Int(0) => Ok(target),
            // .tree(n) — tree to n levels
            Value::Int(n) if *n > 0 => Ok(Value::array(self.tree_depth(&items, *n as usize)?)),
            // .tree(*) — full depth (same as .tree()); * compiles to Inf
            Value::Num(f) if f.is_infinite() && f.is_sign_positive() => {
                Ok(Value::array(self.tree_depth(&items, usize::MAX)?))
            }
            // .tree([&first, *@rest]) — array of closures form
            Value::Array(closure_list, ..) => {
                if closure_list.is_empty() {
                    return Ok(target);
                }
                let first = &closure_list[0];
                self.call_sub_value(first.clone(), vec![target], false)
            }
            // .tree(&closure, ...) — apply closures at depth levels
            Value::Sub(_) => {
                let closures: Vec<Value> = args.to_vec();
                self.tree_with_closures(&items, &closures, 0)
            }
            _ => Ok(target),
        }
    }

    fn tree_depth(&mut self, items: &[Value], depth: usize) -> Result<Vec<Value>, RuntimeError> {
        let mut result = Vec::new();
        for item in items {
            match item {
                Value::Array(inner, ..) if depth > 0 => {
                    result.push(Value::array(self.tree_depth(inner, depth - 1)?));
                }
                other => result.push(other.clone()),
            }
        }
        Ok(result)
    }

    fn tree_with_closures(
        &mut self,
        items: &[Value],
        closures: &[Value],
        depth: usize,
    ) -> Result<Value, RuntimeError> {
        let mut processed = Vec::new();
        for item in items {
            match item {
                Value::Array(inner, ..) if closures.len() > depth + 1 => {
                    let sub_result = self.tree_with_closures(inner, closures, depth + 1)?;
                    processed.push(sub_result);
                }
                Value::Array(inner, ..) => {
                    // Apply the last closure to leaf arrays
                    if let Some(closure) = closures.last()
                        && closures.len() > 1
                    {
                        processed.push(self.call_sub_value(
                            closure.clone(),
                            vec![Value::Array(inner.clone(), false)],
                            false,
                        )?);
                    } else {
                        processed.push(Value::Array(inner.clone(), false)); // already Arc-wrapped
                    }
                }
                other => processed.push(other.clone()),
            }
        }
        // Apply the closure at this depth level
        if let Some(closure) = closures.get(depth) {
            self.call_sub_value(closure.clone(), vec![Value::array(processed)], false)
        } else {
            Ok(Value::array(processed))
        }
    }

    pub(super) fn dispatch_socket_connect(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let host = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let port = args
            .get(1)
            .map(|v| match v {
                Value::Int(i) => *i as u16,
                Value::Num(f) => *f as u16,
                other => other.to_string_value().parse::<u16>().unwrap_or(0),
            })
            .unwrap_or(0);
        let addr = format!("{}:{}", host, port);
        let stream = std::net::TcpStream::connect_timeout(
            &addr
                .to_socket_addrs()
                .map_err(|e| RuntimeError::new(format!("Failed to resolve '{}': {}", addr, e)))?
                .next()
                .ok_or_else(|| RuntimeError::new(format!("No addresses found for '{}'", addr)))?,
            Duration::from_secs(10),
        )
        .map_err(|e| RuntimeError::new(format!("Failed to connect to '{}': {}", addr, e)))?;
        let id = self.next_handle_id;
        self.next_handle_id += 1;
        let state = IoHandleState {
            target: IoHandleTarget::Socket,
            mode: IoHandleMode::ReadWrite,
            path: None,
            line_separators: self.default_line_separators(),
            line_chomp: true,
            encoding: "utf-8".to_string(),
            file: None,
            socket: Some(stream),
            closed: false,
            bin: false,
        };
        self.handles.insert(id, state);
        let mut attrs = HashMap::new();
        attrs.insert("handle".to_string(), Value::Int(id as i64));
        attrs.insert("host".to_string(), Value::Str(host));
        attrs.insert("port".to_string(), Value::Int(port as i64));
        Ok(Value::make_instance("IO::Socket::INET".to_string(), attrs))
    }

    /// Replay deferred Proc::Async taps on the main thread.
    /// Called when a Proc result is retrieved via .result or await.
    pub(super) fn replay_proc_taps(&mut self, attributes: &Arc<HashMap<String, Value>>) {
        let stdout_taps = match attributes.get("stdout_taps") {
            Some(Value::Array(taps, ..)) => taps.to_vec(),
            _ => Vec::new(),
        };
        let stderr_taps = match attributes.get("stderr_taps") {
            Some(Value::Array(taps, ..)) => taps.to_vec(),
            _ => Vec::new(),
        };
        let collected_stdout = match attributes.get("collected_stdout") {
            Some(Value::Str(s)) => s.clone(),
            _ => String::new(),
        };
        let collected_stderr = match attributes.get("collected_stderr") {
            Some(Value::Str(s)) => s.clone(),
            _ => String::new(),
        };

        if !collected_stdout.is_empty() && !stdout_taps.is_empty() {
            for tap in &stdout_taps {
                let _ = self.call_sub_value(
                    tap.clone(),
                    vec![Value::Str(collected_stdout.clone())],
                    true,
                );
            }
        }
        if !collected_stderr.is_empty() && !stderr_taps.is_empty() {
            for tap in &stderr_taps {
                let _ = self.call_sub_value(
                    tap.clone(),
                    vec![Value::Str(collected_stderr.clone())],
                    true,
                );
            }
        }
    }

    /// Returns Some(class_name) if target is Promise or a Promise subclass package.
    fn promise_class_name(&mut self, target: &Value) -> Option<String> {
        match target {
            Value::Package(name) => {
                if name == "Promise" {
                    Some("Promise".to_string())
                } else if self.class_mro(name).contains(&"Promise".to_string()) {
                    Some(name.clone())
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn dispatch_encoding_registry_find(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let name = args
            .first()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        if let Some(entry) = self.find_encoding(&name) {
            if let Some(ref user_type) = entry.user_type {
                // User-registered encoding: return an instance of the user's type
                return Ok(user_type.clone());
            }
            // Built-in encoding: create an Encoding::Builtin instance
            let mut attrs = HashMap::new();
            attrs.insert("name".to_string(), Value::Str(entry.name.clone()));
            let alt_names: Vec<Value> = entry
                .alternative_names
                .iter()
                .map(|s| Value::Str(s.clone()))
                .collect();
            attrs.insert("alternative-names".to_string(), Value::array(alt_names));
            Ok(Value::make_instance("Encoding::Builtin".to_string(), attrs))
        } else {
            // Throw X::Encoding::Unknown
            let mut ex_attrs = HashMap::new();
            ex_attrs.insert("name".to_string(), Value::Str(name.clone()));
            let ex = Value::make_instance("X::Encoding::Unknown".to_string(), ex_attrs);
            let mut err = RuntimeError::new(format!("Unknown encoding '{}'", name));
            err.exception = Some(Box::new(ex));
            Err(err)
        }
    }

    fn dispatch_encoding_registry_register(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let encoding_val = args.first().cloned().unwrap_or(Value::Nil);
        // The encoding is a type object (Package) or class instance.
        // We need to call .name and .alternative-names on it to get registration info.
        let enc_name = self.call_method_with_values(encoding_val.clone(), "name", vec![])?;
        let enc_name_str = enc_name.to_string_value();

        let alt_names_val = self
            .call_method_with_values(encoding_val.clone(), "alternative-names", vec![])
            .unwrap_or(Value::array(Vec::new()));
        let alt_names: Vec<String> = match alt_names_val {
            Value::Array(items, ..) => items.iter().map(|v| v.to_string_value()).collect(),
            Value::Slip(items) => items.iter().map(|v| v.to_string_value()).collect(),
            _ => Vec::new(),
        };

        let entry = super::EncodingEntry {
            name: enc_name_str.clone(),
            alternative_names: alt_names,
            user_type: Some(encoding_val),
        };

        match self.register_encoding(entry) {
            Ok(()) => Ok(Value::Nil),
            Err(conflicting_name) => {
                let mut ex_attrs = HashMap::new();
                ex_attrs.insert("name".to_string(), Value::Str(conflicting_name.clone()));
                let ex =
                    Value::make_instance("X::Encoding::AlreadyRegistered".to_string(), ex_attrs);
                let mut err = RuntimeError::new(format!(
                    "Encoding '{}' is already registered",
                    conflicting_name
                ));
                err.exception = Some(Box::new(ex));
                Err(err)
            }
        }
    }

    fn dispatch_rotor(&mut self, target: Value, args: &[Value]) -> Result<Value, RuntimeError> {
        use crate::runtime::utils::to_float_value;

        // Extract :partial named arg
        let mut partial = false;
        let mut positional_args: Vec<Value> = Vec::new();
        for arg in args {
            match arg {
                Value::Pair(key, val) if key == "partial" => {
                    partial = val.truthy();
                }
                _ => positional_args.push(arg.clone()),
            }
        }

        // Build spec list from positional args
        // If single arg is a list/array, use its elements as specs
        let specs = if positional_args.len() == 1 {
            match &positional_args[0] {
                Value::Array(items, ..) => items.to_vec(),
                Value::Seq(items) => items.to_vec(),
                Value::LazyList(_) => Self::value_to_list(&positional_args[0]),
                other => vec![other.clone()],
            }
        } else {
            positional_args
        };

        // Parse each spec into (count, gap) pairs
        // count can be: Int, Whatever (*), Inf, Range
        // gap is from Pair's value
        struct RotorSpec {
            count: RotorCount,
            gap: i64,
        }
        #[derive(Clone)]
        enum RotorCount {
            Fixed(usize),
            Whatever,        // * — take everything remaining
            Inf,             // Inf — take everything remaining
            Range(Vec<i64>), // 1..* or similar — cycling counts
        }

        // Flatten any nested Seq/Array specs into a flat list
        let mut flat_specs: Vec<Value> = Vec::new();
        let mut to_process: std::collections::VecDeque<Value> = specs.into();
        while let Some(spec) = to_process.pop_front() {
            match &spec {
                Value::Seq(items) => {
                    for item in items.iter() {
                        to_process.push_back(item.clone());
                    }
                }
                Value::Array(items, ..) => {
                    for item in items.iter() {
                        to_process.push_back(item.clone());
                    }
                }
                _ => flat_specs.push(spec),
            }
        }

        let mut rotor_specs: Vec<RotorSpec> = Vec::new();
        for spec in &flat_specs {
            match spec {
                Value::Int(n) => {
                    let count = *n;
                    if count < 0 {
                        let mut attrs = HashMap::new();
                        attrs.insert("got".to_string(), Value::Int(count));
                        attrs.insert(
                            "message".to_string(),
                            Value::Str(format!(
                                "Expected a non-negative integer for rotor count, got {}",
                                count
                            )),
                        );
                        let ex = Value::make_instance("X::OutOfRange".to_string(), attrs);
                        let mut err = RuntimeError::new(format!(
                            "X::OutOfRange: Expected non-negative count, got {}",
                            count
                        ));
                        err.exception = Some(Box::new(ex));
                        return Err(err);
                    }
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Fixed(count as usize),
                        gap: 0,
                    });
                }
                Value::Whatever => {
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Inf,
                        gap: 0,
                    });
                }
                Value::Num(n) if n.is_infinite() && n.is_sign_positive() => {
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Inf,
                        gap: 0,
                    });
                }
                Value::Num(n) => {
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Fixed(*n as usize),
                        gap: 0,
                    });
                }
                Value::Rat(n, d) => {
                    let count = if *d != 0 { n / d } else { 0 };
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Fixed(count as usize),
                        gap: 0,
                    });
                }
                Value::Pair(..) | Value::ValuePair(..) => {
                    let (count_val, gap_val) = match spec {
                        Value::Pair(k, v) => (Value::Str(k.clone()), v.as_ref().clone()),
                        Value::ValuePair(k, v) => (k.as_ref().clone(), v.as_ref().clone()),
                        _ => unreachable!(),
                    };
                    let count = match &count_val {
                        Value::Int(n) => RotorCount::Fixed(*n as usize),
                        Value::Num(n) => RotorCount::Fixed(*n as usize),
                        Value::Rat(n, d) => {
                            RotorCount::Fixed(if *d != 0 { (n / d) as usize } else { 0 })
                        }
                        Value::Str(s) => RotorCount::Fixed(s.parse::<i64>().unwrap_or(0) as usize),
                        _ => RotorCount::Fixed(0),
                    };
                    let gap = match &gap_val {
                        Value::Int(n) => *n,
                        Value::Num(n) => *n as i64,
                        Value::Rat(n, d) => {
                            if *d != 0 {
                                n / d
                            } else {
                                0
                            }
                        }
                        _ => 0,
                    };
                    rotor_specs.push(RotorSpec { count, gap });
                }
                Value::HyperWhatever => {
                    rotor_specs.push(RotorSpec {
                        count: RotorCount::Whatever,
                        gap: 0,
                    });
                }
                Value::Range(start, end) | Value::RangeExcl(start, end) => {
                    let is_excl = matches!(spec, Value::RangeExcl(..));
                    let end_val = if is_excl { *end - 1 } else { *end };
                    if end_val == i64::MAX || *end == i64::MAX {
                        // 1..* — infinite range, treated like cycling counts
                        let mut counts = Vec::new();
                        for i in *start.. {
                            counts.push(i);
                            if counts.len() > 10000 {
                                break; // safety limit
                            }
                        }
                        rotor_specs.push(RotorSpec {
                            count: RotorCount::Range(counts),
                            gap: 0,
                        });
                    } else {
                        let mut counts = Vec::new();
                        for i in *start..=end_val {
                            counts.push(i);
                        }
                        rotor_specs.push(RotorSpec {
                            count: RotorCount::Range(counts),
                            gap: 0,
                        });
                    }
                }
                _ => {
                    // Try to coerce to int
                    if let Some(n) = to_float_value(spec) {
                        if n.is_infinite() && n.is_sign_positive() {
                            rotor_specs.push(RotorSpec {
                                count: RotorCount::Inf,
                                gap: 0,
                            });
                        } else {
                            rotor_specs.push(RotorSpec {
                                count: RotorCount::Fixed(n as usize),
                                gap: 0,
                            });
                        }
                    }
                }
            }
        }

        if rotor_specs.is_empty() {
            return Ok(Value::Seq(Arc::new(Vec::new())));
        }

        // Get the items to rotor over (force LazyList if needed)
        let target = if let Value::LazyList(ll) = &target {
            Value::array(self.force_lazy_list_bridge(ll)?)
        } else {
            target
        };
        let items = Self::value_to_list(&target);

        if items.is_empty() {
            return Ok(Value::Seq(Arc::new(Vec::new())));
        }

        let mut result: Vec<Value> = Vec::new();
        let mut pos = 0usize;
        let mut spec_idx = 0usize;
        let mut range_sub_idx = 0usize; // for Range specs

        loop {
            if pos >= items.len() {
                break;
            }

            let spec = &rotor_specs[spec_idx % rotor_specs.len()];

            let count = match &spec.count {
                RotorCount::Fixed(n) => *n,
                RotorCount::Whatever | RotorCount::Inf => items.len() - pos,
                RotorCount::Range(counts) => {
                    let c = counts[range_sub_idx % counts.len()];
                    if c == i64::MAX {
                        items.len() - pos
                    } else {
                        c as usize
                    }
                }
            };

            let gap = spec.gap;

            // Take `count` items starting at pos
            let end = std::cmp::min(pos.saturating_add(count), items.len());
            let chunk_len = end - pos;
            let chunk: Vec<Value> = items[pos..end].to_vec();

            if (chunk_len == count || partial) && (!chunk.is_empty() || count == 0) {
                result.push(Value::array(chunk));
            }

            if chunk_len < count && !partial {
                break;
            }

            // Advance position: count + gap (gap can be negative for overlap)
            let new_pos = (pos as i64).saturating_add((count as i64).saturating_add(gap));
            if new_pos < 0 {
                // Negative gap past start of list
                let mut attrs = HashMap::new();
                attrs.insert("got".to_string(), Value::Int(new_pos));
                attrs.insert(
                    "message".to_string(),
                    Value::Str(
                        "Rotoring gap is too large and causes an index below zero".to_string(),
                    ),
                );
                let ex = Value::make_instance("X::OutOfRange".to_string(), attrs);
                let mut err =
                    RuntimeError::new("X::OutOfRange: Rotoring gap is too large".to_string());
                err.exception = Some(Box::new(ex));
                return Err(err);
            }
            pos = new_pos as usize;

            // Advance spec index
            match &spec.count {
                RotorCount::Range(counts) => {
                    range_sub_idx += 1;
                    if range_sub_idx >= counts.len() {
                        range_sub_idx = 0;
                        spec_idx += 1;
                    }
                }
                _ => {
                    spec_idx += 1;
                }
            }
        }

        Ok(Value::Seq(Arc::new(result)))
    }
}
