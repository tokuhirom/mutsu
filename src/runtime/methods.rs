use super::*;
use crate::ast::CallArg;
use crate::value::signature::make_params_value_from_param_defs;

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
                | Expr::Exists(expr)
                | Expr::Reduction { expr, .. } => scan_expr(expr, positional, named),
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
                | Expr::AnonSub(stmts)
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

    fn render_signature_value(expr: &Expr) -> String {
        match expr {
            Expr::Literal(v) => match v {
                Value::Str(s) => format!("\"{}\"", s.replace('"', "\\\"")),
                _ => v.to_string_value(),
            },
            _ => "...".to_string(),
        }
    }

    fn render_where_constraint(where_expr: &Expr) -> String {
        if let Expr::AnonSub(body) = where_expr
            && body.len() == 1
            && let Stmt::Expr(Expr::Literal(Value::Bool(true))) = &body[0]
        {
            return "{ True }".to_string();
        }
        "{ ... }".to_string()
    }

    fn render_signature_param(pd: &ParamDef) -> String {
        let mut out = String::new();
        if let Some(tc) = &pd.type_constraint {
            out.push_str(tc);
            out.push(' ');
        }
        if pd.named {
            out.push(':');
        }
        if pd.slurpy {
            out.push('*');
        }
        if !pd.name.starts_with('$')
            && !pd.name.starts_with('@')
            && !pd.name.starts_with('%')
            && !pd.name.starts_with('_')
            && !pd.slurpy
        {
            out.push('$');
        }
        out.push_str(&pd.name);
        if pd.required && pd.default.is_none() {
            out.push('!');
        }
        for trait_name in &pd.traits {
            out.push_str(" is ");
            out.push_str(trait_name);
        }
        if let Some(where_expr) = &pd.where_constraint {
            out.push_str(" where ");
            out.push_str(&Self::render_where_constraint(where_expr));
        }
        if let Some(default) = &pd.default {
            out.push_str(" = ");
            out.push_str(&Self::render_signature_value(default));
        }
        out
    }

    fn render_sub_signature(
        &self,
        data: &crate::value::SubData,
        assumed_positional: &[Value],
        assumed_named: &std::collections::HashMap<String, Value>,
    ) -> String {
        if let Some(param_defs) =
            Self::assumed_signature_param_defs(data, assumed_positional, assumed_named)
        {
            let rendered: Vec<String> = param_defs
                .iter()
                .map(Self::render_signature_param)
                .collect();
            return format!(":({})", rendered.join(", "));
        }
        if !data.params.is_empty() {
            let rendered: Vec<String> = data.params.iter().map(|p| format!("${}", p)).collect();
            return format!(":({})", rendered.join(", "));
        }
        // Auto-detect @_ / %_ usage for subs without explicit signatures
        let (use_positional, use_named) = Self::auto_signature_uses(&data.body);
        if use_positional || use_named {
            let mut parts = Vec::new();
            if use_positional {
                parts.push("*@_".to_string());
            }
            if use_named {
                parts.push("*%_".to_string());
            }
            return format!(":({})", parts.join(", "));
        }
        ":()".to_string()
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

    fn shaped_dims_from_new_args(args: &[Value]) -> Option<Vec<usize>> {
        let shape_val = args.iter().find_map(|arg| match arg {
            Value::Pair(name, value) if name == "shape" => Some(value.as_ref().clone()),
            _ => None,
        })?;
        let dims_vals = match shape_val {
            Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => items.to_vec(),
            Value::Int(i) => vec![Value::Int(i)],
            _ => return None,
        };
        let mut dims = Vec::with_capacity(dims_vals.len());
        for dim in dims_vals {
            let n = match dim {
                Value::Int(i) if i >= 0 => i as usize,
                Value::Num(f) if f >= 0.0 => f as usize,
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
            return Value::array(vec![Value::Nil; len]);
        }
        let child = Self::make_shaped_array(&dims[1..]);
        Value::array((0..len).map(|_| child.clone()).collect())
    }

    fn infer_array_shape(value: &Value) -> Option<Vec<usize>> {
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

    pub(crate) fn call_method_with_values(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if matches!(method, "max" | "min")
            && matches!(&target, Value::Package(name) if name == "Supply")
        {
            return Err(RuntimeError::new(format!(
                "Cannot call .{} on a Supply type object",
                method
            )));
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
                    let (result, _updated) = self.run_instance_method_resolved(
                        &role_name,
                        &role_name,
                        def,
                        role_attrs,
                        args,
                        Some(target.clone()),
                    )?;
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

        // Skip native fast path for pseudo-methods when called with quoted syntax
        let skip_pseudo = self
            .skip_pseudo_method_native
            .as_ref()
            .is_some_and(|m| m == method);
        if skip_pseudo {
            self.skip_pseudo_method_native = None;
        }
        let bypass_native_fastpath = skip_pseudo
            || (matches!(method, "max" | "min")
                && matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply"))
            || (method == "Supply"
                && matches!(&target, Value::Instance { class_name, .. } if class_name == "Supplier"));
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

        // Resolve Value::Routine to Value::Sub for method dispatch
        if let Value::Routine {
            ref name,
            ref package,
        } = target
            && method == "assuming"
        {
            // Create a wrapper Sub that delegates to the multi-dispatch routine
            let mut sub_data = crate::value::SubData {
                package: package.clone(),
                name: name.clone(),
                params: Vec::new(),
                param_defs: Vec::new(),
                body: vec![],
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
        if let Value::Sub(data) = &target {
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
                for name in incoming_named.keys() {
                    if !known_named.contains(name) {
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
            if method == "signature" && args.is_empty() {
                let sig =
                    self.render_sub_signature(data, &data.assumed_positional, &data.assumed_named);
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("raku".to_string(), Value::Str(sig.clone()));
                attrs.insert("perl".to_string(), Value::Str(sig.clone()));
                attrs.insert("Str".to_string(), Value::Str(sig.clone()));
                attrs.insert("gist".to_string(), Value::Str(sig));
                let param_defs = Self::assumed_signature_param_defs(
                    data,
                    &data.assumed_positional,
                    &data.assumed_named,
                )
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
                            });
                        }
                        defs
                    }
                });
                attrs.insert(
                    "params".to_string(),
                    make_params_value_from_param_defs(&param_defs),
                );
                return Ok(Value::make_instance("Signature".to_string(), attrs));
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
            && class_name == "Perl6::Metamodel::ClassHOW"
            && matches!(
                method,
                "can" | "isa" | "lookup" | "find_method" | "add_method" | "name" | "ver" | "auth"
            )
        {
            return self.dispatch_classhow_method(method, args);
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
                self.output.push_str(&crate::runtime::gist_value(&target));
                self.output.push('\n');
                return Ok(Value::Nil);
            }
            "print" if args.is_empty() => {
                self.output.push_str(&target.to_string_value());
                return Ok(Value::Nil);
            }
            "put" if args.is_empty() => {
                self.output.push_str(&crate::runtime::gist_value(&target));
                self.output.push('\n');
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
                    let now = SystemTime::now()
                        .duration_since(UNIX_EPOCH)
                        .unwrap_or_default()
                        .as_secs_f64();
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
                    Value::Complex(_, _) => "Complex",
                    Value::Set(_) => "Set",
                    Value::Bag(_) => "Bag",
                    Value::Mix(_) => "Mix",
                    Value::Pair(_, _) | Value::ValuePair(_, _) => "Pair",
                    Value::Enum { enum_type, .. } => enum_type.as_str(),
                    Value::Nil => "Any",
                    Value::Package(name) => name.as_str(),
                    Value::Routine { .. } => "Routine",
                    Value::Sub(_) | Value::WeakSub(_) => "Sub",
                    Value::CompUnitDepSpec { .. } => "CompUnit::DependencySpecification",
                    Value::Instance { class_name, .. } => class_name.as_str(),
                    Value::Junction { .. } => "Junction",
                    Value::Regex(_) | Value::RegexWithAdverbs { .. } => "Regex",
                    Value::Version { .. } => "Version",
                    Value::Slip(_) => "Slip",
                    Value::Seq(_) => "Seq",
                    Value::Promise(_) => "Promise",
                    Value::Channel(_) => "Channel",
                    Value::HyperWhatever => "HyperWhatever",
                    Value::Capture { .. } => "Capture",
                    Value::Uni { form, .. } => form.as_str(),
                    Value::Mixin(inner, _) => {
                        return self.call_method_with_values(*inner.clone(), "WHAT", args.clone());
                    }
                    Value::Proxy { .. } => "Proxy",
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
                return Ok(Value::Package(type_name.to_string()));
            }
            "HOW" => {
                if !args.is_empty() {
                    return Err(RuntimeError::new(
                        "X::Syntax::Argument::MOPMacro: HOW does not take arguments",
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
                let mut attrs = HashMap::new();
                attrs.insert("name".to_string(), Value::Str(type_name));
                return Ok(Value::make_instance(
                    "Perl6::Metamodel::ClassHOW".to_string(),
                    attrs,
                ));
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
            "subparse" | "parse" => {
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
                let mut pattern_arg: Option<&Value> = None;
                for arg in &args {
                    if let Value::Pair(key, value) = arg {
                        if (key == "ov" || key == "overlap") && value.truthy() {
                            overlap = true;
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
                        if let Some(captures) = self.regex_match_with_captures(pat, &text) {
                            let matched = captures.matched.clone();
                            let from = captures.from as i64;
                            let to = captures.to as i64;
                            for (i, v) in captures.positional.iter().enumerate() {
                                self.env.insert(i.to_string(), Value::Str(v.clone()));
                            }
                            for (k, v) in &captures.named {
                                let value = if v.len() == 1 {
                                    Value::Str(v[0].clone())
                                } else {
                                    Value::array(v.iter().cloned().map(Value::Str).collect())
                                };
                                self.env.insert(format!("<{}>", k), value);
                            }
                            Ok(Value::make_match_object_with_captures(
                                matched,
                                from,
                                to,
                                &captures.positional,
                                &captures.named,
                            ))
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
            "collate" if args.is_empty() => {
                return self.dispatch_collate(target);
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
                    let secs = SystemTime::now()
                        .duration_since(UNIX_EPOCH)
                        .map(|d| d.as_secs_f64())
                        .unwrap_or(0.0);
                    let mut attrs = HashMap::new();
                    attrs.insert("epoch".to_string(), Value::Num(secs));
                    return Ok(Value::make_instance("DateTime".to_string(), attrs));
                }
            }
            "today" if args.is_empty() => {
                if let Value::Package(ref class_name) = target
                    && class_name == "Date"
                {
                    let secs = SystemTime::now()
                        .duration_since(UNIX_EPOCH)
                        .map(|d| d.as_secs())
                        .unwrap_or(0);
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

        // Package (type object) dispatch — check user-defined methods
        if let Value::Package(ref name) = target
            && self.has_user_method(name, method)
        {
            let attrs = HashMap::new();
            let (result, _updated) = self.run_instance_method(name, attrs, method, args, None)?;
            return Ok(result);
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

        // Fallback methods
        match method {
            "DUMP" if args.is_empty() => match target {
                Value::Package(name) => Ok(Value::Str(format!("{}()", name))),
                other => Ok(Value::Str(other.to_string_value())),
            },
            "gist" if args.is_empty() => match target {
                Value::Package(name) => {
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
        let mut source: Option<String> = None;
        let mut start_rule = "TOP".to_string();
        for arg in args {
            if let Value::Pair(key, value) = arg {
                if key == "rule" || key == "token" {
                    start_rule = value.to_string_value();
                }
                continue;
            }
            if source.is_none() {
                source = Some(arg.to_string_value());
            }
        }
        let Some(text) = source else {
            return Ok(Value::Nil);
        };

        let saved_package = self.current_package.clone();
        let saved_topic = self.env.get("_").cloned();
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
            let pattern = match self.eval_token_call_values(&start_rule, &[]) {
                Ok(Some(pattern)) => pattern,
                Ok(None) => {
                    self.env.insert("/".to_string(), Value::Nil);
                    return Ok(Value::Nil);
                }
                Err(err)
                    if err
                        .message
                        .contains("No matching candidates for proto token") =>
                {
                    self.env.insert("/".to_string(), Value::Nil);
                    return Ok(Value::Nil);
                }
                Err(err) => return Err(err),
            };

            let Some(captures) = self.regex_match_with_captures(&pattern, &text) else {
                self.env.insert("/".to_string(), Value::Nil);
                return Ok(Value::Nil);
            };
            if captures.from != 0 {
                self.env.insert("/".to_string(), Value::Nil);
                return Ok(Value::Nil);
            }
            if method == "parse" && captures.to != text.chars().count() {
                self.env.insert("/".to_string(), Value::Nil);
                return Ok(Value::Nil);
            }
            for (i, v) in captures.positional.iter().enumerate() {
                self.env.insert(i.to_string(), Value::Str(v.clone()));
            }
            for (k, v) in &captures.named {
                let value = if v.len() == 1 {
                    Value::Str(v[0].clone())
                } else {
                    Value::array(v.iter().cloned().map(Value::Str).collect())
                };
                self.env.insert(format!("<{}>", k), value);
            }
            let match_obj = Value::make_match_object_with_captures(
                captures.matched,
                captures.from as i64,
                captures.to as i64,
                &captures.positional,
                &captures.named,
            );
            self.env.insert("/".to_string(), match_obj.clone());
            Ok(match_obj)
        })();

        self.current_package = saved_package;
        if let Some(old_topic) = saved_topic {
            self.env.insert("_".to_string(), old_topic);
        } else {
            self.env.remove("_");
        }
        result
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
            HashMap::new(),
        ))
    }

    fn classhow_find_method(&self, invocant: &Value, method_name: &str) -> Option<Value> {
        if let Some(value) = self.classhow_lookup(invocant, method_name) {
            return Some(value);
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
                    return Err(RuntimeError::new(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): ver",
                    ));
                };
                let Some(value) = meta.get("ver").cloned() else {
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
                        })
                        .collect(),
                    body: sub_data.body.clone(),
                    is_rw: false,
                    is_private: false,
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
            _ => Err(RuntimeError::new(format!(
                "X::Method::NotFound: Unknown method value dispatch (fallback disabled): {}",
                method
            ))),
        }
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
                    *weights.entry(item.to_string_value()).or_insert(0.0) += 1.0;
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
            other => {
                let mut map = HashMap::new();
                map.insert(other.to_string_value(), Value::Bool(true));
                Ok(Value::hash(map))
            }
        }
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
                if matches!(item, Value::Nil) {
                    continue;
                }
                let ord = if let Some(current) = &best {
                    match (to_float_value(item), to_float_value(current)) {
                        (Some(a), Some(b)) => {
                            a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal)
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
                    out.push(Value::Pair(idx.to_string(), Box::new(item.clone())));
                } else if ord == std::cmp::Ordering::Equal {
                    out.push(Value::Pair(idx.to_string(), Box::new(item.clone())));
                }
            }
            Value::array(out)
        };
        Ok(match target {
            Value::Array(items, ..) => to_pairs(&items),
            other => Value::array(vec![Value::Pair("0".to_string(), Box::new(other))]),
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
                            key_a.to_string_value().cmp(&key_b.to_string_value())
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
        if let Value::Package(class_name) = &target {
            match class_name.as_str() {
                "Array" | "List" | "Positional" => {
                    if let Some(dims) = Self::shaped_dims_from_new_args(&args) {
                        return Ok(Self::make_shaped_array(&dims));
                    }
                    let mut items = Vec::new();
                    for arg in &args {
                        match arg {
                            Value::Slip(vals) => items.extend(vals.iter().cloned()),
                            other => items.push(other.clone()),
                        }
                    }
                    return Ok(Value::array(items));
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
                            other => {
                                let key = other.to_string_value();
                                let value = iter.next().unwrap_or(Value::Nil);
                                map.insert(key, value);
                            }
                        }
                    }
                    return Ok(Value::hash(map));
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
                            Value::Range(start, end) => (*start..=*end).map(Value::Int).collect(),
                            Value::RangeExcl(start, end) => {
                                (*start..*end).map(Value::Int).collect()
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
                "Lock" => {
                    let mut attrs = HashMap::new();
                    let lock_id = super::native_methods::next_lock_id() as i64;
                    attrs.insert("lock-id".to_string(), Value::Int(lock_id));
                    return Ok(Value::make_instance("Lock".to_string(), attrs));
                }
                "Slip" => {
                    return Ok(Value::slip(args.clone()));
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
                return Ok(Value::Mixin(Box::new(Value::Nil), mixins));
            }
            if self.classes.contains_key(class_name) {
                // Check for user-defined .new method first
                if self.has_user_method(class_name, "new") {
                    let empty_attrs = HashMap::new();
                    let (result, _updated) =
                        self.run_instance_method(class_name, empty_attrs, "new", args, None)?;
                    return Ok(result);
                }
                let mut attrs = HashMap::new();
                for (attr_name, _is_public, default, _is_rw) in
                    self.collect_class_attributes(class_name)
                {
                    let val = if let Some(expr) = default {
                        self.eval_block_value(&[Stmt::Expr(expr)])?
                    } else {
                        Value::Nil
                    };
                    attrs.insert(attr_name, val);
                }
                let class_mro = self.class_mro(class_name);
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
                if self.class_has_method(class_name, "BUILD") {
                    let (_v, updated) = self.run_instance_method(
                        class_name,
                        attrs.clone(),
                        "BUILD",
                        Vec::new(),
                        Some(Value::make_instance(class_name.clone(), attrs.clone())),
                    )?;
                    attrs = updated;
                }
                if self.class_has_method(class_name, "TWEAK") {
                    let (_v, updated) = self.run_instance_method(
                        class_name,
                        attrs.clone(),
                        "TWEAK",
                        Vec::new(),
                        Some(Value::make_instance(class_name.clone(), attrs.clone())),
                    )?;
                    attrs = updated;
                }
                return Ok(Value::make_instance(class_name.clone(), attrs));
            }
        }
        // Fallback .new on basic types
        match target {
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
}
