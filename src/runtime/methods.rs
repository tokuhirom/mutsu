use super::*;
use crate::ast::CallArg;
use crate::symbol::Symbol;
use crate::value::signature::{extract_sig_info, make_signature_value, param_defs_to_sig_info};

impl Interpreter {
    /// Coerce a value based on attribute sigil: @ → Array, % → Hash
    pub(crate) fn coerce_attr_value_by_sigil(val: Value, sigil: char) -> Value {
        match sigil {
            '@' => match &val {
                Value::Array(..) => val,
                Value::Range(start, end) => {
                    let items: Vec<Value> = (*start..=*end).map(Value::Int).collect();
                    Value::Array(std::sync::Arc::new(items), false)
                }
                Value::RangeExcl(start, end) => {
                    let items: Vec<Value> = (*start..*end).map(Value::Int).collect();
                    Value::Array(std::sync::Arc::new(items), false)
                }
                _ => val,
            },
            '%' => match &val {
                Value::Hash(_) => val,
                Value::Array(arr, _) => {
                    // Convert array of pairs to hash
                    let mut map = HashMap::new();
                    let mut has_pairs = false;
                    for item in arr.iter() {
                        if let Value::Pair(k, v) = item {
                            map.insert(k.clone(), *v.clone());
                            has_pairs = true;
                        }
                    }
                    if has_pairs {
                        Value::Hash(std::sync::Arc::new(map))
                    } else {
                        val
                    }
                }
                _ => val,
            },
            _ => val,
        }
    }

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
                | Expr::HyperMethodCall { target, args, .. } => {
                    scan_expr(target, positional, named);
                    for a in args {
                        scan_expr(a, positional, named);
                    }
                }
                Expr::DynamicMethodCall {
                    target,
                    name_expr,
                    args,
                }
                | Expr::HyperMethodCallDynamic {
                    target,
                    name_expr,
                    args,
                    ..
                } => {
                    scan_expr(target, positional, named);
                    scan_expr(name_expr, positional, named);
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

    pub(super) fn collect_named_param_keys(
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

    pub(super) fn has_named_slurpy_param(param_defs: &[ParamDef]) -> bool {
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

    pub(super) fn capture_to_call_args(value: &Value) -> Vec<Value> {
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

    fn varref_parts(value: &Value) -> Option<(String, Value)> {
        if let Value::Capture { positional, named } = value
            && positional.is_empty()
            && let Some(Value::Str(name)) = named.get("__mutsu_varref_name")
            && let Some(inner) = named.get("__mutsu_varref_value")
        {
            return Some((name.clone(), inner.clone()));
        }
        None
    }

    fn var_target_from_meta_value(value: &Value) -> Option<String> {
        match value {
            Value::Mixin(inner, _) => Self::var_target_from_meta_value(inner),
            Value::Instance { attributes, .. } => match attributes.get("__mutsu_var_target") {
                Some(Value::Str(name)) => Some(name.clone()),
                _ => None,
            },
            _ => None,
        }
    }

    pub(super) fn candidate_matches_call_args(
        &mut self,
        candidate: &Value,
        args: &[Value],
    ) -> bool {
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

    pub(super) fn routine_candidate_subs(&self, package: &str, name: &str) -> Vec<Value> {
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
                        def.package.resolve(),
                        def.name.resolve(),
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

    pub(super) fn sub_signature_value(&self, data: &crate::value::SubData) -> Value {
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

    pub(super) fn signature_required_positional_count(
        info: &crate::value::signature::SigInfo,
    ) -> i64 {
        info.params
            .iter()
            .filter(|p| !p.named && !p.slurpy && !p.has_default && !p.optional_marker)
            .count() as i64
    }

    fn signature_positional_count(info: &crate::value::signature::SigInfo) -> Option<i64> {
        let mut count = 0i64;
        for p in &info.params {
            if p.named || (p.slurpy && p.sigil == '%') {
                continue;
            }
            if p.slurpy {
                return None;
            }
            count += 1;
        }
        Some(count)
    }

    pub(super) fn signature_count_value(info: &crate::value::signature::SigInfo) -> Value {
        match Self::signature_positional_count(info) {
            Some(count) => Value::Int(count),
            None => Value::Num(f64::INFINITY),
        }
    }

    pub(super) fn candidate_arity_value(infos: &[crate::value::signature::SigInfo]) -> Value {
        let arity = infos
            .iter()
            .map(Self::signature_required_positional_count)
            .min()
            .unwrap_or(0);
        Value::Int(arity)
    }

    pub(super) fn candidate_count_value(infos: &[crate::value::signature::SigInfo]) -> Value {
        let mut max_count = 0i64;
        for info in infos {
            match Self::signature_positional_count(info) {
                Some(count) => {
                    if count > max_count {
                        max_count = count;
                    }
                }
                None => return Value::Num(f64::INFINITY),
            }
        }
        Value::Int(max_count)
    }

    pub(super) fn shaped_dims_from_new_args(&self, args: &[Value]) -> Option<Vec<usize>> {
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

    pub(super) fn make_shaped_array(dims: &[usize]) -> Value {
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

    pub(super) fn parse_parametric_type_name(name: &str) -> Option<(String, Vec<String>)> {
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

    pub(crate) fn call_method_all_with_values(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Vec<Value>, RuntimeError> {
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
                let (result, updated) = self.run_instance_method_resolved(
                    class_name,
                    &resolved_owner,
                    method_def,
                    (**attributes).clone(),
                    args,
                    Some(target.clone()),
                )?;
                self.overwrite_instance_bindings_by_identity(class_name, *target_id, updated);
                return Ok(vec![result]);
            }

            if let Some((owner_class, private_method_name)) = method.split_once("::")
                && let Some((resolved_owner, method_def)) = self.resolve_private_method_with_owner(
                    class_name,
                    owner_class,
                    private_method_name,
                    &args,
                )
            {
                let caller_class = self
                    .method_class_stack
                    .last()
                    .cloned()
                    .or_else(|| Some(self.current_package().to_string()));
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
                return Ok(vec![result]);
            }

            let candidates = self.resolve_all_methods_with_owner(class_name, method, &args);
            if !candidates.is_empty() {
                let mut attrs = (**attributes).clone();
                let mut out = Vec::with_capacity(candidates.len());
                for (resolved_owner, method_def) in candidates {
                    let (result, updated) = self.run_instance_method_resolved(
                        class_name,
                        &resolved_owner,
                        method_def,
                        attrs,
                        args.clone(),
                        Some(target.clone()),
                    )?;
                    attrs = updated;
                    out.push(result);
                }
                self.overwrite_instance_bindings_by_identity(class_name, *target_id, attrs);
                return Ok(out);
            }
        }

        Ok(vec![self.call_method_with_values(target, method, args)?])
    }

    pub(crate) fn overwrite_array_items_by_identity_for_vm(
        &mut self,
        needle: &std::sync::Arc<Vec<Value>>,
        updated_items: Vec<Value>,
        is_array: bool,
    ) {
        self.overwrite_array_bindings_by_identity(
            needle,
            Value::Array(std::sync::Arc::new(updated_items), is_array),
        );
    }

    pub(crate) fn call_method_with_values(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let mut args = args;
        if matches!(method, "log" | "exp" | "atan2") {
            for arg in &mut args {
                if !matches!(arg, Value::Instance { .. }) {
                    continue;
                }
                let original = arg.clone();
                if let Ok(coerced) = self
                    .call_method_with_values(original.clone(), "Numeric", vec![])
                    .or_else(|_| self.call_method_with_values(original.clone(), "Bridge", vec![]))
                {
                    *arg = coerced;
                }
            }
        }
        if matches!(method, "arity" | "count")
            && args.is_empty()
            && let Some(sig_info) = extract_sig_info(&target)
        {
            return Ok(if method == "arity" {
                Value::Int(Self::signature_required_positional_count(&sig_info))
            } else {
                Self::signature_count_value(&sig_info)
            });
        }

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
        if method == "gist" && args.is_empty() {
            fn collection_contains_instance(value: &Value) -> bool {
                match value {
                    Value::Instance { .. } => true,
                    Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => {
                        items.iter().any(collection_contains_instance)
                    }
                    Value::Hash(map) => map.values().any(collection_contains_instance),
                    _ => false,
                }
            }
            fn gist_item(interp: &mut Interpreter, value: &Value) -> String {
                match value {
                    Value::Nil => "Nil".to_string(),
                    Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => {
                        let inner = items
                            .iter()
                            .map(|item| gist_item(interp, item))
                            .collect::<Vec<_>>()
                            .join(" ");
                        format!("({inner})")
                    }
                    other => match interp.call_method_with_values(other.clone(), "gist", vec![]) {
                        Ok(Value::Str(s)) => s,
                        Ok(v) => v.to_string_value(),
                        Err(_) => other.to_string_value(),
                    },
                }
            }
            match &target {
                Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items)
                    if items.iter().any(collection_contains_instance) =>
                {
                    let inner = items
                        .iter()
                        .map(|item| gist_item(self, item))
                        .collect::<Vec<_>>()
                        .join(" ");
                    return Ok(Value::Str(format!("({inner})")));
                }
                _ => {}
            }
        }
        if matches!(method, "max" | "min" | "lines" | "delayed")
            && matches!(&target, Value::Package(name) if name == "Supply")
        {
            return Err(RuntimeError::new(format!(
                "Cannot call .{} on a Supply type object",
                method
            )));
        }
        if method == "delayed"
            && matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply")
            && args.first().is_some_and(|delay| delay.to_f64() <= 0.0)
        {
            return Ok(target);
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
                    let method_result = self.run_instance_method_resolved(
                        &role_name,
                        &role_name,
                        def,
                        role_attrs,
                        args,
                        Some(target.clone()),
                    );
                    for (name, previous) in &saved_role_params {
                        if let Some(prev) = previous {
                            self.env.insert(name.clone(), prev.clone());
                        } else {
                            self.env.remove(name);
                        }
                    }
                    let (result, _updated) = method_result?;
                    return Ok(result);
                }
                for (name, previous) in saved_role_params {
                    if let Some(prev) = previous {
                        self.env.insert(name, prev);
                    } else {
                        self.env.remove(&name);
                    }
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
                        .any(|(attr_name, is_public, ..)| *is_public && attr_name == method);
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
                        .any(|(attr_name, is_public, ..)| *is_public && attr_name == method);
                if role.methods.contains_key(method) || is_public_attr_accessor {
                    let instance = self.dispatch_new(target.clone(), Vec::new())?;
                    return self.call_method_with_values(instance, method, args);
                }
            }
        }

        // Enum type-object dispatch for Bool and user enums (e.g. `Order.roll`).
        let enum_type_name = match &target {
            Value::Package(type_name) => Some(type_name),
            Value::Str(type_name) if self.enum_types.contains_key(type_name) => Some(type_name),
            Value::Mixin(_, mixins) => mixins.values().find_map(|v| match v {
                Value::Enum { enum_type, .. } if self.enum_types.contains_key(enum_type) => {
                    Some(enum_type)
                }
                _ => None,
            }),
            _ => None,
        };
        if let Some(type_name) = enum_type_name
            && matches!(method, "pick" | "roll")
            && args.len() <= 1
        {
            let pool: Option<Vec<Value>> = if type_name == "Bool" {
                Some(vec![Value::Bool(false), Value::Bool(true)])
            } else {
                self.enum_types.get(type_name).map(|variants| {
                    variants
                        .iter()
                        .enumerate()
                        .map(|(index, (key, value))| Value::Enum {
                            enum_type: type_name.clone(),
                            key: key.clone(),
                            value: *value,
                            index,
                        })
                        .collect()
                })
            };
            if let Some(pool) = pool {
                if pool.is_empty() {
                    return Ok(Value::Nil);
                }
                if args.is_empty() {
                    let idx = (crate::builtins::rng::builtin_rand() * pool.len() as f64) as usize
                        % pool.len();
                    return Ok(pool[idx].clone());
                }
                let count = match &args[0] {
                    Value::Int(i) if *i > 0 => Some(*i as usize),
                    Value::Int(_) => Some(0),
                    Value::Num(f) if f.is_infinite() && f.is_sign_positive() => None,
                    Value::Whatever => None,
                    Value::Str(s) => s.trim().parse::<i64>().ok().map(|n| n.max(0) as usize),
                    _ => None,
                };
                let Some(count) = count else {
                    if method == "pick" {
                        let mut items = pool.clone();
                        let len = items.len();
                        for i in (1..len).rev() {
                            let j = (crate::builtins::rng::builtin_rand() * (i + 1) as f64)
                                as usize
                                % (i + 1);
                            items.swap(i, j);
                        }
                        return Ok(Value::array(items));
                    }
                    let generated = 1024usize;
                    let mut out = Vec::with_capacity(generated);
                    for _ in 0..generated {
                        let idx = (crate::builtins::rng::builtin_rand() * pool.len() as f64)
                            as usize
                            % pool.len();
                        out.push(pool[idx].clone());
                    }
                    return Ok(Value::LazyList(std::sync::Arc::new(
                        crate::value::LazyList {
                            body: vec![],
                            env: HashMap::new(),
                            cache: std::sync::Mutex::new(Some(out)),
                        },
                    )));
                };
                if method == "pick" {
                    if count == 0 {
                        return Ok(Value::array(Vec::new()));
                    }
                    let mut items = pool.clone();
                    let mut out = Vec::with_capacity(count.min(items.len()));
                    for _ in 0..count.min(items.len()) {
                        let idx = (crate::builtins::rng::builtin_rand() * items.len() as f64)
                            as usize
                            % items.len();
                        out.push(items.swap_remove(idx));
                    }
                    return Ok(Value::array(out));
                }
                if count == 0 {
                    return Ok(Value::array(Vec::new()));
                }
                if count == 1 {
                    let idx = (crate::builtins::rng::builtin_rand() * pool.len() as f64) as usize
                        % pool.len();
                    return Ok(pool[idx].clone());
                }
                let mut out = Vec::with_capacity(count);
                for _ in 0..count {
                    let idx = (crate::builtins::rng::builtin_rand() * pool.len() as f64) as usize
                        % pool.len();
                    out.push(pool[idx].clone());
                }
                return Ok(Value::array(out));
            }
        }

        // String pick/roll (character-wise), used when VM bypasses native fast path.
        if let Value::Str(s) = &target
            && matches!(method, "pick" | "roll")
            && args.len() <= 1
        {
            let chars: Vec<Value> = s.chars().map(|c| Value::Str(c.to_string())).collect();
            if chars.is_empty() {
                return Ok(if args.is_empty() {
                    Value::Nil
                } else {
                    Value::array(Vec::new())
                });
            }
            if args.is_empty() {
                let idx = (crate::builtins::rng::builtin_rand() * chars.len() as f64) as usize
                    % chars.len();
                return Ok(chars[idx].clone());
            }
            let count = match &args[0] {
                Value::Int(i) if *i > 0 => Some(*i as usize),
                Value::Int(_) => Some(0),
                Value::Num(f) if f.is_infinite() && f.is_sign_positive() => None,
                Value::Whatever => None,
                Value::Str(n) => n.trim().parse::<i64>().ok().map(|v| v.max(0) as usize),
                _ => None,
            };
            let Some(count) = count else {
                if method == "pick" {
                    let mut items = chars.clone();
                    let len = items.len();
                    for i in (1..len).rev() {
                        let j = (crate::builtins::rng::builtin_rand() * (i + 1) as f64) as usize
                            % (i + 1);
                        items.swap(i, j);
                    }
                    return Ok(Value::array(items));
                }
                let generated = 1024usize;
                let mut out = Vec::with_capacity(generated);
                for _ in 0..generated {
                    let idx = (crate::builtins::rng::builtin_rand() * chars.len() as f64) as usize
                        % chars.len();
                    out.push(chars[idx].clone());
                }
                return Ok(Value::LazyList(std::sync::Arc::new(
                    crate::value::LazyList {
                        body: vec![],
                        env: HashMap::new(),
                        cache: std::sync::Mutex::new(Some(out)),
                    },
                )));
            };
            if method == "pick" {
                if count == 0 {
                    return Ok(Value::array(Vec::new()));
                }
                let mut items = chars.clone();
                let mut out = Vec::with_capacity(count.min(items.len()));
                for _ in 0..count.min(items.len()) {
                    let idx = (crate::builtins::rng::builtin_rand() * items.len() as f64) as usize
                        % items.len();
                    out.push(items.swap_remove(idx));
                }
                return Ok(Value::array(out));
            }
            if count == 0 {
                return Ok(Value::array(Vec::new()));
            }
            let mut out = Vec::with_capacity(count);
            for _ in 0..count {
                let idx = (crate::builtins::rng::builtin_rand() * chars.len() as f64) as usize
                    % chars.len();
                out.push(chars[idx].clone());
            }
            return Ok(Value::array(out));
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
            || method == "squish"
            || (matches!(method, "max" | "min")
                && matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply"))
            || (method == "Supply"
                && matches!(&target, Value::Instance { class_name, .. } if class_name == "Supplier"))
            || (matches!(&target, Value::Instance { .. })
                && (target.does_check("Real") || target.does_check("Numeric")))
            || matches!(&target, Value::Instance { class_name, .. } if self.has_user_method(class_name, "Bridge"))
            || (matches!(method, "AT-KEY" | "keys")
                && matches!(&target, Value::Instance { class_name, .. } if class_name == "Stash"))
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
        if matches!(
            &target,
            Value::Routine { .. } | Value::Sub(_) | Value::WeakSub(_)
        ) && let Some(result) = self.dispatch_callable_method(&target, method, &args)
        {
            return result;
        }

        if method == "VAR"
            && args.is_empty()
            && let Some((source_name, inner)) = Self::varref_parts(&target)
        {
            return self.call_method_mut_with_values(&source_name, inner, "VAR", vec![]);
        }

        if method == "var"
            && args.is_empty()
            && let Some(source_name) = Self::var_target_from_meta_value(&target)
        {
            let source_value = self.env.get(&source_name).cloned().unwrap_or(Value::Nil);
            let mut named = std::collections::HashMap::new();
            named.insert("__mutsu_varref_name".to_string(), Value::Str(source_name));
            named.insert("__mutsu_varref_value".to_string(), source_value);
            return Ok(Value::Capture {
                positional: Vec::new(),
                named,
            });
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
                    | "candidates"
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

        if method == "leave" {
            return self.builtin_leave_method(target, &args);
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
            "from-loop" | "from_loop" if matches!(&target, Value::Package(name) if name == "Seq") =>
            {
                let mut positional: Vec<Value> = Vec::new();
                let mut label: Option<String> = None;
                let mut repeat = false;

                for arg in &args {
                    if let Value::Pair(name, value) = arg {
                        if name == "label" {
                            label = Some(value.to_string_value());
                            continue;
                        }
                        if name == "repeat" {
                            repeat = value.truthy();
                            continue;
                        }
                        continue;
                    }
                    if let Value::ValuePair(name, value) = arg {
                        if let Value::Str(key) = name.as_ref() {
                            if key == "label" {
                                label = Some(value.to_string_value());
                                continue;
                            }
                            if key == "repeat" {
                                repeat = value.truthy();
                                continue;
                            }
                        }
                        continue;
                    }
                    positional.push(arg.clone());
                }

                let Some(body_callable) = positional.first().cloned() else {
                    return Ok(Value::Seq(std::sync::Arc::new(Vec::new())));
                };
                let cond_callable = positional.get(1).cloned();
                let step_callable = positional.get(2).cloned();

                let label_matches = |error_label: &Option<String>| {
                    error_label.as_deref() == label.as_deref() || error_label.is_none()
                };

                let mut items = Vec::new();
                let mut first_iteration = true;

                'from_loop: loop {
                    if (!first_iteration || !repeat)
                        && let Some(cond) = cond_callable.clone()
                    {
                        let cond_value = self.call_sub_value(cond, vec![], true)?;
                        if !cond_value.truthy() {
                            break;
                        }
                    }
                    first_iteration = false;

                    'body_redo: loop {
                        match self.call_sub_value(body_callable.clone(), vec![], true) {
                            Ok(value) => {
                                if !matches!(value, Value::Nil) {
                                    items.push(value);
                                }
                                break 'body_redo;
                            }
                            Err(e) if e.is_redo && label_matches(&e.label) => continue 'body_redo,
                            Err(e) if e.is_next && label_matches(&e.label) => break 'body_redo,
                            Err(e) if e.is_last && label_matches(&e.label) => break 'from_loop,
                            Err(e) => return Err(e),
                        }
                    }

                    if let Some(step) = step_callable.clone() {
                        match self.call_sub_value(step, vec![], true) {
                            Ok(_) => {}
                            Err(e) if e.is_next && label_matches(&e.label) => continue 'from_loop,
                            Err(e) if e.is_redo && label_matches(&e.label) => continue 'from_loop,
                            Err(e) if e.is_last && label_matches(&e.label) => break 'from_loop,
                            Err(e) => return Err(e),
                        }
                    }
                }

                return Ok(Value::Seq(std::sync::Arc::new(items)));
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
                        Some(Value::Str(kind)) if kind == "WhateverCode" => "WhateverCode",
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
                // Use appropriate HOW metaclass for each type kind
                let how_name = if self.roles.contains_key(&type_name) && !type_name.contains('[')
                    || matches!(
                        type_name.as_str(),
                        "Numeric"
                            | "Real"
                            | "Stringy"
                            | "Positional"
                            | "Associative"
                            | "Callable"
                            | "Setty"
                            | "Baggy"
                            | "Mixy"
                            | "Dateish"
                            | "Iterable"
                            | "Iterator"
                            | "PositionalBindFailover"
                    ) {
                    "Perl6::Metamodel::ParametricRoleGroupHOW"
                } else if self.enum_types.contains_key(&type_name) {
                    "Perl6::Metamodel::EnumHOW"
                } else if self.subsets.contains_key(&type_name)
                    || matches!(type_name.as_str(), "UInt" | "NativeInt")
                {
                    "Perl6::Metamodel::SubsetHOW"
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
                let type_name = match &target {
                    Value::Str(name) | Value::Package(name) => Some(name.as_str()),
                    _ => None,
                };
                if let Some(type_name) = type_name
                    && let Some(variants) = self.enum_types.get(type_name)
                {
                    let mut map = HashMap::new();
                    for (k, v) in variants {
                        map.insert(k.clone(), Value::Int(*v));
                    }
                    return Ok(Value::hash(map));
                }
            }
            "invert" => {
                if let Value::Str(type_name) = &target
                    && let Some(variants) = self.enum_types.get(type_name)
                {
                    let mut result = Vec::new();
                    for (k, v) in variants {
                        result.push(Value::Pair(v.to_string(), Box::new(Value::Str(k.clone()))));
                    }
                    return Ok(Value::array(result));
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
                match &args[0] {
                    Value::Str(needle) => {
                        if needle.is_empty() {
                            let chars = text
                                .chars()
                                .map(|ch| Value::Str(ch.to_string()))
                                .collect::<Vec<_>>();
                            return Ok(Value::array(chars));
                        }
                        let mut result = Vec::new();
                        let mut offset = 0usize;
                        while offset <= text.len() {
                            let Some(pos) = text[offset..].find(needle) else {
                                break;
                            };
                            let start = offset + pos;
                            let end = start + needle.len();
                            result.push(Value::Str(text[start..end].to_string()));
                            offset = end;
                        }
                        return Ok(Value::array(result));
                    }
                    Value::Regex(pat) => {
                        let matches = self.regex_find_all(pat, &text);
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
                    _ => {
                        let pattern = args[0].to_string_value();
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
                }
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
                    Value::Slip(items) => Value::Seq(items),
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
                if let Value::Seq(items) = &target {
                    let seq_id = std::sync::Arc::as_ptr(items) as usize;
                    if let Some(meta) = self.squish_iterator_meta.remove(&seq_id) {
                        for key in meta.revert_remove {
                            self.env.remove(&key);
                        }
                        for (key, value) in meta.revert_values {
                            self.env.insert(key, value);
                        }
                        let mut attrs = HashMap::new();
                        attrs.insert("squish_source".to_string(), Value::array(meta.source_items));
                        attrs.insert("squish_as".to_string(), meta.as_func.unwrap_or(Value::Nil));
                        attrs.insert(
                            "squish_with".to_string(),
                            meta.with_func.unwrap_or(Value::Nil),
                        );
                        attrs.insert("squish_scan_index".to_string(), Value::Int(0));
                        attrs.insert("squish_prev_key".to_string(), Value::Nil);
                        attrs.insert("squish_initialized".to_string(), Value::Bool(false));
                        return Ok(Value::make_instance("Iterator".to_string(), attrs));
                    }
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
                if matches!(target, Value::Hash(_)) {
                    let mut call_args = vec![target.clone()];
                    if let Some(first) = args.first() {
                        if matches!(
                            first,
                            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
                        ) {
                            call_args.push(Value::Pair("by".to_string(), Box::new(first.clone())));
                        } else {
                            call_args.extend(args.clone());
                        }
                    }
                    return if method == "max" {
                        self.builtin_max(&call_args)
                    } else {
                        self.builtin_min(&call_args)
                    };
                }
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
            "squish" => {
                return self.dispatch_squish(target, &args);
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
                    for (attr_name, _is_public, default, _is_rw, _, _) in
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
            "DateTime" if args.is_empty() => {
                if let Value::Instance {
                    ref class_name,
                    ref attributes,
                    ..
                } = target
                    && class_name == "Date"
                    && let Some(Value::Int(days)) = attributes.get("days")
                {
                    let mut attrs = HashMap::new();
                    attrs.insert(
                        "epoch".to_string(),
                        Value::Num(Self::date_days_to_epoch_with_leap_seconds(*days)),
                    );
                    return Ok(Value::make_instance("DateTime".to_string(), attrs));
                }
            }
            "Instant" if args.is_empty() => {
                if let Value::Instance {
                    ref class_name,
                    ref attributes,
                    ..
                } = target
                    && class_name == "DateTime"
                {
                    let epoch = attributes
                        .get("epoch")
                        .and_then(to_float_value)
                        .unwrap_or(0.0);
                    let mut attrs = HashMap::new();
                    attrs.insert("value".to_string(), Value::Num(epoch));
                    return Ok(Value::make_instance("Instant".to_string(), attrs));
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
            "eager" if args.is_empty() => {
                return match target {
                    Value::LazyList(list) => Ok(Value::array(self.force_lazy_list_bridge(&list)?)),
                    Value::Array(..) | Value::Seq(..) | Value::Slip(..) => Ok(target),
                    Value::Range(..)
                    | Value::RangeExcl(..)
                    | Value::RangeExclStart(..)
                    | Value::RangeExclBoth(..)
                    | Value::GenericRange { .. } => {
                        Ok(Value::array(crate::runtime::utils::value_to_list(&target)))
                    }
                    other => Ok(other),
                };
            }
            "is-lazy" if args.is_empty() => {
                let value_is_lazy = |v: &Value| match v {
                    Value::LazyList(_) => true,
                    Value::Range(_, end)
                    | Value::RangeExcl(_, end)
                    | Value::RangeExclStart(_, end)
                    | Value::RangeExclBoth(_, end) => *end == i64::MAX,
                    Value::GenericRange { end, .. } => {
                        let end_f = end.to_f64();
                        end_f.is_infinite() && end_f.is_sign_positive()
                    }
                    _ => false,
                };
                let is_lazy = match &target {
                    v if value_is_lazy(v) => true,
                    Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => {
                        items.iter().any(value_is_lazy)
                    }
                    _ => false,
                };
                return Ok(Value::Bool(is_lazy));
            }
            "first" if !args.is_empty() => {
                return self.dispatch_first(target, &args);
            }
            "tree" if !args.is_empty() => {
                return self.dispatch_tree(target, &args);
            }
            "keys" if args.is_empty() => match target {
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "Stash" => {
                    let keys = match attributes.get("symbols") {
                        Some(Value::Hash(map)) => {
                            map.keys().cloned().map(Value::Str).collect::<Vec<Value>>()
                        }
                        _ => Vec::new(),
                    };
                    return Ok(Value::array(keys));
                }
                _ => {}
            },
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
            "AT-KEY" if args.len() == 1 => match (&target, &args[0]) {
                (
                    Value::Instance {
                        class_name,
                        attributes,
                        ..
                    },
                    idx,
                ) if class_name == "Stash" => {
                    let key = idx.to_string_value();
                    if let Some(Value::Hash(symbols)) = attributes.get("symbols") {
                        if let Some(value) = symbols.get(&key) {
                            return Ok(value.clone());
                        }
                        if !key.starts_with('$')
                            && !key.starts_with('@')
                            && !key.starts_with('%')
                            && !key.starts_with('&')
                        {
                            let scalar = format!("${key}");
                            if let Some(value) = symbols.get(&scalar) {
                                return Ok(value.clone());
                            }
                        }
                    }
                    return Ok(Value::Nil);
                }
                (Value::Pair(key, value), idx) => {
                    if key == &idx.to_string_value() {
                        return Ok(*value.clone());
                    }
                    return Ok(Value::Nil);
                }
                (Value::ValuePair(key, value), idx) => {
                    if key.to_string_value() == idx.to_string_value() {
                        return Ok(*value.clone());
                    }
                    return Ok(Value::Nil);
                }
                _ => {}
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
                "ACCEPTS" => {
                    if args.is_empty() {
                        return Err(RuntimeError::new("ACCEPTS requires an argument"));
                    }
                    let other = &args[0];
                    return Ok(Value::Bool(match other {
                        Value::Enum {
                            enum_type: other_type,
                            key: other_key,
                            ..
                        } => enum_type == other_type && key == other_key,
                        _ => false,
                    }));
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
            return self.dispatch_promise_method(shared, method, args, &target);
        }

        // SharedChannel dispatch
        if let Value::Channel(ref ch) = target {
            return self.dispatch_channel_method(ch, method, args);
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
                let caller_class = self
                    .method_class_stack
                    .last()
                    .cloned()
                    .or_else(|| Some(self.current_package().to_string()));
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
            if method == "Bool"
                && args.is_empty()
                && ((target.does_check("Real") || target.does_check("Numeric"))
                    || self.has_user_method(class_name, "Bridge"))
                && let Ok(coerced) = self
                    .call_method_with_values(target.clone(), "Numeric", vec![])
                    .or_else(|_| self.call_method_with_values(target.clone(), "Bridge", vec![]))
            {
                return Ok(Value::Bool(coerced.truthy()));
            }
            if method == "Bridge"
                && args.is_empty()
                && target.does_check("Real")
                && !self.has_user_method(class_name, "Bridge")
            {
                if let Ok(coerced) = self.call_method_with_values(target.clone(), "Numeric", vec![])
                    && coerced != target
                {
                    return Ok(coerced);
                }
                if let Ok(coerced) = self.call_method_with_values(target.clone(), "Num", vec![])
                    && coerced != target
                {
                    return Ok(coerced);
                }
            }
            if method == "Bridge"
                && args.is_empty()
                && self.has_user_method(class_name, "Num")
                && !self.has_user_method(class_name, "Bridge")
                && let Ok(coerced) = self.call_method_with_values(target.clone(), "Num", vec![])
                && coerced != target
            {
                return Ok(coerced);
            }
            if method == "log"
                && args.len() == 1
                && self.has_user_method(class_name, "Bridge")
                && let Ok(bridged) = self.call_method_with_values(target.clone(), "Bridge", vec![])
            {
                let base = if let Some(arg) = args.first() {
                    if matches!(arg, Value::Instance { class_name, .. }
                        if self.has_user_method(class_name, "Bridge"))
                    {
                        self.call_method_with_values(arg.clone(), "Numeric", vec![])
                            .or_else(|_| {
                                self.call_method_with_values(arg.clone(), "Bridge", vec![])
                            })
                            .unwrap_or_else(|_| arg.clone())
                    } else {
                        arg.clone()
                    }
                } else {
                    Value::Nil
                };
                return self.call_method_with_values(bridged, "log", vec![base]);
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
                    for (attr_name, is_public, ..) in &class_attrs {
                        if *is_public && attr_name == method {
                            return Ok(attributes.get(method).cloned().unwrap_or(Value::Nil));
                        }
                    }
                }
            }
        }

        // For user-defined numeric/real-like objects, delegate unknown methods through
        // their coercion bridge so default Real behavior is available.
        if matches!(target, Value::Instance { ref class_name, .. }
            if (target.does_check("Real") || target.does_check("Numeric"))
                || self.has_user_method(class_name, "Bridge"))
        {
            if matches!(method, "Bridge" | "Real")
                && let Ok(coerced) = self.call_method_with_values(target.clone(), "Numeric", vec![])
                && coerced != target
            {
                return Ok(coerced);
            }
            if !matches!(method, "Numeric" | "Real" | "Bridge")
                && let Ok(coerced) = self
                    .call_method_with_values(target.clone(), "Numeric", vec![])
                    .or_else(|_| self.call_method_with_values(target.clone(), "Bridge", vec![]))
                && coerced != target
                && let Ok(result) = {
                    let mut delegated_args = Vec::with_capacity(args.len());
                    for arg in &args {
                        let coerced_arg = if matches!(arg, Value::Instance { class_name, .. }
                            if self.has_user_method(class_name, "Bridge")
                                || arg.does_check("Real")
                                || arg.does_check("Numeric"))
                        {
                            self.call_method_with_values(arg.clone(), "Numeric", vec![])
                                .or_else(|_| {
                                    self.call_method_with_values(arg.clone(), "Bridge", vec![])
                                })
                                .unwrap_or_else(|_| arg.clone())
                        } else {
                            arg.clone()
                        };
                        delegated_args.push(coerced_arg);
                    }
                    if delegated_args.is_empty()
                        && let Some(result) = crate::builtins::native_method_0arg(&coerced, method)
                    {
                        result
                    } else if delegated_args.len() == 1
                        && let Some(result) = crate::builtins::native_method_1arg(
                            &coerced,
                            method,
                            &delegated_args[0],
                        )
                    {
                        result
                    } else if delegated_args.len() == 2
                        && let Some(result) = crate::builtins::native_method_2arg(
                            &coerced,
                            method,
                            &delegated_args[0],
                            &delegated_args[1],
                        )
                    {
                        result
                    } else {
                        self.call_method_with_values(coerced, method, delegated_args)
                    }
                }
            {
                return Ok(result);
            }
        }

        // Package (type object) dispatch — private method call
        if let Value::Package(ref name) = target {
            let normalized_method: String = method
                .chars()
                .map(|ch| {
                    if ch.is_ascii_alphanumeric() || ch == '_' {
                        ch
                    } else {
                        '_'
                    }
                })
                .collect();
            if name == "Instant" && normalized_method == "from_posix" {
                let secs = args.first().and_then(to_float_value).unwrap_or(0.0);
                let mut attrs = HashMap::new();
                // Match Rakudo's Instant.from-posix behavior (TAI includes leap-second offset).
                attrs.insert("value".to_string(), Value::Num(secs + 10.0));
                return Ok(Value::make_instance("Instant".to_string(), attrs));
            }
            if name == "Supply" && method == "interval" {
                let seconds = args.first().map_or(1.0, |value| match value {
                    Value::Int(i) => *i as f64,
                    Value::Num(n) => *n,
                    other => other.to_string_value().parse::<f64>().unwrap_or(1.0),
                });
                let period_secs = if seconds.is_finite() && seconds > 0.0 {
                    seconds
                } else {
                    1.0
                };

                let supply_id = super::native_methods::next_supply_id();
                let (tx, rx) = std::sync::mpsc::channel();
                if let Ok(mut map) = super::native_methods::supply_channel_map_pub().lock() {
                    map.insert(supply_id, rx);
                }

                std::thread::spawn(move || {
                    let delay = std::time::Duration::from_secs_f64(period_secs);
                    let mut tick = 0i64;
                    loop {
                        std::thread::sleep(delay);
                        if tx
                            .send(super::native_methods::SupplyEvent::Emit(Value::Int(tick)))
                            .is_err()
                        {
                            break;
                        }
                        tick = tick.saturating_add(1);
                    }
                });

                let mut attrs = HashMap::new();
                attrs.insert("values".to_string(), Value::array(Vec::new()));
                attrs.insert("taps".to_string(), Value::array(Vec::new()));
                attrs.insert("supply_id".to_string(), Value::Int(supply_id as i64));
                attrs.insert("live".to_string(), Value::Bool(true));
                return Ok(Value::make_instance("Supply".to_string(), attrs));
            }
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

        if let Some(callable) = self.env.get(&format!("&{}", method)).cloned()
            && matches!(
                callable,
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
            )
        {
            return self.call_sub_value(callable, args, true);
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
                            name: Symbol::intern("__method_compose_target__"),
                            args: call_args,
                        }),
                        name: Symbol::intern(method),
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
}
