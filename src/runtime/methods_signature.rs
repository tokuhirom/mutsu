use super::*;
use crate::ast::CallArg;
use crate::symbol::Symbol;
use crate::value::signature::{make_signature_value, param_defs_to_sig_info};

/// Create a structured X::Method::Private::Permission error.
pub(super) fn make_private_permission_error(method_name: &str, class_name: &str) -> RuntimeError {
    let msg = format!(
        "Cannot call private method '{}' on package {}",
        method_name, class_name
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("method".to_string(), Value::str(method_name.to_string()));
    attrs.insert(
        "source-package".to_string(),
        Value::str(class_name.to_string()),
    );
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Method::Private::Permission"), attrs);
    let mut err = RuntimeError::new(&msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Create a structured X::Method::NotFound error.
pub(super) fn make_method_not_found_error(
    method_name: &str,
    type_name: &str,
    private: bool,
) -> RuntimeError {
    let msg = format!(
        "No such {} method '{}' for invocant of type '{}'",
        if private { "private" } else { "public" },
        method_name,
        type_name
    );
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("method".to_string(), Value::str(method_name.to_string()));
    attrs.insert("typename".to_string(), Value::str(type_name.to_string()));
    attrs.insert("private".to_string(), Value::Bool(private));
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Method::NotFound"), attrs);
    let mut err = RuntimeError::new(&msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Create a structured X::Multi::NoMatch error.
pub(super) fn make_multi_no_match_error(method_name: &str) -> RuntimeError {
    let msg = format!("No matching candidates for method: {}", method_name);
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Multi::NoMatch"), attrs);
    let mut err = RuntimeError::new(&msg);
    err.exception = Some(Box::new(ex));
    err
}

impl Interpreter {
    /// Coerce a value based on attribute sigil: @ → Array, % → Hash
    pub(crate) fn coerce_attr_value_by_sigil(val: Value, sigil: char) -> Value {
        match sigil {
            '@' => match val {
                // @-sigiled attributes always produce Array (not List)
                Value::Array(items, kind) if kind.is_real_array() => {
                    Value::Array(items, ArrayKind::Array)
                }
                Value::Array(items, _) => Value::Array(items, ArrayKind::Array),
                Value::Range(start, end) => {
                    let items: Vec<Value> = (start..=end).map(Value::Int).collect();
                    Value::real_array(items)
                }
                Value::RangeExcl(start, end) => {
                    let items: Vec<Value> = (start..end).map(Value::Int).collect();
                    Value::real_array(items)
                }
                other => other,
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
                Expr::SymbolicDeref { expr, .. } => scan_expr(expr, positional, named),
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

    pub(super) fn varref_parts(value: &Value) -> Option<(String, Value)> {
        if let Value::Capture { positional, named } = value
            && positional.is_empty()
            && let Some(Value::Str(name)) = named.get("__mutsu_varref_name")
            && let Some(inner) = named.get("__mutsu_varref_value")
        {
            return Some((name.to_string(), inner.clone()));
        }
        None
    }

    pub(super) fn var_target_from_meta_value(value: &Value) -> Option<String> {
        match value {
            Value::Mixin(inner, _) => Self::var_target_from_meta_value(inner),
            Value::Instance { attributes, .. } => match attributes.get("__mutsu_var_target") {
                Some(Value::Str(name)) => Some(name.to_string()),
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
            Value::Routine { name, .. } => self
                .resolve_function_with_types(&name.resolve(), args)
                .is_some(),
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
            let key_s = key.resolve();
            if key_s == exact_local
                || key_s == exact_global
                || key_s.starts_with(&prefix_local)
                || key_s.starts_with(&prefix_global)
            {
                let fp =
                    crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body);
                if seen.insert(fp) {
                    out.push(Value::make_sub(
                        def.package,
                        def.name,
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
            Value::Str(s) => Some(s.to_string()),
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
        let dims_vals = if let Some(items) = shape_val.as_list_items() {
            items.to_vec()
        } else {
            match shape_val {
                Value::Int(i) => vec![Value::Int(i)],
                Value::Package(ref name) => {
                    // Enum type as shape: use the number of enum variants
                    if let Some(variants) = self.enum_types.get(&name.resolve()) {
                        vec![Value::Int(variants.len() as i64)]
                    } else if name == "Bool" {
                        // Bool is a builtin enum with 2 values (False, True)
                        vec![Value::Int(2)]
                    } else {
                        return None;
                    }
                }
                _ => return None,
            }
        };
        let mut dims = Vec::with_capacity(dims_vals.len());
        for dim in &dims_vals {
            let n = match dim {
                Value::Int(i) if *i >= 0 => *i as usize,
                Value::Num(f) if *f >= 0.0 => *f as usize,
                Value::Package(name) => {
                    if let Some(variants) = self.enum_types.get(&name.resolve()) {
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
            let value = Value::shaped_array(vec![Value::Nil; len]);
            crate::runtime::utils::mark_shaped_array(&value, Some(dims));
            return value;
        }
        let child = Self::make_shaped_array(&dims[1..]);
        crate::runtime::utils::mark_shaped_array(&child, Some(&dims[1..]));
        let value = Value::shaped_array((0..len).map(|_| child.clone()).collect());
        crate::runtime::utils::mark_shaped_array(&value, Some(dims));
        value
    }

    pub(super) fn infer_array_shape(value: &Value) -> Option<Vec<usize>> {
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
            if let Some(private_rest) = method.strip_prefix('!') {
                // Resolve: owner-qualified (!Owner::method) or unqualified (!method)
                let resolved = if let Some((owner_class, pm_name)) = private_rest.split_once("::") {
                    self.resolve_private_method_with_owner(
                        &class_name.resolve(),
                        owner_class,
                        pm_name,
                        &args,
                    )
                    .map(|r| (r, pm_name))
                } else {
                    self.resolve_private_method_any_owner(
                        &class_name.resolve(),
                        private_rest,
                        &args,
                    )
                    .map(|r| (r, private_rest))
                };
                if let Some(((resolved_owner, method_def), pm_name)) = resolved {
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
                        return Err(make_private_permission_error(
                            pm_name,
                            &class_name.resolve(),
                        ));
                    }
                    let (result, updated) = self.run_instance_method_resolved(
                        &class_name.resolve(),
                        &resolved_owner,
                        method_def,
                        (**attributes).clone(),
                        args,
                        Some(target.clone()),
                    )?;
                    self.overwrite_instance_bindings_by_identity(
                        &class_name.resolve(),
                        *target_id,
                        updated,
                    );
                    return Ok(vec![result]);
                } else {
                    // Private method not found -- return structured X::Method::NotFound
                    return Err(make_method_not_found_error(
                        private_rest,
                        &class_name.resolve(),
                        true,
                    ));
                }
            }

            let candidates =
                self.resolve_all_methods_with_owner(&class_name.resolve(), method, &args);
            if !candidates.is_empty() {
                let mut attrs = (**attributes).clone();
                let mut out = Vec::with_capacity(candidates.len());
                for (resolved_owner, method_def) in candidates {
                    let (result, updated) = self.run_instance_method_resolved(
                        &class_name.resolve(),
                        &resolved_owner,
                        method_def,
                        attrs,
                        args.clone(),
                        Some(target.clone()),
                    )?;
                    attrs = updated;
                    out.push(result);
                }
                self.overwrite_instance_bindings_by_identity(
                    &class_name.resolve(),
                    *target_id,
                    attrs,
                );
                return Ok(out);
            }
        }

        Ok(vec![self.call_method_with_values(target, method, args)?])
    }

    pub(crate) fn overwrite_array_items_by_identity_for_vm(
        &mut self,
        needle: &std::sync::Arc<Vec<Value>>,
        updated_items: Vec<Value>,
        kind: ArrayKind,
    ) {
        self.overwrite_array_bindings_by_identity(
            needle,
            Value::Array(std::sync::Arc::new(updated_items), kind),
        );
    }
}
