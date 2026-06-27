use super::*;
use crate::ast::CallArg;

impl Interpreter {
    /// Extract shape dimensions from a default expression that matches the pattern
    /// `Array.new(:shape(N))` or `Array.new(:shape(N, M, ...))`, as generated
    /// for `has @.a[2]` or `has @.a[2;3]` declarations.
    /// Returns `Some(vec![dim1, dim2, ...])` if the pattern matches.
    pub(crate) fn extract_shape_from_default(
        default: Option<&crate::ast::Expr>,
    ) -> Option<Vec<usize>> {
        use crate::ast::Expr;
        use crate::token_kind::TokenKind;

        let expr = default?;
        // Match Array.new(:shape(...)) or Array.new(:shape(...), :data(...))
        let Expr::MethodCall {
            target, name, args, ..
        } = expr
        else {
            return None;
        };
        if name.resolve() != "new" {
            return None;
        }
        if !matches!(target.as_ref(), Expr::BareWord(s) if s == "Array") {
            return None;
        }
        // Find the :shape(...) pair in args
        for arg in args {
            if let Expr::Binary {
                left,
                op: TokenKind::FatArrow,
                right,
            } = arg
                && let Expr::Literal(Value::Str(key)) = left.as_ref()
                && key.as_str() == "shape"
            {
                return Self::extract_dims_from_shape_expr(right);
            }
        }
        None
    }

    fn extract_dims_from_shape_expr(expr: &crate::ast::Expr) -> Option<Vec<usize>> {
        use crate::ast::Expr;
        match expr {
            Expr::Literal(Value::Int(n)) if *n >= 0 => Some(vec![*n as usize]),
            Expr::ArrayLiteral(items) => {
                let mut dims = Vec::new();
                for item in items {
                    if let Expr::Literal(Value::Int(n)) = item {
                        if *n >= 0 {
                            dims.push(*n as usize);
                        } else {
                            return None;
                        }
                    } else {
                        return None;
                    }
                }
                if dims.is_empty() { None } else { Some(dims) }
            }
            _ => None,
        }
    }

    /// Coerce a value based on attribute sigil: @ → Array, % → Hash
    pub(crate) fn coerce_attr_value_by_sigil(val: Value, sigil: char) -> Value {
        match sigil {
            '@' => match val {
                // @-sigiled attributes always produce Array (not List)
                // Preserve Shaped kind for shaped array attributes
                Value::Array(items, ArrayKind::Shaped) => Value::Array(items, ArrayKind::Shaped),
                // Itemized arrays/lists ($[...] or $(...)) follow the one-arg rule:
                // they are treated as a single item when assigned to an @-sigiled attribute.
                Value::Array(items, kind) if kind.is_itemized() => {
                    Value::real_array(vec![Value::Array(items, kind)])
                }
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
                Value::Pair(k, v) => {
                    // A single Pair coerces to a one-element hash
                    let mut map = HashMap::new();
                    map.insert(k.clone(), *v.clone());
                    Value::Hash(Value::hash_arc(map))
                }
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
                        Value::Hash(Value::hash_arc(map))
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
                Stmt::Expr(e)
                | Stmt::Return(e)
                | Stmt::Die(e)
                | Stmt::Fail(e)
                | Stmt::Take(e, _) => {
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
                Stmt::Say(es) | Stmt::Put(es) | Stmt::Print(es) | Stmt::Note(es) => {
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
                    ..
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
                Expr::Index { target, index, .. } => {
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
                | Expr::BracketArray(es, _)
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

    pub(super) fn assumed_signature_param_defs(
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
        // Helper to check if an assumed value is a Whatever placeholder
        let is_placeholder = |v: &Value| {
            matches!(v, Value::Whatever)
                || matches!(v, Value::Num(f) if f.is_infinite())
                || matches!(v, Value::Rat(_, 0))
        };
        {
            let mut pos_idx = 0usize;
            for pd in &param_defs {
                if !pd.named && !pd.slurpy {
                    if pos_idx < assumed_positional.len() {
                        if !is_placeholder(&assumed_positional[pos_idx])
                            && let Some(tc) = &pd.type_constraint
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
        // Remove params that have been primed (non-placeholder assumed values).
        // Whatever (*) placeholders leave the corresponding param in the signature.
        let mut assumed_iter = assumed_positional.iter();
        param_defs.retain(|pd| {
            if !pd.named
                && !pd.slurpy
                && let Some(assumed) = assumed_iter.next()
            {
                // Placeholder (*) means keep this param in the signature
                return is_placeholder(assumed);
            }
            true
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
}
