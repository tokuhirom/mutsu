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
                && let Expr::Literal(lit) = left.as_ref()
                && let ValueView::Str(key) = lit.view()
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
            Expr::Literal(lit) => match lit.view() {
                ValueView::Int(n) if n >= 0 => Some(vec![n as usize]),
                _ => None,
            },
            Expr::ArrayLiteral(items) => {
                let mut dims = Vec::new();
                for item in items {
                    if let Expr::Literal(lit) = item
                        && let ValueView::Int(n) = lit.view()
                    {
                        if n >= 0 {
                            dims.push(n as usize);
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

    /// Coerce a value bound to a `%`-sigil target (variable or attribute) to a
    /// Hash, list-contextualizing a non-Hash object the way Raku's `Hash.STORE`
    /// does. An object with a custom `.iterator`/`.list` (e.g. delegated via
    /// `handles <iterator list>`) contributes its pairs, so `my %h = $obj` /
    /// `has %.x` bound to such an object materializes those pairs into the hash
    /// instead of degrading to a single stringified key.
    ///
    /// A plain object without a custom list interface has `.list` == `(self,)`,
    /// which coerces to the same scalar fallback as the object itself, so this
    /// is safe for any Instance. `Match` keeps its dedicated `%(...)` handling
    /// in `coerce_to_hash`.
    pub(crate) fn coerce_object_to_hash(&mut self, value: Value) -> Value {
        if let ValueView::Instance { class_name, .. } = value.view()
            && class_name != "Match"
            && let Ok(listed) = self.call_method_with_values(value.clone(), "list", Vec::new())
        {
            return crate::runtime::utils::coerce_to_hash(listed);
        }
        // A lazy sequence (e.g. `%h = gather { take ... }` or `%h = ....map(...)`)
        // must be reified before it can be split into key/value pairs. Without
        // forcing, `coerce_to_hash` falls to its `_` arm and stringifies the whole
        // unreified LazyList into a single bogus key. Force it into an eager Seq
        // first, matching Raku (assignment to a `%` container is eager).
        if let ValueView::LazyList(ll) = value.view()
            && let Ok(items) = self.force_lazy_list(&ll)
        {
            return crate::runtime::utils::coerce_to_hash(Value::seq(items));
        }
        crate::runtime::utils::coerce_to_hash(value)
    }

    /// Coerce a value based on attribute sigil: @ → Array, % → Hash
    pub(crate) fn coerce_attr_value_by_sigil(val: Value, sigil: char) -> Value {
        match sigil {
            '@' => match val.view() {
                // @-sigiled attributes always produce Array (not List)
                // Preserve Shaped kind for shaped array attributes
                ValueView::Array(items, ArrayKind::Shaped) => {
                    Value::array_with_kind(items.clone(), ArrayKind::Shaped)
                }
                // Itemized arrays/lists ($[...] or $(...)) follow the one-arg rule:
                // they are treated as a single item when assigned to an @-sigiled attribute.
                ValueView::Array(items, kind) if kind.is_itemized() => {
                    Value::real_array(vec![Value::array_with_kind(items.clone(), kind)])
                }
                ValueView::Array(items, kind) if kind.is_real_array() => {
                    Value::array_with_kind(items.clone(), ArrayKind::Array)
                }
                ValueView::Array(items, _) => {
                    Value::array_with_kind(items.clone(), ArrayKind::Array)
                }
                ValueView::Range(start, end) => {
                    let items: Vec<Value> = (start..=end).map(Value::int).collect();
                    Value::real_array(items)
                }
                ValueView::RangeExcl(start, end) => {
                    let items: Vec<Value> = (start..end).map(Value::int).collect();
                    Value::real_array(items)
                }
                _ => val.clone(),
            },
            '%' => match val.view() {
                ValueView::Hash(_) => val.clone(),
                ValueView::Pair(k, v) => {
                    // A single Pair coerces to a one-element hash
                    let mut map = HashMap::new();
                    map.insert(k.clone(), v.clone());
                    Value::hash(map)
                }
                // A general-key Pair (`"A" => "b"` builds ValuePair, not the
                // interned-Str-key Pair variant) coerces the same way, with the
                // hash-key stringification `build_hash_from_items` uses.
                ValueView::ValuePair(k, v) => {
                    let mut map = HashMap::new();
                    map.insert(Value::hash_key_encode(k), v.clone());
                    Value::hash(map)
                }
                // A list coerces to a Hash exactly like `my %h = list`: Pairs
                // flatten, bare elements pair up `key => value`, and an *empty*
                // list yields an empty Hash (previously a no-pair / empty list
                // was kept as an Array, so `%!attr<k>` then died "does not
                // support associative indexing" — surfaced by HTTP::MediaType's
                // `has %.parameters` when a media type carried no parameters).
                // An odd non-pair count raku-throws "Odd number of elements";
                // `coerce_attr_value_by_sigil` cannot throw, so keep the raw
                // value on that error and let a later type check report it.
                ValueView::Array(arr, _) => {
                    match crate::runtime::utils::build_hash_from_items(arr.to_vec()) {
                        Ok(h) => h,
                        Err(_) => val.clone(),
                    }
                }
                ValueView::Slip(items) => {
                    match crate::runtime::utils::build_hash_from_items(items.to_vec()) {
                        Ok(h) => h,
                        Err(_) => val.clone(),
                    }
                }
                _ => val.clone(),
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
            matches!(v.view(), ValueView::Whatever)
                || matches!(v.view(), ValueView::Num(f) if f.is_infinite())
                || matches!(v.view(), ValueView::Rat(_, 0))
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
        // When .assuming() binds a named argument, that parameter is shown in
        // the primed signature with the bound value as its default (and becomes
        // optional, dropping any `!`). Named parameters that were NOT primed
        // keep their original state, including their own defaults. A named
        // parameter can be bound by any of its alias names (`:b(:c($a))` binds
        // to either `b` or `c`).
        for pd in &mut param_defs {
            if pd.named
                && let Some(value) = assumed_named_binding(pd, assumed_named)
            {
                pd.default = Some(crate::ast::Expr::Literal(value));
                pd.required = false;
                pd.optional_marker = false;
                pd.where_constraint = None;
            }
        }
        Some(param_defs)
    }
}

/// Returns the bound value if `.assuming` primed this named parameter, matching
/// on the parameter's primary name or any of its alias names (`:b(:c($a))` can
/// be bound as either `b` or `c`). Mirrors `collect_named_names`: only nested
/// named sub-signature params contribute alias names.
fn assumed_named_binding(
    pd: &ParamDef,
    assumed_named: &std::collections::HashMap<String, Value>,
) -> Option<Value> {
    fn strip_sigil(name: &str) -> &str {
        name.strip_prefix(['@', '%', '&']).unwrap_or(name)
    }
    if let Some(v) = assumed_named.get(strip_sigil(&pd.name)) {
        return Some(v.clone());
    }
    let mut cur = pd;
    while cur.named
        && let Some(subs) = &cur.sub_signature
        && subs.len() == 1
        && subs[0].named
    {
        cur = &subs[0];
        if let Some(v) = assumed_named.get(strip_sigil(&cur.name)) {
            return Some(v.clone());
        }
    }
    None
}
