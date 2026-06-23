use super::*;
use crate::ast::{CallArg, ControlFlowKind};

impl Interpreter {
    pub(in crate::runtime) fn dispatch_grep(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        fn stmt_contains_last(stmt: &Stmt) -> bool {
            match stmt {
                Stmt::Last(_) => true,
                Stmt::Expr(expr)
                | Stmt::Return(expr)
                | Stmt::Die(expr)
                | Stmt::Fail(expr)
                | Stmt::Take(expr, _) => expr_contains_last(expr),
                Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => expr_contains_last(expr),
                Stmt::If {
                    cond,
                    then_branch,
                    else_branch,
                    ..
                } => {
                    expr_contains_last(cond)
                        || then_branch.iter().any(stmt_contains_last)
                        || else_branch.iter().any(stmt_contains_last)
                }
                Stmt::While { cond, body, .. } => {
                    expr_contains_last(cond) || body.iter().any(stmt_contains_last)
                }
                Stmt::For { iterable, body, .. } => {
                    expr_contains_last(iterable) || body.iter().any(stmt_contains_last)
                }
                Stmt::Loop {
                    init,
                    cond,
                    step,
                    body,
                    ..
                } => {
                    init.as_ref().is_some_and(|s| stmt_contains_last(s))
                        || cond.as_ref().is_some_and(expr_contains_last)
                        || step.as_ref().is_some_and(expr_contains_last)
                        || body.iter().any(stmt_contains_last)
                }
                Stmt::Block(body)
                | Stmt::SyntheticBlock(body)
                | Stmt::React { body }
                | Stmt::Catch(body)
                | Stmt::Control(body)
                | Stmt::Default(body) => body.iter().any(stmt_contains_last),
                Stmt::Whenever { supply, body, .. } => {
                    expr_contains_last(supply) || body.iter().any(stmt_contains_last)
                }
                Stmt::Given { topic, body } => {
                    expr_contains_last(topic) || body.iter().any(stmt_contains_last)
                }
                Stmt::When { cond, body } => {
                    expr_contains_last(cond) || body.iter().any(stmt_contains_last)
                }
                Stmt::ClassDecl { body, .. }
                | Stmt::RoleDecl { body, .. }
                | Stmt::Package { body, .. }
                | Stmt::SubDecl { body, .. }
                | Stmt::MethodDecl { body, .. } => body.iter().any(stmt_contains_last),
                Stmt::HasDecl { default, .. } => default.as_ref().is_some_and(expr_contains_last),
                Stmt::Call { args, .. } => args.iter().any(|arg| match arg {
                    CallArg::Positional(e) | CallArg::Slip(e) | CallArg::Invocant(e) => {
                        expr_contains_last(e)
                    }
                    CallArg::Named { value, .. } => value.as_ref().is_some_and(expr_contains_last),
                }),
                Stmt::Label { stmt, .. } => stmt_contains_last(stmt),
                Stmt::EnumDecl { variants, .. } => variants
                    .iter()
                    .any(|(_, v)| v.as_ref().is_some_and(expr_contains_last)),
                Stmt::Goto(expr) => expr_contains_last(expr),
                _ => false,
            }
        }

        fn expr_contains_last(expr: &Expr) -> bool {
            match expr {
                Expr::ControlFlow {
                    kind: ControlFlowKind::Last,
                    ..
                } => true,
                Expr::Unary { expr, .. }
                | Expr::PostfixOp { expr, .. }
                | Expr::Reduction { expr, .. }
                | Expr::PositionalPair(expr)
                | Expr::ZenSlice(expr)
                | Expr::IndirectTypeLookup(expr) => expr_contains_last(expr),
                Expr::DoStmt(stmt) => stmt_contains_last(stmt),
                Expr::Binary { left, right, .. }
                | Expr::HyperOp { left, right, .. }
                | Expr::MetaOp { left, right, .. } => {
                    expr_contains_last(left) || expr_contains_last(right)
                }
                Expr::InfixFunc { left, right, .. } => {
                    expr_contains_last(left) || right.iter().any(expr_contains_last)
                }
                Expr::Ternary {
                    cond,
                    then_expr,
                    else_expr,
                } => {
                    expr_contains_last(cond)
                        || expr_contains_last(then_expr)
                        || expr_contains_last(else_expr)
                }
                Expr::Index { target, index, .. } => {
                    expr_contains_last(target) || expr_contains_last(index)
                }
                Expr::Exists { target, arg, .. } => {
                    expr_contains_last(target)
                        || arg
                            .as_ref()
                            .is_some_and(|index_expr| expr_contains_last(index_expr))
                }
                Expr::MethodCall { target, args, .. }
                | Expr::DynamicMethodCall { target, args, .. }
                | Expr::HyperMethodCall { target, args, .. }
                | Expr::HyperMethodCallDynamic { target, args, .. } => {
                    expr_contains_last(target) || args.iter().any(expr_contains_last)
                }
                Expr::CallOn { target, args } => {
                    expr_contains_last(target) || args.iter().any(expr_contains_last)
                }
                Expr::Call { args, .. } => args.iter().any(expr_contains_last),
                Expr::StringInterpolation(items)
                | Expr::ArrayLiteral(items)
                | Expr::BracketArray(items, _)
                | Expr::CaptureLiteral(items) => items.iter().any(expr_contains_last),
                Expr::Hash(items) => items
                    .iter()
                    .any(|(_, val)| val.as_ref().is_some_and(expr_contains_last)),
                Expr::Block(body)
                | Expr::AnonSub { body, .. }
                | Expr::AnonSubParams { body, .. }
                | Expr::Gather(body)
                | Expr::DoBlock { body, .. } => body.iter().any(stmt_contains_last),
                Expr::Try { body, catch } => {
                    body.iter().any(stmt_contains_last)
                        || catch
                            .as_ref()
                            .is_some_and(|stmts| stmts.iter().any(stmt_contains_last))
                }
                Expr::IndexAssign {
                    target,
                    index,
                    value,
                    ..
                } => {
                    expr_contains_last(target)
                        || expr_contains_last(index)
                        || expr_contains_last(value)
                }
                Expr::AssignExpr { expr, .. } => expr_contains_last(expr),
                Expr::Lambda { body, .. } => body.iter().any(stmt_contains_last),
                Expr::Subst { .. }
                | Expr::NonDestructiveSubst { .. }
                | Expr::Transliterate { .. }
                | Expr::MatchRegex(_)
                | Expr::Literal(_)
                | Expr::Whatever
                | Expr::HyperWhatever
                | Expr::BareWord(_)
                | Expr::Var(_)
                | Expr::CaptureVar(_)
                | Expr::ArrayVar(_)
                | Expr::HashVar(_)
                | Expr::CodeVar(_)
                | Expr::EnvIndex(_)
                | Expr::RoutineMagic
                | Expr::BlockMagic
                | Expr::PseudoStash(_) => false,
                Expr::IndirectCodeLookup { package, .. } => expr_contains_last(package),
                Expr::SymbolicDeref { expr, .. } => expr_contains_last(expr),
                Expr::HyperSlice { target, .. } => expr_contains_last(target),
                Expr::HyperIndex { target, keys } => {
                    expr_contains_last(target) || expr_contains_last(keys)
                }
                _ => false,
            }
        }

        // Parse named adverbs (:k, :v, :kv, :p) from args
        let mut has_k = false;
        let mut has_kv = false;
        let mut has_p = false;
        let mut positional_args: Vec<Value> = Vec::new();
        for arg in args {
            match arg {
                Value::Pair(key, value) if key == "k" => has_k = value.truthy(),
                Value::Pair(key, value) if key == "kv" => has_kv = value.truthy(),
                Value::Pair(key, value) if key == "p" => has_p = value.truthy(),
                Value::Pair(key, value) if key == "v" => {
                    if !value.truthy() {
                        return Err(RuntimeError::new(
                            "X::Adverb: Unexpected adverb 'v' passed to grep",
                        ));
                    }
                    // :v is the default behavior, just ignore when truthy
                }
                Value::Pair(key, _) => {
                    return Err(RuntimeError::new(format!(
                        "X::Adverb: Unexpected adverb '{}'",
                        key
                    )));
                }
                _ => positional_args.push(arg.clone()),
            }
        }
        let grep_adverb = if has_k {
            GrepAdverb::K
        } else if has_kv {
            GrepAdverb::Kv
        } else if has_p {
            GrepAdverb::P
        } else {
            GrepAdverb::V
        };
        let args = &positional_args;

        // Infinite/lazy source with the default (`:v`) adverb: return a truly
        // lazy `grep` pipeline stage instead of materializing the (possibly
        // infinite) source. Adverbed greps (`:k`/`:kv`/`:p`) need positional
        // indices over the whole result, so they keep the eager path.
        if matches!(grep_adverb, GrepAdverb::V)
            && Self::is_lazy_pipe_source(&target)
            && let Some(func) = args.first().cloned()
            && let Some(pipe) = Self::make_lazy_pipe(target.clone(), func, true)
        {
            return Ok(pipe);
        }

        match target {
            Value::Package(class_name) if class_name == "Supply" => Err(RuntimeError::new(
                "Cannot call .grep on a Supply type object",
            )),
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Supply" => {
                let source_values =
                    if let Some(on_demand_cb) = attributes.as_map().get("on_demand_callback") {
                        let emitter = Value::make_instance(Symbol::intern("Supplier"), {
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
                            .as_map()
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
                        .as_map()
                        .get("live")
                        .cloned()
                        .unwrap_or(Value::Bool(false)),
                );
                Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
            }
            Value::Array(items, arr_kind) => {
                let (filtered, mutated_items) =
                    self.eval_grep_over_items_with_mutated(args.first().cloned(), items.to_vec())?;
                // Determine which source positions matched (identity scan against
                // the post-grep source), so the matched slots can be shared with
                // the result as first-class element containers.
                let mut indices = Vec::new();
                if let Value::Array(filtered_items, ..) = &filtered {
                    let mut scan_from = 0usize;
                    for needle in filtered_items.iter() {
                        if let Some(rel) = mutated_items[scan_from..].iter().position(|candidate| {
                            crate::runtime::utils::values_identical(candidate, needle)
                        }) {
                            let absolute = scan_from + rel;
                            indices.push(absolute);
                            scan_from = absolute.saturating_add(1);
                        }
                    }
                }
                // Promote each matched source slot to a shared `ContainerRef`
                // cell and reference the SAME cells from the grep result. A
                // writeback loop (`for @a.grep(...) { $_++ }` / `@a.grep(...)>>++`)
                // then mutates THROUGH the cell into @a's slot via the ordinary
                // element-cell write path — no GrepView side channel needed. A
                // later `=` assignment (`my @g = @a.grep(...)`) decontainerizes the
                // cells, so the named copy owns its values and never writes back.
                let mut promoted = mutated_items;
                let mut shared_cells: Vec<Value> = Vec::with_capacity(indices.len());
                for &i in &indices {
                    let cell = match &promoted[i] {
                        v @ Value::ContainerRef(_) => v.clone(),
                        other => Value::ContainerRef(std::sync::Arc::new(std::sync::Mutex::new(
                            other.clone(),
                        ))),
                    };
                    promoted[i] = cell.clone();
                    shared_cells.push(cell);
                }
                let updated_source = crate::value::Value::array_data_like(&items, promoted);
                self.overwrite_array_bindings_by_identity(
                    &items,
                    Value::Array(updated_source, arr_kind),
                );
                // Build the result array from the shared cells (default `:v`
                // adverb). The `:k`/`:kv`/`:p` adverbs rebuild a fresh array in
                // `transform_result` from `indices`, which drops the aliasing (a
                // keys/pairs copy owns its values).
                let filtered = match filtered {
                    Value::Array(filtered_items, fkind) if !indices.is_empty() => {
                        let mut data = (*filtered_items).clone();
                        data.items = shared_cells;
                        Value::Array(std::sync::Arc::new(data), fkind)
                    }
                    other => other,
                };
                grep_adverb.transform_result(filtered, &indices)
            }
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..) => {
                // Route integer ranges through the unified pull iterator so an
                // open-ended range (`1..Inf` == `Range(1, i64::MAX)`) is
                // truncated at the cap instead of panicking with
                // `capacity overflow` (ANALYSIS §8.2). This matches the cap the
                // `GenericRange` arm already uses via `value_to_list`.
                let items = crate::runtime::value_iterator::materialize_capped(
                    &target,
                    crate::runtime::utils::MAX_RANGE_EXPAND as usize,
                );
                self.eval_grep_with_adverb(args.first().cloned(), items, &grep_adverb)
            }
            Value::GenericRange { .. } => {
                if let Value::GenericRange {
                    start,
                    end,
                    excl_start,
                    ..
                } = &target
                {
                    let end_num = end.to_f64();
                    if end_num.is_infinite()
                        && end_num.is_sign_positive()
                        && let Some(Value::Sub(data)) = args.first().cloned()
                        && data.body.iter().any(stmt_contains_last)
                    {
                        let mut current = start.to_f64() as i64;
                        if *excl_start {
                            current += 1;
                        }
                        let mut result = Vec::new();
                        let mut result_indices = Vec::new();
                        let limit = 1_000_000usize;
                        let mut item_idx = 0usize;
                        while result.len() < limit {
                            let item = match start.as_ref() {
                                Value::Num(_) => Value::Num(current as f64),
                                Value::Rat(_, den) => crate::value::make_rat(current * *den, *den),
                                _ => Value::Int(current),
                            };
                            'redo_item: loop {
                                match self.call_sub_value(
                                    Value::Sub(data.clone()),
                                    vec![item.clone()],
                                    false,
                                ) {
                                    Ok(pred) => {
                                        if pred.truthy() {
                                            result.push(item.clone());
                                            result_indices.push(item_idx);
                                        }
                                        break 'redo_item;
                                    }
                                    Err(e) if e.is_redo => continue 'redo_item,
                                    Err(e) if e.is_next => break 'redo_item,
                                    Err(e) if e.is_last => {
                                        return grep_adverb.transform_result(
                                            Value::array(result),
                                            &result_indices,
                                        );
                                    }
                                    Err(e) => return Err(e),
                                }
                            }
                            current += 1;
                            item_idx += 1;
                        }
                        return grep_adverb.transform_result(Value::array(result), &result_indices);
                    }
                    if end_num.is_infinite() && end_num.is_sign_positive() {
                        // Preserve laziness for open-ended ranges in grep.
                        return Ok(target);
                    }
                }
                let items = crate::runtime::utils::value_to_list(&target);
                self.eval_grep_with_adverb(args.first().cloned(), items, &grep_adverb)
            }
            Value::Str(s) => {
                if let Some(Value::Sub(data)) = args.first()
                    && matches!(
                        data.body.last(),
                        Some(Stmt::Expr(Expr::Literal(Value::Regex(_))))
                    )
                {
                    return self.eval_grep_with_adverb(
                        args.first().cloned(),
                        vec![Value::Str(s.clone())],
                        &grep_adverb,
                    );
                }
                match grep_adverb {
                    GrepAdverb::K => Ok(Value::Int(0)),
                    GrepAdverb::Kv => Ok(Value::array(vec![Value::Int(0), Value::Str(s.clone())])),
                    GrepAdverb::P => Ok(Value::ValuePair(
                        Box::new(Value::Int(0)),
                        Box::new(Value::Str(s.clone())),
                    )),
                    GrepAdverb::V => Ok(Value::Str(s.clone())),
                }
            }
            Value::Seq(items) | Value::Slip(items) => {
                self.eval_grep_with_adverb(args.first().cloned(), items.to_vec(), &grep_adverb)
            }
            Value::Set(..) | Value::Bag(..) | Value::Mix(..) | Value::Hash(..) => {
                let items = crate::runtime::utils::value_to_list(&target);
                self.eval_grep_with_adverb(args.first().cloned(), items, &grep_adverb)
            }
            other => {
                // A Blob/Buf greps over its bytes (matches raku iteration).
                if let Some(bytes) = Self::buf_as_byte_items(&other) {
                    return self.eval_grep_with_adverb(args.first().cloned(), bytes, &grep_adverb);
                }
                // Treat any other value as a single-element list for grep
                self.eval_grep_with_adverb(args.first().cloned(), vec![other], &grep_adverb)
            }
        }
    }

    /// Helper: run grep over items, compute indices, and apply adverb transformation.
    fn eval_grep_with_adverb(
        &mut self,
        func: Option<Value>,
        items: Vec<Value>,
        adverb: &GrepAdverb,
    ) -> Result<Value, RuntimeError> {
        let original_items = items.clone();
        let filtered = self.eval_grep_over_items(func, items)?;
        if matches!(adverb, GrepAdverb::V) {
            return Ok(filtered);
        }
        let indices = compute_grep_indices(&original_items, &filtered);
        adverb.transform_result(filtered, &indices)
    }
}
