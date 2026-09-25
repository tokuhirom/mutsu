use super::vm_helpers_lazy_adaptor::is_unbounded_operand;
use super::*;
use crate::compiled_operator::MetaKind;

impl Interpreter {
    /// How a `Z`/`X` with infix `op` combines one row (`is_zip` picks the
    /// `Z=>` Pair form, which keeps a List key intact).
    /// Takes the compiler-interned `op` so nothing is interned per execution.
    pub(super) fn meta_row_combine(op: Symbol, is_zip: bool) -> crate::value::RowCombine {
        use crate::value::RowCombine;
        match op.as_str() {
            "" | "," => RowCombine::List,
            "=>" if is_zip => RowCombine::Pair,
            "~~" => RowCombine::SmartMatch,
            _ => RowCombine::Infix(op),
        }
    }

    pub(super) fn canonical_infix_lookup_name(name: &str) -> std::borrow::Cow<'_, str> {
        if name == "(+)" {
            return std::borrow::Cow::Borrowed("+");
        }
        std::borrow::Cow::Borrowed(name)
    }

    pub(super) fn should_retry_with_canonical_infix_name(name: &str) -> bool {
        matches!(
            name,
            "(<=)" | "⊆" | "(>=)" | "⊇" | "(<)" | "⊂" | "(>)" | "⊃" | "⊈" | "⊉" | "⊄" | "⊅"
        )
    }

    pub(super) fn exec_meta_op(&mut self, meta: MetaKind, op: Symbol) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap_or(Value::NIL);
        let left = self.stack.pop().unwrap_or(Value::NIL);
        // ADR-0058: the meta-ops below read their operands' elements through
        // pure helpers (`value_to_list`), so a
        // still-deferred `.map` operand has to run its callback first.
        self.reify_map_grep_seq(&left)?;
        self.reify_map_grep_seq(&right)?;
        // Both operands are compile-time data: `meta` is typed and `op` was
        // interned by the compiler, so `as_str` hands back the interner's own
        // `&'static str` and nothing here allocates. Its structure is decoded
        // once here rather than once per cross/zip pair.
        let op_sym = op;
        let op = op.as_str();
        let op_shape = crate::compiled_operator::InfixShape::lower(op);
        let result = match meta {
            // `[op]=` compound assignment (e.g. `$x [+]= 6`) lowers to a "reduce"
            // meta-op: reducing the base op over the two operands is just the base
            // op applied once.
            MetaKind::Reduce => self.eval_infix_shape(op_shape.as_ref(), &left, &right)?,
            MetaKind::Reverse => {
                if op == "..." || op == "...^" {
                    let exclude_end = op == "...^";
                    loan_env!(self, eval_sequence_values(right, left, exclude_end))?
                } else if op == "~~" {
                    Value::truth(self.vm_smart_match(&right, &left))
                } else if matches!(op, ".." | "..^" | "^.." | "^..^") {
                    // `a R.. b` == `b .. a`: build the range with operands
                    // reversed. Reuse the dedicated range builders (which pop
                    // `left`/`right` off the stack) so endpoint coercion and
                    // canonical Range-variant selection stay in one place.
                    self.stack.push(right);
                    self.stack.push(left);
                    match op {
                        ".." => self.exec_make_range_op()?,
                        "..^" => self.exec_make_range_excl_op()?,
                        "^.." => self.exec_make_range_excl_start_op()?,
                        "^..^" => self.exec_make_range_excl_both_op()?,
                        _ => unreachable!(),
                    }
                    return Ok(());
                } else {
                    self.eval_infix_shape(op_shape.as_ref(), &right, &left)?
                }
            }
            // Cost: O(e_l + e_r + e_l * e_r), e = elements of each operand, every
            // pair built eagerly, when both are finite. With an infinite operand
            // the product is a lazy `PipeAdaptor::Cross` stage: O(1) pulls per
            // pair produced, as in Rakudo (#9159).
            MetaKind::Cross => {
                let columns = [left, right];
                match Self::lazy_cross_pipe(&columns, Self::meta_row_combine(op_sym, false)) {
                    Some(pipe) => pipe,
                    None => {
                        let [left, right] = columns;
                        let left_list = self.cross_operand_list(&left)?;
                        let right_list = self.cross_operand_list(&right)?;
                        let mut results = Vec::with_capacity(left_list.len() * right_list.len());
                        if op.is_empty() || op == "," {
                            for l in &left_list {
                                for r in &right_list {
                                    results.push(Value::array(vec![l.clone(), r.clone()]));
                                }
                            }
                        } else if op == "~~" {
                            for l in &left_list {
                                for r in &right_list {
                                    results.push(Value::truth(self.vm_smart_match(l, r)));
                                }
                            }
                        } else {
                            for l in &left_list {
                                for r in &right_list {
                                    results.push(self.eval_infix_shape(op_shape.as_ref(), l, r)?);
                                }
                            }
                        }
                        // `X` is a Seq (so `.^name` is Seq, `.raku` shows `.Seq`).
                        Value::seq(results)
                    }
                }
            }
            // Cost: O(e_f + r) pulls, e_f = elements of a finite operand, r = rows
            // (its length), when some operand is finite: an unbounded operand is
            // only pulled as far as the rows need. With every operand unbounded
            // the zip is a lazy `PipeAdaptor::Zip` stage: O(1) pulls per row
            // produced, as in Rakudo (#9159).
            MetaKind::Zip => {
                let columns = [left, right];
                match Self::lazy_zip_pipe(&columns, Self::meta_row_combine(op_sym, true)) {
                    Some(pipe) => pipe,
                    None => {
                        let rows = self.zip_rows_bounded(&columns)?;
                        let [left, _] = columns;
                        let mut results = Vec::with_capacity(rows.len());
                        if op.is_empty() || op == "," {
                            results.extend(rows.into_iter().map(Value::array));
                        } else if op == "=>" {
                            // ADR-0021 I2: data-minted pairs default positional,
                            // and the key keeps its own value/type -- including
                            // a List key produced by `cross()`/tuple-valued left
                            // operands, which stringifying here would flatten
                            // into `"1 2 3"`.
                            for row in rows {
                                let mut it = row.into_iter();
                                let k = it.next().unwrap_or(Value::NIL);
                                results.push(Value::value_pair(k, it.next().unwrap_or(Value::NIL)));
                            }
                        } else {
                            // Check for 3-way zip reduction case ([Z+] a, b, c)
                            // where left has exactly 2 elements and the second
                            // is a list.
                            let left_list = if is_unbounded_operand(&left) {
                                Vec::new()
                            } else {
                                Self::zip_operand_list(&left)
                            };
                            let nested_left = if left_list.len() == 2 {
                                match left_list[1].view() {
                                    ValueView::Array(..)
                                    | ValueView::Seq(_)
                                    | ValueView::Slip(_) => Some((
                                        left_list[0].clone(),
                                        runtime::value_to_list(&left_list[1]),
                                    )),
                                    _ => None,
                                }
                            } else {
                                None
                            };
                            for (i, row) in rows.into_iter().enumerate() {
                                let mut it = row.into_iter();
                                let l = it.next().unwrap_or(Value::NIL);
                                let r = it.next().unwrap_or(Value::NIL);
                                if let Some((ref first, ref extra)) = nested_left {
                                    let mut v =
                                        self.eval_infix_shape(op_shape.as_ref(), first, &r)?;
                                    if let Some(extra_i) = extra.get(i) {
                                        v =
                                            self.eval_infix_shape(op_shape.as_ref(), &v, extra_i)?;
                                    }
                                    results.push(v);
                                } else {
                                    results.push(self.eval_infix_shape(
                                        op_shape.as_ref(),
                                        &l,
                                        &r,
                                    )?);
                                }
                            }
                        }
                        // `Z` is a Seq (so `.^name` is Seq, `.raku` shows `.Seq`).
                        Value::seq(results)
                    }
                }
            }
            MetaKind::Negate => {
                let inner = self.eval_infix_shape(op_shape.as_ref(), &left, &right)?;
                Value::truth(!inner.truthy())
            }
        };
        self.stack.push(result);
        Ok(())
    }

    /// X/Z meta-assignment: `@a X[+=] @b`, `@a Z[+=] @b`. The inner op is an
    /// in-place assignment operator (`+=`, `~=`, `**=`, `min=`, …). Each cross
    /// (`X`, left index slowest) or zip (`Z`) pair mutates the corresponding
    /// left cell with the base op, in place. Pushes two values: the Seq of the
    /// per-op assignment results (bottom) and the mutated left container (top).
    /// The compiler stores the mutated container back into the lvalue, leaving
    /// the Seq as the expression value.
    pub(super) fn exec_meta_op_assign(
        &mut self,
        meta: MetaKind,
        op: Symbol,
    ) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap_or(Value::NIL);
        let left = self.stack.pop().unwrap_or(Value::NIL);
        let op = op.as_str();
        // Strip the trailing `=` to get the base op (`+=` -> `+`, `min=` -> `min`),
        // and decode its shape once for every pair below.
        let base_op = &op[..op.len() - 1];
        let op_shape = crate::compiled_operator::InfixShape::lower(base_op);

        // A scalar left operand (`$a X[+=] @b`) folds into a single cell; a
        // list-like left (`@a`) keeps its per-element cells.
        let left_is_listy = matches!(
            left.view(),
            ValueView::Array(..)
                | ValueView::Seq(_)
                | ValueView::Slip(_)
                | ValueView::Range(..)
                | ValueView::RangeExcl(..)
                | ValueView::RangeExclStart(..)
                | ValueView::RangeExclBoth(..)
                | ValueView::GenericRange { .. }
                | ValueView::LazyList(_)
        );
        let mut left_cells = runtime::value_to_list(&left);
        let right_list = runtime::value_to_list(&right);

        // Per-op assignment results, in evaluation order.
        let mut results: Vec<Value> = Vec::new();
        if meta == MetaKind::Cross {
            // Cross: left index slowest, so all right values accumulate into the
            // first left cell before advancing to the next.
            for cell in left_cells.iter_mut() {
                for r in &right_list {
                    let v = self.eval_infix_shape(op_shape.as_ref(), cell, r)?;
                    *cell = v.clone();
                    results.push(v);
                }
            }
        } else {
            // Zip: element-wise up to the shorter length.
            let n = left_cells.len().min(right_list.len());
            for i in 0..n {
                let v = self.eval_infix_shape(op_shape.as_ref(), &left_cells[i], &right_list[i])?;
                left_cells[i] = v.clone();
                results.push(v);
            }
        }

        let result_seq = Value::seq(results);
        let mutated_left = if left_is_listy {
            Value::real_array(left_cells)
        } else {
            left_cells.into_iter().next().unwrap_or(Value::NIL)
        };
        // Bottom: the Seq (expression value). Top: the mutated container, which
        // the compiler stores back into the lvalue.
        self.stack.push(result_seq);
        self.stack.push(mutated_left);
        Ok(())
    }

    /// List-associative n-ary cross (`X`) / zip (`Z`). `a X b X c` combines all
    /// operands at once so each result is a flat n-tuple (or an n-way reduction
    /// when an operator is attached), matching Raku's list associativity.
    ///
    /// Cost: `X` is O(sum e_i + prod e_i) and `Z` is O(sum e_i + n * min e_i),
    /// e_i = elements of the i-th of n operands (each copied, results built
    /// eagerly) when finite; an infinite operand makes the result a lazy
    /// adaptor stage costing O(n) pulls per element, as in `exec_meta_op`.
    pub(super) fn exec_meta_op_nary(
        &mut self,
        meta: MetaKind,
        op: Symbol,
        count: u32,
    ) -> Result<(), RuntimeError> {
        let n = count as usize;
        let mut operands: Vec<Value> = Vec::with_capacity(n);
        for _ in 0..n {
            operands.push(self.stack.pop().unwrap_or(Value::NIL));
        }
        operands.reverse();
        let op_sym = op;

        let result = match meta {
            MetaKind::Cross => {
                let combine = Self::meta_row_combine(op_sym, false);
                match Self::lazy_cross_pipe(&operands, combine.clone()) {
                    Some(pipe) => pipe,
                    None => {
                        let lists: Vec<Vec<Value>> = operands
                            .iter()
                            .map(|v| self.cross_operand_list(v))
                            .collect::<Result<_, _>>()?;
                        // Cartesian product: iterate combinations in row-major
                        // order, varying the last operand fastest (matches
                        // Raku's X ordering).
                        let mut results: Vec<Value> = Vec::new();
                        let mut indices = vec![0usize; n];
                        if !lists.iter().any(|l| l.is_empty()) {
                            'outer: loop {
                                let combo: Vec<Value> =
                                    (0..n).map(|k| lists[k][indices[k]].clone()).collect();
                                results.push(self.combine_row(&combine, combo)?);
                                // Increment the mixed-radix index from the right.
                                let mut k = n;
                                loop {
                                    if k == 0 {
                                        break 'outer;
                                    }
                                    k -= 1;
                                    indices[k] += 1;
                                    if indices[k] < lists[k].len() {
                                        break;
                                    }
                                    indices[k] = 0;
                                }
                            }
                        }
                        // `X` is a Seq (so `.^name` is Seq, `.raku` shows `.Seq`).
                        Value::seq(results)
                    }
                }
            }
            MetaKind::Zip => {
                // `=>` is not list-associative, so an n-ary zip folds it
                // like any other infix.
                let combine = Self::meta_row_combine(op_sym, n == 2);
                match Self::lazy_zip_pipe(&operands, combine.clone()) {
                    Some(pipe) => pipe,
                    None => {
                        let rows = self.zip_rows_bounded(&operands)?;
                        let mut results: Vec<Value> = Vec::with_capacity(rows.len());
                        for row in rows {
                            results.push(self.combine_row(&combine, row)?);
                        }
                        // `Z` is a Seq (so `.^name` is Seq, `.raku` shows `.Seq`).
                        Value::seq(results)
                    }
                }
            }
            // Only `X` and `Z` chain list-associatively, so the compiler
            // never emits this opcode for another meta-operator.
            MetaKind::Reduce | MetaKind::Reverse | MetaKind::Negate => {
                return Err(RuntimeError::new(format!(
                    "Unknown n-ary meta operator: {}",
                    meta.as_str()
                )));
            }
        };
        self.stack.push(result);
        Ok(())
    }
}
