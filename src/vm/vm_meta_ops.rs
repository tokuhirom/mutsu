use super::*;

impl Interpreter {
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

    pub(super) fn exec_meta_op(
        &mut self,
        code: &CompiledCode,
        meta_idx: u32,
        op_idx: u32,
    ) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap_or(Value::Nil);
        let left = self.stack.pop().unwrap_or(Value::Nil);
        let meta = Self::const_str(code, meta_idx).to_string();
        let op = Self::const_str(code, op_idx).to_string();
        let result = match meta.as_str() {
            // `[op]=` compound assignment (e.g. `$x [+]= 6`) lowers to a "reduce"
            // meta-op: reducing the base op over the two operands is just the base
            // op applied once.
            "reduce" => self.eval_reduction_operator_values(&op, &left, &right)?,
            "R" => {
                if op == "..." || op == "...^" {
                    let exclude_end = op == "...^";
                    loan_env!(self, eval_sequence_values(right, left, exclude_end))?
                } else if op == "~~" {
                    Value::Bool(self.vm_smart_match(&right, &left))
                } else if matches!(op.as_str(), ".." | "..^" | "^.." | "^..^") {
                    // `a R.. b` == `b .. a`: build the range with operands
                    // reversed. Reuse the dedicated range builders (which pop
                    // `left`/`right` off the stack) so endpoint coercion and
                    // canonical Range-variant selection stay in one place.
                    self.stack.push(right);
                    self.stack.push(left);
                    match op.as_str() {
                        ".." => self.exec_make_range_op()?,
                        "..^" => self.exec_make_range_excl_op()?,
                        "^.." => self.exec_make_range_excl_start_op()?,
                        "^..^" => self.exec_make_range_excl_both_op()?,
                        _ => unreachable!(),
                    }
                    return Ok(());
                } else {
                    self.eval_reduction_operator_values(&op, &right, &left)?
                }
            }
            "X" => {
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
                let lazy_inputs = value_is_lazy(&left) || value_is_lazy(&right);
                let lazy_limit = 256usize;
                let materialize_side = |v: &Value| -> Vec<Value> {
                    if value_is_lazy(v) {
                        let iter = ZipIter::from_value(v);
                        let len = iter.len().min(lazy_limit);
                        (0..len).map(|i| iter.nth(i)).collect()
                    } else {
                        runtime::value_to_list(v)
                    }
                };
                let left_list = materialize_side(&left);
                let right_list = materialize_side(&right);
                let mut results = Vec::new();
                if op.is_empty() || op == "," {
                    for l in &left_list {
                        for r in &right_list {
                            results.push(Value::array(vec![l.clone(), r.clone()]));
                        }
                    }
                } else if op == "~~" {
                    for l in &left_list {
                        for r in &right_list {
                            results.push(Value::Bool(self.vm_smart_match(l, r)));
                        }
                    }
                } else {
                    for l in &left_list {
                        for r in &right_list {
                            results.push(self.eval_reduction_operator_values(&op, l, r)?);
                        }
                    }
                }
                if lazy_inputs {
                    Value::LazyList(std::sync::Arc::new(crate::value::LazyList::new_cached(
                        results,
                    )))
                } else {
                    // `X` is a Seq (so `.^name` is Seq, `.raku` shows `.Seq`).
                    Value::Seq(std::sync::Arc::new(results))
                }
            }
            "Z" => {
                // Use lazy index-based iteration for ranges to avoid
                // materializing huge/infinite lists like 1..*.
                let left_iter = ZipIter::from_value(&left);
                let right_iter = ZipIter::from_value(&right);
                let all_lazy = left_iter.is_lazy() && right_iter.is_lazy();
                let len = left_iter.len().min(right_iter.len()).min(MAX_ZIP_EXPAND);
                let mut results = Vec::new();
                if op.is_empty() || op == "," {
                    for i in 0..len {
                        results.push(Value::array(vec![left_iter.nth(i), right_iter.nth(i)]));
                    }
                } else if op == "=>" {
                    for i in 0..len {
                        let key = left_iter.nth(i).to_string_value();
                        results.push(Value::Pair(key, Box::new(right_iter.nth(i))));
                    }
                } else {
                    // Check for 3-way zip reduction case ([Z+] a, b, c)
                    // where left has exactly 2 elements and the second is a list.
                    let nested_left = if left_iter.len() == 2 {
                        let second = left_iter.nth(1);
                        match &second {
                            Value::Array(..) | Value::Seq(_) | Value::Slip(_) => {
                                Some((left_iter.nth(0), runtime::value_to_list(&second)))
                            }
                            _ => None,
                        }
                    } else {
                        None
                    };
                    for i in 0..len {
                        if let Some((ref first, ref extra)) = nested_left {
                            let mut v = self.eval_reduction_operator_values(
                                &op,
                                first,
                                &right_iter.nth(i),
                            )?;
                            if let Some(extra_i) = extra.get(i) {
                                v = self.eval_reduction_operator_values(&op, &v, extra_i)?;
                            }
                            results.push(v);
                        } else {
                            results.push(self.eval_reduction_operator_values(
                                &op,
                                &left_iter.nth(i),
                                &right_iter.nth(i),
                            )?);
                        }
                    }
                }
                if all_lazy {
                    Value::LazyList(std::sync::Arc::new(crate::value::LazyList::new_cached(
                        results,
                    )))
                } else {
                    // `Z` is a Seq (so `.^name` is Seq, `.raku` shows `.Seq`).
                    Value::Seq(std::sync::Arc::new(results))
                }
            }
            "!" => {
                let inner = self.eval_reduction_operator_values(&op, &left, &right)?;
                Value::Bool(!inner.truthy())
            }
            _ => {
                return Err(RuntimeError::new(format!(
                    "Unknown meta operator: {}",
                    meta
                )));
            }
        };
        self.stack.push(result);
        Ok(())
    }

    /// List-associative n-ary cross (`X`) / zip (`Z`). `a X b X c` combines all
    /// operands at once so each result is a flat n-tuple (or an n-way reduction
    /// when an operator is attached), matching Raku's list associativity.
    pub(super) fn exec_meta_op_nary(
        &mut self,
        code: &CompiledCode,
        meta_idx: u32,
        op_idx: u32,
        count: u32,
    ) -> Result<(), RuntimeError> {
        let n = count as usize;
        let mut operands: Vec<Value> = Vec::with_capacity(n);
        for _ in 0..n {
            operands.push(self.stack.pop().unwrap_or(Value::Nil));
        }
        operands.reverse();
        let meta = Self::const_str(code, meta_idx).to_string();
        let op = Self::const_str(code, op_idx).to_string();
        let make_tuple = op.is_empty() || op == ",";

        let result = match meta.as_str() {
            "X" => {
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
                let lazy_inputs = operands.iter().any(value_is_lazy);
                let lazy_limit = 256usize;
                let lists: Vec<Vec<Value>> = operands
                    .iter()
                    .map(|v| {
                        if value_is_lazy(v) {
                            let iter = ZipIter::from_value(v);
                            let len = iter.len().min(lazy_limit);
                            (0..len).map(|i| iter.nth(i)).collect()
                        } else {
                            runtime::value_to_list(v)
                        }
                    })
                    .collect();
                // Cartesian product: iterate combinations in row-major order,
                // varying the last operand fastest (matches Raku's X ordering).
                let mut results: Vec<Value> = Vec::new();
                let mut indices = vec![0usize; n];
                let any_empty = lists.iter().any(|l| l.is_empty());
                if !any_empty {
                    'outer: loop {
                        let combo: Vec<Value> =
                            (0..n).map(|k| lists[k][indices[k]].clone()).collect();
                        results.push(self.combine_meta_tuple(&op, make_tuple, combo)?);
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
                if lazy_inputs {
                    Value::LazyList(std::sync::Arc::new(crate::value::LazyList::new_cached(
                        results,
                    )))
                } else {
                    // `X` is a Seq (so `.^name` is Seq, `.raku` shows `.Seq`).
                    Value::Seq(std::sync::Arc::new(results))
                }
            }
            "Z" => {
                let iters: Vec<ZipIter> = operands.iter().map(ZipIter::from_value).collect();
                let all_lazy = iters.iter().all(|it| it.is_lazy());
                let len = iters
                    .iter()
                    .map(|it| it.len())
                    .min()
                    .unwrap_or(0)
                    .min(MAX_ZIP_EXPAND);
                let mut results: Vec<Value> = Vec::with_capacity(len);
                for i in 0..len {
                    let combo: Vec<Value> = iters.iter().map(|it| it.nth(i)).collect();
                    results.push(self.combine_meta_tuple(&op, make_tuple, combo)?);
                }
                if all_lazy {
                    Value::LazyList(std::sync::Arc::new(crate::value::LazyList::new_cached(
                        results,
                    )))
                } else {
                    // `Z` is a Seq (so `.^name` is Seq, `.raku` shows `.Seq`).
                    Value::Seq(std::sync::Arc::new(results))
                }
            }
            _ => {
                return Err(RuntimeError::new(format!(
                    "Unknown n-ary meta operator: {}",
                    meta
                )));
            }
        };
        self.stack.push(result);
        Ok(())
    }

    /// Combine one tuple of operands for an n-ary X/Z: either build a flat
    /// tuple (no operator) or left-fold the operator across all elements.
    fn combine_meta_tuple(
        &mut self,
        op: &str,
        make_tuple: bool,
        combo: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if make_tuple {
            return Ok(Value::array(combo));
        }
        let mut iter = combo.into_iter();
        let mut acc = iter.next().unwrap_or(Value::Nil);
        for elem in iter {
            acc = if op == "~~" {
                Value::Bool(self.vm_smart_match(&acc, &elem))
            } else {
                self.eval_reduction_operator_values(op, &acc, &elem)?
            };
        }
        Ok(acc)
    }
}

/// Maximum elements to produce from Z (zip) when iterating over ranges.
/// This caps the output for infinite ranges (e.g., `1..* Z** 1..*`).
/// Kept small because the meta-operator (e.g. `**`) may be expensive
/// for large values. The caller (e.g., `.[^5]`) will further limit.
// TODO: Ideally Z should return a lazy Seq and only compute elements on demand.
const MAX_ZIP_EXPAND: usize = 1_000;

/// Helper for lazy index-based iteration over values in Z (zip) operations.
/// Avoids materializing huge ranges like `1..*` into million-element Vecs.
enum ZipIter {
    /// Inclusive integer range: elements are start, start+1, ..., end
    IntRange { start: i64, count: usize },
    /// Exclusive-end integer range: elements are start, start+1, ..., end-1
    IntRangeExcl { start: i64, count: usize },
    /// Already-materialized list
    List(Vec<Value>),
    /// A list that ends with `*` (Whatever): the last real element is repeated
    /// to extend the list to any requested length.
    ExtendedList {
        items: Vec<Value>,
        /// The last real element (before `*`), used for extension
        fill: Value,
    },
    /// A lazy list (preserves laziness for is-lazy propagation)
    Lazy(Vec<Value>),
}

impl ZipIter {
    fn from_value(val: &Value) -> Self {
        match val {
            Value::Range(a, b) => {
                let count = if *b >= *a {
                    ((*b - *a + 1) as usize).min(MAX_ZIP_EXPAND)
                } else {
                    0
                };
                ZipIter::IntRange { start: *a, count }
            }
            Value::RangeExcl(a, b) => {
                let count = if *b > *a {
                    ((*b - *a) as usize).min(MAX_ZIP_EXPAND)
                } else {
                    0
                };
                ZipIter::IntRangeExcl { start: *a, count }
            }
            Value::RangeExclStart(a, b) => {
                let start = *a + 1;
                let count = if *b >= start {
                    ((*b - start + 1) as usize).min(MAX_ZIP_EXPAND)
                } else {
                    0
                };
                ZipIter::IntRange { start, count }
            }
            Value::RangeExclBoth(a, b) => {
                let start = *a + 1;
                let count = if *b > start {
                    ((*b - start) as usize).min(MAX_ZIP_EXPAND)
                } else {
                    0
                };
                ZipIter::IntRangeExcl { start, count }
            }
            // Nil in zip context is a 1-element list (not empty), matching Raku behavior
            // where `Nil Z+ 2` yields `(2)` (Nil coerces to 0).
            Value::Nil => ZipIter::List(vec![Value::Nil]),
            Value::LazyList(_) => {
                let list = runtime::value_to_list(val);
                let len = list.len().min(MAX_ZIP_EXPAND);
                ZipIter::Lazy(list[..len].to_vec())
            }
            _ => {
                let list = runtime::value_to_list(val);
                // Check for trailing Whatever (*) — extends the list by
                // repeating the last real element.
                if list.len() >= 2 && matches!(list.last(), Some(Value::Whatever)) {
                    let items: Vec<Value> = list[..list.len() - 1].to_vec();
                    let fill = items.last().cloned().unwrap_or(Value::Nil);
                    ZipIter::ExtendedList { items, fill }
                } else {
                    ZipIter::List(list)
                }
            }
        }
    }

    /// Returns true if this side represents an infinite / lazy source.
    fn is_lazy(&self) -> bool {
        match self {
            ZipIter::IntRange { count, .. } | ZipIter::IntRangeExcl { count, .. } => {
                *count >= MAX_ZIP_EXPAND
            }
            ZipIter::ExtendedList { .. } | ZipIter::Lazy(_) => true,
            ZipIter::List(_) => false,
        }
    }

    fn len(&self) -> usize {
        match self {
            ZipIter::IntRange { count, .. } | ZipIter::IntRangeExcl { count, .. } => *count,
            ZipIter::List(v) | ZipIter::Lazy(v) => v.len(),
            // Extended lists can match any length from the other side
            ZipIter::ExtendedList { .. } => usize::MAX,
        }
    }

    fn nth(&self, i: usize) -> Value {
        match self {
            ZipIter::IntRange { start, .. } | ZipIter::IntRangeExcl { start, .. } => {
                Value::Int(*start + i as i64)
            }
            ZipIter::List(v) | ZipIter::Lazy(v) => v[i].clone(),
            ZipIter::ExtendedList { items, fill } => {
                if i < items.len() {
                    items[i].clone()
                } else {
                    fill.clone()
                }
            }
        }
    }
}
