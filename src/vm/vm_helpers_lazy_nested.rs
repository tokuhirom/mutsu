use super::*;

impl Interpreter {
    /// Reify nested map results only when a reader needs their elements.
    // Cost: O(n + c), n = nested collection elements, c = callbacks run.
    pub(crate) fn reify_nested_map_grep_for_read(
        &mut self,
        value: &Value,
    ) -> Result<(), RuntimeError> {
        let mut seen = std::collections::HashSet::new();
        self.reify_nested_map_grep_for_read_inner(value, &mut seen)
    }

    fn reify_nested_map_grep_for_read_inner(
        &mut self,
        value: &Value,
        seen: &mut std::collections::HashSet<(u8, usize)>,
    ) -> Result<(), RuntimeError> {
        self.reify_map_grep_seq(value)?;
        match value.view() {
            ValueView::Seq(body) => {
                if seen.insert((0, body.identity())) {
                    for item in body.iter() {
                        self.reify_nested_map_grep_for_read_inner(item, seen)?;
                    }
                }
            }
            ValueView::Array(items, _) => {
                if seen.insert((1, crate::gc::Gc::as_ptr(&items) as usize)) {
                    for item in items.iter() {
                        self.reify_nested_map_grep_for_read_inner(item, seen)?;
                    }
                }
            }
            ValueView::Scalar(inner) => {
                self.reify_nested_map_grep_for_read_inner(inner, seen)?;
            }
            // A finite `gather` is a LazyList until pulled: fill its cache so
            // pure readers see its elements (see the Seq arm above).
            ValueView::LazyList(list) => {
                if !list.is_genuinely_lazy()
                    && seen.insert((3, crate::gc::Gc::as_ptr(&list) as *const () as usize))
                {
                    let list = crate::gc::Gc::clone(&list);
                    for item in self.force_lazy_list_vm(&list)? {
                        self.reify_nested_map_grep_for_read_inner(&item, seen)?;
                    }
                }
            }
            // A Pair's value is an item container, so `$k => @seq.map(...)`
            // built from a variable (or by a `for` loop body) holds the
            // deferred Seq behind the Pair or a `ContainerRef` cell.
            ValueView::Pair(_, inner) => {
                self.reify_nested_map_grep_for_read_inner(inner, seen)?;
            }
            ValueView::ValuePair(key, inner) => {
                self.reify_nested_map_grep_for_read_inner(key, seen)?;
                self.reify_nested_map_grep_for_read_inner(inner, seen)?;
            }
            ValueView::Hash(h) => {
                if seen.insert((2, crate::gc::Gc::as_ptr(&*h) as *const () as usize)) {
                    let values: Vec<Value> = h.map.values().cloned().collect();
                    for item in &values {
                        self.reify_nested_map_grep_for_read_inner(item, seen)?;
                    }
                }
            }
            ValueView::ContainerRef(cell) => {
                let inner = cell.lock().ok().map(|held| held.clone());
                if let Some(inner) = inner {
                    self.reify_nested_map_grep_for_read_inner(&inner, seen)?;
                }
            }
            _ => {}
        }
        Ok(())
    }
}
