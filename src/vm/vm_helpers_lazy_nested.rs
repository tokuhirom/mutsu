use super::*;

impl Interpreter {
    /// Reify nested map results only when a reader needs their elements.
    // Cost: O(n + c), n = nested collection elements, c = callbacks run.
    pub(crate) fn reify_nested_map_grep_for_read(
        &mut self,
        value: &Value,
    ) -> Result<(), RuntimeError> {
        let mut seen = std::collections::HashSet::new();
        self.reify_nested_map_grep_for_read_inner(value, false, &mut seen)
    }

    fn reify_nested_map_grep_for_read_inner(
        &mut self,
        value: &Value,
        nested: bool,
        seen: &mut std::collections::HashSet<(u8, usize)>,
    ) -> Result<(), RuntimeError> {
        // One native frame per nesting level: raise rather than overflow on a
        // structure nested deeper than the stack holds (#11217).
        crate::vm::vm_stack_guard::check_now()?;
        self.reify_map_grep_seq(value)?;
        match value.view() {
            ValueView::Seq(body) => {
                // A plain gather nested in a collection (ADR-11678): run it so
                // pure readers see its elements. The value itself is left to
                // its reader (`nested`).
                if nested && body.is_gather_source() {
                    let body = std::sync::Arc::clone(&body);
                    self.reify_seq_body(&body)?;
                }
                if seen.insert((0, body.identity())) {
                    for item in body.iter() {
                        self.reify_nested_map_grep_for_read_inner(item, true, seen)?;
                    }
                }
            }
            ValueView::Array(items, _) => {
                if seen.insert((1, crate::gc::Gc::as_ptr(&items) as usize)) {
                    // Clone the elements under the stripe (a leaf: no user code),
                    // then recurse with nothing held (#12631).
                    let elems: Vec<Value> = {
                        let _guard =
                            crate::value::container_lock::ContainerStructGuard::acquire_for(
                                None, value,
                            );
                        items.iter().cloned().collect()
                    };
                    for item in &elems {
                        self.reify_nested_map_grep_for_read_inner(item, true, seen)?;
                    }
                }
            }
            ValueView::Scalar(inner) => {
                self.reify_nested_map_grep_for_read_inner(inner, nested, seen)?;
            }
            // A Pair's value is an item container, so `$k => @seq.map(...)`
            // built from a variable (or by a `for` loop body) holds the
            // deferred Seq behind the Pair or a `ContainerRef` cell.
            ValueView::Pair(_, inner) => {
                self.reify_nested_map_grep_for_read_inner(inner, true, seen)?;
            }
            ValueView::ValuePair(key, inner) => {
                self.reify_nested_map_grep_for_read_inner(key, true, seen)?;
                self.reify_nested_map_grep_for_read_inner(inner, true, seen)?;
            }
            ValueView::Hash(h) => {
                if seen.insert((2, crate::gc::Gc::as_ptr(&*h) as *const () as usize)) {
                    // Under the container stripe: another thread may be inserting
                    // into a shared hash (#12631).
                    let values: Vec<Value> = {
                        let _guard =
                            crate::value::container_lock::ContainerStructGuard::acquire_for(
                                None, value,
                            );
                        h.map.values().cloned().collect()
                    };
                    for item in &values {
                        self.reify_nested_map_grep_for_read_inner(item, true, seen)?;
                    }
                }
            }
            // A cell can close a cycle through a Pair value (`$a = 1 => $a`),
            // so it is visited once like the other containers.
            ValueView::ContainerRef(cell)
                if seen.insert((4, crate::gc::Gc::as_ptr(&cell) as *const () as usize)) =>
            {
                let inner = cell.lock().ok().map(|held| held.clone());
                if let Some(inner) = inner {
                    self.reify_nested_map_grep_for_read_inner(&inner, nested, seen)?;
                }
            }
            _ => {}
        }
        Ok(())
    }
}
