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
            _ => {}
        }
        Ok(())
    }
}
