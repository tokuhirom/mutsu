use crate::value::{Value, ValueView};

/// Materialize the Cartesian product of finite operand columns. A reduction
/// fold extends an existing tuple in its first column; a direct `X` call
/// starts a new tuple and preserves every itemized List as one element.
///
/// Cost: O(p * n), p = product of column lengths, n = number of columns.
pub(crate) fn cross_product_rows(columns: &[Vec<Value>], extend_first_tuple: bool) -> Vec<Value> {
    if columns.is_empty() || columns.iter().any(Vec::is_empty) {
        return Vec::new();
    }
    let mut indices = vec![0; columns.len()];
    let mut rows = Vec::new();
    loop {
        let first = &columns[0][indices[0]];
        let mut tuple = match first.view() {
            ValueView::Array(items, kind) if extend_first_tuple && !kind.is_itemized() => {
                items.to_vec()
            }
            _ => vec![first.clone()],
        };
        tuple.extend(
            columns
                .iter()
                .enumerate()
                .skip(1)
                .map(|(column, items)| items[indices[column]].clone()),
        );
        rows.push(Value::array(tuple));

        let mut column = columns.len();
        loop {
            if column == 0 {
                return rows;
            }
            column -= 1;
            indices[column] += 1;
            if indices[column] < columns[column].len() {
                break;
            }
            indices[column] = 0;
        }
    }
}
