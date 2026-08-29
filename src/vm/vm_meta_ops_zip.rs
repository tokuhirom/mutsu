use super::*;

/// Maximum elements to produce from Z (zip) when iterating over ranges.
/// This caps the output for infinite ranges (e.g., `1..* Z** 1..*`).
/// Kept small because the meta-operator (e.g. `**`) may be expensive
/// for large values. The caller (e.g., `.[^5]`) will further limit.
// TODO: Ideally Z should return a lazy Seq and only compute elements on demand.
pub(super) const MAX_ZIP_EXPAND: usize = 1_000;

/// Helper for lazy index-based iteration over values in Z (zip) operations.
/// Avoids materializing huge ranges like `1..*` into million-element Vecs.
pub(super) enum ZipIter {
    IntRange { start: i64, count: usize },
    IntRangeExcl { start: i64, count: usize },
    List(Vec<Value>),
    ExtendedList { items: Vec<Value>, fill: Value },
    Lazy(Vec<Value>),
}

impl ZipIter {
    pub(super) fn from_value(val: &Value) -> Self {
        match val.view() {
            ValueView::Range(a, b) => {
                let count = if b >= a {
                    b.saturating_sub(a)
                        .saturating_add(1)
                        .try_into()
                        .unwrap_or(usize::MAX)
                        .min(MAX_ZIP_EXPAND)
                } else {
                    0
                };
                Self::IntRange { start: a, count }
            }
            ValueView::RangeExcl(a, b) => {
                let count = if b > a {
                    ((b - a) as usize).min(MAX_ZIP_EXPAND)
                } else {
                    0
                };
                Self::IntRangeExcl { start: a, count }
            }
            ValueView::RangeExclStart(a, b) => {
                let start = a.saturating_add(1);
                let count = if b >= start {
                    b.saturating_sub(start)
                        .saturating_add(1)
                        .try_into()
                        .unwrap_or(usize::MAX)
                        .min(MAX_ZIP_EXPAND)
                } else {
                    0
                };
                Self::IntRange { start, count }
            }
            ValueView::RangeExclBoth(a, b) => {
                let start = a.saturating_add(1);
                let count = if b > start {
                    b.saturating_sub(start)
                        .try_into()
                        .unwrap_or(usize::MAX)
                        .min(MAX_ZIP_EXPAND)
                } else {
                    0
                };
                Self::IntRangeExcl { start, count }
            }
            // Nil in zip context is a 1-element list (not empty), matching Raku behavior
            // where `Nil Z+ 2` yields `(2)` (Nil coerces to 0).
            ValueView::Nil => Self::List(vec![Value::NIL]),
            ValueView::LazyList(_) => {
                let list = runtime::value_to_list(val);
                let len = list.len().min(MAX_ZIP_EXPAND);
                Self::Lazy(list[..len].to_vec())
            }
            _ => {
                let list = runtime::value_to_list(val);
                // Check for trailing Whatever (`*`) — extends the list by
                // repeating the last real element.
                if list.len() >= 2
                    && matches!(list.last().map(Value::view), Some(ValueView::Whatever))
                {
                    let items: Vec<Value> = list[..list.len() - 1].to_vec();
                    let fill = items.last().cloned().unwrap_or(Value::NIL);
                    Self::ExtendedList { items, fill }
                } else {
                    Self::List(list)
                }
            }
        }
    }

    pub(super) fn is_lazy(&self) -> bool {
        match self {
            Self::IntRange { count, .. } | Self::IntRangeExcl { count, .. } => {
                *count >= MAX_ZIP_EXPAND
            }
            Self::ExtendedList { .. } | Self::Lazy(_) => true,
            Self::List(_) => false,
        }
    }

    pub(super) fn len(&self) -> usize {
        match self {
            Self::IntRange { count, .. } | Self::IntRangeExcl { count, .. } => *count,
            Self::List(v) | Self::Lazy(v) => v.len(),
            Self::ExtendedList { .. } => usize::MAX,
        }
    }

    pub(super) fn nth(&self, i: usize) -> Value {
        match self {
            Self::IntRange { start, .. } | Self::IntRangeExcl { start, .. } => {
                Value::int(*start + i as i64)
            }
            Self::List(v) | Self::Lazy(v) => v[i].clone(),
            Self::ExtendedList { items, fill } => {
                items.get(i).cloned().unwrap_or_else(|| fill.clone())
            }
        }
    }
}
