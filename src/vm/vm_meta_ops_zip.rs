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
    IntRange {
        start: i64,
        count: usize,
        infinite: bool,
    },
    IntRangeExcl {
        start: i64,
        count: usize,
        infinite: bool,
    },
    List(Vec<Value>),
    ExtendedList {
        items: Vec<Value>,
        fill: Value,
    },
    Lazy(Vec<Value>),
}

impl ZipIter {
    /// True only for a Range whose upper endpoint is unbounded (`1..*` and
    /// friends), matching the infinity test the `X` meta-op already uses.
    /// A merely *large* finite range (millions of elements) is not lazy.
    fn range_is_infinite(b: i64) -> bool {
        b == i64::MAX
    }

    pub(super) fn from_value(val: &Value) -> Self {
        match val.view() {
            ValueView::Range(a, b) => {
                let infinite = Self::range_is_infinite(b);
                let count = if infinite {
                    MAX_ZIP_EXPAND
                } else if b >= a {
                    b.saturating_sub(a)
                        .saturating_add(1)
                        .try_into()
                        .unwrap_or(usize::MAX)
                } else {
                    0
                };
                Self::IntRange {
                    start: a,
                    count,
                    infinite,
                }
            }
            ValueView::RangeExcl(a, b) => {
                let infinite = Self::range_is_infinite(b);
                let count = if infinite {
                    MAX_ZIP_EXPAND
                } else if b > a {
                    (b - a) as usize
                } else {
                    0
                };
                Self::IntRangeExcl {
                    start: a,
                    count,
                    infinite,
                }
            }
            ValueView::RangeExclStart(a, b) => {
                let start = a.saturating_add(1);
                let infinite = Self::range_is_infinite(b);
                let count = if infinite {
                    MAX_ZIP_EXPAND
                } else if b >= start {
                    b.saturating_sub(start)
                        .saturating_add(1)
                        .try_into()
                        .unwrap_or(usize::MAX)
                } else {
                    0
                };
                Self::IntRange {
                    start,
                    count,
                    infinite,
                }
            }
            ValueView::RangeExclBoth(a, b) => {
                let start = a.saturating_add(1);
                let infinite = Self::range_is_infinite(b);
                let count = if infinite {
                    MAX_ZIP_EXPAND
                } else if b > start {
                    b.saturating_sub(start).try_into().unwrap_or(usize::MAX)
                } else {
                    0
                };
                Self::IntRangeExcl {
                    start,
                    count,
                    infinite,
                }
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
            Self::IntRange { infinite, .. } | Self::IntRangeExcl { infinite, .. } => *infinite,
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

    /// Build a bounded iterator for a genuinely infinite Range (`1..*` and
    /// its exclusive/both-excl variants), honoring `needed` — typically the
    /// other zip operand's real length — instead of the coarse
    /// `MAX_ZIP_EXPAND` probe cap that `from_value` falls back to when it has
    /// no such hint. Returns `None` for anything else (finite ranges and
    /// non-Range values already report their true length via `from_value`),
    /// so the caller keeps using that.
    pub(super) fn from_infinite_range(val: &Value, needed: usize) -> Option<Self> {
        match val.view() {
            ValueView::Range(a, b) if Self::range_is_infinite(b) => Some(Self::IntRange {
                start: a,
                count: needed,
                infinite: true,
            }),
            ValueView::RangeExclStart(a, b) if Self::range_is_infinite(b) => Some(Self::IntRange {
                start: a.saturating_add(1),
                count: needed,
                infinite: true,
            }),
            ValueView::RangeExcl(a, b) if Self::range_is_infinite(b) => Some(Self::IntRangeExcl {
                start: a,
                count: needed,
                infinite: true,
            }),
            ValueView::RangeExclBoth(a, b) if Self::range_is_infinite(b) => {
                Some(Self::IntRangeExcl {
                    start: a.saturating_add(1),
                    count: needed,
                    infinite: true,
                })
            }
            _ => None,
        }
    }
}
