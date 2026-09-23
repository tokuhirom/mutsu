//! How a quantifier treats an iteration whose atom matched zero-width.
//!
//! Rakudo has no zero-width guard at all: every iteration counts, so a counted
//! quantifier over a zero-width atom (`<?before x>**2`) repeats it at the same
//! position until its maximum and succeeds -- `**3..5` records five
//! `<before>` captures -- while an unbounded one (`<?before x>+`) never
//! terminates. mutsu keeps a guard for the unbounded case, but it must not
//! leave a count short of its minimum: before #9180 every loop broke on the
//! first zero-width iteration WITHOUT counting it, so `<?before x>**2` (and
//! even `**1`) failed.

/// Whether an iteration that matched zero-width still counts toward the
/// quantifier, given `count` iterations already accepted.
///
/// A bounded quantifier counts it up to its maximum, as Rakudo does. An
/// unbounded one counts it only while `count` is below the minimum -- enough to
/// satisfy `+`, and the point where Rakudo would instead loop forever.
pub(super) fn zero_width_iter_counts(count: usize, min: usize, max: Option<usize>) -> bool {
    match max {
        Some(max) => count < max,
        None => count < min,
    }
}

/// The count a position-only scan reaches once an atom has matched zero-width
/// after `count` accepted iterations: every further iteration would match the
/// same empty string at the same position, so the scan jumps straight to the
/// last count [`zero_width_iter_counts`] admits.
pub(super) fn zero_width_saturated_count(count: usize, min: usize, max: Option<usize>) -> usize {
    match max {
        Some(max) => count.max(max),
        None => count.max(min),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bounded_counts_up_to_max() {
        assert!(zero_width_iter_counts(0, 2, Some(2)));
        assert!(zero_width_iter_counts(1, 2, Some(2)));
        assert!(!zero_width_iter_counts(2, 2, Some(2)));
        assert_eq!(zero_width_saturated_count(0, 3, Some(5)), 5);
    }

    #[test]
    fn unbounded_counts_only_to_min() {
        assert!(zero_width_iter_counts(0, 1, None));
        assert!(!zero_width_iter_counts(1, 1, None));
        assert!(!zero_width_iter_counts(0, 0, None));
        assert_eq!(zero_width_saturated_count(0, 1, None), 1);
        assert_eq!(zero_width_saturated_count(3, 1, None), 3);
    }
}
