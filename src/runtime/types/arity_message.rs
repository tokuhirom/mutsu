//! The arity wording of rakudo's "Too few/many positionals passed" errors.
//!
//! A signature with optional positionals states a range, a slurpy one a lower
//! bound, measured on rakudo 2026.09:
//!
//! ```text
//! sub f($x, $y)            f()     expected 2 arguments but got 0
//! sub f($x)                f()     expected 1 argument but got 0
//! sub f($x, $y?)           f(1,2,3) expected 1 or 2 arguments but got 3
//! sub f($x, $y?, $z?, $w?) f()     expected 1 to 4 arguments but got 0
//! sub f($x, *@r)           f()     expected at least 1 arguments but got only 0
//! ```

/// `expected ...` clause for a signature taking `min` required and, when a
/// slurpy does not make it unbounded, at most `max` positional arguments.
// Cost: O(1).
pub(crate) fn expected_arguments(min: usize, max: Option<usize>) -> String {
    match max {
        None => format!("at least {min} arguments"),
        Some(max) if max == min => format!("{min} argument{}", if min == 1 { "" } else { "s" }),
        Some(max) if max == min + 1 => format!("{min} or {max} arguments"),
        Some(max) => format!("{min} to {max} arguments"),
    }
}

/// The whole "Too few positionals passed" message.
// Cost: O(1).
pub(crate) fn too_few_positionals(min: usize, max: Option<usize>, got: usize) -> String {
    format!(
        "Too few positionals passed; expected {} but got {}{got}",
        expected_arguments(min, max),
        if max.is_none() { "only " } else { "" },
    )
}

/// The whole "Too many positionals passed" message (a slurpy never gets here).
// Cost: O(1).
pub(crate) fn too_many_positionals(min: usize, max: usize, got: usize) -> String {
    format!(
        "Too many positionals passed; expected {} but got {got}",
        expected_arguments(min, Some(max)),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wording_follows_the_arity_shape() {
        assert_eq!(too_few_positionals(1, Some(1), 0), "Too few positionals passed; expected 1 argument but got 0");
        assert_eq!(too_few_positionals(2, Some(2), 1), "Too few positionals passed; expected 2 arguments but got 1");
        assert_eq!(too_few_positionals(1, Some(4), 0), "Too few positionals passed; expected 1 to 4 arguments but got 0");
        assert_eq!(too_few_positionals(1, None, 0), "Too few positionals passed; expected at least 1 arguments but got only 0");
        assert_eq!(too_many_positionals(1, 2, 3), "Too many positionals passed; expected 1 or 2 arguments but got 3");
        assert_eq!(too_many_positionals(0, 1, 3), "Too many positionals passed; expected 0 or 1 arguments but got 3");
    }
}
