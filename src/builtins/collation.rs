//! Unicode collation support for Raku's `coll` operator and `.collate` method.
//!
//! Uses ICU4X for proper Unicode Collation Algorithm (UCA) support.
//! The collation respects `$*COLLATION` settings for primary, secondary,
//! tertiary, and quaternary levels.

use icu_collator::options::{CollatorOptions, Strength};
use icu_collator::{Collator, CollatorPreferences};

use crate::runtime::utils::make_order;
use crate::value::Value;

/// Collation settings from `$*COLLATION`.
/// Each level is -1 (reversed), 0 (disabled), or 1 (normal).
#[derive(Debug, Clone)]
pub struct CollationSettings {
    pub primary: i64,
    pub secondary: i64,
    pub tertiary: i64,
    pub quaternary: i64,
}

impl Default for CollationSettings {
    fn default() -> Self {
        Self {
            primary: 1,
            secondary: 1,
            tertiary: 1,
            quaternary: 1,
        }
    }
}

impl CollationSettings {
    /// Extract collation settings from a `$*COLLATION` Value (Instance).
    pub fn from_value(val: &Value) -> Self {
        match val {
            Value::Instance { attributes, .. } => {
                let get_i64 = |name: &str| -> i64 {
                    attributes
                        .get(name)
                        .map(|v| match v {
                            Value::Int(n) => *n,
                            Value::Bool(b) => {
                                if *b {
                                    1
                                } else {
                                    0
                                }
                            }
                            _ => crate::runtime::utils::to_int(v),
                        })
                        .unwrap_or(1)
                };
                Self {
                    primary: get_i64("primary"),
                    secondary: get_i64("secondary"),
                    tertiary: get_i64("tertiary"),
                    quaternary: get_i64("quaternary"),
                }
            }
            _ => Self::default(),
        }
    }

    /// Compute the collation-level numeric encoding used in Raku's Collation.gist.
    /// Each level maps to 2 bits: 0->00, 1->01, -1->10.
    /// Primary occupies bits 0-1, secondary bits 2-3, tertiary bits 4-5, quaternary bits 6-7.
    pub fn collation_level(&self) -> i64 {
        fn encode(v: i64) -> i64 {
            match v {
                0 => 0,
                1 => 1,
                -1 => 2,
                _ => {
                    let clamped = v.clamp(-1, 1);
                    match clamped {
                        0 => 0,
                        1 => 1,
                        _ => 2,
                    }
                }
            }
        }
        encode(self.primary)
            + encode(self.secondary) * 4
            + encode(self.tertiary) * 16
            + encode(self.quaternary) * 64
    }
}

/// Compare two strings using the full Unicode Collation Algorithm at the
/// specified strength, returning a `std::cmp::Ordering`.
fn icu_compare_at_strength(left: &str, right: &str, strength: Strength) -> std::cmp::Ordering {
    let mut opts = CollatorOptions::default();
    opts.strength = Some(strength);
    let coll = Collator::try_new(CollatorPreferences::default(), opts)
        .expect("ICU collator should be available");
    coll.compare(left, right)
}

/// Compare two strings using Unicode collation, respecting the given settings.
/// Returns an Order value (Less, Same, More).
pub fn coll_compare(left: &str, right: &str, settings: &CollationSettings) -> Value {
    let ord = coll_ordering(left, right, settings);
    make_order(ord)
}

/// Compare two strings using Unicode collation, respecting the given settings.
/// Returns a `std::cmp::Ordering`.
///
/// Raku's collation has 4 levels:
/// - Primary: base character (ICU Primary)
/// - Secondary: accent/diacritic (ICU Secondary)
/// - Tertiary: case (ICU Tertiary)
/// - Quaternary: codepoint comparison (NOT ICU Quaternary; uses raw codepoint order)
pub fn coll_ordering(left: &str, right: &str, settings: &CollationSettings) -> std::cmp::Ordering {
    // Compare at each ICU strength level to isolate per-level differences.
    let at_primary = icu_compare_at_strength(left, right, Strength::Primary);
    let at_secondary = icu_compare_at_strength(left, right, Strength::Secondary);
    let at_tertiary = icu_compare_at_strength(left, right, Strength::Tertiary);

    // Extract per-level deltas
    let delta_primary = at_primary;
    let delta_secondary = if at_primary == std::cmp::Ordering::Equal {
        at_secondary
    } else {
        std::cmp::Ordering::Equal
    };
    let delta_tertiary = if at_secondary == std::cmp::Ordering::Equal {
        at_tertiary
    } else {
        std::cmp::Ordering::Equal
    };
    // Raku's quaternary level uses codepoint comparison
    let delta_quaternary = left.cmp(right);

    // Apply direction settings (0 = disabled, 1 = normal, -1 = reversed)
    let levels = [
        (delta_primary, settings.primary),
        (delta_secondary, settings.secondary),
        (delta_tertiary, settings.tertiary),
        (delta_quaternary, settings.quaternary),
    ];

    for (delta, direction) in levels {
        let effective = match direction {
            0 => std::cmp::Ordering::Equal,
            -1 => delta.reverse(),
            _ => delta, // 1 or any positive
        };
        if effective != std::cmp::Ordering::Equal {
            return effective;
        }
    }

    std::cmp::Ordering::Equal
}

/// Sort values using Unicode collation, respecting the given settings.
pub fn collate_sort(values: Vec<Value>, settings: &CollationSettings) -> Vec<Value> {
    let mut keyed: Vec<(String, Value)> = values
        .into_iter()
        .map(|v| {
            let s = v.to_string_value();
            (s, v)
        })
        .collect();

    // Use default collator for simple case
    if settings.primary == 1
        && settings.secondary == 1
        && settings.tertiary == 1
        && settings.quaternary == 1
    {
        let coll = Collator::try_new(CollatorPreferences::default(), Default::default())
            .expect("ICU collator should be available");
        keyed.sort_by(|a, b| coll.compare(&a.0, &b.0));
    } else {
        keyed.sort_by(|a, b| coll_ordering(&a.0, &b.0, settings));
    }

    keyed.into_iter().map(|(_, v)| v).collect()
}
