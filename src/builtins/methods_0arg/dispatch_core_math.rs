/// Miscellaneous methods: tclc, wordcase, succ, pred, Rat, FatRat, tree, encode,
/// sink, item, race/hyper, NFC/NFD/NFKC/NFKD. (The transcendental math methods
/// are rows of `builtins::method_table::scalars::math`.)
use crate::value::{RuntimeError, Value, ValueView, make_rat};

use crate::builtins::method_table::str_numeric::{fat_rat_of_str, rat_of_str};

/// Levenshtein edit distance between two strings (by Unicode scalar), used for
/// the numeric value of a `StrDistance`.
fn levenshtein(a: &str, b: &str) -> usize {
    let a: Vec<char> = a.chars().collect();
    let b: Vec<char> = b.chars().collect();
    let mut prev: Vec<usize> = (0..=b.len()).collect();
    let mut cur = vec![0usize; b.len() + 1];
    for (i, ca) in a.iter().enumerate() {
        cur[0] = i + 1;
        for (j, cb) in b.iter().enumerate() {
            let cost = if ca == cb { 0 } else { 1 };
            cur[j + 1] = (prev[j + 1] + 1).min(cur[j] + 1).min(prev[j] + cost);
        }
        std::mem::swap(&mut prev, &mut cur);
    }
    prev[b.len()]
}

/// Real (numeric) value of a "Cool" builtin instance, used by the `.Rat`/`.FatRat`
/// coercers: an `IO::Path` numifies via its path string, a `Match` via its
/// matched substring, and a `StrDistance` via the edit distance between its
/// `before`/`after` strings.
pub(super) fn cool_instance_numeric(target: &Value) -> Option<f64> {
    let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
    else {
        return None;
    };
    // A grammar cursor is a Match (raku: `Grammar` IS a `Match` subclass) but
    // reports the grammar's own class name, so route it by shape, not name.
    let owner = if target.is_match_instance() {
        "Match".to_string()
    } else {
        class_name.resolve()
    };
    match owner.as_str() {
        "IO::Path" => target.to_string_value().trim().parse::<f64>().ok(),
        "Match" => target
            .match_str_value()
            .and_then(|v| v.to_string_value().trim().parse::<f64>().ok()),
        "StrDistance" => {
            let before = attributes
                .as_map()
                .get("before")
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            let after = attributes
                .as_map()
                .get("after")
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            Some(levenshtein(&before, &after) as f64)
        }
        _ => None,
    }
}

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    match method {
        // Cost: O(1) for a numeric invocant; O(n) for a Str, n = chars (the string
        // is rebuilt around the incremented segment).
        "succ" => Some(match target.view() {
            ValueView::Enum { .. } | ValueView::Instance { .. } => None,
            _ => Some(crate::builtins::method_table::succ_pred::succ(target, &[])),
        }),
        // Cost: O(1) for a numeric invocant; O(n) for a Str, n = chars (the string
        // is rebuilt around the decremented segment).
        "pred" => Some(match target.view() {
            ValueView::Enum { .. } | ValueView::Instance { .. } => None,
            _ => Some(crate::builtins::method_table::succ_pred::pred(target, &[])),
        }),
        // A number: the numeric types' `Rat` rows' implementation
        // (`method_table::numeric_coerce`).
        "Rat" if let Some(rat) = crate::builtins::method_table::numeric_coerce::rat_of(target) => {
            Some(Some(Ok(rat)))
        }
        "Rat" => Some(match target.view() {
            // The `Cool.Rat` row's implementation (`method_table::str_numeric`); a
            // string that numifies to a `Complex` (`"1+2i".Rat` is `Complex.Rat`)
            // is the interpreter's.
            ValueView::Str(s) => rat_of_str(&s).map(Ok),
            // A Complex is Real when its imaginary part is `≅ 0`; the runtime
            // reads `$*TOLERANCE` (`Interpreter::dispatch_complex_to_real`).
            ValueView::Complex(..) => None,
            ValueView::Array(items, ..) => Some(Ok(make_rat(items.len() as i64, 1))),
            ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
                Some(Ok(make_rat(items.len() as i64, 1)))
            }
            ValueView::Hash(map) => Some(Ok(make_rat(map.len() as i64, 1))),
            ValueView::Range(a, b) | ValueView::RangeExclStart(a, b) => {
                let n = if b >= a { b - a + 1 } else { 0 };
                Some(Ok(make_rat(n, 1)))
            }
            ValueView::RangeExcl(a, b) | ValueView::RangeExclBoth(a, b) => {
                let n = if b > a { b - a } else { 0 };
                Some(Ok(make_rat(n, 1)))
            }
            ValueView::GenericRange {
                start,
                end,
                excl_end,
                ..
            } => {
                let s = start.to_f64();
                let e = end.to_f64();
                if s.is_finite() && e.is_finite() {
                    let mut count = (e - s).floor() as i64 + 1;
                    if excl_end {
                        count -= 1;
                    }
                    if count < 0 {
                        count = 0;
                    }
                    Some(Ok(make_rat(count, 1)))
                } else {
                    Some(Ok(make_rat(0, 1)))
                }
            }
            // `Instant` and `Duration` are the rows' (`method_table::instances::instant`).
            ValueView::Instance { class_name, .. }
                if class_name == "Instant" || class_name == "Duration" =>
            {
                None
            }
            ValueView::Instance { .. } => cool_instance_numeric(target).map(|n| {
                if n.fract() == 0.0 && n.is_finite() {
                    Ok(make_rat(n as i64, 1))
                } else if n.is_finite() {
                    Ok(crate::builtins::num_to_rat_with_epsilon(n, 1e-6))
                } else {
                    Ok(make_rat(0, 1))
                }
            }),
            ValueView::Package(_) => Some(Ok(make_rat(0, 1))),
            _ => {
                let n = target.to_f64();
                if n.fract() == 0.0 && n.is_finite() {
                    Some(Ok(make_rat(n as i64, 1)))
                } else if n.is_finite() {
                    Some(Ok(crate::builtins::num_to_rat_with_epsilon(n, 1e-6)))
                } else {
                    Some(Ok(make_rat(0, 1)))
                }
            }
        }),
        // A number: the numeric types' `FatRat` rows' implementation
        // (`method_table::numeric_coerce`).
        "FatRat"
            if let Some(fat) =
                crate::builtins::method_table::numeric_coerce::fat_rat_of(target) =>
        {
            Some(Some(Ok(fat)))
        }
        "FatRat" => Some(match target.view() {
            // The `Str.FatRat` conversion (`method_table::str_numeric`); a string that
            // numifies to a `Complex` is the interpreter's, as for `.Rat`.
            ValueView::Str(s) => fat_rat_of_str(&s).map(Ok),
            // As `.Rat`: the runtime checks `$*TOLERANCE`.
            ValueView::Complex(..) => None,
            ValueView::Array(items, ..) => Some(Ok(Value::fat_rat_raw(items.len() as i64, 1))),
            ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
                Some(Ok(Value::fat_rat_raw(items.len() as i64, 1)))
            }
            ValueView::Hash(map) => Some(Ok(Value::fat_rat_raw(map.len() as i64, 1))),
            ValueView::Range(a, b) | ValueView::RangeExclStart(a, b) => {
                let n = if b >= a { b - a + 1 } else { 0 };
                Some(Ok(Value::fat_rat_raw(n, 1)))
            }
            ValueView::RangeExcl(a, b) | ValueView::RangeExclBoth(a, b) => {
                let n = if b > a { b - a } else { 0 };
                Some(Ok(Value::fat_rat_raw(n, 1)))
            }
            ValueView::GenericRange {
                start,
                end,
                excl_end,
                ..
            } => {
                let s = start.to_f64();
                let e = end.to_f64();
                if s.is_finite() && e.is_finite() {
                    let mut count = (e - s).floor() as i64 + 1;
                    if excl_end {
                        count -= 1;
                    }
                    if count < 0 {
                        count = 0;
                    }
                    Some(Ok(Value::fat_rat_raw(count, 1)))
                } else {
                    Some(Ok(Value::fat_rat_raw(0, 1)))
                }
            }
            ValueView::Package(_) => Some(Ok(Value::fat_rat_raw(0, 1))),
            // IO::Path/Match/StrDistance numify via their Cool string/distance.
            // `Instant` and `Duration` are the rows' (`method_table::instances::instant`).
            ValueView::Instance { class_name, .. }
                if class_name == "Instant" || class_name == "Duration" =>
            {
                None
            }
            ValueView::Instance { .. } if cool_instance_numeric(target).is_some() => {
                let n = cool_instance_numeric(target).unwrap_or(0.0);
                if n.fract() == 0.0 && n.is_finite() {
                    Some(Ok(Value::fat_rat_raw(n as i64, 1)))
                } else if n.is_finite() {
                    match crate::builtins::num_to_rat_with_epsilon(n, 1e-6).view() {
                        ValueView::Rat(n, d) => Some(Ok(Value::fat_rat_raw(n, d))),
                        _ => Some(Ok(Value::fat_rat_raw(0, 1))),
                    }
                } else {
                    Some(Ok(Value::fat_rat_raw(0, 1)))
                }
            }
            _ => {
                let n = target.to_f64();
                if n.fract() == 0.0 && n.is_finite() {
                    Some(Ok(Value::fat_rat_raw(n as i64, 1)))
                } else if n.is_finite() {
                    let rat = crate::builtins::num_to_rat_with_epsilon(n, 1e-6);
                    match rat.view() {
                        ValueView::Rat(n, d) => Some(Ok(Value::fat_rat_raw(n, d))),
                        _ => Some(Ok(Value::fat_rat_raw(0, 1))),
                    }
                } else {
                    Some(Ok(Value::fat_rat_raw(0, 1)))
                }
            }
        }),
        "tree" => Some(Some(Ok(crate::value::flat::tree_to_depth(
            target,
            usize::MAX,
        )))),
        "sink" => {
            crate::builtins::method_table::list::sink(target, &[])
                .map(Some)
                .or_else(|| {
                    Some(match target.view() {
                        ValueView::Instance {
                            class_name,
                            attributes,
                            ..
                        } if class_name == "Failure" => {
                            // Sinking a *handled* Failure is a no-op; only an unhandled
                            // Failure throws its exception when sunk.
                            if target.is_failure_handled() {
                                Some(Ok(Value::NIL))
                            } else if let Some(ex) = attributes.as_map().get("exception") {
                                let mut err = RuntimeError::new(ex.to_string_value());
                                err.exception = Some(Box::new(ex.clone()));
                                Some(Err(err))
                            } else {
                                Some(Ok(Value::NIL))
                            }
                        }
                        ValueView::Seq(body) => {
                            // If there's a deferred source and `.cache` was not requested,
                            // fall through to the runtime, which can actually pull it.
                            if body.has_deferred_source() && !body.is_cached() {
                                return Some(None); // fall through to runtime
                            }
                            // No pull to run here (already reified/cache-requested/taken):
                            // `sink`'s pull closure is unreachable on this branch.
                            let _ = body.sink_explicit(|_| {
                    unreachable!("sink() only pulls a deferred, non-cached source, excluded above")
                });
                            Some(Ok(Value::NIL))
                        }
                        // The runtime owns LazyList forcing. In particular, a gather body
                        // may run compiled code and must run before `.sink` returns.
                        ValueView::LazyList(_) => None,
                        _ => Some(Ok(Value::NIL)),
                    })
                })
        }
        // The `item` rows' implementation (`method_table::lazy`).
        "item" => Some(crate::builtins::method_table::lazy::item(target, &[])),
        // The `race`/`hyper` rows' implementation (`method_table::lazy`).
        "race" => Some(crate::builtins::method_table::lazy::race(target, &[])),
        "hyper" => Some(crate::builtins::method_table::lazy::hyper(target, &[])),
        _ => None,
    }
}
