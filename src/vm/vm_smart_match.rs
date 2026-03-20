use std::path::PathBuf;

use crate::runtime::Interpreter;
use crate::value::Value;
use crate::vm::VM;

/// Normalize an IO::Path for ACCEPTS comparison: cleanup + absolute.
/// Returns the cleaned-up absolute path string.
fn io_path_cleanup_absolute(path: &str, cwd: &str) -> String {
    let cleaned = crate::runtime::Interpreter::cleanup_io_path_lexical(path);
    if std::path::Path::new(&cleaned).is_absolute() {
        cleaned
    } else {
        let abs = PathBuf::from(cwd).join(&cleaned);
        crate::runtime::Interpreter::cleanup_io_path_lexical(abs.to_string_lossy().as_ref())
    }
}

/// Extract path and CWD from IO::Path attributes.
fn io_path_attrs(attrs: &std::sync::Arc<crate::value::InstanceAttrs>) -> (String, String) {
    let path = attrs
        .get("path")
        .map(|v: &Value| v.to_string_value())
        .unwrap_or_default();
    let cwd = attrs
        .get("cwd")
        .map(|v: &Value| v.to_string_value())
        .unwrap_or_else(|| {
            std::env::current_dir()
                .map(|p| p.to_string_lossy().to_string())
                .unwrap_or_else(|_| ".".to_string())
        });
    (path, cwd)
}

/// Check if a value requires interpreter-based smart matching when used as LHS.
/// These types have their own dispatch (junction threading, callable invocation,
/// regex matching, type hierarchy checks, etc.).
fn needs_interpreter_lhs(v: &Value) -> bool {
    matches!(
        v,
        Value::Junction { .. }
            | Value::Sub(_)
            | Value::Regex(_)
            | Value::RegexWithAdverbs { .. }
            | Value::Routine { .. }
            | Value::Package(_)
            | Value::CustomType { .. }
            | Value::ParametricRole { .. }
    )
}

/// Try to evaluate a smart match between `left` and `right` without needing the
/// interpreter.  Returns `Some(bool)` for type pairs that can be resolved purely
/// from the values themselves, or `None` when the interpreter is required (regex
/// matching, callable invocation, type-object checks with class registries, etc.).
pub(crate) fn pure_smart_match(left: &Value, right: &Value) -> Option<bool> {
    match (left, right) {
        // Whatever on RHS always matches
        (_, Value::Whatever) => Some(true),

        // DateTime ~~ Date: compare date parts
        (
            Value::Instance {
                class_name: left_class,
                attributes: left_attrs,
                ..
            },
            Value::Instance {
                class_name: right_class,
                attributes: right_attrs,
                ..
            },
        ) if left_class == "DateTime" && right_class == "Date" => {
            let (y, m, d, _, _, _, _) =
                crate::builtins::methods_0arg::temporal::datetime_attrs(left_attrs);
            let (ry, rm, rd) = crate::builtins::methods_0arg::temporal::date_attrs(right_attrs);
            Some(y == ry && m == rm && d == rd)
        }

        // Version ~~ Version
        (Value::Version { .. }, Value::Version { parts, plus, minus }) => {
            Some(Interpreter::version_smart_match(left, parts, *plus, *minus))
        }

        // When RHS is NaN, check if LHS is also NaN
        (_, Value::Num(b)) if b.is_nan() && !needs_interpreter_lhs(left) => {
            Some(Interpreter::value_is_nan(left))
        }
        (Value::Num(a), _) if a.is_nan() && !needs_interpreter_lhs(right) => {
            Some(Interpreter::value_is_nan(right))
        }

        // Complex comparisons (NaN-aware)
        (Value::Complex(ar, ai), Value::Complex(br, bi)) => {
            let a_nan = ar.is_nan() || ai.is_nan();
            let b_nan = br.is_nan() || bi.is_nan();
            if a_nan && b_nan {
                Some(true)
            } else if a_nan || b_nan {
                Some(false)
            } else {
                Some(ar == br && ai == bi)
            }
        }
        (Value::Int(a), Value::Complex(br, bi)) => Some((*a as f64) == *br && *bi == 0.0),
        (Value::Complex(ar, ai), Value::Int(b)) => Some(*ar == (*b as f64) && *ai == 0.0),
        (Value::Num(a), Value::Complex(br, bi)) => {
            if a.is_nan() && (br.is_nan() || bi.is_nan()) {
                Some(true)
            } else {
                Some(*a == *br && *bi == 0.0)
            }
        }
        (Value::Complex(ar, ai), Value::Num(b)) => {
            if b.is_nan() && (ar.is_nan() || ai.is_nan()) {
                Some(true)
            } else {
                Some(*ar == *b && *ai == 0.0)
            }
        }
        (Value::Complex(ar, ai), Value::Rat(n, d)) => {
            if *d != 0 {
                Some(*ar == (*n as f64 / *d as f64) && *ai == 0.0)
            } else {
                Some(false)
            }
        }
        (Value::Rat(n, d), Value::Complex(br, bi)) => {
            if *d != 0 {
                Some((*n as f64 / *d as f64) == *br && *bi == 0.0)
            } else {
                Some(false)
            }
        }

        // Numeric equality
        (Value::Int(a), Value::Int(b)) => Some(a == b),
        (Value::Num(a), Value::Num(b)) => Some(a == b),
        (Value::Int(a), Value::Num(b)) => Some((*a as f64) == *b),
        (Value::Num(a), Value::Int(b)) => Some(*a == (*b as f64)),
        (Value::Rat(an, ad), Value::Rat(bn, bd)) => Some(an * bd == bn * ad),
        (Value::Int(a), Value::Rat(n, d)) => Some(*a * d == *n),
        (Value::Rat(n, d), Value::Int(b)) => Some(*n == *b * d),

        // String equality
        (Value::Str(a), Value::Str(b)) => Some(a == b),

        // Str ~~ Numeric: numify LHS and compare
        (Value::Str(a), Value::Int(b)) => Some(a.trim().parse::<f64>() == Ok(*b as f64)),
        (Value::Str(a), Value::Num(b)) => {
            let trimmed = a.trim().replace('\u{2212}', "-");
            Some(trimmed.parse::<f64>().is_ok_and(|v| {
                if v.is_nan() && b.is_nan() {
                    true
                } else {
                    v == *b
                }
            }))
        }
        (Value::Str(a), Value::Rat(n, d)) => {
            if *d != 0 {
                Some(
                    a.trim()
                        .parse::<f64>()
                        .is_ok_and(|v| v == *n as f64 / *d as f64),
                )
            } else {
                Some(false)
            }
        }
        (Value::Int(a), Value::Str(b)) => Some(b.trim().parse::<f64>() == Ok(*a as f64)),
        (Value::Nil, Value::Str(s)) => Some(s.is_empty()),

        // IO::Path ~~ IO::Path: compare by cleanup.absolute
        (
            Value::Instance {
                class_name: cn_a,
                attributes: attrs_a,
                ..
            },
            Value::Instance {
                class_name: cn_b,
                attributes: attrs_b,
                ..
            },
        ) if cn_a == "IO::Path" && cn_b == "IO::Path" => {
            let (path_a, cwd_a) = io_path_attrs(attrs_a);
            let (path_b, cwd_b) = io_path_attrs(attrs_b);
            Some(
                io_path_cleanup_absolute(&path_a, &cwd_a)
                    == io_path_cleanup_absolute(&path_b, &cwd_b),
            )
        }

        // Buf/Blob ~~ Buf/Blob: compare byte contents
        (
            Value::Instance {
                class_name: cn_a, ..
            },
            Value::Instance {
                class_name: cn_b, ..
            },
        ) if {
            let a = cn_a.resolve();
            let b = cn_b.resolve();
            let is_buf = |cn: &str| {
                cn == "Buf"
                    || cn == "Blob"
                    || cn == "utf8"
                    || cn == "utf16"
                    || cn.starts_with("Buf[")
                    || cn.starts_with("Blob[")
                    || cn.starts_with("buf")
                    || cn.starts_with("blob")
            };
            is_buf(&a) && is_buf(&b)
        } =>
        {
            let lb = VM::extract_buf_bytes(left);
            let rb = VM::extract_buf_bytes(right);
            Some(lb == rb)
        }

        // Instance ~~ Instance: value types use eqv, others use identity
        (
            Value::Instance {
                class_name: lc,
                id: id_a,
                ..
            },
            Value::Instance {
                class_name: rc,
                id: id_b,
                ..
            },
        ) if lc == rc
            && matches!(
                lc.resolve().as_ref(),
                "Date" | "DateTime" | "Duration" | "Instant"
            ) =>
        {
            Some(left.eqv(right))
        }

        // Mu instances: Mu ~~ Mu.new is True
        (
            Value::Package(lhs_type),
            Value::Instance {
                class_name: rhs_class,
                ..
            },
        ) if rhs_class == "Mu" => Some(lhs_type == "Mu"),

        // When LHS is a type object (Package) and RHS is not a Package/type,
        // return false (handled more completely in the interpreter for type hierarchy)
        (Value::Package(_), Value::Instance { .. }) => Some(false),

        // When RHS is a Bool, result is that Bool
        (_, Value::Bool(b))
            if !matches!(left, Value::Package(_) | Value::Instance { .. })
                && !needs_interpreter_lhs(left) =>
        {
            Some(*b)
        }

        // Hash ~~ Hash: structural equality
        (Value::Hash(lmap), Value::Hash(rmap)) => {
            if lmap.len() != rmap.len() {
                return Some(false);
            }
            for (k, lv) in lmap.iter() {
                match rmap.get(k) {
                    Some(rv) => {
                        // For nested hashes, recurse
                        if let Some(result) = pure_smart_match(lv, rv) {
                            if !result {
                                return Some(false);
                            }
                        } else {
                            // Cannot resolve without interpreter
                            return None;
                        }
                    }
                    None => return Some(false),
                }
            }
            Some(true)
        }

        // Set ~~ Bag: all set elements must exist in Bag with count 1
        (Value::Set(set), Value::Bag(bag)) => Some(
            set.len() == bag.len()
                && set
                    .iter()
                    .all(|key| bag.get(key).is_some_and(|count| *count == 1)),
        ),

        // Bag ~~ Set: all bag elements must have count 1 and exist in set
        (Value::Bag(bag), Value::Set(set)) => Some(
            bag.len() == set.len()
                && bag
                    .iter()
                    .all(|(key, count)| *count == 1 && set.contains(key)),
        ),

        // Set ~~ Mix: all set elements must exist in Mix with unit weights
        (Value::Set(set), Value::Mix(mix)) => Some(
            set.len() == mix.len()
                && set.iter().all(|key| {
                    mix.get(key)
                        .is_some_and(|weight| weight.is_finite() && *weight == 1.0)
                }),
        ),

        // Mix ~~ Set: all mix elements must have unit weights and exist in set
        (Value::Mix(mix), Value::Set(set)) => Some(
            mix.len() == set.len()
                && mix
                    .iter()
                    .all(|(key, weight)| weight.is_finite() && *weight == 1.0 && set.contains(key)),
        ),

        // Array ~~ Hash: check if any element exists as a key
        (Value::Array(items, ..), Value::Hash(map)) => Some(items.iter().any(|item| {
            let key = item.to_string_value();
            map.contains_key(&key)
        })),

        // Scalar ~~ Hash: check key existence
        // (but not for types that have more specific matches above)
        (_, Value::Hash(map))
            if !matches!(
                left,
                Value::Regex(_)
                    | Value::RegexWithAdverbs { .. }
                    | Value::Hash(_)
                    | Value::Array(..)
                    | Value::Pair(..)
            ) && !needs_interpreter_lhs(left) =>
        {
            let key = left.to_string_value();
            Some(map.contains_key(&key))
        }

        // Range ~~ Range: LHS is subset of RHS
        (l, r) if l.is_range() && r.is_range() => {
            let r_str = Interpreter::range_has_string_endpoints(r);
            let (_, _, l_es, l_ee) = Interpreter::range_exclusivity(l);
            let (_, _, r_es, r_ee) = Interpreter::range_exclusivity(r);
            if r_str {
                let (l_min_s, l_max_s) = Interpreter::range_raw_string_bounds(l);
                let (r_min_s, r_max_s) = Interpreter::range_raw_string_bounds(r);
                let min_ok = if r_es {
                    l_min_s > r_min_s || (l_min_s == r_min_s && l_es)
                } else {
                    l_min_s >= r_min_s
                };
                let max_ok = if r_ee {
                    l_max_s < r_max_s || (l_max_s == r_max_s && l_ee)
                } else {
                    l_max_s <= r_max_s
                };
                Some(min_ok && max_ok)
            } else {
                let (l_min, l_max) = Interpreter::range_raw_bounds_f64(l);
                let (r_min, r_max) = Interpreter::range_raw_bounds_f64(r);
                let min_ok = if r_es {
                    l_min > r_min || (l_min == r_min && l_es)
                } else {
                    l_min >= r_min
                        || (l_min.is_nan() && r_min.is_nan())
                        || (l_min.is_nan() && r_min.is_infinite() && r_min < 0.0)
                };
                let max_ok = if r_ee {
                    l_max < r_max || (l_max == r_max && l_ee)
                } else {
                    l_max <= r_max
                        || (l_max.is_nan() && r_max.is_nan())
                        || (l_max.is_nan() && r_max.is_infinite() && r_max > 0.0)
                };
                Some(min_ok && max_ok)
            }
        }

        // Range ~~ Numeric: numify range (element count) and compare
        (l, r) if l.is_range() && r.is_numeric() => {
            let elems = Interpreter::range_elems_f64(l);
            let rval = r.to_f64();
            Some(elems == rval)
        }

        // Value ~~ Range: containment check
        // Exclude types that have their own smartmatch handling so they fall
        // through to the interpreter.
        (l, r)
            if r.is_range()
                && !needs_interpreter_lhs(l)
                && !matches!(
                    l,
                    Value::Hash(_)
                        | Value::Array(..)
                        | Value::Seq(_)
                        | Value::Slip(_)
                        | Value::LazyList(_)
                ) =>
        {
            Some(Interpreter::value_in_range(l, r))
        }

        // IO::Path/Str ~~ Pair(:e), :d, :f, :r, :w, :x, :s, :z file tests
        (_, Value::Pair(key, val))
            if matches!(val.as_ref(), Value::Bool(true))
                && matches!(key.as_str(), "e" | "d" | "f" | "r" | "w" | "x" | "s" | "z")
                && !needs_interpreter_lhs(left) =>
        {
            let path_str = match left {
                Value::Instance {
                    class_name,
                    attributes,
                    ..
                } if class_name == "IO::Path" => {
                    attributes.get("path").map(|v| v.to_string_value())
                }
                Value::Str(s) => Some(s.to_string()),
                _ => None,
            };
            if let Some(p) = path_str {
                let path = std::path::Path::new(&p);
                Some(match key.as_str() {
                    "e" => path.exists(),
                    "d" => path.is_dir(),
                    "f" => path.is_file(),
                    "r" => path.exists(),
                    "w" => path.exists(),
                    "x" => {
                        #[cfg(unix)]
                        {
                            use std::os::unix::fs::PermissionsExt;
                            std::fs::metadata(&p)
                                .map(|m| m.permissions().mode() & 0o111 != 0)
                                .unwrap_or(false)
                        }
                        #[cfg(not(unix))]
                        {
                            false
                        }
                    }
                    "s" => std::fs::metadata(&p).map(|m| m.len() > 0).unwrap_or(false),
                    "z" => std::fs::metadata(&p).map(|m| m.len() == 0).unwrap_or(false),
                    _ => false,
                })
            } else {
                Some(false)
            }
        }

        // Str/Int/Cool ~~ IO::Path: convert LHS to IO::Path and compare cleanup.absolute
        (
            _,
            Value::Instance {
                class_name: cn_b,
                attributes: attrs_b,
                ..
            },
        ) if cn_b == "IO::Path" && !needs_interpreter_lhs(left) => {
            let lhs_str = left.to_string_value();
            let (path_b, cwd_b) = io_path_attrs(attrs_b);
            let cwd = std::env::current_dir()
                .map(|p| p.to_string_lossy().to_string())
                .unwrap_or_else(|_| ".".to_string());
            Some(
                io_path_cleanup_absolute(&lhs_str, &cwd)
                    == io_path_cleanup_absolute(&path_b, &cwd_b),
            )
        }

        // Instance ~~ Instance identity check (generic, after all specific instance checks).
        // Exclude Signature (needs special ACCEPTS logic) and X::AdHoc (needs payload delegation).
        (
            Value::Instance {
                class_name: lc,
                id: id_a,
                ..
            },
            Value::Instance {
                class_name: rc,
                id: id_b,
                ..
            },
        ) if lc != "Signature" && rc != "Signature" && lc != "X::AdHoc" => Some(id_a == id_b),

        // Default: fall through to interpreter
        _ => None,
    }
}

impl VM {
    /// Perform a smart match, trying pure value matching first, falling back to
    /// the interpreter for complex cases (regex, callable, type checks, etc.).
    pub(super) fn vm_smart_match(&mut self, left: &Value, right: &Value) -> bool {
        // Try pure matching first
        if let Some(result) = pure_smart_match(left, right) {
            return result;
        }
        // Fall back to interpreter for complex cases
        self.interpreter.smart_match_values(left, right)
    }
}
