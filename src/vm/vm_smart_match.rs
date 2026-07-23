use std::path::PathBuf;

use crate::runtime::Interpreter;
use crate::value::{Value, ValueView};

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
fn io_path_attrs(attrs: &crate::gc::Gc<crate::value::InstanceAttrs>) -> (String, String) {
    let path = attrs
        .as_map()
        .get("path")
        .map(|v: &Value| v.to_string_value())
        .unwrap_or_default();
    let cwd = attrs
        .as_map()
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
        v.view(),
        ValueView::Junction { .. }
            | ValueView::Sub(_)
            | ValueView::Regex(_)
            | ValueView::RegexWithAdverbs { .. }
            | ValueView::Routine { .. }
            | ValueView::Package(_)
            | ValueView::CustomType { .. }
            | ValueView::ParametricRole { .. }
    )
}

/// Try to evaluate a smart match between `left` and `right` without needing the
/// interpreter.  Returns `Some(bool)` for type pairs that can be resolved purely
/// from the values themselves, or `None` when the interpreter is required (regex
/// matching, callable invocation, type-object checks with class registries, etc.).
/// True when `v` is a numeric matcher for smart-match purposes — a bare numeric
/// type or a numeric allomorph/mixin (`<5>`, `<5.0>`, `42 but Role`). Used to
/// decide whether an allomorph LHS should compare by its numeric base.
fn is_numericish_matcher(v: &Value) -> bool {
    match v.view() {
        ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Rat(_, _)
        | ValueView::FatRat(_, _)
        | ValueView::BigRat(_, _)
        | ValueView::Complex(_, _) => true,
        ValueView::Mixin(inner, _) => is_numericish_matcher(inner.as_ref()),
        _ => false,
    }
}

pub(crate) fn pure_smart_match(left: &Value, right: &Value) -> Option<bool> {
    // An allomorph / numeric mixin (`<5.0>` RatStr) on the LHS compares by its
    // numeric base when the matcher is numeric: `<5.0> ~~ 5` and `<5.0> ~~ <5>`
    // are True. Guard on a numeric RHS so a Str matcher keeps the allomorph's
    // string half (`<5.0> ~~ "5"` stays False, `~~ "5.0"` stays True).
    if let ValueView::Mixin(inner, _) = left.view()
        && is_numericish_matcher(right)
    {
        return pure_smart_match(&inner.as_ref().clone(), right);
    }
    // Symmetrically, a numeric LHS against an allomorph RHS matcher compares by
    // the RHS's numeric base (`5.0 ~~ <5>` is True). A Str LHS is excluded, so
    // `"5.0" ~~ <5>` keeps the RHS allomorph's string-matcher semantics (False).
    if let ValueView::Mixin(inner, _) = right.view()
        && is_numericish_matcher(left)
    {
        return pure_smart_match(left, &inner.as_ref().clone());
    }

    match (left.view(), right.view()) {
        // Whatever / HyperWhatever on RHS always matches (`$x ~~ (**)` is True for
        // any value — a bare `**` pattern matches everything, like `*`).
        (_, ValueView::Whatever | ValueView::HyperWhatever) => Some(true),

        // Junction ~~ Junction/Mu type: a Junction IS a Junction, don't autothread
        (ValueView::Junction { .. }, ValueView::Package(name))
            if matches!(name.resolve().as_str(), "Junction" | "Mu") =>
        {
            Some(true)
        }

        // DateTime ~~ Date: compare date parts
        (
            ValueView::Instance {
                class_name: left_class,
                attributes: left_attrs,
                ..
            },
            ValueView::Instance {
                class_name: right_class,
                attributes: right_attrs,
                ..
            },
        ) if left_class == "DateTime" && right_class == "Date" => {
            let (y, m, d, _, _, _, _) =
                crate::builtins::methods_0arg::temporal::datetime_attrs(&(left_attrs).as_map());
            let (ry, rm, rd) =
                crate::builtins::methods_0arg::temporal::date_attrs(&(right_attrs).as_map());
            Some(y == ry && m == rm && d == rd)
        }

        // Version ~~ Version
        (ValueView::Version { .. }, ValueView::Version { parts, plus, minus }) => {
            Some(Interpreter::version_smart_match(left, parts, plus, minus))
        }

        // When RHS is NaN, check if LHS is also NaN
        (_, ValueView::Num(b)) if b.is_nan() && !needs_interpreter_lhs(left) => {
            Some(Interpreter::value_is_nan(left))
        }
        (ValueView::Num(a), _) if a.is_nan() && !needs_interpreter_lhs(right) => {
            Some(Interpreter::value_is_nan(right))
        }

        // Complex comparisons (NaN-aware)
        (ValueView::Complex(ar, ai), ValueView::Complex(br, bi)) => {
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
        (ValueView::Int(a), ValueView::Complex(br, bi)) => Some((a as f64) == br && bi == 0.0),
        (ValueView::Complex(ar, ai), ValueView::Int(b)) => Some(ar == (b as f64) && ai == 0.0),
        (ValueView::Num(a), ValueView::Complex(br, bi)) => {
            if a.is_nan() && (br.is_nan() || bi.is_nan()) {
                Some(true)
            } else {
                Some(a == br && bi == 0.0)
            }
        }
        (ValueView::Complex(ar, ai), ValueView::Num(b)) => {
            if b.is_nan() && (ar.is_nan() || ai.is_nan()) {
                Some(true)
            } else {
                Some(ar == b && ai == 0.0)
            }
        }
        (ValueView::Complex(ar, ai), ValueView::Rat(n, d)) => {
            if d != 0 {
                Some(ar == (n as f64 / d as f64) && ai == 0.0)
            } else {
                Some(false)
            }
        }
        (ValueView::Rat(n, d), ValueView::Complex(br, bi)) => {
            if d != 0 {
                Some((n as f64 / d as f64) == br && bi == 0.0)
            } else {
                Some(false)
            }
        }

        // Numeric equality
        (ValueView::Int(a), ValueView::Int(b)) => Some(a == b),
        (ValueView::Num(a), ValueView::Num(b)) => Some(a == b),
        (ValueView::Int(a), ValueView::Num(b)) => Some((a as f64) == b),
        (ValueView::Num(a), ValueView::Int(b)) => Some(a == (b as f64)),
        (ValueView::Rat(an, ad), ValueView::Rat(bn, bd)) => Some(an * bd == bn * ad),
        (ValueView::Int(a), ValueView::Rat(n, d)) => Some(a * d == n),
        (ValueView::Rat(n, d), ValueView::Int(b)) => Some(n == b * d),

        // String equality
        (ValueView::Str(a), ValueView::Str(b)) => Some(*a == *b),

        // Str ~~ Numeric: numify LHS and compare
        (ValueView::Str(a), ValueView::Int(b)) => Some(a.trim().parse::<f64>() == Ok(b as f64)),
        (ValueView::Str(a), ValueView::Num(b)) => {
            let trimmed = a.trim().replace('\u{2212}', "-");
            Some(trimmed.parse::<f64>().is_ok_and(|v| {
                if v.is_nan() && b.is_nan() {
                    true
                } else {
                    v == b
                }
            }))
        }
        (ValueView::Str(a), ValueView::Rat(n, d)) => {
            if d != 0 {
                Some(
                    a.trim()
                        .parse::<f64>()
                        .is_ok_and(|v| v == n as f64 / d as f64),
                )
            } else {
                Some(false)
            }
        }
        (ValueView::Int(a), ValueView::Str(b)) => Some(b.trim().parse::<f64>() == Ok(a as f64)),
        (ValueView::Nil, ValueView::Str(s)) => Some(s.is_empty()),

        // Array/List ~~ Numeric: a list smart-matched against a number compares
        // numerically (`@a == $n`), and a list's numeric value is its element
        // count — so `[1,2,3] ~~ 3` is True (3 elems), `~~ 2` is False.
        (ValueView::Array(a, _), ValueView::Int(b)) => Some(a.items.len() as i64 == b),
        (ValueView::Array(a, _), ValueView::Num(b)) => Some(a.items.len() as f64 == b),
        (ValueView::Array(a, _), ValueView::Rat(n, d)) => {
            Some(d != 0 && a.items.len() as i64 * d == n)
        }

        // IO::Path ~~ IO::Path: compare by cleanup.absolute
        (
            ValueView::Instance {
                class_name: cn_a,
                attributes: attrs_a,
                ..
            },
            ValueView::Instance {
                class_name: cn_b,
                attributes: attrs_b,
                ..
            },
        ) if cn_a == "IO::Path" && cn_b == "IO::Path" => {
            let (path_a, cwd_a) = io_path_attrs(&attrs_a);
            let (path_b, cwd_b) = io_path_attrs(&attrs_b);
            Some(
                io_path_cleanup_absolute(&path_a, &cwd_a)
                    == io_path_cleanup_absolute(&path_b, &cwd_b),
            )
        }

        // Buf/Blob ~~ Buf/Blob: compare byte contents
        (
            ValueView::Instance {
                class_name: cn_a, ..
            },
            ValueView::Instance {
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
            let lb = Interpreter::extract_buf_bytes(left);
            let rb = Interpreter::extract_buf_bytes(right);
            Some(lb == rb)
        }

        // Instance ~~ Instance: value types use eqv, others use identity
        (
            ValueView::Instance { class_name: lc, .. },
            ValueView::Instance { class_name: rc, .. },
        ) if lc == rc
            && matches!(
                lc.resolve().as_ref(),
                "Date" | "DateTime" | "Duration" | "Instant"
            ) =>
        {
            Some(left.eqv(right))
        }

        // A Match on the right is an already-decided match result, so it passes through
        // like a Bool does: `$_ ~~ ($str ~~ /a/)` is true whatever the topic is. This is
        // what makes `EXPR when $x ~~ /re/` work with no `given` to set the topic.
        (
            _,
            ValueView::Instance {
                class_name: rhs_class,
                ..
            },
        ) if rhs_class == "Match" => Some(right.truthy()),

        // Mu instances: Mu ~~ Mu.new is True
        (
            ValueView::Package(lhs_type),
            ValueView::Instance {
                class_name: rhs_class,
                ..
            },
        ) if rhs_class == "Mu" => Some(lhs_type == "Mu"),

        // When LHS is a type object (Package) and RHS is not a Package/type,
        // return false (handled more completely in the interpreter for type hierarchy)
        (ValueView::Package(_), ValueView::Instance { .. }) => Some(false),

        // When RHS is a Bool, result is that Bool
        (_, ValueView::Bool(b))
            if !matches!(
                left.view(),
                ValueView::Package(_) | ValueView::Instance { .. }
            ) && !needs_interpreter_lhs(left) =>
        {
            Some(b)
        }

        // Hash ~~ Hash: structural equality
        (ValueView::Hash(lmap), ValueView::Hash(rmap)) => {
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
        (ValueView::Set(set, _), ValueView::Bag(bag, _)) => Some(
            set.len() == bag.len()
                && set.iter().all(|key| {
                    bag.get(key)
                        .is_some_and(|count| *count == num_bigint::BigInt::from(1))
                }),
        ),

        // Bag ~~ Set: keys must match (counts are ignored)
        (ValueView::Bag(bag, _), ValueView::Set(set, _)) => {
            let bag_keys: std::collections::HashSet<&String> = bag.keys().collect();
            Some(bag_keys.len() == set.len() && bag_keys.iter().all(|key| set.contains(*key)))
        }

        // Set ~~ Mix: all set elements must exist in Mix with unit weights
        (ValueView::Set(set, _), ValueView::Mix(mix, _)) => Some(
            set.len() == mix.len()
                && set.iter().all(|key| {
                    mix.get(key)
                        .is_some_and(|weight| weight.is_finite() && *weight == 1.0)
                }),
        ),

        // Mix ~~ Set: all mix elements must have unit weights and exist in set
        (ValueView::Mix(mix, _), ValueView::Set(set, _)) => Some(
            mix.len() == set.len()
                && mix
                    .iter()
                    .all(|(key, weight)| weight.is_finite() && *weight == 1.0 && set.contains(key)),
        ),

        // Array ~~ Hash: check if any element exists as a key
        (ValueView::Array(items, ..), ValueView::Hash(map)) => Some(items.iter().any(|item| {
            let key = item.to_string_value();
            map.contains_key(&key)
        })),

        // Scalar ~~ Hash: check key existence
        // (but not for types that have more specific matches above)
        (_, ValueView::Hash(map))
            if !matches!(
                left.view(),
                ValueView::Regex(_)
                    | ValueView::RegexWithAdverbs { .. }
                    | ValueView::Hash(_)
                    | ValueView::Array(..)
                    | ValueView::Pair(..)
            ) && !needs_interpreter_lhs(left) =>
        {
            let key = left.to_string_value();
            Some(map.contains_key(&key))
        }

        // Range ~~ Range: LHS is subset of RHS
        (_, _) if left.is_range() && right.is_range() => {
            let r_str = Interpreter::range_has_string_endpoints(right);
            let (_, _, l_es, l_ee) = Interpreter::range_exclusivity(left);
            let (_, _, r_es, r_ee) = Interpreter::range_exclusivity(right);
            if r_str {
                let (l_min_s, l_max_s) = Interpreter::range_raw_string_bounds(left);
                let (r_min_s, r_max_s) = Interpreter::range_raw_string_bounds(right);
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
                let (l_min, l_max) = Interpreter::range_raw_bounds_f64(left);
                let (r_min, r_max) = Interpreter::range_raw_bounds_f64(right);
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
        (_, _) if left.is_range() && right.is_numeric() => {
            let elems = Interpreter::range_elems_f64(left);
            let rval = right.to_f64();
            Some(elems == rval)
        }

        // Value ~~ Range: containment check
        // Exclude types that have their own smartmatch handling so they fall
        // through to the interpreter.
        (_, _)
            if right.is_range()
                && !needs_interpreter_lhs(left)
                && !matches!(
                    left.view(),
                    ValueView::Hash(_)
                        | ValueView::Array(..)
                        | ValueView::Seq(_)
                        | ValueView::Slip(_)
                        | ValueView::LazyList(_)
                ) =>
        {
            Some(Interpreter::value_in_range(left, right))
        }

        // IO::Path ~~ Pair(:e), :d, :f, :r, :w, :x, :rw, :rwx, :s, :z file tests
        // Also handles negated forms: :!e, :!d, :!f, :!r, :!w, :!x, :!rw, :!rwx, :!s, :!z
        // Note: Str ~~ :d is NOT supported (per Raku spec, only IO::Path has these methods)
        (
            ValueView::Instance {
                class_name,
                attributes,
                ..
            },
            ValueView::Pair(key, val),
        ) if class_name == "IO::Path"
            && matches!(val.view(), ValueView::Bool(_))
            && matches!(
                key.as_str(),
                "e" | "d" | "f" | "l" | "r" | "w" | "x" | "rw" | "rwx" | "s" | "z"
            ) =>
        {
            let negated = matches!(val.view(), ValueView::Bool(false));
            let path_str = attributes.as_map().get("path").map(|v| v.to_string_value());
            if let Some(p) = path_str {
                let path = std::path::Path::new(&p);
                let result = match key.as_str() {
                    "e" => path.exists(),
                    "d" => path.is_dir(),
                    "f" => path.is_file(),
                    "r" => {
                        #[cfg(unix)]
                        {
                            use std::os::unix::fs::PermissionsExt;
                            std::fs::metadata(&p)
                                .map(|m| m.permissions().mode() & 0o444 != 0)
                                .unwrap_or(false)
                        }
                        #[cfg(not(unix))]
                        {
                            path.exists()
                        }
                    }
                    "w" => {
                        #[cfg(unix)]
                        {
                            use std::os::unix::fs::PermissionsExt;
                            std::fs::metadata(&p)
                                .map(|m| m.permissions().mode() & 0o222 != 0)
                                .unwrap_or(false)
                        }
                        #[cfg(not(unix))]
                        {
                            path.exists()
                        }
                    }
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
                    "rw" => {
                        #[cfg(unix)]
                        {
                            use std::os::unix::fs::PermissionsExt;
                            std::fs::metadata(&p)
                                .map(|m| {
                                    let mode = m.permissions().mode();
                                    mode & 0o444 != 0 && mode & 0o222 != 0
                                })
                                .unwrap_or(false)
                        }
                        #[cfg(not(unix))]
                        {
                            std::fs::metadata(&p)
                                .map(|m| !m.permissions().readonly())
                                .unwrap_or(false)
                        }
                    }
                    "rwx" => {
                        #[cfg(unix)]
                        {
                            use std::os::unix::fs::PermissionsExt;
                            std::fs::metadata(&p)
                                .map(|m| {
                                    let mode = m.permissions().mode();
                                    mode & 0o444 != 0 && mode & 0o222 != 0 && mode & 0o111 != 0
                                })
                                .unwrap_or(false)
                        }
                        #[cfg(not(unix))]
                        {
                            false
                        }
                    }
                    "l" => std::fs::symlink_metadata(&p)
                        .map(|m| m.file_type().is_symlink())
                        .unwrap_or(false),
                    "s" => std::fs::metadata(&p).map(|m| m.len() > 0).unwrap_or(false),
                    "z" => std::fs::metadata(&p).map(|m| m.len() == 0).unwrap_or(false),
                    _ => false,
                };
                Some(if negated { !result } else { result })
            } else {
                Some(negated)
            }
        }

        // Str/Int/Cool ~~ IO::Path: convert LHS to IO::Path and compare cleanup.absolute
        (
            _,
            ValueView::Instance {
                class_name: cn_b,
                attributes: attrs_b,
                ..
            },
        ) if cn_b == "IO::Path" && !needs_interpreter_lhs(left) => {
            let lhs_str = left.to_string_value();
            let (path_b, cwd_b) = io_path_attrs(&attrs_b);
            let cwd = std::env::current_dir()
                .map(|p| p.to_string_lossy().to_string())
                .unwrap_or_else(|_| ".".to_string());
            Some(
                io_path_cleanup_absolute(&lhs_str, &cwd)
                    == io_path_cleanup_absolute(&path_b, &cwd_b),
            )
        }

        // IO::Path ~~ Str: compare path string representation with string.
        // In Raku, Str.ACCEPTS(IO::Path) stringifies the IO::Path and compares.
        // This handles cases like `dir().grep("filename")` after chdir.
        (ValueView::Instance { class_name: cn, .. }, ValueView::Str(s)) if cn == "IO::Path" => {
            Some(left.to_string_value() == s.as_str())
        }

        // Instance ~~ Instance identity check (generic, after all specific instance checks).
        // Exclude Signature (needs special ACCEPTS logic) and X::AdHoc (needs payload delegation).
        (
            ValueView::Instance {
                class_name: lc,
                id: id_a,
                ..
            },
            ValueView::Instance {
                class_name: rc,
                id: id_b,
                ..
            },
        ) if lc != "Signature" && rc != "Signature" && lc != "X::AdHoc" => Some(id_a == id_b),

        // Default: fall through to interpreter
        _ => None,
    }
}

impl Interpreter {
    /// Perform a smart match, trying pure value matching first, falling back to
    /// the interpreter for complex cases (regex, callable, type checks, etc.).
    /// Reify a *safely finite* lazy list (a plain gather / a pipe bottoming
    /// out in a finite source) to a `Seq` so it participates in list
    /// smartmatch by value — `(gather { take 1; take 2 }) ~~ (1, 2)` must
    /// compare elements, not an unforced placeholder. Returns `None` for
    /// anything genuinely lazy (never forces an infinite source).
    fn reify_finite_lazy_for_match(&mut self, v: &Value) -> Option<Value> {
        let ValueView::LazyList(ll) = v.view() else {
            return None;
        };
        let finite = if ll.lazy_pipe.is_some() {
            ll.pipe_bottoms_out_finite()
        } else {
            !ll.is_genuinely_lazy()
                && (ll.coroutine.is_some() || !ll.body.is_empty() || ll.compiled_code.is_some())
        };
        if !finite {
            return None;
        }
        match self.force_lazy_list_vm(&ll) {
            Ok(items) => Some(Value::seq(items)),
            Err(e) => {
                self.set_pending_dispatch_error(e);
                None
            }
        }
    }

    pub(super) fn vm_smart_match(&mut self, left: &Value, right: &Value) -> bool {
        // Force finite lazy operands first so list matching sees their
        // elements (see reify_finite_lazy_for_match).
        if let Some(forced) = self.reify_finite_lazy_for_match(left) {
            return self.vm_smart_match(&forced, right);
        }
        if let Some(forced) = self.reify_finite_lazy_for_match(right) {
            return self.vm_smart_match(left, &forced);
        }
        // `$x ~~ $obj` where $obj's class defines a user `ACCEPTS` dispatches
        // `$obj.ACCEPTS($x)` — the core smartmatch protocol. Check this BEFORE
        // `pure_smart_match`, whose generic Instance~~Instance arm would otherwise
        // short-circuit to an identity check and never reach the dispatch. Junction
        // LHS is excluded so junction autothreading still applies. Mirrors the
        // interpreter `smart_match` arm (the grep/given paths reach that one).
        if let ValueView::Instance { class_name, .. } = right.view()
            && !matches!(left.view(), ValueView::Junction { .. })
            && self.has_user_method(&class_name.resolve(), "ACCEPTS")
        {
            match self.call_method_with_values(right.clone(), "ACCEPTS", vec![left.clone()]) {
                Ok(v) => return v.truthy(),
                Err(e) => {
                    self.set_pending_dispatch_error(e);
                    return false;
                }
            }
        }
        // Try pure matching first
        if let Some(result) = pure_smart_match(left, right) {
            return result;
        }

        // Any ~~ Pair: call method named by pair key on the object, coerce result
        // to Bool, and compare with pair value coerced to Bool.
        // Per S03: ?."{X.key}" === ?X.value
        // Exclude types with their own Pair smartmatch semantics (Hash checks key+value).
        // Both `Pair` (string-keyed) and `ValuePair` (`key => val`
        // literals are built as ValuePair) reach here.
        let pair_key_val = match right.view() {
            ValueView::Pair(key, val) => Some((key.clone(), val.clone())),
            ValueView::ValuePair(key, val) => Some((key.to_string_value(), val.clone())),
            _ => None,
        };
        if let Some((method_name, val)) = pair_key_val
            && !matches!(
                left.view(),
                ValueView::Hash(_)
                    | ValueView::Array(..)
                    | ValueView::Seq(_)
                    | ValueView::Slip(_)
                    | ValueView::LazyList(_)
                    // A Pair LHS has its own Pair-vs-Pair smartmatch (key AND value
                    // equality), NOT method-key dispatch — `("a"=>"b") ~~ ("a"=>"b")`
                    // must compare, not call method `a` on the left Pair.
                    | ValueView::Pair(..)
                    | ValueView::ValuePair(..)
            )
        {
            // Route the key-method call through the Interpreter's unified compiled-first
            // dispatch (ledger §1): user-defined methods run as compiled bytecode;
            // only native/reflective methods bottom out at the interpreter. `left`
            // is never an Array/Hash/Seq here (excluded above), so the native
            // array/list fast paths inside are inert.
            match self.try_compiled_method_or_interpret(left.clone(), &method_name, Vec::new()) {
                Ok(result) => {
                    return result.truthy() == val.truthy();
                }
                Err(err) => {
                    // Non-existing method or attribute dies.
                    // Set pending dispatch error so the caller can propagate it.
                    self.set_pending_dispatch_error(err);
                    return false;
                }
            }
        }

        // Fall back to interpreter for complex cases
        loan_env!(self, smart_match_values(left, right))
    }
}
