use super::*;
use crate::value::ValueView;
use num_traits::ToPrimitive;
use std::path::Component;

/// Why `.resolve(:completely)` could not resolve a path: rakudo's
/// `X::IO::Resolve.reason` texts.
pub(crate) const RESOLVE_NO_PARENT: &str = "No parent dir to go back to";
pub(crate) const RESOLVE_NOT_EXISTING: &str = "Doesn't resolve to an existing path";

impl Interpreter {
    /// rakudo's `IO::Path.resolve` walk over an absolute path: a `..` drops
    /// the last part resolved so far (which is already symlink-free), an
    /// existing part is canonicalized (following its symlinks), and a part
    /// that does not exist is kept as written — later `..` parts still drop
    /// it, so `/zip/loc/../../foo` resolves to `/foo` even when `/zip` does
    /// not exist. With `completely`, a missing part that is not the last one,
    /// or a `..` above the root, is an error carrying the rakudo reason text.
    // Cost: O(p) plus one filesystem query per path segment, p = path length.
    pub(crate) fn resolve_io_path(
        path: &Path,
        completely: bool,
        _display_path: &str,
    ) -> Result<String, RuntimeError> {
        let mut resolved = PathBuf::new();
        let components: Vec<Component> = path.components().collect();
        let last = components.len().saturating_sub(1);

        for (idx, component) in components.into_iter().enumerate() {
            match component {
                Component::Prefix(_) | Component::RootDir => {
                    resolved.push(component.as_os_str());
                }
                Component::CurDir => {}
                Component::ParentDir => {
                    if !resolved.pop() && completely {
                        return Err(RuntimeError::new(RESOLVE_NO_PARENT));
                    }
                }
                Component::Normal(part) => {
                    let candidate = resolved.join(part);
                    if fs::symlink_metadata(&candidate).is_ok() {
                        resolved = fs::canonicalize(&candidate).unwrap_or(candidate);
                        continue;
                    }
                    if completely && idx != last {
                        return Err(RuntimeError::new(RESOLVE_NOT_EXISTING));
                    }
                    resolved = candidate;
                }
            }
        }

        Ok(Self::cleanup_io_path_lexical(&Self::stringify_path(
            &resolved,
        )))
    }

    fn io_path_extension_dot_index_for_n_parts(basename: &str, parts: i64) -> Option<usize> {
        if parts <= 0 {
            return None;
        }
        let mut seen = 0i64;
        for (idx, ch) in basename.char_indices().rev() {
            if ch == '.' {
                seen += 1;
                if seen == parts {
                    return Some(idx);
                }
            }
        }
        None
    }

    pub(crate) fn io_path_extension_with_n_parts(path: &str, parts: i64) -> Option<String> {
        if parts == 0 {
            return Some(String::new());
        }
        let (_, basename) = Self::split_path_for_extension(path);
        let dot_idx = Self::io_path_extension_dot_index_for_n_parts(basename, parts)?;
        Some(basename[dot_idx + 1..].to_string())
    }

    pub(crate) fn io_path_extension_strip_n_parts(basename: &str, parts: i64) -> Option<String> {
        if parts == 0 {
            return Some(basename.to_string());
        }
        let dot_idx = Self::io_path_extension_dot_index_for_n_parts(basename, parts)?;
        Some(basename[..dot_idx].to_string())
    }

    fn io_path_extension_normalize_count(v: &Value) -> Result<i64, RuntimeError> {
        let n = match v.view() {
            ValueView::Int(i) => i,
            ValueView::BigInt(i) => i.to_i64().unwrap_or_else(|| {
                if i.sign() == num_bigint::Sign::Minus {
                    i64::MIN
                } else {
                    i64::MAX
                }
            }),
            ValueView::Num(f) => {
                if f.is_nan() {
                    return Err(RuntimeError::new(
                        "Exception: IO::Path.extension: :parts endpoint must be numeric",
                    ));
                }
                if f.is_infinite() {
                    if f.is_sign_positive() {
                        i64::MAX
                    } else {
                        i64::MIN
                    }
                } else {
                    f as i64
                }
            }
            ValueView::Whatever | ValueView::HyperWhatever => i64::MAX,
            ValueView::Str(_) => {
                return Err(RuntimeError::new(
                    "Exception: IO::Path.extension: :parts endpoint must be numeric",
                ));
            }
            _ => {
                let f = v.to_f64();
                if f.is_nan() {
                    return Err(RuntimeError::new(
                        "Exception: IO::Path.extension: :parts endpoint must be numeric",
                    ));
                }
                if f.is_infinite() {
                    if f.is_sign_positive() {
                        i64::MAX
                    } else {
                        i64::MIN
                    }
                } else {
                    f as i64
                }
            }
        };
        Ok(n.max(0))
    }

    fn io_path_extension_endpoint(v: &Value) -> Result<(Option<i64>, bool), RuntimeError> {
        match v.view() {
            ValueView::Whatever | ValueView::HyperWhatever => Ok((None, false)),
            ValueView::Num(f) if f.is_infinite() => Ok((None, false)),
            ValueView::Num(f) if f.is_nan() => Err(RuntimeError::new(
                "Exception: IO::Path.extension: :parts endpoint must be numeric",
            )),
            ValueView::Str(_) => Err(RuntimeError::new(
                "Exception: IO::Path.extension: :parts endpoint must be numeric",
            )),
            _ => Ok((Some(Self::io_path_extension_normalize_count(v)?), true)),
        }
    }

    fn io_path_extension_range_bounds(
        start: &Value,
        end: &Value,
        excl_start: bool,
        excl_end: bool,
    ) -> Result<(i64, i64), RuntimeError> {
        let (mut low, low_finite) = Self::io_path_extension_endpoint(start)?;
        let (mut high, high_finite) = Self::io_path_extension_endpoint(end)?;

        if low_finite && excl_start {
            low = low.map(|v| v.saturating_add(1));
        }
        if high_finite && excl_end {
            high = high.map(|v| v.saturating_sub(1));
        }

        let low = low.unwrap_or(0).max(0);
        let high = high.unwrap_or(i64::MAX).max(0);
        Ok((low, high))
    }

    /// The `:parts` of `.extension`: how many dot-separated parts count, as an
    /// exact number (one when the adverb is absent) or a range.
    pub(crate) fn io_path_extension_parts_spec_of(
        parts: Option<&Value>,
    ) -> Result<IoPathExtensionPartsSpec, RuntimeError> {
        let Some(parts_val) = parts.cloned() else {
            return Ok(IoPathExtensionPartsSpec::Exact(1));
        };

        match parts_val.view() {
            ValueView::Range(start, end) => {
                let (low, high) = Self::io_path_extension_range_bounds(
                    &Value::int(start),
                    &Value::int(end),
                    false,
                    false,
                )?;
                Ok(IoPathExtensionPartsSpec::Range { low, high })
            }
            ValueView::RangeExcl(start, end) => {
                let (low, high) = Self::io_path_extension_range_bounds(
                    &Value::int(start),
                    &Value::int(end),
                    false,
                    true,
                )?;
                Ok(IoPathExtensionPartsSpec::Range { low, high })
            }
            ValueView::RangeExclStart(start, end) => {
                let (low, high) = Self::io_path_extension_range_bounds(
                    &Value::int(start),
                    &Value::int(end),
                    true,
                    false,
                )?;
                Ok(IoPathExtensionPartsSpec::Range { low, high })
            }
            ValueView::RangeExclBoth(start, end) => {
                let (low, high) = Self::io_path_extension_range_bounds(
                    &Value::int(start),
                    &Value::int(end),
                    true,
                    true,
                )?;
                Ok(IoPathExtensionPartsSpec::Range { low, high })
            }
            ValueView::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                let (low, high) = Self::io_path_extension_range_bounds(
                    start.as_ref(),
                    end.as_ref(),
                    excl_start,
                    excl_end,
                )?;
                Ok(IoPathExtensionPartsSpec::Range { low, high })
            }
            _ => Ok(IoPathExtensionPartsSpec::Exact(
                Self::io_path_extension_normalize_count(&parts_val)?,
            )),
        }
    }
}
