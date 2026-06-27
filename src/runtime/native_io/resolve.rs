use super::*;
use num_traits::ToPrimitive;
use std::path::Component;

impl Interpreter {
    pub(crate) fn resolve_io_path(
        path: &Path,
        completely: bool,
        display_path: &str,
    ) -> Result<String, RuntimeError> {
        let mut resolved = PathBuf::new();
        let mut unresolved = PathBuf::new();
        let mut unresolved_started = false;
        let component_count = path.components().count();

        for (idx, component) in path.components().enumerate() {
            match component {
                Component::Prefix(prefix) => {
                    resolved.push(prefix.as_os_str());
                    if unresolved_started {
                        unresolved.push(prefix.as_os_str());
                    }
                }
                Component::RootDir => {
                    resolved.push(component.as_os_str());
                    if unresolved_started {
                        unresolved.push(component.as_os_str());
                    }
                }
                Component::CurDir => {}
                Component::ParentDir => {
                    if unresolved_started {
                        unresolved.push("..");
                    } else if !resolved.pop() && completely {
                        return Err(RuntimeError::new(format!(
                            "X::IO::Resolve: Failed to completely resolve {}",
                            display_path
                        )));
                    }
                }
                Component::Normal(part) => {
                    if unresolved_started {
                        unresolved.push(part);
                        continue;
                    }

                    let candidate = resolved.join(part);
                    match fs::symlink_metadata(&candidate) {
                        Ok(_) => {
                            resolved = fs::canonicalize(&candidate).unwrap_or(candidate);
                        }
                        Err(_) => {
                            let is_last = idx + 1 == component_count;
                            if completely && !is_last {
                                return Err(RuntimeError::new(format!(
                                    "X::IO::Resolve: Failed to completely resolve {}",
                                    display_path
                                )));
                            }
                            unresolved_started = true;
                            unresolved.push(part);
                        }
                    }
                }
            }
        }

        let combined = if unresolved_started {
            resolved.join(unresolved)
        } else {
            resolved
        };
        Ok(Self::cleanup_io_path_lexical(&Self::stringify_path(
            &combined,
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
        let n = match v {
            Value::Int(i) => *i,
            Value::BigInt(i) => i.to_i64().unwrap_or_else(|| {
                if i.sign() == num_bigint::Sign::Minus {
                    i64::MIN
                } else {
                    i64::MAX
                }
            }),
            Value::Num(f) => {
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
                    *f as i64
                }
            }
            Value::Whatever | Value::HyperWhatever => i64::MAX,
            Value::Str(_) => {
                return Err(RuntimeError::new(
                    "Exception: IO::Path.extension: :parts endpoint must be numeric",
                ));
            }
            other => {
                let f = other.to_f64();
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
        match v {
            Value::Whatever | Value::HyperWhatever => Ok((None, false)),
            Value::Num(f) if f.is_infinite() => Ok((None, false)),
            Value::Num(f) if f.is_nan() => Err(RuntimeError::new(
                "Exception: IO::Path.extension: :parts endpoint must be numeric",
            )),
            Value::Str(_) => Err(RuntimeError::new(
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

    pub(crate) fn io_path_extension_parts_spec(
        args: &[Value],
    ) -> Result<IoPathExtensionPartsSpec, RuntimeError> {
        let Some(parts_val) = Self::named_value(args, "parts") else {
            return Ok(IoPathExtensionPartsSpec::Exact(1));
        };

        match parts_val {
            Value::Range(start, end) => {
                let (low, high) = Self::io_path_extension_range_bounds(
                    &Value::Int(start),
                    &Value::Int(end),
                    false,
                    false,
                )?;
                Ok(IoPathExtensionPartsSpec::Range { low, high })
            }
            Value::RangeExcl(start, end) => {
                let (low, high) = Self::io_path_extension_range_bounds(
                    &Value::Int(start),
                    &Value::Int(end),
                    false,
                    true,
                )?;
                Ok(IoPathExtensionPartsSpec::Range { low, high })
            }
            Value::RangeExclStart(start, end) => {
                let (low, high) = Self::io_path_extension_range_bounds(
                    &Value::Int(start),
                    &Value::Int(end),
                    true,
                    false,
                )?;
                Ok(IoPathExtensionPartsSpec::Range { low, high })
            }
            Value::RangeExclBoth(start, end) => {
                let (low, high) = Self::io_path_extension_range_bounds(
                    &Value::Int(start),
                    &Value::Int(end),
                    true,
                    true,
                )?;
                Ok(IoPathExtensionPartsSpec::Range { low, high })
            }
            Value::GenericRange {
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
            other => Ok(IoPathExtensionPartsSpec::Exact(
                Self::io_path_extension_normalize_count(&other)?,
            )),
        }
    }
}
