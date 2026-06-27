use super::*;
use num_traits::ToPrimitive;

/// Coerce a positional limit argument (the `$limit` of `.lines`/`.words`/`.get`
/// reads) to a row count. Accepts any non-negative numeric, including an
/// allomorph (`<3>`, `<3e0>`, `<3+0i>`) by unwrapping the `Mixin` to its inner
/// numeric. Returns `None` for non-numeric args, `*`/`Whatever`, and `+Inf`
/// (all meaning "no limit").
pub(crate) fn numeric_limit_arg(arg: &Value) -> Option<usize> {
    match arg {
        Value::Int(i) => Some((*i).max(0) as usize),
        Value::BigInt(bi) => Some(bi.to_usize().unwrap_or(usize::MAX)),
        Value::Num(f) if f.is_infinite() => None,
        Value::Num(f) if *f >= 0.0 => Some(*f as usize),
        Value::Rat(n, d) if *d != 0 => Some(((*n as f64 / *d as f64) as i64).max(0) as usize),
        Value::Complex(re, im) if *im == 0.0 && *re >= 0.0 => Some(*re as usize),
        Value::Mixin(inner, _) => numeric_limit_arg(inner),
        _ => None,
    }
}

pub(crate) fn io_exception(class_name: &str, message: String) -> RuntimeError {
    let mut err = RuntimeError::new(message);
    err.exception = Some(Box::new(Value::make_instance(
        Symbol::intern(class_name),
        HashMap::new(),
    )));
    err
}

pub(crate) fn io_exception_failure(class_name: &str, message: String) -> Value {
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(message));
    let ex = Value::make_instance(Symbol::intern(class_name), attrs);
    let mut failure_attrs = HashMap::new();
    failure_attrs.insert("exception".to_string(), ex);
    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
}

pub(crate) enum IoPathExtensionPartsSpec {
    Exact(i64),
    Range { low: i64, high: i64 },
}

pub(crate) fn io_path_missing_failure(path: &str, method: &str) -> Value {
    let message = format!("Failed to find '{}' while trying to do '.{}'", path, method);
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(message));
    attrs.insert("path".to_string(), Value::str(path.to_string()));
    attrs.insert("trying".to_string(), Value::str(method.to_string()));
    let ex = Value::make_instance(Symbol::intern("X::IO::DoesNotExist"), attrs);
    let mut failure_attrs = HashMap::new();
    failure_attrs.insert("exception".to_string(), ex);
    Value::make_instance(Symbol::intern("Failure"), failure_attrs)
}

fn io_path_missing_error(path: &str, method: &str) -> RuntimeError {
    let message = format!("Failed to find '{}' while trying to do '.{}'", path, method);
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.clone()));
    attrs.insert("path".to_string(), Value::str(path.to_string()));
    attrs.insert("trying".to_string(), Value::str(method.to_string()));
    let mut err = RuntimeError::new(message);
    err.exception = Some(Box::new(Value::make_instance(
        Symbol::intern("X::IO::DoesNotExist"),
        attrs,
    )));
    err
}

pub(crate) fn io_path_metadata(
    path: &Path,
    display_path: &str,
    method: &str,
) -> Result<fs::Metadata, RuntimeError> {
    fs::metadata(path).map_err(|_| io_path_missing_error(display_path, method))
}

#[cfg(unix)]
fn path_access(path: &Path, mode: libc::c_int) -> bool {
    use std::ffi::CString;
    use std::os::unix::ffi::OsStrExt;
    let Ok(cpath) = CString::new(path.as_os_str().as_bytes()) else {
        return false;
    };
    unsafe { libc::access(cpath.as_ptr(), mode) == 0 }
}

#[cfg(unix)]
pub(crate) fn path_is_readable(path: &Path) -> bool {
    path_access(path, libc::R_OK)
}

#[cfg(unix)]
pub(crate) fn path_is_writable(path: &Path) -> bool {
    path_access(path, libc::W_OK)
}

#[cfg(unix)]
pub(crate) fn path_is_executable(path: &Path) -> bool {
    path_access(path, libc::X_OK)
}

#[cfg(not(unix))]
pub(crate) fn path_is_readable(path: &Path) -> bool {
    fs::metadata(path).is_ok()
}

#[cfg(not(unix))]
pub(crate) fn path_is_writable(path: &Path) -> bool {
    fs::metadata(path)
        .map(|m| !m.permissions().readonly())
        .unwrap_or(false)
}

#[cfg(not(unix))]
pub(crate) fn path_is_executable(path: &Path) -> bool {
    fs::metadata(path).map(|m| m.is_file()).unwrap_or(false)
}

impl IoPathExtensionPartsSpec {
    pub(crate) fn select(&self, available: i64) -> Option<i64> {
        match self {
            Self::Exact(n) => {
                if *n <= available {
                    Some(*n)
                } else {
                    None
                }
            }
            Self::Range { low, high } => {
                if low > high {
                    return None;
                }
                let best = available.min(*high);
                if best < *low { None } else { Some(best) }
            }
        }
    }
}
