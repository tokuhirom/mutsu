use super::*;
use num_traits::ToPrimitive;

/// Coerce a positional limit argument (the `$limit` of `.lines`/`.words`/`.get`
/// reads) to a row count. Accepts any non-negative numeric, including an
/// allomorph (`<3>`, `<3e0>`, `<3+0i>`) by unwrapping the `Mixin` to its inner
/// numeric. Returns `None` for non-numeric args, `*`/`Whatever`, and `+Inf`
/// (all meaning "no limit").
pub(crate) fn numeric_limit_arg(arg: &Value) -> Option<usize> {
    match arg.view() {
        ValueView::Int(i) => Some(i.max(0) as usize),
        ValueView::BigInt(bi) => Some(bi.to_usize().unwrap_or(usize::MAX)),
        ValueView::Num(f) if f.is_infinite() => None,
        ValueView::Num(f) if f >= 0.0 => Some(f as usize),
        ValueView::Rat(n, d) if d != 0 => Some(((n as f64 / d as f64) as i64).max(0) as usize),
        ValueView::Complex(re, im) if im == 0.0 && re >= 0.0 => Some(re as usize),
        ValueView::Mixin(inner, _) => numeric_limit_arg(inner),
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

#[cfg(unix)]
fn path_access(path: &Path, mode: libc::c_int) -> bool {
    use std::ffi::CString;
    use std::os::unix::ffi::OsStrExt;
    let Ok(cpath) = CString::new(path.as_os_str().as_bytes()) else {
        return false;
    };
    unsafe { libc::access(cpath.as_ptr(), mode) == 0 }
}

/// The one body of the IO::Path file tests -- the `.e`/`.f`/`.d`/`.l`/`.r`/
/// `.w`/`.x`/`.rw`/`.rwx`/`.s`/`.z` methods and `$path ~~ :r` (rakudo's Pair
/// smartmatch is the method call). `None` when the path does not exist and
/// the test needs a `stat` (the methods turn that into a
/// `X::IO::DoesNotExist` Failure, the smartmatch into False). `.s` answers
/// "is non-empty" here; the method reports the size itself.
///
/// Permission tests ask `access(2)` for the calling user, as MoarVM does,
/// not the mode bits: a mode-000 file is readable by root.
// Cost: O(1) system calls (at most three `access`).
pub(crate) fn io_file_test(path: &Path, test: &str) -> Option<bool> {
    if test == "e" {
        return Some(path.exists());
    }
    if test == "l" {
        return fs::symlink_metadata(path)
            .ok()
            .map(|m| m.file_type().is_symlink());
    }
    let meta = fs::metadata(path).ok()?;
    Some(match test {
        "f" => meta.is_file(),
        "d" => meta.is_dir(),
        "r" => path_is_readable(path),
        "w" => path_is_writable(path),
        "x" => path_is_executable(path),
        "rw" => path_is_readable(path) && path_is_writable(path),
        "rwx" => path_is_readable(path) && path_is_writable(path) && path_is_executable(path),
        "s" => meta.len() > 0,
        "z" => meta.len() == 0,
        _ => false,
    })
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
