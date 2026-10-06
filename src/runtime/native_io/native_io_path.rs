use super::*;
use crate::value::AttrMap;

impl Interpreter {
    /// Build a `Failure` value wrapping an `X::IO::*` exception with the given
    /// type name and offending path. The Failure throws the exception when sunk.
    fn make_io_failure(&self, ex_type: &str, path: &str) -> Value {
        let mut ex_attrs = HashMap::new();
        ex_attrs.insert("path".to_string(), Value::str(path.to_string()));
        ex_attrs.insert(
            "message".to_string(),
            Value::str(format!("{}: {}", ex_type, path)),
        );
        let exception = Value::make_instance(Symbol::intern(ex_type), ex_attrs);
        let mut failure_attrs = HashMap::new();
        failure_attrs.insert("exception".to_string(), exception);
        failure_attrs.insert("handled".to_string(), Value::FALSE);
        Value::make_instance(Symbol::intern("Failure"), failure_attrs)
    }

    /// `IO::Path.Numeric`: IO::Path is Cool, so it coerces the *basename* to a
    /// number (raku-doc Type/IO/Path: method Numeric), failing with an
    /// X::Str::Numeric Failure (a soft fail, what `fails-like` expects) when the
    /// basename is not numerical.
    // Cost: O(p), p = chars of the path.
    pub(crate) fn io_path_numeric(&mut self, attributes: &AttrMap) -> Result<Value, RuntimeError> {
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let (_, _, bname) = Self::io_path_parts_spec(&p, attributes);
        let bname_val = Value::str(bname.clone());
        if crate::runtime::str_numeric::parse_raku_str_to_numeric(&bname).is_some() {
            // Coerce the basename to its natural numeric (so "3.5" -> Rat,
            // "1+1i" -> Complex): going straight to `Str.Num` would choke on a
            // complex-valued basename like "3+0i".
            self.call_method_with_values(bname_val, "Numeric", vec![])
        } else {
            let err = crate::runtime::utils::check_str_numeric(&bname_val)
                .err()
                .unwrap_or_else(|| {
                    crate::runtime::utils::str_numeric_error(&bname, 0, "malformed number")
                });
            Ok(self.fail_error_to_failure_value(&err))
        }
    }

    /// `IO::Path.child($name, :secure)`: the path of `$name` inside the
    /// directory. With `:secure` it verifies that the resulting path is a real
    /// child of the (completely resolved) parent: it fails with X::IO::Resolve
    /// when the parent or the child path cannot be completely resolved, and
    /// with X::IO::NotAChild when the resolved child escapes the parent
    /// directory. Path-deriving methods round-trip the receiver's class
    /// (`IO::Path::Win32.child` stays an `IO::Path::Win32`).
    // Cost: O(p + n), p = chars of the path, n = chars of the name, plus the
    // filesystem walk of `:secure`.
    pub(crate) fn io_path_child(
        &mut self,
        attributes: &AttrMap,
        class_name: Symbol,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let instance_cwd = attributes.get("cwd").map(|v| v.to_string_value());
        let path_buf = self.resolve_io_path_buf(attributes, &p);
        let cwd_path = self.get_cwd_path();
        let original = Path::new(&p);
        let child_name = Self::positional_value(args, 0)
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let joined = Self::io_path_join_child(attributes, &p, &child_name)?;
        let mut new_attrs = attributes.clone();
        new_attrs.insert("path".to_string(), Value::str(joined.clone()));
        let child = Value::make_instance(class_name, new_attrs);
        if Self::named_bool(args, "secure") {
            let parent_abs = if original.is_absolute() {
                path_buf.clone()
            } else if let Some(cwd) = &instance_cwd {
                PathBuf::from(cwd).join(original)
            } else {
                cwd_path.join(original)
            };
            let child_path = Path::new(&joined);
            let child_abs = if child_path.is_absolute() {
                child_path.to_path_buf()
            } else if let Some(cwd) = &instance_cwd {
                PathBuf::from(cwd).join(child_path)
            } else {
                cwd_path.join(child_path)
            };
            let res_parent = Self::resolve_io_path(&parent_abs, true, &p);
            let res_child = Self::resolve_io_path(&child_abs, true, &joined);
            match (res_parent, res_child) {
                (Err(_), _) | (_, Err(_)) => {
                    return Ok(self.make_io_failure("X::IO::Resolve", &p));
                }
                (Ok(rp), Ok(rc)) => {
                    let sep = Self::io_path_sep(attributes);
                    let prefix = format!("{}{}", rp, sep);
                    if !rc.starts_with(&prefix) || rc == rp {
                        return Ok(self.make_io_failure("X::IO::NotAChild", &joined));
                    }
                }
            }
        }
        Ok(child)
    }

    /// `IO::Path.resolve(:completely)`: the path with its symlinks and `..`
    /// resolved against the filesystem. `.resolve(:completely)` fails (returns
    /// a Failure) rather than throwing when the path cannot be fully resolved.
    // Cost: O(p) plus one filesystem query per path segment, p = path length.
    pub(crate) fn io_path_resolve(
        &mut self,
        attributes: &AttrMap,
        class_name: Symbol,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let path_buf = self.resolve_io_path_buf(attributes, &p);
        let completely = Self::named_bool(args, "completely");
        let resolved = match Self::resolve_io_path(&path_buf, completely, &p) {
            Ok(r) => r,
            Err(_) => return Ok(self.make_io_failure("X::IO::Resolve", &p)),
        };
        // A resolved path is absolute, so its CWD becomes the volume root
        // (the SPEC's dir separator on POSIX).
        let mut new_attrs = attributes.clone();
        new_attrs.insert("path".to_string(), Value::str(resolved));
        let sep = Self::io_path_sep(attributes).to_string();
        new_attrs.insert("cwd".to_string(), Value::str(sep));
        Ok(Value::make_instance(class_name, new_attrs))
    }

    /// `IO::Path.dir(:test)`: the entries of the directory. `sub dir` and this
    /// method are one body (`dir_listing`).
    // Cost: O(n), n = directory entries (plus the `test` smartmatch per entry).
    pub(crate) fn io_path_dir(
        &mut self,
        attributes: &AttrMap,
        class_name: Symbol,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let instance_cwd = attributes.get("cwd").map(|v| v.to_string_value());
        let test_opt = args.iter().find_map(|arg| match arg.view() {
            ValueView::Pair(key, value) if key == "test" => Some(value.clone()),
            _ => None,
        });
        self.dir_listing(Some(p), instance_cwd, test_opt, class_name)
    }

    /// The native methods of an `IO::Path` instance whose class has no shape in
    /// the method table (a user subclass, reached by its MRO). Every method
    /// `IO::Path` declares is a row of the table and is answered through its
    /// owner; what is left here is `Cool`'s numeric coercions, which take the
    /// path's basename as a number.
    // Cost: O(1) to find the row, plus the handler's own cost.
    pub(crate) fn native_io_path(
        &mut self,
        attributes: &AttrMap,
        class_name: &str,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // `Mu.perl` is `self.raku`, so the row of `raku` answers it.
        let row_method = if method == "perl" { "raku" } else { method };
        if let Some(result) = crate::builtins::method_table::invoke_owner(
            self,
            Symbol::intern("IO::Path"),
            row_method,
            &args,
            || Value::make_instance_without_destroy(Symbol::intern(class_name), attributes.clone()),
        ) {
            return result;
        }
        match method {
            // `Cool`'s `.Real`/`.Int`/`.Rat`/`.Num`/`.FatRat` fall out of
            // `.Numeric`: coerce to the natural numeric first, then to the
            // requested type. A basename that is not numerical is a `Failure`.
            // TODO: these are `Cool`'s rows (ADR-11276 3B remainder); move them
            // there when `IoPath` is opened to the `Cool` rows.
            "Real" | "Int" | "Rat" | "Num" | "FatRat" => {
                let numeric = self.io_path_numeric(attributes)?;
                let failed = matches!(numeric.view(), ValueView::Instance { class_name, .. }
                    if class_name == "Failure");
                if failed || method == "Real" {
                    Ok(numeric)
                } else {
                    self.call_method_with_values(numeric, method, args)
                }
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on IO::Path",
                method
            ))),
        }
    }
}
