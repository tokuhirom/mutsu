use super::*;

impl Interpreter {
    /// Whether `class_name` is one of the built-in `IO::Path` family classes
    /// (`IO::Path` plus the SPEC-variant subclasses) whose pure lexical methods
    /// the VM may dispatch natively via [`try_io_path_lexical`](Self::try_io_path_lexical).
    /// A user subclass that overrides a method is dispatched as compiled bytecode
    /// before the catch-all is reached, so restricting the native fast path to the
    /// built-in family keeps any user override authoritative.
    pub(crate) fn is_io_path_lexical_class(class_name: &str) -> bool {
        matches!(
            class_name,
            "IO::Path"
                | "IO::Path::Unix"
                | "IO::Path::Win32"
                | "IO::Path::Cygwin"
                | "IO::Path::QNX"
        )
    }

    /// Pure, lexical `IO::Path` methods: those that derive a new path (or a
    /// string/bool) purely from the receiver's `path`/`SPEC` *attributes* and the
    /// arguments, with no filesystem access, no cwd resolution (`&self`), and no
    /// env. This is the single authoritative implementation shared by the
    /// bytecode VM's catch-all native dispatch (`try_compiled_method_*`) and the
    /// interpreter's [`native_io_path`](Self::native_io_path), which delegates to
    /// it (`1 operation = 1 implementation`).
    ///
    /// Returns `None` (fall through to `native_io_path`'s richer handling) for any
    /// method that needs interpreter-owned state: filesystem stat/read/write
    /// (`e`/`f`/`d`/`slurp`/`spurt`/`open`/`resolve`/…), cwd-relative forms
    /// (`absolute`/`relative`/`CWD`/`raku`), numeric coercion that re-dispatches
    /// through `self` (`Numeric`/`Int`/…), and `child :secure` (which resolves the
    /// path against the filesystem). Path-deriving methods round-trip the concrete
    /// receiver class (`IO::Path::Win32.parent` stays `IO::Path::Win32`).
    pub(crate) fn try_io_path_lexical(
        class_name: &str,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let io_path_class = Symbol::intern(class_name);
        let p = attributes
            .get("path")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let original = Path::new(&p);
        let result = match method {
            "Str" => Ok(Value::str(p.clone())),
            "gist" => {
                let shown = if Self::io_path_is_absolute_spec(attributes, &p, original) {
                    if Self::is_win32_spec(attributes) {
                        Self::canonpath_win32(&p, false)
                    } else if Self::is_cygwin_spec(attributes) {
                        Self::canonpath_cygwin(&p.replace('\\', "/"), false)
                    } else {
                        p.clone()
                    }
                } else {
                    p.clone()
                };
                Ok(Value::str(format!("\"{}\".IO", shown.replace('"', "\\\""))))
            }
            "IO" => Ok(Value::make_instance(io_path_class, attributes.clone())),
            "SPEC" => {
                let spec_name = attributes
                    .get("SPEC")
                    .and_then(|s| match s {
                        Value::Package(n) => Some(n.resolve().to_string()),
                        Value::Instance { class_name, .. } => {
                            Some(class_name.resolve().to_string())
                        }
                        _ => None,
                    })
                    .unwrap_or_else(|| "IO::Spec::Unix".to_string());
                Ok(Value::Package(Symbol::intern(&spec_name)))
            }
            "basename" => {
                let (_, _, bname) = Self::io_path_parts_spec(&p, attributes);
                Ok(Value::str(bname))
            }
            "dirname" => {
                let (_, dname, _) = Self::io_path_parts_spec(&p, attributes);
                Ok(Value::str(dname))
            }
            "volume" => {
                let (volume, _, _) = Self::io_path_parts_spec(&p, attributes);
                Ok(Value::str(volume))
            }
            "cleanup" => {
                let normalized = if Self::is_win32_spec(attributes) {
                    Self::cleanup_io_path_lexical_win32(&p)
                } else {
                    Self::cleanup_io_path_lexical(&p)
                };
                Ok(Self::clone_io_path_with_path(
                    attributes,
                    io_path_class,
                    normalized,
                ))
            }
            "parts" => {
                let (volume, dirname, basename) = Self::io_path_parts_spec(&p, attributes);
                let mut parts = HashMap::new();
                parts.insert("volume".to_string(), Value::str(volume));
                parts.insert("dirname".to_string(), Value::str(dirname));
                parts.insert("basename".to_string(), Value::str(basename));
                Ok(Value::hash(parts))
            }
            "parent" => {
                let mut levels = 1i64;
                if let Some(Value::Int(i)) = args.first() {
                    if *i < 0 {
                        return Some(Err(RuntimeError::new(
                            "X::IO::ParentOutOfRange: Cannot go to a negative parent",
                        )));
                    }
                    levels = *i;
                }
                if levels == 0 {
                    return Some(Ok(Value::make_instance(io_path_class, attributes.clone())));
                }
                let sep = Self::io_path_sep(attributes);
                let mut path = if Self::is_cygwin_spec(attributes) {
                    p.replace('\\', "/")
                } else {
                    p.clone()
                };
                for _ in 0..levels {
                    if path == "." {
                        path = "..".to_string();
                        continue;
                    }
                    if path == ".." {
                        path = format!("..{}{}", sep, "..");
                        continue;
                    }
                    let (volume, dirname, _basename) = Self::io_path_parts(&path);
                    let full_vol_dir = format!("{}{}", volume, dirname);
                    if dirname == "/" || dirname == "\\" {
                        let new_path = format!("{}{}", volume, dirname);
                        if new_path == path {
                            break;
                        }
                        path = new_path;
                    } else if dirname == "." && volume.is_empty() {
                        path = ".".to_string();
                    } else {
                        path = full_vol_dir;
                    }
                }
                let mut new_attrs = attributes.clone();
                new_attrs.insert("path".to_string(), Value::str(path));
                Ok(Value::make_instance(io_path_class, new_attrs))
            }
            "sibling" => {
                let sibling_name = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let (volume, dirname, _) = Self::io_path_parts(&p);
                let dir_with_volume = format!("{}{}", volume, dirname);
                let sibling_path = if dir_with_volume == "/" || dir_with_volume == "\\" {
                    format!("{}{}", dir_with_volume, sibling_name)
                } else if dir_with_volume == "." {
                    sibling_name
                } else if dir_with_volume.ends_with('/') || dir_with_volume.ends_with('\\') {
                    format!("{}{}", dir_with_volume, sibling_name)
                } else {
                    format!("{}/{}", dir_with_volume, sibling_name)
                };
                Ok(Self::clone_io_path_with_path(
                    attributes,
                    io_path_class,
                    sibling_path,
                ))
            }
            // `.add($name)` and `.child($name)` without `:secure` are pure lexical
            // joins. `.child(:secure)` resolves against the filesystem and stays in
            // `native_io_path`.
            "add" | "child" => {
                if method == "child" && Self::named_bool(args, "secure") {
                    return None;
                }
                let child_name = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                match Self::io_path_join_child(attributes, &p, &child_name) {
                    Ok(joined) => {
                        let mut new_attrs = attributes.clone();
                        new_attrs.insert("path".to_string(), Value::str(joined));
                        Ok(Value::make_instance(io_path_class, new_attrs))
                    }
                    Err(e) => Err(e),
                }
            }
            "extension" => (|| -> Result<Value, RuntimeError> {
                let subst = Self::positional_value(args, 0).map(|v| v.to_string_value());
                let parts_spec = Self::io_path_extension_parts_spec(args)?;
                let selected_parts = parts_spec.select(Self::io_path_extension_part_count(&p));

                if let Some(subst) = subst {
                    let Some(parts_to_replace) = selected_parts else {
                        return Ok(Value::make_instance(io_path_class, attributes.clone()));
                    };
                    let joiner = Self::named_value(args, "joiner")
                        .map(|v| v.to_string_value())
                        .unwrap_or_else(|| {
                            if subst.is_empty() {
                                "".to_string()
                            } else {
                                ".".to_string()
                            }
                        });
                    let (dir_prefix, basename) = Self::split_path_for_extension(&p);
                    let Some(base_without_ext) =
                        Self::io_path_extension_strip_n_parts(basename, parts_to_replace)
                    else {
                        return Ok(Value::make_instance(io_path_class, attributes.clone()));
                    };
                    let mut new_basename = format!("{base_without_ext}{joiner}{subst}");
                    if new_basename.is_empty() {
                        new_basename = ".".to_string();
                    }
                    let mut new_attrs = attributes.clone();
                    new_attrs.insert(
                        "path".to_string(),
                        Value::str(format!("{dir_prefix}{new_basename}")),
                    );
                    Ok(Value::make_instance(io_path_class, new_attrs))
                } else {
                    let Some(parts) = selected_parts else {
                        return Ok(Value::str(String::new()));
                    };
                    Ok(Value::str(
                        Self::io_path_extension_with_n_parts(&p, parts).unwrap_or_default(),
                    ))
                }
            })(),
            "starts-with" => {
                let prefix = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Ok(Value::Bool(p.starts_with(&prefix)))
            }
            "is-absolute" => Ok(Value::Bool(Self::io_path_is_absolute_spec(
                attributes, &p, original,
            ))),
            "is-relative" => Ok(Value::Bool(!Self::io_path_is_absolute_spec(
                attributes, &p, original,
            ))),
            "succ" => {
                let (volume, dirname, basename) = Self::io_path_parts(&p);
                let new_basename = crate::builtins::str_increment::string_succ(&basename);
                let sep = Self::io_path_sep(attributes);
                let new_path = Self::join_io_path_parts(&volume, &dirname, &new_basename, sep);
                let mut new_attrs = attributes.clone();
                new_attrs.insert("path".to_string(), Value::str(new_path));
                Ok(Value::make_instance(io_path_class, new_attrs))
            }
            "pred" => {
                let (volume, dirname, basename) = Self::io_path_parts(&p);
                let new_basename = crate::builtins::str_increment::string_pred(&basename);
                let sep = Self::io_path_sep(attributes);
                let new_path = Self::join_io_path_parts(&volume, &dirname, &new_basename, sep);
                let mut new_attrs = attributes.clone();
                new_attrs.insert("path".to_string(), Value::str(new_path));
                Ok(Value::make_instance(io_path_class, new_attrs))
            }
            _ => return None,
        };
        Some(result)
    }
}
