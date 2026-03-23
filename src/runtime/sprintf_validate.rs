use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};

/// Validate sprintf format directives. Throws typed exceptions for:
/// - Unsupported directives (X::Str::Sprintf::Directives::Unsupported)
/// - Arg count mismatch (X::Str::Sprintf::Directives::Count)
pub(crate) fn validate_sprintf_directives(fmt: &str, arg_count: usize) -> Result<(), RuntimeError> {
    let bytes = fmt.as_bytes();
    let len = bytes.len();
    let mut pos = 0usize;
    let mut sequential_args = 0usize;
    let mut max_positional = 0usize;
    let mut uses_positional = false;
    while pos < len {
        if bytes[pos] != b'%' {
            pos += 1;
            continue;
        }
        pos += 1; // skip '%'
        if pos < len && bytes[pos] == b'%' {
            pos += 1;
            continue;
        }
        // Check for positional argument specifier: N$ (digits followed by '$')
        let mut positional_arg = None;
        {
            let start = pos;
            while pos < len && bytes[pos].is_ascii_digit() {
                pos += 1;
            }
            if pos > start && pos < len && bytes[pos] == b'$' {
                let n: usize = fmt[start..pos].parse().unwrap_or(0);
                positional_arg = Some(n);
                uses_positional = true;
                pos += 1; // skip '$'
            } else {
                pos = start; // reset
            }
        }
        // Skip flags
        while pos < len {
            let b = bytes[pos];
            if b == b'-' || b == b'+' || b == b' ' || b == b'#' || b == b'0' {
                pos += 1;
            } else {
                break;
            }
        }
        // Skip width
        if pos < len && bytes[pos] == b'*' {
            pos += 1;
            if positional_arg.is_none() {
                sequential_args += 1;
            }
        } else {
            while pos < len && bytes[pos].is_ascii_digit() {
                pos += 1;
            }
        }
        // Skip precision
        if pos < len && bytes[pos] == b'.' {
            pos += 1;
            if pos < len && bytes[pos] == b'*' {
                pos += 1;
                if positional_arg.is_none() {
                    sequential_args += 1;
                }
            } else {
                while pos < len && bytes[pos].is_ascii_digit() {
                    pos += 1;
                }
            }
        }
        let spec = if pos < len {
            let s = bytes[pos] as char;
            pos += 1;
            s
        } else {
            '?'
        };
        if !matches!(
            spec,
            's' | 'd'
                | 'i'
                | 'u'
                | 'x'
                | 'X'
                | 'o'
                | 'b'
                | 'B'
                | 'f'
                | 'F'
                | 'e'
                | 'E'
                | 'g'
                | 'G'
                | 'c'
        ) {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("directive".to_string(), Value::str(format!("%{}", spec)));
            attrs.insert(
                "message".to_string(),
                Value::str(format!(
                    "Directive {} is not valid in a sprintf format",
                    spec
                )),
            );
            let mut err = RuntimeError::new(format!(
                "Directive {} is not valid in a sprintf format",
                spec
            ));
            err.exception = Some(Box::new(Value::make_instance(
                Symbol::intern("X::Str::Sprintf::Directives::Unsupported"),
                attrs,
            )));
            return Err(err);
        }
        if let Some(p) = positional_arg {
            if p > max_positional {
                max_positional = p;
            }
        } else {
            sequential_args += 1;
        }
    }
    // When positional args are used, expected_args is the max position referenced
    let expected_args = if uses_positional {
        max_positional
    } else {
        sequential_args
    };
    if expected_args != arg_count {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("args-have".to_string(), Value::Int(arg_count as i64));
        attrs.insert("args-used".to_string(), Value::Int(expected_args as i64));
        attrs.insert(
            "message".to_string(),
            Value::str(format!(
                "Your printf-style directives specify {} arguments, but {} argument{} {} supplied",
                expected_args,
                arg_count,
                if arg_count == 1 { "" } else { "s" },
                if arg_count == 1 { "was" } else { "were" },
            )),
        );
        let mut err = RuntimeError::new(format!(
            "Your printf-style directives specify {} arguments, but {} argument{} {} supplied",
            expected_args,
            arg_count,
            if arg_count == 1 { "" } else { "s" },
            if arg_count == 1 { "was" } else { "were" },
        ));
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::Str::Sprintf::Directives::Count"),
            attrs,
        )));
        return Err(err);
    }
    Ok(())
}

/// Validate that argument types are compatible with their corresponding directives.
/// Throws X::Str::Sprintf::Directives::BadType for incompatible types (e.g., Junction with %d).
pub(crate) fn validate_sprintf_arg_types(fmt: &str, args: &[Value]) -> Result<(), RuntimeError> {
    let bytes = fmt.as_bytes();
    let len = bytes.len();
    let mut pos = 0usize;
    let mut arg_index = 0usize;
    while pos < len {
        if bytes[pos] != b'%' {
            pos += 1;
            continue;
        }
        pos += 1;
        if pos < len && bytes[pos] == b'%' {
            pos += 1;
            continue;
        }
        // Check for positional argument specifier: N$
        let mut positional_arg: Option<usize> = None;
        {
            let start = pos;
            while pos < len && bytes[pos].is_ascii_digit() {
                pos += 1;
            }
            if pos > start && pos < len && bytes[pos] == b'$' {
                let n: usize = fmt[start..pos].parse().unwrap_or(1);
                positional_arg = Some(n - 1);
                pos += 1;
            } else {
                pos = start;
            }
        }
        // Skip flags
        while pos < len {
            let b = bytes[pos];
            if b == b'-' || b == b'+' || b == b' ' || b == b'#' || b == b'0' {
                pos += 1;
            } else {
                break;
            }
        }
        // Skip width
        if pos < len && bytes[pos] == b'*' {
            pos += 1;
            if positional_arg.is_none() {
                arg_index += 1;
            }
        } else {
            while pos < len && bytes[pos].is_ascii_digit() {
                pos += 1;
            }
        }
        // Skip precision
        if pos < len && bytes[pos] == b'.' {
            pos += 1;
            if pos < len && bytes[pos] == b'*' {
                pos += 1;
                if positional_arg.is_none() {
                    arg_index += 1;
                }
            } else {
                while pos < len && bytes[pos].is_ascii_digit() {
                    pos += 1;
                }
            }
        }
        let spec = if pos < len {
            let s = bytes[pos] as char;
            pos += 1;
            s
        } else {
            '?'
        };
        let effective_index = positional_arg.unwrap_or(arg_index);
        if let Some(arg) = args.get(effective_index)
            && matches!(arg, Value::Junction { .. })
        {
            let type_name = "Junction";
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("type".to_string(), Value::str(type_name.to_string()));
            attrs.insert("directive".to_string(), Value::str(format!("%{}", spec)));
            attrs.insert(
                "message".to_string(),
                Value::str(format!(
                    "Directive %{} not applicable for type {}",
                    spec, type_name
                )),
            );
            let mut err = RuntimeError::new(format!(
                "Directive %{} not applicable for type {}",
                spec, type_name
            ));
            err.exception = Some(Box::new(Value::make_instance(
                Symbol::intern("X::Str::Sprintf::Directives::BadType"),
                attrs,
            )));
            return Err(err);
        }
        if positional_arg.is_none() {
            arg_index += 1;
        }
    }
    Ok(())
}
