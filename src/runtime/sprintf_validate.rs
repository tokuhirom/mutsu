use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};

/// Build rakudo's `X::Str::Sprintf::Directives::Count` message: the directive
/// count (`args_used`) is pluralized simply, the supplied count (`args_have`)
/// renders `0` as "no argument was" / `1` as "1 argument was" / `n` as
/// "n arguments were", the format is echoed after a newline, and the
/// interpolated-`$` hint is appended when fewer arguments were supplied than
/// directives expect.
pub(crate) fn directives_count_message(fmt: &str, args_used: usize, args_have: usize) -> String {
    let (have_count, have_unit, have_verb) = match args_have {
        0 => ("no".to_string(), "argument", "was"),
        1 => ("1".to_string(), "argument", "was"),
        n => (n.to_string(), "arguments", "were"),
    };
    let hint = if args_have < args_used {
        "  Are you using an interpolated '$'?"
    } else {
        ""
    };
    format!(
        "Your printf-style directives specify {} argument{}, but {} {} {}\nsupplied to format '{}'.{}",
        args_used,
        if args_used == 1 { "" } else { "s" },
        have_count,
        have_unit,
        have_verb,
        fmt,
        hint,
    )
}

/// Count the number of arguments a sprintf-style format string consumes.
/// This is the number of directives (excluding `%%`), counting `*` width/precision
/// as additional sequential args, and using the highest positional index when
/// `N$` positional directives are used. Used for `Format.count` / `Format.arity`.
pub(crate) fn sprintf_directive_count(fmt: &str) -> usize {
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
        // Positional argument specifier: N$
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
                pos += 1;
            } else {
                pos = start;
            }
        }
        // Flags
        while pos < len {
            let b = bytes[pos];
            if b == b'-' || b == b'+' || b == b' ' || b == b'#' || b == b'0' {
                pos += 1;
            } else {
                break;
            }
        }
        // Width
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
        // Precision
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
        // Conversion specifier
        if pos < len {
            pos += 1;
        }
        if let Some(p) = positional_arg {
            if p > max_positional {
                max_positional = p;
            }
        } else {
            sequential_args += 1;
        }
    }
    if uses_positional {
        max_positional
    } else {
        sequential_args
    }
}

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
        // Everything from here through the conversion char is the "directive"
        // reported in X::Str::Sprintf::Directives::Unsupported (Raku includes
        // flags / width / the vector flag in `.directive`, e.g. `%5vd` reports
        // directive `5vd`).
        let directive_start = pos;
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
        // Perl 5 vector flag (`%vd`, `%5vd`, ...) is not supported in Raku and
        // makes the whole directive invalid regardless of the following spec.
        let vector_flag = pos < len && (bytes[pos] == b'v' || bytes[pos] == b'V');
        if vector_flag {
            pos += 1;
        }
        // Read the conversion char as a full UTF-8 scalar so `pos` always lands
        // on a char boundary (the directive may be a non-ASCII char such as
        // `%♥`, which must still slice cleanly into `.directive` / `.sequence`).
        let spec = if pos < len {
            let s = fmt[pos..].chars().next().unwrap();
            pos += s.len_utf8();
            s
        } else {
            '?'
        };
        if vector_flag
            || !matches!(
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
            )
        {
            // Raku reports `.directive` as everything after the `%` (flags,
            // width, vector flag, conversion char) and `.sequence` as the FULL
            // original format string (not just the offending `%directive`), so
            // an embedded bad directive such as `a%vdb` reports the whole
            // `a%vdb`, matching the `... in sprintf format '<fmt>'` message.
            let directive = &fmt[directive_start..pos];
            let sequence = fmt.to_string();
            let message = format!(
                "Directive {} is not valid in sprintf format '{}'",
                directive, sequence
            );
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("directive".to_string(), Value::str(directive.to_string()));
            attrs.insert("sequence".to_string(), Value::str(sequence));
            attrs.insert("message".to_string(), Value::str(message.clone()));
            let mut err = RuntimeError::new(message);
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
        let message = directives_count_message(fmt, expected_args, arg_count);
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("args-have".to_string(), Value::Int(arg_count as i64));
        attrs.insert("args-used".to_string(), Value::Int(expected_args as i64));
        attrs.insert("message".to_string(), Value::str(message.clone()));
        let mut err = RuntimeError::new(message);
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
            let s = fmt[pos..].chars().next().unwrap();
            pos += s.len_utf8();
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
