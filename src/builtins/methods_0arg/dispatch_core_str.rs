/// String and text methods: words, codes, lines, trim, trim-leading, trim-trailing,
/// flip, so, not, is-lazy, lazy, chomp, chop, comb, fmt, join
use std::sync::Arc;

use crate::runtime;
use crate::value::{RuntimeError, Value};
use unicode_segmentation::UnicodeSegmentation;

use super::{fmt_0arg_item, is_value_lazy};

pub(super) fn dispatch(
    target: &Value,
    method: &str,
) -> Option<Option<Result<Value, RuntimeError>>> {
    match method {
        "words" => {
            let s = target.to_string_value();
            let words: Vec<Value> = s
                .split_whitespace()
                .map(|w| Value::str(w.to_string()))
                .collect();
            Some(Some(Ok(Value::Seq(std::sync::Arc::new(words)))))
        }
        "codes" => {
            let s = target.to_string_value();
            Some(Some(Ok(Value::Int(s.chars().count() as i64))))
        }
        "lines" => {
            // Skip for Supply instances -- handled by native Supply.lines
            if let Value::Instance { class_name, .. } = target
                && (class_name == "Supply"
                    || class_name == "IO::Handle"
                    || class_name == "IO::Path"
                    || class_name == "IO::Socket::INET")
            {
                return Some(None);
            }
            let s = target.to_string_value();
            let lines: Vec<Value> = crate::builtins::split_lines_chomped(&s)
                .into_iter()
                .map(Value::str)
                .collect();
            Some(Some(Ok(Value::Seq(Arc::new(lines)))))
        }
        "trim" => Some(Some(Ok(Value::str(
            target.to_string_value().trim().to_string(),
        )))),
        "trim-leading" => Some(Some(Ok(Value::str(
            target.to_string_value().trim_start().to_string(),
        )))),
        "trim-trailing" => Some(Some(Ok(Value::str(
            target.to_string_value().trim_end().to_string(),
        )))),
        "flip" => {
            let s = target.to_string_value();
            use unicode_normalization::UnicodeNormalization;
            let reversed: String = s.graphemes(true).rev().collect::<String>().nfc().collect();
            Some(Some(Ok(Value::str(reversed))))
        }
        "so" => {
            // Calling .so on a Failure marks it as handled
            if let Value::Instance { class_name, .. } = target
                && class_name == "Failure"
            {
                target.mark_failure_handled();
            }
            Some(Some(Ok(Value::Bool(target.truthy()))))
        }
        "not" => {
            // Calling .not on a Failure marks it as handled
            if let Value::Instance { class_name, .. } = target
                && class_name == "Failure"
            {
                target.mark_failure_handled();
            }
            Some(Some(Ok(Value::Bool(!target.truthy()))))
        }
        "is-lazy" => {
            // For Iterator instances, check the stored is_lazy attribute
            if let Value::Instance {
                class_name,
                attributes,
                ..
            } = target
                && class_name == "Iterator"
            {
                let lazy = matches!(attributes.get("is_lazy"), Some(Value::Bool(true)));
                return Some(Some(Ok(Value::Bool(lazy))));
            }
            Some(Some(Ok(Value::Bool(is_value_lazy(target)))))
        }
        "lazy" => {
            if is_value_lazy(target) {
                if let Value::LazyList(list) = target {
                    let mut env = list.env.clone();
                    env.insert(
                        "__mutsu_preserve_lazy_on_array_assign".to_string(),
                        Value::Bool(true),
                    );
                    let cache = list.cache.lock().unwrap().clone();
                    return Some(Some(Ok(Value::LazyList(std::sync::Arc::new(
                        crate::value::LazyList {
                            body: list.body.clone(),
                            env,
                            cache: std::sync::Mutex::new(cache),
                            compiled_code: list.compiled_code.clone(),
                            compiled_fns: list.compiled_fns.clone(),
                            elems_count: list.elems_count.clone(),
                        },
                    )))));
                }
                return Some(Some(Ok(target.clone())));
            }
            let items = if let Some(items) = target.as_list_items() {
                items.as_ref().clone()
            } else if matches!(
                target,
                Value::Range(..)
                    | Value::RangeExcl(..)
                    | Value::RangeExclStart(..)
                    | Value::RangeExclBoth(..)
                    | Value::GenericRange { .. }
            ) {
                crate::runtime::utils::value_to_list(target)
            } else if matches!(target, Value::Sub(..)) {
                // lazy { block } -- create a lazy thunk that evaluates the block on first access
                return Some(Some(Ok(Value::LazyThunk(std::sync::Arc::new(
                    crate::value::LazyThunkData {
                        thunk: target.clone(),
                        cache: std::sync::Mutex::new(None),
                    },
                )))));
            } else {
                return Some(Some(Ok(target.clone())));
            };
            let mut env = crate::env::Env::new();
            env.insert(
                "__mutsu_preserve_lazy_on_array_assign".to_string(),
                Value::Bool(true),
            );
            Some(Some(Ok(Value::LazyList(std::sync::Arc::new(
                crate::value::LazyList {
                    body: vec![],
                    env,
                    cache: std::sync::Mutex::new(Some(items)),
                    compiled_code: None,
                    compiled_fns: None,
                    elems_count: None,
                },
            )))))
        }
        "chomp" => {
            // IO::Handle.chomp is an attribute accessor, not the Str method
            if matches!(target, Value::Instance { class_name, .. } if class_name == "IO::Handle") {
                return Some(None);
            }
            Some(Some(Ok(Value::str(crate::builtins::chomp_one(
                &target.to_string_value(),
            )))))
        }
        "chop" => {
            if let Value::Package(type_name) = target {
                return Some(Some(Err(RuntimeError::new(format!(
                    "Cannot resolve caller chop({}:U)",
                    type_name,
                )))));
            }
            let mut s = target.to_string_value();
            s.pop();
            Some(Some(Ok(Value::str(s))))
        }
        "comb" => {
            let s = target.to_string_value();
            let parts: Vec<Value> = s
                .graphemes(true)
                .map(|g| Value::str(g.to_string()))
                .collect();
            Some(Some(Ok(Value::Seq(std::sync::Arc::new(parts)))))
        }
        "fmt" => {
            // .fmt() with no arguments: use default format and separator
            Some(match target {
                Value::Hash(items) => {
                    let rendered = items
                        .iter()
                        .map(|(k, v)| {
                            runtime::format_sprintf_args(
                                "%s\t%s",
                                &[Value::str(k.to_string()), v.clone()],
                            )
                        })
                        .collect::<Vec<_>>()
                        .join("\n");
                    Some(Ok(Value::str(rendered)))
                }
                Value::Pair(k, v) => {
                    let rendered = runtime::format_sprintf_args(
                        "%s\t%s",
                        &[Value::str(k.to_string()), *v.clone()],
                    );
                    Some(Ok(Value::str(rendered)))
                }
                Value::ValuePair(k, v) => {
                    let rendered =
                        runtime::format_sprintf_args("%s\t%s", &[*k.clone(), *v.clone()]);
                    Some(Ok(Value::str(rendered)))
                }
                _ if super::super::methods_narg::fmt_joinable_target(target) => {
                    let rendered = runtime::value_to_list(target)
                        .into_iter()
                        .map(|item| fmt_0arg_item(&item))
                        .collect::<Vec<_>>()
                        .join(" ");
                    Some(Ok(Value::str(rendered)))
                }
                _ => {
                    let rendered = runtime::format_sprintf("%s", Some(target));
                    Some(Ok(Value::str(rendered)))
                }
            })
        }
        "join" => {
            if matches!(target, Value::LazyList(_)) {
                return Some(None); // fall through to runtime to force
            }
            if crate::runtime::utils::is_shaped_array(target) {
                let leaves = crate::runtime::utils::shaped_array_leaves(target);
                let joined = leaves
                    .iter()
                    .map(|v| v.to_str_context())
                    .collect::<Vec<_>>()
                    .join("");
                return Some(Some(Ok(Value::str(joined))));
            }
            if let Some(items) = target.as_list_items() {
                // If any item is an Instance, fall through to runtime
                // so user-defined Str() methods can be called.
                if items.iter().any(|v| matches!(v, Value::Instance { .. })) {
                    return Some(None);
                }
                let joined = items
                    .iter()
                    .map(|v| v.to_str_context())
                    .collect::<Vec<_>>()
                    .join("");
                Some(Some(Ok(Value::str(joined))))
            } else {
                Some(Some(Ok(Value::str(target.to_string_value()))))
            }
        }
        _ => None,
    }
}
