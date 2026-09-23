use super::*;

impl Interpreter {
    /// Build an X::Str::Trans::InvalidArg error for a non-Pair argument passed
    /// to `.trans`. `got` is the offending value's type object.
    pub(super) fn str_trans_invalid_arg_error(&self, arg: &Value) -> RuntimeError {
        let type_name = crate::value::types::what_type_name(arg);
        let msg = format!(
            "Only Pair objects are allowed as arguments to Str.trans, got {}",
            type_name
        );
        let mut attrs = std::collections::HashMap::new();
        attrs.insert(
            "got".to_string(),
            Value::package(crate::symbol::Symbol::intern(&type_name)),
        );
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        let mut err = RuntimeError::new(msg);
        err.exception = Some(Box::new(Value::make_instance(
            crate::symbol::Symbol::intern("X::Str::Trans::InvalidArg"),
            attrs,
        )));
        err
    }

    /// Build an X::Str::Trans::IllegalKey error for a substitution key element
    /// that is not a Str or Regex. `key` is the offending value.
    pub(super) fn str_trans_illegal_key_error(&self, key: &Value) -> RuntimeError {
        let type_name = crate::value::types::what_type_name(key);
        let msg = format!(
            "in Str.trans, got illegal substitution key of type {} (should be a Regex or Str)",
            type_name
        );
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("key".to_string(), key.clone());
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        let mut err = RuntimeError::new(msg);
        err.exception = Some(Box::new(Value::make_instance(
            crate::symbol::Symbol::intern("X::Str::Trans::IllegalKey"),
            attrs,
        )));
        err
    }

    pub(super) fn parse_trans_pair(&self, key: &str, value: &Value) -> TransRule {
        // Detect regex keys: when a Regex value is used as a Pair key,
        // it gets stringified to `/pattern/` by MakePair.
        if let Some(pattern) = extract_regex_pattern(key) {
            return TransRule::Regex {
                pattern: pattern.to_string(),
                replacement: value.to_string_value(),
            };
        }

        let to_list = value_to_string_list(value);
        let from_list = value_to_string_list(&Value::str(key.to_string()));

        // A multi-char TO entry (e.g. a `\r\n` grapheme unit from
        // `expand_trans_spec`) can never be represented by `CharMap`'s
        // one-`char`-per-position `to_chars`, which would silently truncate
        // it to just its first character. Route it through `TokenMap`
        // instead, the same as a multi-char FROM entry already is.
        let has_multichar = from_list.iter().any(|s| s.chars().count() > 1)
            || to_list.iter().any(|s| s.chars().count() > 1);

        if has_multichar {
            TransRule::TokenMap {
                from_tokens: from_list,
                to_tokens: to_list,
            }
        } else {
            let from_chars: Vec<char> = from_list.iter().filter_map(|s| s.chars().next()).collect();
            let to_chars: Vec<char> = to_list.iter().filter_map(|s| s.chars().next()).collect();
            // The Str=>Str first-multi form cycles a short replacement to the
            // key length (`'123' => 'þð'` gives `þðþ`); a Str=>list/range target
            // is dispatched to the list form, which repeats the last char.
            let cycle = matches!(value.view(), ValueView::Str(_));
            TransRule::CharMap {
                from_chars,
                to_chars,
                cycle,
            }
        }
    }
}
