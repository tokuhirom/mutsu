use super::super::*;
#[cfg(feature = "pcre2")]
use pcre2::bytes::{Regex as PcreRegex, RegexBuilder as PcreRegexBuilder};

impl Interpreter {
    pub(in crate::runtime) fn interpolate_regex_pattern(&self, pattern: &str) -> String {
        fn take_regex_interpolation_name(input: &str) -> Option<(&str, String)> {
            let mut chars = input.char_indices();
            let mut end = if let Some((_, first)) = chars.next() {
                if matches!(first, '*' | '?' | '!' | '^')
                    || first.is_ascii_alphabetic()
                    || first == '_'
                {
                    first.len_utf8()
                } else {
                    return None;
                }
            } else {
                return None;
            };
            for (idx, ch) in chars {
                if ch.is_ascii_alphanumeric() || matches!(ch, '_' | ':' | '-') {
                    end = idx + ch.len_utf8();
                } else {
                    break;
                }
            }
            let name = input[..end].to_string();
            Some((&input[end..], name))
        }

        let mut out = String::new();
        let mut i = 0usize;
        while i < pattern.len() {
            let rest = &pattern[i..];
            if let Some(escaped) = rest.strip_prefix('\\')
                && let Some(ch) = escaped.chars().next()
            {
                out.push('\\');
                out.push(ch);
                i += 1 + ch.len_utf8();
                continue;
            }
            if let Some(after_dollar) = rest.strip_prefix("${")
                && let Some(close_idx) = after_dollar.find('}')
            {
                let name = &after_dollar[..close_idx];
                let value = self.env.get(name).cloned().unwrap_or(Value::Nil);
                out.push_str(&value.to_string_value());
                i += 2 + close_idx + 1;
                continue;
            }
            if let Some(after_dollar) = rest.strip_prefix('$')
                && let Some((var_rest, name)) = take_regex_interpolation_name(after_dollar)
            {
                let value = self.env.get(&name).cloned().unwrap_or(Value::Nil);
                out.push_str(&value.to_string_value());
                i += 1 + after_dollar.len() - var_rest.len();
                continue;
            }
            let ch = rest.chars().next().unwrap();
            out.push(ch);
            i += ch.len_utf8();
        }
        out
    }

    fn p5_pattern_to_rust_regex(pattern: &str) -> String {
        let chars: Vec<char> = pattern.chars().collect();
        let mut i = 0usize;
        let mut out = String::new();
        while i < chars.len() {
            if chars[i] == '(' && i + 3 < chars.len() && chars[i + 1] == '?' {
                if chars[i + 2] == '<' {
                    let mut j = i + 3;
                    let mut name = String::new();
                    while j < chars.len() && chars[j] != '>' {
                        name.push(chars[j]);
                        j += 1;
                    }
                    if j < chars.len() && !name.is_empty() {
                        out.push_str("(?P<");
                        out.push_str(&name);
                        out.push('>');
                        i = j + 1;
                        continue;
                    }
                } else if chars[i + 2] == '\'' {
                    let mut j = i + 3;
                    let mut name = String::new();
                    while j < chars.len() && chars[j] != '\'' {
                        name.push(chars[j]);
                        j += 1;
                    }
                    if j < chars.len() && !name.is_empty() {
                        out.push_str("(?P<");
                        out.push_str(&name);
                        out.push('>');
                        i = j + 1;
                        continue;
                    }
                }
            }
            out.push(chars[i]);
            i += 1;
        }
        out
    }

    fn expand_p5_interpolation(&self, pattern: &str) -> String {
        let chars: Vec<char> = pattern.chars().collect();
        let mut out = String::new();
        let mut i = 0usize;
        while i < chars.len() {
            if chars[i] == '\\' {
                out.push(chars[i]);
                i += 1;
                if i < chars.len() {
                    out.push(chars[i]);
                    i += 1;
                }
                continue;
            }
            if chars[i] == '$'
                && i + 1 < chars.len()
                && (chars[i + 1] == '_' || chars[i + 1].is_ascii_alphabetic())
            {
                let mut j = i + 1;
                while j < chars.len() && (chars[j] == '_' || chars[j].is_ascii_alphanumeric()) {
                    j += 1;
                }
                let name: String = chars[i + 1..j].iter().collect();
                if let Some(value) = self.env.get(&name) {
                    out.push_str(&value.to_string_value());
                }
                i = j;
                continue;
            }
            out.push(chars[i]);
            i += 1;
        }
        out
    }

    #[cfg(feature = "pcre2")]
    pub(in crate::runtime) fn compile_p5_regex(&self, pattern: &str) -> Option<PcreRegex> {
        let interpolated = self.expand_p5_interpolation(pattern);
        let converted = Self::p5_pattern_to_rust_regex(&interpolated);
        let transformed = Self::transform_p5_pattern(&converted);
        let mut builder = PcreRegexBuilder::new();
        builder.utf(true).ucp(true);
        builder.build(&transformed).ok()
    }
}
