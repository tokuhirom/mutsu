use crate::ast::Expr;
use crate::parser::parse_result::{PError, PResult};
use crate::symbol::Symbol;
use crate::value::Value;

/// Build the structured `X::Backslash::UnrecognizedSequence` parse error for an
/// unknown backslash escape (e.g. `\u`) inside an interpolating string.
pub(crate) fn unrecognized_backslash_perror(seq: char) -> PError {
    let msg = format!("Unrecognized backslash sequence: '\\{seq}'");
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("sequence".to_string(), Value::str(seq.to_string()));
    let ex = Value::make_instance(Symbol::intern("X::Backslash::UnrecognizedSequence"), attrs);
    PError::fatal_with_exception(msg, Box::new(ex))
}

/// Build the structured `X::Backslash::NonVariableDollar` parse error for a bare
/// `$` inside an interpolating string that does not introduce a variable.
pub(crate) fn non_variable_dollar_perror() -> PError {
    let msg = "Non-variable $ must be backslashed".to_string();
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Backslash::NonVariableDollar"), attrs);
    PError::fatal_with_exception(msg, Box::new(ex))
}

pub(crate) fn make_list_expr(items: Vec<Expr>) -> Expr {
    Expr::Call {
        name: Symbol::intern("list"),
        args: items,
    }
}

pub(crate) fn make_word_result_expr(items: Vec<Expr>) -> Expr {
    if items.len() == 1 {
        Expr::Call {
            name: Symbol::intern("__mutsu_qw_result"),
            args: items,
        }
    } else {
        make_list_expr(items)
    }
}

pub(crate) fn quotewords_literal_marker(s: String) -> Expr {
    Expr::Literal(Value::Scalar(Box::new(Value::Pair(
        "__mutsu_qw_literal".to_string(),
        Box::new(Value::str(s)),
    ))))
}

pub(crate) fn quotewords_atom_expr(atom_expr: Expr) -> Expr {
    quotewords_atom_expr_allomorphic(atom_expr, true)
}

pub(crate) fn quotewords_atom_expr_allomorphic(atom_expr: Expr, allomorphic: bool) -> Expr {
    let args = match atom_expr {
        Expr::StringInterpolation(parts) => parts
            .into_iter()
            .map(|part| match part {
                Expr::Literal(Value::Str(s)) => quotewords_literal_marker(s.to_string()),
                other => other,
            })
            .collect(),
        other => vec![other],
    };
    let func_name = if allomorphic {
        "__mutsu_quotewords_atom"
    } else {
        "__mutsu_words_atom"
    };
    Expr::Call {
        name: Symbol::intern(func_name),
        args,
    }
}

/// Map a Unicode opening bracket to its closing counterpart (public for sibling modules).
pub(crate) fn unicode_bracket_close_pub(open: char) -> Option<char> {
    unicode_bracket_close(open)
}

/// Map a Unicode opening bracket to its closing counterpart.
/// Only includes pairs that Raku recognizes for quoting.
pub(crate) fn unicode_bracket_close(open: char) -> Option<char> {
    match open {
        '\u{0028}' => Some('\u{0029}'), // ( )
        '\u{003C}' => Some('\u{003E}'), // < >
        '\u{005B}' => Some('\u{005D}'), // [ ]
        '\u{007B}' => Some('\u{007D}'), // { }
        '\u{00AB}' => Some('\u{00BB}'), // « »
        '\u{0F3A}' => Some('\u{0F3B}'), // ༺ ༻
        '\u{0F3C}' => Some('\u{0F3D}'), // ༼ ༽
        '\u{169B}' => Some('\u{169C}'), // ᚛ ᚜
        '\u{2018}' => Some('\u{2019}'), // ' '
        '\u{201A}' => Some('\u{2019}'), // ‚ '
        '\u{201C}' => Some('\u{201D}'), // " "
        '\u{201E}' => Some('\u{201D}'), // „ "
        '\u{2039}' => Some('\u{203A}'), // ‹ ›
        '\u{2045}' => Some('\u{2046}'), // ⁅ ⁆
        '\u{207D}' => Some('\u{207E}'), // ⁽ ⁾
        '\u{208D}' => Some('\u{208E}'), // ₍ ₎
        '\u{2308}' => Some('\u{2309}'), // ⌈ ⌉
        '\u{230A}' => Some('\u{230B}'), // ⌊ ⌋
        '\u{2329}' => Some('\u{232A}'), // 〈 〉
        '\u{2768}' => Some('\u{2769}'), // ❨ ❩
        '\u{276A}' => Some('\u{276B}'), // ❪ ❫
        '\u{276C}' => Some('\u{276D}'), // ❬ ❭
        '\u{276E}' => Some('\u{276F}'), // ❮ ❯
        '\u{2770}' => Some('\u{2771}'), // ❰ ❱
        '\u{2772}' => Some('\u{2773}'), // ❲ ❳
        '\u{2774}' => Some('\u{2775}'), // ❴ ❵
        '\u{27C5}' => Some('\u{27C6}'), // ⟅ ⟆
        '\u{27E6}' => Some('\u{27E7}'), // ⟦ ⟧
        '\u{27E8}' => Some('\u{27E9}'), // ⟨ ⟩
        '\u{27EA}' => Some('\u{27EB}'), // ⟪ ⟫
        '\u{27EC}' => Some('\u{27ED}'), // ⟬ ⟭
        '\u{27EE}' => Some('\u{27EF}'), // ⟮ ⟯
        '\u{2983}' => Some('\u{2984}'), // ⦃ ⦄
        '\u{2985}' => Some('\u{2986}'), // ⦅ ⦆
        '\u{2987}' => Some('\u{2988}'), // ⦇ ⦈
        '\u{2989}' => Some('\u{298A}'), // ⦉ ⦊
        '\u{298B}' => Some('\u{298C}'), // ⦋ ⦌
        '\u{298D}' => Some('\u{2990}'), // ⦍ ⦐
        '\u{298F}' => Some('\u{298E}'), // ⦏ ⦎
        '\u{2991}' => Some('\u{2992}'), // ⦑ ⦒
        '\u{2993}' => Some('\u{2994}'), // ⦓ ⦔
        '\u{2995}' => Some('\u{2996}'), // ⦕ ⦖
        '\u{2997}' => Some('\u{2998}'), // ⦗ ⦘
        '\u{29D8}' => Some('\u{29D9}'), // ⧘ ⧙
        '\u{29DA}' => Some('\u{29DB}'), // ⧚ ⧛
        '\u{29FC}' => Some('\u{29FD}'), // ⧼ ⧽
        '\u{2E22}' => Some('\u{2E23}'), // ⸢ ⸣
        '\u{2E24}' => Some('\u{2E25}'), // ⸤ ⸥
        '\u{2E26}' => Some('\u{2E27}'), // ⸦ ⸧
        '\u{2E28}' => Some('\u{2E29}'), // ⸨ ⸩
        '\u{3008}' => Some('\u{3009}'), // 〈 〉
        '\u{300A}' => Some('\u{300B}'), // 《 》
        '\u{300C}' => Some('\u{300D}'), // 「 」
        '\u{300E}' => Some('\u{300F}'), // 『 』
        '\u{3010}' => Some('\u{3011}'), // 【 】
        '\u{3014}' => Some('\u{3015}'), // 〔 〕
        '\u{3016}' => Some('\u{3017}'), // 〖 〗
        '\u{3018}' => Some('\u{3019}'), // 〘 〙
        '\u{301A}' => Some('\u{301B}'), // 〚 〛
        '\u{301D}' => Some('\u{301E}'), // 〝 〞
        '\u{FD3E}' => Some('\u{FD3F}'), // ﴾ ﴿
        '\u{FE17}' => Some('\u{FE18}'), // ︗ ︘
        '\u{FE35}' => Some('\u{FE36}'), // ︵ ︶
        '\u{FE37}' => Some('\u{FE38}'), // ︷ ︸
        '\u{FE39}' => Some('\u{FE3A}'), // ︹ ︺
        '\u{FE3B}' => Some('\u{FE3C}'), // ︻ ︼
        '\u{FE3D}' => Some('\u{FE3E}'), // ︽ ︾
        '\u{FE3F}' => Some('\u{FE40}'), // ︿ ﹀
        '\u{FE41}' => Some('\u{FE42}'), // ﹁ ﹂
        '\u{FE43}' => Some('\u{FE44}'), // ﹃ ﹄
        '\u{FE47}' => Some('\u{FE48}'), // ﹇ ﹈
        '\u{FE59}' => Some('\u{FE5A}'), // ﹙ ﹚
        '\u{FE5B}' => Some('\u{FE5C}'), // ﹛ ﹜
        '\u{FE5D}' => Some('\u{FE5E}'), // ﹝ ﹞
        '\u{FF08}' => Some('\u{FF09}'), // （ ）
        '\u{FF3B}' => Some('\u{FF3D}'), // ［ ］
        '\u{FF5B}' => Some('\u{FF5D}'), // ｛ ｝
        '\u{FF5F}' => Some('\u{FF60}'), // ｟ ｠
        '\u{FF62}' => Some('\u{FF63}'), // ｢ ｣
        _ => None,
    }
}

/// Read a bracketed string with nesting support (e.g., `{...{...}...}`).
/// When `allow_escape` is true, `\x` consumes both chars so escaped delimiters
/// do not affect nesting.
pub(crate) fn read_bracketed(
    input: &str,
    open: char,
    close: char,
    allow_escape: bool,
) -> PResult<'_, &str> {
    if !input.starts_with(open) {
        return Err(PError::expected(&format!("'{}'", open)));
    }
    let mut rest = &input[open.len_utf8()..];
    let start = rest;
    let mut depth = 1u32;
    loop {
        if rest.is_empty() {
            return Err(PError::expected(&format!("closing '{}'", close)));
        }
        let ch = rest.chars().next().unwrap();
        if allow_escape && ch == '\\' && rest.len() > 1 {
            let next_ch = rest[1..].chars().next().unwrap();
            rest = &rest[1 + next_ch.len_utf8()..];
            continue;
        }
        if ch == open {
            depth += 1;
        } else if ch == close {
            depth -= 1;
            if depth == 0 {
                let content = &start[..start.len() - rest.len()];
                return Ok((&rest[close.len_utf8()..], content));
            }
        }
        rest = &rest[ch.len_utf8()..];
    }
}

/// Read a multi-character bracketed string (e.g. `{{ ... }}`, `[[ ... ]]`).
/// `open_str` and `close_str` are the multi-character delimiters (e.g. "{{" and "}}").
/// Nested occurrences of `open_str` and `close_str` are tracked for proper balancing.
/// When `allow_escape` is true, `\x` consumes both chars so escaped delimiters
/// do not affect nesting.
pub(crate) fn read_multi_bracketed<'a>(
    input: &'a str,
    open_str: &str,
    close_str: &str,
    allow_escape: bool,
) -> PResult<'a, &'a str> {
    if !input.starts_with(open_str) {
        return Err(PError::expected(&format!("'{}'", open_str)));
    }
    let mut rest = &input[open_str.len()..];
    let start = rest;
    let mut depth = 1u32;
    loop {
        if rest.is_empty() {
            return Err(PError::expected(&format!("closing '{}'", close_str)));
        }
        if allow_escape && rest.starts_with('\\') && rest.len() > 1 {
            // Skip escape sequence (backslash + next char)
            let next_ch = rest[1..].chars().next().unwrap();
            rest = &rest[1 + next_ch.len_utf8()..];
            continue;
        }
        if rest.starts_with(open_str) {
            depth += 1;
            rest = &rest[open_str.len()..];
            continue;
        }
        if rest.starts_with(close_str) {
            depth -= 1;
            if depth == 0 {
                let content = &start[..start.len() - rest.len()];
                return Ok((&rest[close_str.len()..], content));
            }
            rest = &rest[close_str.len()..];
            continue;
        }
        let ch = rest.chars().next().unwrap();
        rest = &rest[ch.len_utf8()..];
    }
}

/// Count how many times the given bracket char is repeated at the start of `input`.
pub(crate) fn count_repeated_bracket(input: &str, ch: char) -> usize {
    let mut count = 0;
    for c in input.chars() {
        if c == ch {
            count += 1;
        } else {
            break;
        }
    }
    count
}

/// Read delimited content from input. Handles bracket-style and symmetric delimiters.
pub(crate) fn read_delimited_content<'a>(
    input: &'a str,
    escape_backslash: bool,
) -> PResult<'a, &'a str> {
    let rest = input.trim_start_matches(' ');
    let delim_char = rest
        .chars()
        .next()
        .ok_or_else(|| PError::expected("Q string delimiter"))?;
    if delim_char.is_alphanumeric() || delim_char.is_whitespace() || delim_char == '#' {
        return Err(PError::expected("Q string delimiter"));
    }
    let bracket_close = match delim_char {
        '{' => Some('}'),
        '[' => Some(']'),
        '(' => Some(')'),
        '<' => Some('>'),
        _ => unicode_bracket_close(delim_char),
    };
    if let Some(close_char) = bracket_close {
        let repeat_count = count_repeated_bracket(rest, delim_char);
        if repeat_count > 1 {
            let open_str: String = std::iter::repeat_n(delim_char, repeat_count).collect();
            let close_str: String = std::iter::repeat_n(close_char, repeat_count).collect();
            return read_multi_bracketed(rest, &open_str, &close_str, escape_backslash);
        }
        return read_bracketed(rest, delim_char, close_char, escape_backslash);
    }
    // Non-bracket delimiter
    let after_open = &rest[delim_char.len_utf8()..];
    let end = if escape_backslash {
        find_unescaped(after_open, delim_char)
    } else {
        after_open.find(delim_char)
    }
    .ok_or_else(|| PError::expected("closing Q delimiter"))?;
    let content = &after_open[..end];
    let after = &after_open[end + delim_char.len_utf8()..];
    Ok((after, content))
}

pub(crate) fn quote_delimiters(input: &str) -> Option<(char, char)> {
    let rest = input.trim_start_matches(' ');
    let open = rest.chars().next()?;
    let close = match open {
        '{' => '}',
        '[' => ']',
        '(' => ')',
        '<' => '>',
        _ => unicode_bracket_close(open).unwrap_or(open),
    };
    Some((open, close))
}

/// Find the position of the first unescaped occurrence of `delim` in `s`.
/// A `\` before the delimiter prevents it from matching.
pub(crate) fn find_unescaped(s: &str, delim: char) -> Option<usize> {
    let mut chars = s.char_indices();
    while let Some((i, ch)) = chars.next() {
        if ch == '\\' {
            chars.next(); // skip escaped character
            continue;
        }
        if ch == delim {
            return Some(i);
        }
    }
    None
}

/// Process escape sequences in q-string content for a given delimiter.
/// Only `\\` → `\` and `\<delim>` → `<delim>` are recognized.
pub(crate) fn process_q_escapes(content: &str, delim: char) -> String {
    let mut result = String::new();
    let mut chars = content.chars();
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            if let Some(next) = chars.next() {
                if next == '\\' || next == delim {
                    result.push(next);
                } else {
                    result.push('\\');
                    result.push(next);
                }
            } else {
                result.push('\\');
            }
        } else {
            result.push(ch);
        }
    }
    result
}
