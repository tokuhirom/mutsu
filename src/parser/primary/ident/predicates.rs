use crate::parser::helpers::consume_unspace;
use crate::parser::parse_result::{PError, take_while1};
use crate::symbol::Symbol;
use crate::value::Value;

/// Check if input looks like it contains a binding operator `:=` or `::=`
/// before a statement terminator. Used to decide whether `try STMT` should
/// attempt parsing as an assignment/binding statement.
pub(crate) fn looks_like_binding(input: &str) -> bool {
    // Scan for `:=` before `;`, `}`, or end of input. Snap the 200-byte window
    // down to the nearest char boundary so a multi-byte char (e.g. `⚛`) straddling
    // byte 200 doesn't panic the slice.
    let mut search_len = input.len().min(200);
    while search_len > 0 && !input.is_char_boundary(search_len) {
        search_len -= 1;
    }
    let search = &input[..search_len];
    for (i, c) in search.char_indices() {
        if c == ';' || c == '}' {
            return false;
        }
        if c == ':' && search[i + 1..].starts_with('=') {
            return true;
        }
    }
    false
}

/// Check if input starts with a term keyword (like `i`, `e`, `pi`, etc.)
/// that can appear as a listop argument without parentheses.
pub(crate) fn starts_with_term_keyword(input: &str) -> bool {
    let first = input.chars().next().unwrap_or('\0');
    if first == '\u{03C0}' || first == '\u{03C4}' || first == '\u{1D452}' {
        return true;
    }
    let word_end = input
        .find(|c: char| !c.is_alphanumeric() && c != '_')
        .unwrap_or(input.len());
    let word = &input[..word_end];
    matches!(
        word,
        "i" | "e"
            | "pi"
            | "tau"
            | "Inf"
            | "NaN"
            | "True"
            | "False"
            | "Nil"
            | "self"
            | "Any"
            | "Mu"
            | "now"
            | "time"
            | "rand"
    )
}

/// Check if a name is a Raku keyword (not a function call).
pub(crate) fn is_keyword(name: &str) -> bool {
    matches!(
        name,
        "if" | "unless"
            | "for"
            | "while"
            | "until"
            | "given"
            | "when"
            | "loop"
            | "repeat"
            | "try"
            | "do"
            | "gather"
            | "sub"
            | "my"
            | "our"
            | "has"
            | "class"
            | "role"
            | "module"
            | "use"
            | "need"
            | "import"
            | "require"
            | "return"
            | "last"
            | "next"
            | "redo"
            | "die"
            | "say"
            | "print"
            | "put"
            | "note"
            | "with"
            | "without"
            | "supply"
            | "react"
            | "whenever"
            | "start"
            | "quietly"
            | "sink"
            | "list"
            | "cache"
            | "let"
    )
}

/// Check if a name is a listop (can take args without parens).
pub(crate) fn is_listop(name: &str) -> bool {
    matches!(
        name,
        "shift"
            | "unshift"
            | "push"
            | "pop"
            | "grep"
            | "map"
            | "sort"
            | "classify"
            | "categorize"
            | "any"
            | "all"
            | "none"
            | "one"
            | "print"
            | "say"
            | "put"
            | "note"
            | "return"
            | "die"
            | "fail"
            | "warn"
            | "make"
            | "take"
            | "take-rw"
            | "emit"
            | "split"
            | "index"
            | "indices"
            | "join"
            | "abs"
            | "exp"
            | "log"
            | "log2"
            | "log10"
            | "sqrt"
            | "sin"
            | "cos"
            | "tan"
            | "asin"
            | "acos"
            | "atan"
            | "sec"
            | "cosec"
            | "cotan"
            | "asec"
            | "acosec"
            | "acotan"
            | "sign"
            | "ceiling"
            | "floor"
            | "round"
            | "truncate"
            | "conj"
            | "reverse"
            | "min"
            | "max"
            | "sum"
            | "pick"
            | "roll"
            | "sleep"
            | "sleep-timer"
            | "sleep-until"
            | "sleep-till"
            | "dir"
            | "open"
            | "elems"
            | "end"
            | "uniprop"
            | "unimatch"
            | "uniname"
            | "uninames"
            | "unival"
            | "univals"
            | "lines"
            | "slurp"
            | "spurt"
            | "localtime"
            | "gmtime"
            | "times"
            | "undefine"
    ) || is_expr_listop(name)
}

/// Functions that take multiple comma-separated expression arguments in listop style.
/// These are parsed with full expression arguments (not just primaries).
/// Test/Test::Util functions are NOT listed here — they are registered dynamically
/// via `register_module_exports()` when `use Test` / `use Test::Util` is parsed.
pub(crate) fn is_expr_listop(name: &str) -> bool {
    matches!(
        name,
        "EVAL"
            | "flat"
            | "is_run"
            | "slip"
            | "produce"
            | "reduce"
            | "run"
            | "shell"
            | "indir"
            | "cross"
            | "await"
            | "sleep"
            | "sleep-timer"
            | "sleep-until"
            | "sleep-till"
            | "dir"
            | "first"
            | "deepmap"
            | "duckmap"
            | "make"
            | "take"
            | "take-rw"
            | "snip"
            | "set"
            | "bag"
            | "mix"
            | "localtime"
            | "gmtime"
            | "times"
    ) || crate::parser::stmt::simple::is_imported_function(name)
}

/// True when `input` begins with a builtin function word that produces a value
/// usable as a term argument — a list-prefix op (`reverse`/`join`/`map`/…, via
/// [`is_listop`]) or a value-producing named unary (`uc`/`lc`/`item`/`splice`/…).
///
/// Used so a builtin/named-unary bareword immediately followed by another builtin
/// call nests as a call argument (`item reverse 1,2,3` => `item(reverse(1,2,3))`,
/// `uc reverse "ab","cd"`) instead of misparsing the inner word as a user-defined
/// infix operator in the precedence loop.
pub(crate) fn next_word_is_builtin_term_op(input: &str) -> bool {
    let first = match input.chars().next() {
        Some(c) => c,
        None => return false,
    };
    if !(first.is_alphabetic() || first == '_') {
        return false;
    }
    let mut end = first.len_utf8();
    for ch in input[end..].chars() {
        if ch.is_alphanumeric() || ch == '_' || ch == '-' {
            end += ch.len_utf8();
        } else {
            break;
        }
    }
    let word = &input[..end];
    is_listop(word)
        || matches!(
            word,
            "splice"
                | "item"
                | "uc"
                | "lc"
                | "tc"
                | "tclc"
                | "fc"
                | "wordcase"
                | "flip"
                | "chop"
                | "chomp"
                | "ord"
                | "ords"
                | "chr"
                | "chrs"
                | "chars"
                | "defined"
        )
}

/// Check if a name is an infix word operator (should not be treated as a listop call).
pub(crate) fn is_infix_word_op(name: &str) -> bool {
    matches!(
        name,
        "Z" | "X"
            | "R"
            | "x"
            | "xx"
            | "eq"
            | "ne"
            | "lt"
            | "gt"
            | "le"
            | "ge"
            | "cmp"
            | "coll"
            | "unicmp"
            | "leg"
            | "and"
            | "or"
            | "not"
            | "div"
            | "mod"
            | "gcd"
            | "lcm"
            | "but"
            | "does"
            | "min"
            | "max"
            | "ff"
            | "fff"
            | "before"
            | "after"
            | "andthen"
            | "orelse"
            | "notandthen"
    )
}

/// Check if input starts with a statement modifier keyword.
pub(crate) fn is_stmt_modifier_ahead(input: &str) -> bool {
    let input = input.trim_start();
    for kw in &["if", "unless", "for", "while", "until", "given", "when"] {
        if input.starts_with(kw)
            && !input
                .as_bytes()
                .get(kw.len())
                .is_some_and(|&c| c.is_ascii_alphanumeric() || c == b'_' || c == b'-')
        {
            return true;
        }
    }
    false
}

/// Check if input starts with unspace (`\`) followed by a postfix operator.
/// This prevents `foo\.method` from being parsed as `foo(\(.method))`.
pub(crate) fn is_unspace_before_postfix(input: &str) -> bool {
    if !input.starts_with('\\') {
        return false;
    }
    let scan = consume_unspace(input);
    if std::ptr::eq(scan, input) {
        return false;
    }
    // Check if what follows is a postfix operator
    (scan.starts_with('.') && !scan.starts_with(".."))
        || scan.starts_with('(')
        || scan.starts_with('[')
        || scan.starts_with('{')
        || scan.starts_with("++")
        || scan.starts_with("--")
}

pub(crate) fn is_require_terminator(input: &str) -> bool {
    let trimmed = input.trim_start();
    trimmed.is_empty()
        || trimmed.starts_with(';')
        || trimmed.starts_with('}')
        || trimmed.starts_with(')')
        || trimmed.starts_with(']')
        || is_stmt_modifier_ahead(trimmed)
}

/// Given input beginning with `(`, return the balanced parenthesised group
/// including the surrounding parens (e.g. `(&)` from `(&) extra`), or None if
/// unbalanced. Used to render the offending text in an X::Syntax::Adverb error.
pub(crate) fn balanced_paren_text(input: &str) -> Option<String> {
    let bytes = input.as_bytes();
    if bytes.first() != Some(&b'(') {
        return None;
    }
    let mut depth = 0usize;
    for (idx, &b) in bytes.iter().enumerate() {
        match b {
            b'(' => depth += 1,
            b')' => {
                depth -= 1;
                if depth == 0 {
                    return Some(input[..=idx].to_string());
                }
            }
            _ => {}
        }
    }
    None
}

/// Given input beginning with `(`, skip the balanced parenthesised group and
/// return true if the next non-whitespace character is `{` (the start of a
/// block). Used to distinguish `if() {}` (keyword-as-function) from `if(5)`
/// (undeclared routine). A simple paren-depth counter is sufficient here.
pub(crate) fn parens_then_block(input: &str) -> bool {
    let bytes = input.as_bytes();
    debug_assert_eq!(bytes.first(), Some(&b'('));
    let mut depth = 0usize;
    let mut idx = 0usize;
    while idx < bytes.len() {
        match bytes[idx] {
            b'(' => depth += 1,
            b')' => {
                depth -= 1;
                if depth == 0 {
                    idx += 1;
                    break;
                }
            }
            _ => {}
        }
        idx += 1;
    }
    if depth != 0 {
        return false;
    }
    input[idx..].trim_start().starts_with('{')
}

/// Try to extend an identifier with colon-pair adverbs (e.g. `meow:foo<bar>`).
/// Returns the extended name and remaining input, or the original name and input unchanged.
pub(crate) fn try_extend_colon_name<'a>(input: &'a str, base: &str) -> (&'a str, String) {
    let mut rest = input;
    let mut name = base.to_string();
    while rest.starts_with(':') && !rest.starts_with(":<") && !rest.starts_with(":<<") {
        let r = &rest[1..];
        // Try to parse identifier part
        if let Ok((r2, part)) =
            take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')
            && let Some(after_open) = r2.strip_prefix('<')
            && let Some(end) = after_open.find('>')
        {
            name.push(':');
            name.push_str(part);
            name.push_str(&r2[..=end + 1]);
            rest = &r2[end + 2..];
            continue;
        }
        break;
    }
    (rest, name)
}

/// Build the X::Comp::Group error for a statement-control keyword used as a
/// function call (`if() {}`). The group's first sorrow is an
/// X::Syntax::KeywordAsFunction instance carrying the offending word.
pub(crate) fn keyword_as_function_error(word: &str) -> PError {
    let msg = format!(
        "The word '{}' is interpreted as a '{}()' function call. \
         Please use whitespace instead of parentheses.",
        word, word
    );
    let mut sorrow_attrs = std::collections::HashMap::new();
    sorrow_attrs.insert("word".to_string(), Value::str(word.to_string()));
    sorrow_attrs.insert("needparens".to_string(), Value::Bool(false));
    sorrow_attrs.insert("message".to_string(), Value::str(msg.clone()));
    let sorrow = Value::make_instance(Symbol::intern("X::Syntax::KeywordAsFunction"), sorrow_attrs);
    let mut group_attrs = std::collections::HashMap::new();
    group_attrs.insert("sorrows".to_string(), Value::array(vec![sorrow]));
    group_attrs.insert("worries".to_string(), Value::array(vec![]));
    group_attrs.insert("panic".to_string(), Value::Nil);
    group_attrs.insert("message".to_string(), Value::str(msg.clone()));
    let exception = Value::make_instance(Symbol::intern("X::Comp::Group"), group_attrs);
    PError::fatal_with_exception(msg, Box::new(exception))
}
