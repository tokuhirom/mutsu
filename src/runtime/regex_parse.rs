use super::*;
use crate::symbol::Symbol;
use std::cell::RefCell;
use std::collections::HashMap;

thread_local! {
    /// Thread-local storage for regex security errors that need to propagate
    /// from inside `parse_regex` (which takes `&self`) to callers that can throw.
    pub(crate) static PENDING_REGEX_ERROR: RefCell<Option<RuntimeError>> = const { RefCell::new(None) };

    /// Compile-sorrow accumulator for regex parsing. Some malformed patterns
    /// produce several diagnostics before the parser gives up — e.g.
    /// `/m ** 1..-1/` raises a `MalformedRange` *sorrow* and an
    /// `UnrecognizedMetachar` *sorrow* on the way to the fatal "couldn't find
    /// final '/'" *panic*. Raku bundles all of these into a single
    /// `X::Comp::Group` rather than throwing the first one in isolation. Parser
    /// checks push non-fatal sorrows here; `validate_regex_structurally` drains
    /// the accumulator and, if any sorrow was recorded, wraps the pending panic
    /// and the sorrows into an `X::Comp::Group`.
    pub(crate) static REGEX_SORROWS: RefCell<Vec<Value>> = const { RefCell::new(Vec::new()) };

    /// Memoization cache for parsing *static* regex patterns (patterns whose
    /// parse result does not depend on runtime state). Parsing a pattern is a
    /// recursive-descent operation that is pure for patterns containing no
    /// scalar/array/hash interpolation sigils (`$`, `@`, `%`); for such patterns
    /// the parsed `RegexPattern` is a deterministic function of the source string,
    /// so it is safe to cache and reuse across calls. This avoids re-parsing the
    /// same (potentially large) pattern on every step of a match — e.g. a token
    /// with dozens of alternations called once per input character.
    /// Cached as `Arc<RegexPattern>` so a cache hit is a cheap refcount bump
    /// rather than a deep clone of the whole token tree — the hot match loop
    /// re-fetches the same compiled pattern on every step / every iteration.
    pub(crate) static REGEX_PARSE_CACHE: RefCell<HashMap<String, std::sync::Arc<RegexPattern>>> =
        RefCell::new(HashMap::new());
}

/// A pattern is cacheable iff parsing it does not depend on runtime variable
/// state. Interpolation (`interpolate_regex_scalars`) only substitutes when the
/// pattern contains a `$`, `@`, or `%` sigil, so a pattern free of those parses
/// deterministically.
pub(super) fn regex_pattern_is_static(pattern: &str) -> bool {
    !pattern.contains(['$', '@', '%'])
}

/// Parsing mode for the shared regex grammar parser (`parse_regex_uncached`).
///
/// The structural parser is the single source of truth for Raku regex grammar.
/// It runs in two modes:
///
/// - `Match` — the runtime path. Interpolates `$`/`@`/`%` variables, resolves
///   grammar tokens against the current package, and builds the `RegexPattern`
///   that the matching engine executes.
/// - `Validate` — a parse-time dry run used to syntax-check a pattern WITHOUT an
///   interpreter. It skips interpolation and grammar-token resolution (variable
///   values and grammar definitions are unavailable at parse time) and treats
///   variable references as opaque, syntactically-valid atoms. Every structural
///   and grammar syntax check still fires, so this replaces the former standalone
///   `regex_validate` module.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum RegexParseMode {
    Match,
    Validate,
}

thread_local! {
    /// A reusable interpreter used only as a `&self` receiver for parse-time
    /// regex validation (`validate_regex_structurally`). In `Validate` mode the
    /// structural parser never consults the interpreter's env / grammar tables
    /// (those reads are gated on `Match` mode), so this instance's state is
    /// irrelevant — it exists solely so we can call the shared `&self` parser
    /// without constructing a fresh `Interpreter` (which reads the whole OS
    /// environment) on every regex literal at compile time.
    static VALIDATE_INTERP: Interpreter = Interpreter::new();
}

/// Parse-time syntax validation via the structural parser — the single grammar
/// source of truth (replaces the former `regex_validate` module).
///
/// Returns `Ok(())` if the pattern is structurally valid, or the structural
/// parser's `X::Syntax::Regex::*` / `X::*` error. A `None` parse with NO pending
/// error means "not a recognized construct here" (the many `?`/`return None`
/// paths inside the parser), which is NOT a syntax error — the legacy validator
/// accepted those, so we treat them as `Ok(())`. We only surface an error when a
/// check explicitly set `PENDING_REGEX_ERROR`.
/// Reject a bare `<sym>` / `<.sym>` token used outside a proto regex. This is a
/// flat scan over the raw pattern (skipping quoted strings) that, like the
/// former validator, deliberately also looks inside embedded `{...}` code blocks
/// — mutsu does not compile a code block's nested regexes at parse time, so this
/// pre-scan is what surfaces e.g. `/grammar { token TOP { <sym> } }/` errors.
pub(super) fn check_bare_sym_usage(source: &str) -> Result<(), RuntimeError> {
    let mut chars = source.chars().peekable();
    while let Some(c) = chars.next() {
        match c {
            '"' | '\'' => {
                let quote = c;
                while let Some(ch) = chars.next() {
                    if ch == '\\' {
                        chars.next();
                    } else if ch == quote {
                        break;
                    }
                }
            }
            '<' => {
                let mut name = String::new();
                let mut depth = 1usize;
                for ch in chars.by_ref() {
                    if ch == '<' {
                        depth += 1;
                        name.push(ch);
                    } else if ch == '>' {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                        name.push(ch);
                    } else {
                        name.push(ch);
                    }
                }
                let trimmed = name.trim();
                if trimmed == "sym" || trimmed == ".sym" {
                    let msg = "Can only use \"<sym>\" token in a proto regex";
                    let mut attrs = HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.to_string()));
                    let ex = Value::make_instance(Symbol::intern("X::Syntax::Regex::Proto"), attrs);
                    let mut err = RuntimeError::new(msg);
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
            }
            _ => {}
        }
    }
    Ok(())
}

/// Reject a `**`-quantifier bare range that has whitespace adjacent to the `..`
/// (`m ** 1 ..2` / `m ** 1.. 2`): Raku requires the bare range to be written
/// without spaces around `..` and otherwise raises X::Syntax::Regex::SpacesInBareRange.
/// `pre`/`post` are reconstructed assuming `/.../` delimiters (the common case).
pub(super) fn check_spaces_in_bare_range(pattern: &str) -> Result<(), RuntimeError> {
    let bytes = pattern.as_bytes();
    let mut i = 0usize;
    while i + 1 < bytes.len() {
        // Skip over quoted literals and `{...}` code blocks so a `**` (or a
        // space-separated `..`) that lives *inside* one is not mistaken for a
        // real quantifier — e.g. a `'/m ** 1 ..'` literal in `m!...!`.
        match bytes[i] {
            b'\\' => {
                i += 2;
                continue;
            }
            q @ (b'\'' | b'"') => {
                i += 1;
                while i < bytes.len() && bytes[i] != q {
                    if bytes[i] == b'\\' {
                        i += 1;
                    }
                    i += 1;
                }
                i += 1;
                continue;
            }
            b'{' => {
                let mut depth = 1usize;
                i += 1;
                while i < bytes.len() && depth > 0 {
                    match bytes[i] {
                        b'{' => depth += 1,
                        b'}' => depth -= 1,
                        _ => {}
                    }
                    i += 1;
                }
                continue;
            }
            _ => {}
        }
        if bytes[i] == b'*' && bytes[i + 1] == b'*' {
            let mut j = i + 2;
            // optional frugal `?`
            if j < bytes.len() && bytes[j] == b'?' {
                j += 1;
            }
            // whitespace after `**`
            while j < bytes.len() && bytes[j].is_ascii_whitespace() {
                j += 1;
            }
            // leading digits (min count)
            let digit_start = j;
            while j < bytes.len() && bytes[j].is_ascii_digit() {
                j += 1;
            }
            if j > digit_start {
                // Whitespace adjacent to `..` on either side is illegal in a bare
                // range: `1 ..2` (space before) or `1.. 2` (space after).
                let mut k = j;
                let space_before = bytes.get(k).is_some_and(|b| b.is_ascii_whitespace());
                while k < bytes.len() && bytes[k].is_ascii_whitespace() {
                    k += 1;
                }
                let at_dotdot = bytes.get(k) == Some(&b'.') && bytes.get(k + 1) == Some(&b'.');
                let space_after =
                    at_dotdot && bytes.get(k + 2).is_some_and(|b| b.is_ascii_whitespace());
                if at_dotdot && (space_before || space_after) {
                    // Eject right after the `..`.
                    let eject = k + 2;
                    let pre = format!("/{}", &pattern[..eject]);
                    let post = format!("{}/", &pattern[eject..]);
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("pre".to_string(), crate::value::Value::str(pre));
                    attrs.insert("post".to_string(), crate::value::Value::str(post));
                    let msg = "Spaces not allowed in bare range".to_string();
                    attrs.insert("message".to_string(), crate::value::Value::str(msg.clone()));
                    let ex = crate::value::Value::make_instance(
                        crate::symbol::Symbol::intern("X::Syntax::Regex::SpacesInBareRange"),
                        attrs,
                    );
                    let mut err = RuntimeError::new(msg);
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
            }
            i = j.max(i + 2);
        } else {
            i += 1;
        }
    }
    Ok(())
}

pub(crate) fn validate_regex_structurally(pattern: &str) -> Result<(), RuntimeError> {
    // Flat pre-scan for bare `<sym>` (also inside code blocks), matching the
    // former validator's first check.
    check_bare_sym_usage(pattern)?;
    check_spaces_in_bare_range(pattern)?;
    PENDING_REGEX_ERROR.with(|e| *e.borrow_mut() = None);
    let _ = take_regex_sorrows();
    let _ = VALIDATE_INTERP
        .with(|interp| interp.parse_regex_with_mode(pattern, RegexParseMode::Validate));
    // Surface any error the structural parse recorded. The parser sometimes
    // sets a pending error and then `continue`s past the bad construct (so it
    // can return a partial pattern that the runtime later rejects via
    // `take_pending_regex_error`), so we must check the pending slot regardless
    // of whether a pattern was returned. A parse that simply returned `None`
    // with no pending error means "not a recognized construct" — NOT a syntax
    // error (the legacy validator accepted those), so that maps to `Ok(())`.
    let pending = PENDING_REGEX_ERROR.with(|e| e.borrow_mut().take());
    let sorrows = take_regex_sorrows();
    // A pattern that accumulated one or more sorrows produced several
    // compile-time diagnostics; bundle them with the fatal panic into a single
    // `X::Comp::Group` (mirroring rakudo's compile-sorrow accumulator).
    if !sorrows.is_empty() {
        let panic = pending
            .and_then(|err| err.exception.map(|b| *b))
            .unwrap_or_else(regex_unparseable_panic_value);
        let group = Value::make_comp_group(
            "Regex compilation failed".to_string(),
            Some(panic),
            sorrows,
            vec![],
        );
        let mut err = RuntimeError::new("X::Comp::Group");
        err.exception = Some(Box::new(group));
        return Err(err);
    }
    match pending {
        Some(err) => Err(err),
        None => Ok(()),
    }
}

/// Consume one opaque variable interpolation reference (`$name`, `${...}`,
/// `$(...)`, `@name`, `@{...}`, `@(...)`, digit runs) from the char stream,
/// after the leading sigil has already been consumed. Used in `Validate` mode
/// where interpolation cannot be performed; mirrors the former validator's
/// `skip_variable_ref`.
pub(super) fn skip_opaque_var_ref(chars: &mut std::iter::Peekable<std::str::Chars>) {
    match chars.peek().copied() {
        Some('<') => {
            chars.next();
            skip_balanced(chars, '<', '>');
        }
        Some('{') => {
            chars.next();
            skip_balanced(chars, '{', '}');
        }
        Some('(') => {
            chars.next();
            skip_balanced(chars, '(', ')');
        }
        Some(c) if c.is_ascii_digit() => {
            while chars.peek().is_some_and(|ch| ch.is_ascii_digit()) {
                chars.next();
            }
        }
        Some(c) if c == '*' || c == '?' || c == '^' || c == '.' => {
            chars.next();
            while chars
                .peek()
                .is_some_and(|ch| ch.is_alphanumeric() || *ch == '_' || *ch == '-')
            {
                chars.next();
            }
        }
        Some(c) if c.is_alphabetic() || c == '_' => {
            while chars
                .peek()
                .is_some_and(|ch| ch.is_alphanumeric() || *ch == '_' || *ch == '-')
            {
                chars.next();
            }
        }
        _ => {}
    }
    // After a variable ref, a trailing `=` is capture aliasing (e.g. `$<x>=(...)`).
    if chars.peek() == Some(&'=') {
        chars.next();
    }
}

/// Skip a balanced bracketed region (`open`..`close`) on the char stream, after
/// the opening bracket has already been consumed. Honors backslash escapes.
pub(super) fn skip_balanced(
    chars: &mut std::iter::Peekable<std::str::Chars>,
    open: char,
    close: char,
) {
    let mut depth = 1u32;
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            chars.next();
            continue;
        }
        if ch == open {
            depth += 1;
        } else if ch == close {
            depth -= 1;
            if depth == 0 {
                return;
            }
        }
    }
}

pub(super) fn single_token_pattern(
    token: RegexToken,
    ignore_case: bool,
    ignore_mark: bool,
) -> RegexPattern {
    RegexPattern {
        tokens: vec![token],
        anchor_start: false,
        anchor_end: false,
        ignore_case,
        ignore_mark,
    }
}

pub(super) fn goal_text_for_token(token: &RegexToken) -> String {
    match &token.atom {
        RegexAtom::Literal(ch) => format!("{ch:?}"),
        RegexAtom::Named(name) => format!("<{name}>"),
        _ => "goal".to_string(),
    }
}

pub(super) fn make_solitary_quantifier_error() -> RuntimeError {
    let msg = "Quantifier quantifies nothing";
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.to_string()));
    let ex = Value::make_instance(
        Symbol::intern("X::Syntax::Regex::SolitaryQuantifier"),
        attrs,
    );
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

pub(super) fn make_solitary_tilde_quantifier_error() -> RuntimeError {
    make_solitary_quantifier_error()
}

pub(super) fn make_non_quantifiable_error() -> RuntimeError {
    let msg = "Can only quantify a construct that produces a match";
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.to_string()));
    let ex = Value::make_instance(Symbol::intern("X::Syntax::Regex::NonQuantifiable"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// `X::Syntax::Regex::NullRegex` — a regex (or one of its alternation /
/// conjunction branches, or a group body) that matches nothing syntactically,
/// e.g. `/ /`, `/ a | /`, `/ () /`, `s//b/`. Raku rejects these at parse time.
/// A *single leading* empty alternation branch is allowed for alignment
/// (`/ | a /`), so callers pass `allow_leading_empty` accordingly.
pub(super) fn make_null_regex_error() -> RuntimeError {
    let msg = "Null regex not allowed";
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.to_string()));
    let ex = Value::make_instance(Symbol::intern("X::Syntax::Regex::NullRegex"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Detect a null branch among a split alternation/conjunction. Returns the
/// NullRegex error if any branch is empty/whitespace-only, except that a single
/// leading empty branch is permitted when `allow_leading_empty` is set (Raku
/// allows `/ | a /` and `/ || a /` for visual alignment).
pub(super) fn null_regex_if_empty_branch(
    branches: &[String],
    allow_leading_empty: bool,
) -> Option<RuntimeError> {
    for (i, b) in branches.iter().enumerate() {
        if b.trim().is_empty() {
            if allow_leading_empty && i == 0 {
                continue;
            }
            return Some(make_null_regex_error());
        }
    }
    None
}

pub(super) fn make_unrecognized_metachar_error(metachar: char) -> RuntimeError {
    let msg =
        format!("Unrecognized regex metacharacter {metachar} (must be quoted to match literally)");
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    attrs.insert("metachar".to_string(), Value::str(metachar.to_string()));
    let ex = Value::make_instance(
        Symbol::intern("X::Syntax::Regex::UnrecognizedMetachar"),
        attrs,
    );
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Push a non-fatal compile-time sorrow onto the regex accumulator.
pub(super) fn push_regex_sorrow(ex: Value) {
    REGEX_SORROWS.with(|s| s.borrow_mut().push(ex));
}

/// Drain and return the accumulated regex sorrows.
pub(super) fn take_regex_sorrows() -> Vec<Value> {
    REGEX_SORROWS.with(|s| std::mem::take(&mut *s.borrow_mut()))
}

/// The `X::Syntax::Regex::UnrecognizedMetachar` exception *value* (not wrapped
/// in a `RuntimeError`), for use as an accumulated sorrow.
pub(super) fn unrecognized_metachar_exception(metachar: char) -> Value {
    let msg =
        format!("Unrecognized regex metacharacter {metachar} (must be quoted to match literally)");
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg));
    attrs.insert("metachar".to_string(), Value::str(metachar.to_string()));
    Value::make_instance(
        Symbol::intern("X::Syntax::Regex::UnrecognizedMetachar"),
        attrs,
    )
}

/// The `X::Syntax::Regex::MalformedRange` exception value, recorded as a sorrow
/// when a `** N..M` quantifier range has a malformed (e.g. negative) endpoint.
pub(super) fn malformed_range_exception() -> Value {
    let msg = "Malformed Range. If attempting to use variables for end points, \
               wrap the entire range in curly braces."
        .to_string();
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg));
    Value::make_instance(Symbol::intern("X::Syntax::Regex::MalformedRange"), attrs)
}

/// The fatal "couldn't find final '/'" panic value. Once a sorrow has been
/// accumulated, the parser bails out here rather than surfacing a later
/// sub-diagnostic as the top-level error.
pub(super) fn regex_unparseable_panic_value() -> Value {
    let msg = "Unable to parse regex; couldn't find final '/'".to_string();
    let mut attrs = HashMap::new();
    attrs.insert("payload".to_string(), Value::str(msg.clone()));
    attrs.insert("message".to_string(), Value::str(msg));
    Value::make_instance(Symbol::intern("X::AdHoc"), attrs)
}

pub(super) fn make_unrecognized_modifier_error(modifier: &str) -> RuntimeError {
    let msg = format!("Unrecognized regex modifier :{modifier}");
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    attrs.insert("modifier".to_string(), Value::str(modifier.to_string()));
    let ex = Value::make_instance(
        Symbol::intern("X::Syntax::Regex::UnrecognizedModifier"),
        attrs,
    );
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

pub(super) fn make_solitary_backtrack_control_error() -> RuntimeError {
    let msg = "Backtrack control ':' does not seem to have a preceding atom to control";
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.to_string()));
    let ex = Value::make_instance(
        Symbol::intern("X::Syntax::Regex::SolitaryBacktrackControl"),
        attrs,
    );
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

pub(super) fn make_backslash_unrecognized_error(esc: char) -> RuntimeError {
    let msg = format!("Unrecognized backslash sequence: \\{esc}");
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern("X::Backslash::UnrecognizedSequence"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// A backslash directly followed by whitespace (`\ `, `\<tab>`, ...) is an
/// "unspace" — a main-slang construct that is not allowed in a regex; Raku
/// rejects it with X::Syntax::Regex::Unspace. `ch` is the offending whitespace
/// character (exposed as the `.char` attribute).
pub(super) fn make_unspace_error(ch: char) -> RuntimeError {
    let msg = format!(
        "No unspace allowed in regex; if you meant to match the literal character, please enclose in single quotes ('{}') or use a backslashed form like \\x{:02x}",
        ch, ch as u32
    );
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    attrs.insert("char".to_string(), Value::str(ch.to_string()));
    let ex = Value::make_instance(Symbol::intern("X::Syntax::Regex::Unspace"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Detect a missing `+`/`-` operator between parts of a compound character
/// class assertion (e.g. `<[abc] [def]>`, `<:Kata :Hira>`). Ported from the
/// former validator; used only at parse time (`Validate` mode).
pub(super) fn check_missing_class_operator(content: &str) -> Result<(), RuntimeError> {
    let trimmed = content.trim();
    let is_charclass = trimmed.starts_with('[')
        || trimmed.starts_with('+')
        || trimmed.starts_with('-')
        || (trimmed.starts_with(':') && trimmed.chars().nth(1).is_some_and(|c| c.is_uppercase()));
    if !is_charclass {
        return Ok(());
    }
    // Simple bracket classes without multiple parts don't need operator checking.
    if trimmed.starts_with('[') && !trimmed.contains("] ") && !trimmed.contains("]+") {
        return Ok(());
    }
    let missing = || RuntimeError::new("Missing + or - in character class expression");
    let mut remaining = trimmed;
    let mut had_part = false;
    while !remaining.is_empty() {
        let first = remaining.chars().next().unwrap();
        if first == '+' || first == '-' {
            remaining = remaining[1..].trim_start();
            had_part = false;
        } else if first == '[' {
            if had_part {
                return Err(missing());
            }
            remaining = &remaining[1..];
            let mut chars = remaining.chars();
            while let Some(c) = chars.next() {
                if c == '\\' {
                    chars.next();
                } else if c == ']' {
                    break;
                }
            }
            remaining = chars.as_str().trim_start();
            had_part = true;
        } else if first == ':' && remaining.chars().nth(1).is_some_and(|c| c.is_uppercase()) {
            if had_part {
                return Err(missing());
            }
            let end = remaining
                .find(|c: char| !c.is_alphanumeric() && c != ':' && c != '-' && c != '_')
                .unwrap_or(remaining.len());
            remaining = remaining[end..].trim_start();
            had_part = true;
        } else if first.is_alphanumeric() && had_part {
            return Err(missing());
        } else if first.is_alphanumeric() {
            let mut end = 0;
            let mut citer = remaining.chars();
            while let Some(ch) = citer.next() {
                if ch.is_alphanumeric() || ch == '_' {
                    end += ch.len_utf8();
                } else if ch == '-' {
                    let after = citer.as_str().trim_start();
                    if after.starts_with('[') || after.starts_with(':') || after.is_empty() {
                        break;
                    }
                    end += 1;
                } else {
                    break;
                }
            }
            remaining = remaining[end..].trim_start();
            had_part = true;
        } else {
            break;
        }
    }
    Ok(())
}

pub(super) fn make_attribute_regex_error(symbol: &str) -> RuntimeError {
    let msg = "Cannot interpolate attribute in a regex";
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.to_string()));
    attrs.insert("symbol".to_string(), Value::str(symbol.to_string()));
    let ex = Value::make_instance(Symbol::intern("X::Attribute::Regex"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

pub(super) fn make_hash_reserved_error() -> RuntimeError {
    let msg = "The use of hashes in regexes is reserved";
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.to_string()));
    let ex = Value::make_instance(Symbol::intern("X::Syntax::Reserved"), attrs);
    let mut err = RuntimeError::new(msg);
    err.exception = Some(Box::new(ex));
    err
}

/// Scan `text` for an attribute interpolation `$!name` and return the full
/// symbol (`$!name`) if found. Used by the parse-time validator to reject
/// `$!attr` interpolation inside regexes and embedded code blocks.
pub(super) fn find_attribute_interpolation(text: &str) -> Option<String> {
    let bytes: Vec<char> = text.chars().collect();
    let mut i = 0;
    while i + 1 < bytes.len() {
        if bytes[i] == '$' && bytes[i + 1] == '!' {
            let mut symbol = String::from("$!");
            let mut j = i + 2;
            while j < bytes.len()
                && (bytes[j].is_alphanumeric() || bytes[j] == '_' || bytes[j] == '-')
            {
                symbol.push(bytes[j]);
                j += 1;
            }
            if symbol.chars().count() > 2 {
                return Some(symbol);
            }
        }
        i += 1;
    }
    None
}

/// Whether `name` is a known regex inline adverb (`:i`, `:ignorecase`, ...).
/// Used by the parse-time validator to reject unknown modifiers like `:iabc`.
pub(super) fn is_known_regex_adverb(name: &str) -> bool {
    matches!(
        name,
        "i" | "ignorecase"
            | "m"
            | "ignoremark"
            | "s"
            | "sigspace"
            | "r"
            | "ratchet"
            | "g"
            | "global"
            | "ii"
            | "samecase"
            | "ss"
            | "samespace"
            | "mm"
            | "samemark"
            | "dba"
    )
}

/// Determine the correct error for a bare quantifier metacharacter (`*`, `+`,
/// `?`) found at atom position in `Validate` mode. A valid quantifier after an
/// atom is consumed by the quantifier peek, so reaching this point means there
/// is nothing valid to quantify. Mirrors the former validator's
/// has_atom / prev_was_anchor / prev_was_tilde tracking, derived here from the
/// token stream built so far.
pub(super) fn quantifier_context_error(tokens: &[RegexToken], anchor_start: bool) -> RuntimeError {
    match tokens.last() {
        None => {
            if anchor_start {
                // `^` start-of-string anchor was consumed without a token.
                make_non_quantifiable_error()
            } else {
                make_solitary_quantifier_error()
            }
        }
        Some(t) => match &t.atom {
            RegexAtom::StartOfLine | RegexAtom::EndOfLine => make_non_quantifiable_error(),
            RegexAtom::TildeMarker => make_solitary_quantifier_error(),
            RegexAtom::CodeAssertion { .. }
            | RegexAtom::VarDecl { .. }
            | RegexAtom::ClosureInterpolation { .. } => make_non_quantifiable_error(),
            // A real atom whose own quantifier was already consumed: a second
            // bare quantifier (e.g. `a+ +`) quantifies nothing.
            _ => make_solitary_quantifier_error(),
        },
    }
}

/// Check if a regex token is a whitespace-like token (WsRule or <.ws> subrule call).
pub(super) fn is_ws_like_token(token: &RegexToken) -> bool {
    match &token.atom {
        RegexAtom::WsRule => true,
        RegexAtom::Named(name) => {
            let mut raw = name.trim();
            if let Some(stripped) = raw.strip_prefix('.') {
                raw = stripped.trim();
            }
            raw == "ws" || raw == "ws?"
        }
        _ => false,
    }
}

pub(super) fn rewrite_tilde_tokens(
    tokens: Vec<RegexToken>,
    ignore_case: bool,
    ignore_mark: bool,
) -> Result<Vec<RegexToken>, RuntimeError> {
    let mut out = Vec::new();
    let mut i = 0usize;
    while i < tokens.len() {
        if matches!(tokens[i].atom, RegexAtom::TildeMarker) {
            if !matches!(tokens[i].quant, RegexQuant::One) {
                return Err(make_solitary_tilde_quantifier_error());
            }
            if out.is_empty() {
                return Ok(tokens);
            }
            // Remove trailing ws-like token from out (before tilde) — inserted
            // by sigspace between the opener and `~`.
            let had_pre_ws = out.last().is_some_and(is_ws_like_token);
            if had_pre_ws {
                out.pop();
            }
            // Skip ws-like tokens after the tilde to find the goal (closer)
            let mut j = i + 1;
            while j < tokens.len() && is_ws_like_token(&tokens[j]) {
                j += 1;
            }
            if j >= tokens.len() {
                return Ok(tokens);
            }
            let goal_token = tokens[j].clone();
            // Skip ws-like tokens after the goal to find the inner (content)
            let mut k = j + 1;
            while k < tokens.len() && is_ws_like_token(&tokens[k]) {
                k += 1;
            }
            if k >= tokens.len() {
                return Ok(tokens);
            }
            let inner_token = tokens[k].clone();
            // Build the inner pattern: the single content token, optionally
            // surrounded by WsRule so `rule` sigspace allows whitespace between
            // opener/content and content/closer.
            let mut inner_tokens = Vec::new();
            if had_pre_ws {
                let ws_tok = RegexToken {
                    atom: RegexAtom::WsRule,
                    quant: RegexQuant::One,
                    named_capture: None,
                    hash_capture: None,
                    secondary_named_capture: None,
                    force_list_capture: false,
                    ratchet: false,
                    frugal: false,
                    separator: None,
                };
                inner_tokens.push(ws_tok.clone());
                inner_tokens.push(inner_token);
                inner_tokens.push(ws_tok);
            } else {
                inner_tokens.push(inner_token);
            }
            let inner_pattern = RegexPattern {
                tokens: inner_tokens,
                anchor_start: false,
                anchor_end: false,
                ignore_case,
                ignore_mark,
            };
            out.push(RegexToken {
                atom: RegexAtom::GoalMatch {
                    goal: single_token_pattern(goal_token.clone(), ignore_case, ignore_mark),
                    inner: inner_pattern,
                    goal_text: goal_text_for_token(&goal_token),
                },
                quant: RegexQuant::One,
                named_capture: None,
                hash_capture: None,
                secondary_named_capture: None,
                force_list_capture: false,
                ratchet: false,
                frugal: false,
                separator: None,
            });
            // Skip past the inner token and any trailing ws-like tokens
            i = k + 1;
            while i < tokens.len() && is_ws_like_token(&tokens[i]) {
                i += 1;
            }
            continue;
        }
        out.push(tokens[i].clone());
        i += 1;
    }
    Ok(out)
}

/// Optimize an Alternation: if all branches are single-token CharClass or Literal
/// patterns (no captures, quantifier One), merge them into a single CharClass atom.
/// This dramatically speeds up grammar tokens with many Unicode range alternations.
pub(super) fn try_collapse_alternation_to_charclass(
    alt_patterns: &[RegexPattern],
) -> Option<RegexAtom> {
    if alt_patterns.len() < 2 {
        return None;
    }
    let mut merged_items: Vec<ClassItem> = Vec::new();
    for pat in alt_patterns {
        if pat.anchor_start || pat.anchor_end {
            return None;
        }
        // Allow single-token patterns, or patterns where all extra tokens are
        // optional ws (sigspace-inserted whitespace matchers)
        let effective_tokens: Vec<&RegexToken> = pat
            .tokens
            .iter()
            .filter(|t| {
                // Keep non-ws tokens; skip optional ws tokens
                if matches!(t.quant, RegexQuant::ZeroOrOne | RegexQuant::ZeroOrMore) {
                    if let RegexAtom::CharClass(class) = &t.atom
                        && !class.negated
                        && class.items.len() == 1
                        && matches!(class.items[0], ClassItem::Space)
                    {
                        return false; // skip this ws token
                    }
                    if let RegexAtom::Named(name) = &t.atom {
                        let raw = name.trim().trim_start_matches('.').trim_start_matches('&');
                        if raw == "ws" {
                            return false;
                        }
                    }
                }
                true
            })
            .collect();
        if effective_tokens.len() != 1 {
            return None;
        }
        let token = effective_tokens[0];
        if !matches!(token.quant, RegexQuant::One) || token.named_capture.is_some() {
            return None;
        }
        match &token.atom {
            RegexAtom::CharClass(class) if !class.negated => {
                merged_items.extend(class.items.iter().cloned());
            }
            RegexAtom::Literal(ch) => {
                merged_items.push(ClassItem::Char(*ch));
            }
            _ => return None,
        }
    }
    Some(RegexAtom::CharClass(CharClass {
        negated: false,
        items: merged_items,
    }))
}

pub(super) fn regex_single_quote_closes(open: char, ch: char) -> bool {
    match open {
        '\'' => ch == '\'',
        '\u{2018}' => ch == '\u{2019}',                     // ‘...’
        '\u{201A}' => ch == '\u{2019}' || ch == '\u{2018}', // ‚...’ and ‚...‘
        '\u{FF62}' => ch == '\u{FF63}',                     // ｢...｣
        _ => false,
    }
}

pub(super) fn is_inside_single_quoted_regex_literal(chars: &[char], pos: usize) -> bool {
    let mut open: Option<char> = None;
    let mut escaped = false;
    for &ch in chars.iter().take(pos) {
        if escaped {
            escaped = false;
            continue;
        }
        if ch == '\\' {
            escaped = true;
            continue;
        }
        if let Some(open_ch) = open {
            if regex_single_quote_closes(open_ch, ch) {
                open = None;
            }
        } else if matches!(ch, '\'' | '\u{2018}' | '\u{201A}' | '\u{FF62}') {
            open = Some(ch);
        }
    }
    open.is_some()
}

pub(super) fn regex_single_quote_atom(literal: String, ignore_case: bool) -> RegexAtom {
    let lit_chars: Vec<char> = literal.chars().collect();
    if lit_chars.is_empty() {
        // Empty string literal matches zero-width
        RegexAtom::ZeroWidth
    } else if lit_chars.len() == 1 {
        RegexAtom::Literal(lit_chars[0])
    } else {
        let tokens = lit_chars
            .into_iter()
            .map(|ch| RegexToken {
                atom: RegexAtom::Literal(ch),
                quant: RegexQuant::One,
                named_capture: None,
                hash_capture: None,
                secondary_named_capture: None,
                force_list_capture: false,
                ratchet: false,
                frugal: false,
                separator: None,
            })
            .collect();
        RegexAtom::Group(RegexPattern {
            tokens,
            anchor_start: false,
            anchor_end: false,
            ignore_case,
            ignore_mark: false,
        })
    }
}

/// Split a property name like "NumericValue(0 ^..^ 1)" into ("NumericValue", Some("0 ^..^ 1")).
pub(super) fn split_prop_args(s: &str) -> (&str, Option<&str>) {
    if let Some(open) = s.find('(')
        && let Some(close) = s.rfind(')')
    {
        return (&s[..open], Some(&s[open + 1..close]));
    }
    // Also handle angle-bracket args: Numeric_Type<Digit> → ("Numeric_Type", Some("Digit"))
    if let Some(open) = s.find('<')
        && let Some(close) = s.rfind('>')
    {
        return (&s[..open], Some(&s[open + 1..close]));
    }
    (s, None)
}

/// Skip `<[...]>` character class content where quotes are literal.
pub(super) fn skip_char_class_content(
    chars: &mut std::iter::Peekable<std::str::Chars>,
    current: &mut String,
    open_ch: char,
) -> bool {
    let is_char_class = {
        let tmp = chars.clone();
        let mut it = tmp;
        let c1 = it.next();
        let c2 = it.next();
        matches!(
            (c1, c2),
            (Some('['), _) | (Some('-' | '+' | '!'), Some('['))
        )
    };
    if !is_char_class {
        return false;
    }
    current.push(open_ch);
    // Scan bracket groups sequentially. Inside each [...], an unescaped '['
    // is a literal character. After ']', check for compound operators (+[, -[)
    // or the closing '>'.
    let mut entered_first = false;
    'char_class: loop {
        if !entered_first {
            // Consume chars until we hit the first '['
            for c in chars.by_ref() {
                current.push(c);
                if c == '[' {
                    break;
                }
            }
            entered_first = true;
        }
        // Scan inside a [...] group until unescaped ']'
        loop {
            match chars.next() {
                Some('\\') => {
                    current.push('\\');
                    if let Some(esc) = chars.next() {
                        current.push(esc);
                    }
                }
                Some(']') => {
                    current.push(']');
                    break;
                }
                Some(c) => current.push(c),
                None => break 'char_class,
            }
        }
        // After ']', check for compound class or closing '>'
        match chars.peek() {
            Some(&'>') => {
                current.push(chars.next().unwrap());
                break;
            }
            Some(&('+' | '-')) => {
                let op = chars.next().unwrap();
                current.push(op);
                if chars.peek() == Some(&'[') {
                    current.push(chars.next().unwrap());
                    continue 'char_class;
                }
                // Not a compound group
                if chars.peek() == Some(&'>') {
                    current.push(chars.next().unwrap());
                }
                break;
            }
            Some(&'[') => {
                current.push(chars.next().unwrap());
                continue 'char_class;
            }
            _ => break,
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: the typed exception name a structural-validation error carries.
    fn validate_exc(pattern: &str) -> Option<String> {
        match validate_regex_structurally(pattern) {
            Ok(()) => None,
            Err(e) => e.exception.as_ref().and_then(|ex| match ex.as_ref() {
                Value::Instance { class_name, .. } => Some(class_name.resolve()),
                _ => None,
            }),
        }
    }

    #[test]
    fn validate_accepts_valid_patterns() {
        // A representative spread of valid Raku regex syntax must validate Ok.
        for p in [
            r"\d+",
            r"<[a..z]>+",
            r"(\w+) \s+ (\d+)",
            r"a+ % ','",
            r"^ foo $",
            r"<?before bar>",
            r"$0",
            r"$<name>",
            r"$var",      // opaque interpolation
            r"@var",      // opaque array interpolation
            r"@$aref[0]", // array-deref interpolation
            r"$var+",     // quantified interpolation
            r"<$var>",    // assertion interpolation
            r":i foo",
            r"a* %% b",
            r"<+alpha+digit>",
        ] {
            assert!(
                validate_regex_structurally(p).is_ok(),
                "expected `{p}` to validate"
            );
        }
    }

    #[test]
    fn validate_rejects_with_correct_exception() {
        // Each invalid pattern must surface the same typed exception the former
        // standalone validator produced.
        let cases = [
            ("*", "X::Syntax::Regex::SolitaryQuantifier"),
            ("a+ +", "X::Syntax::Regex::SolitaryQuantifier"),
            ("^+", "X::Syntax::Regex::NonQuantifiable"),
            ("^^+", "X::Syntax::Regex::NonQuantifiable"),
            ("$+", "X::Syntax::Regex::NonQuantifiable"),
            ("{}*", "X::Syntax::Regex::NonQuantifiable"),
            (":", "X::Syntax::Regex::SolitaryBacktrackControl"),
            ("-", "X::Syntax::Regex::UnrecognizedMetachar"),
            ("!", "X::Syntax::Regex::UnrecognizedMetachar"),
            ("00:11:22", "X::Syntax::Regex::UnrecognizedModifier"),
            (r"\q", "X::Backslash::UnrecognizedSequence"),
            ("%var", "X::Syntax::Reserved"),
            ("$!attr", "X::Attribute::Regex"),
            ("<::IO::File=bar>", "X::Syntax::Regex::Alias::LongName"),
        ];
        for (p, exc) in cases {
            assert_eq!(
                validate_exc(p).as_deref(),
                Some(exc),
                "pattern `{p}` should throw {exc}"
            );
        }
        // Obsolete Perl 5 escapes.
        for p in [r"\A", r"\Z", r"\z"] {
            assert_eq!(validate_exc(p).as_deref(), Some("X::Obsolete"), "{p}");
        }
        // Compound class missing operator (plain message, no typed exception).
        assert!(validate_regex_structurally("<[abc] [def]>").is_err());
    }

    #[test]
    fn interpolate_array_var_into_alternation() {
        let mut interp = Interpreter::default();
        interp.env.insert(
            "@list".to_string(),
            Value::array(vec![
                Value::str_from("x"),
                Value::str_from("xx"),
                Value::str_from("xxxx"),
            ]),
        );
        let interpolated = interp.interpolate_regex_scalars(" ||@list ").unwrap();
        assert_eq!(interpolated, " ||x||xx||xxxx ");
    }

    #[test]
    fn scalar_inside_single_quoted_regex_atom_is_literal() {
        let interp = Interpreter::default();
        let interpolated = interp.interpolate_regex_scalars("'$param'").unwrap();
        // $ inside single-quoted regex atoms should not be interpolated —
        // the $ is kept literal per Raku spec.
        assert_eq!(interpolated, "'$param'");
    }
}
