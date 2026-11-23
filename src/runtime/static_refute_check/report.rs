//! Where a refuted call is and how it is reported: the call's position in its
//! statement's text, and the compile-time `X::TypeCheck::Argument`
//! (`static_refute_check`, #18874).

use crate::ast::{ParamDef, SrcSpan};
use crate::value::{RuntimeError, RuntimeErrorCode, Value};

use super::{Interpreter, Rules, Site};

/// The byte offset of the `index`-th site's call in `source`: the k-th
/// occurrence of its name in its statement's text, when the statement holds
/// exactly as many calls to the name as the text has occurrences.
// Cost: O(c + s), c = sites, s = length of the statement.
pub(super) fn call_offset(sites: &[Site<'_>], index: usize, source: &str) -> Option<usize> {
    let site = &sites[index];
    let span = site.span;
    if span == SrcSpan::NONE {
        return None;
    }
    let inside = |other: &Site<'_>| {
        other.name == site.name && other.span.from >= span.from && other.span.to <= span.to
    };
    let rank = sites[..index].iter().filter(|other| inside(other)).count();
    let total = sites.iter().filter(|other| inside(other)).count();
    let text = source.get(span.from as usize..span.to as usize)?;
    let occurrences = word_occurrences(text, site.name);
    (occurrences.len() == total).then(|| span.from as usize + occurrences[rank])
}

/// Whether the rest of the call's statement, from byte `from`, holds a word
/// quote (`<a>`, `«a»`, `qw/a/`, `q:w/a/`). The parser folds `<a>` to the
/// same `Str` literal as `"a"`, but rakudo does not judge a word quote
/// statically (`f(<a>)` is a run-time type check failure), so such a call
/// is left to the binder. A `<` that is not a quote only makes the pass
/// skip a call it could have judged.
// Cost: O(s), s = length of the statement.
pub(super) fn written_with_word_quote(source: &str, from: usize, span: SrcSpan) -> bool {
    source
        .get(from..(span.to as usize).max(from))
        .is_none_or(|rest| {
            rest.contains(['<', '\u{ab}']) || rest.contains("qw") || rest.contains(":w")
        })
}

/// The byte offsets in `text` where `name` stands as a whole routine name.
// Cost: O(s), s = length of `text`.
fn word_occurrences(text: &str, name: &str) -> Vec<usize> {
    let word_char = |c: char| c.is_alphanumeric() || c == '_';
    text.match_indices(name)
        .map(|(at, _)| at)
        .filter(|&at| {
            let before = text[..at].chars().next_back();
            let after = text[at + name.len()..].chars().next();
            let joins_after = |c: char| {
                word_char(c)
                    || (matches!(c, '-' | '\'')
                        && text[at + name.len() + 1..]
                            .chars()
                            .next()
                            .is_some_and(char::is_alphabetic))
            };
            !before.is_some_and(|c| word_char(c) || matches!(c, '$' | '@' | '%' | '&' | '.' | '!' | '-' | '\'' | ':'))
                && !after.is_some_and(joins_after)
        })
        .collect()
}

/// The compile-time `X::TypeCheck::Argument` for the call at byte `at`.
// Cost: O(p + a + s), p = parameters, a = arguments, s = length of `source`.
pub(super) fn refutation_error(
    name: &str,
    rules: Rules,
    candidates: &[&[ParamDef]],
    args: &[Value],
    source: &str,
    at: usize,
) -> RuntimeError {
    let arg_types = Interpreter::arg_type_names(args);
    let signatures: Vec<String> = candidates
        .iter()
        .map(|params| Interpreter::build_signature_string(params))
        .collect();
    let call = format!("Calling {name}({})", arg_types.join(", "));
    let message = match rules {
        Rules::Only => format!("{call} will never work with declared signature {}", signatures[0]),
        Rules::Multi => format!(
            "{call} will never work with any of these multi signatures:\n    {}",
            signatures.join("\n    ")
        ),
    };
    let signature = signatures.join("\n");
    let line = 1 + source[..at].matches('\n').count();
    let column = 1 + source[..at].rsplit('\n').next().map_or(0, |l| l.chars().count());
    let pos = source[..at].chars().count();
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.clone()));
    attrs.insert("objname".to_string(), Value::str(name.to_string()));
    attrs.insert("signature".to_string(), Value::str(signature));
    attrs.insert(
        "arguments".to_string(),
        Value::array(arg_types.into_iter().map(Value::str).collect()),
    );
    attrs.insert("line".to_string(), Value::int(line as i64));
    attrs.insert("column".to_string(), Value::int(column as i64));
    attrs.insert("pos".to_string(), Value::int(pos as i64));
    let mut err = RuntimeError::new(message);
    err.set_code(Some(RuntimeErrorCode::ParseGeneric));
    err.set_line(Some(line));
    err.set_column(Some(column));
    err.set_parse_pos(pos);
    err.exception = Some(Box::new(Value::make_instance(
        crate::symbol::Symbol::intern("X::TypeCheck::Argument"),
        attrs,
    )));
    err
}
