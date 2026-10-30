//! `RakuAST::CompUnit.add-sorry` / `add-worry`: compile-time diagnostics a
//! program queues on the unit being compiled (#15666).
//!
//! rakudo stamps a queued exception with the unit's file and the position of
//! the first token (`SET_FILE_LINE`, `SET_PRE_POST`) and reports every sorry
//! when the compile phase ends -- a lone one as itself, several as the
//! `X::Comp::Group` rendering -- as a `===SORRY!===` and exit code 1. Worries
//! are printed as "Potential difficulties" and compilation goes on. The
//! caller may overwrite the position afterwards, which is what `static` does.

use crate::unit_source_text::{UNITS, active_index};
use crate::value::{Value, ValueView};

/// How much of the first line rakudo echoes after the `<HERE>` marker.
const POST_CHARS: usize = 40;

/// Queue `exception` on the innermost unit still in its compile phase and
/// stamp it with the position rakudo gives it. Anything that is not an
/// instance, or a unit past its compile phase, is ignored as rakudo does.
// Cost: O(w + p), w = leading whitespace of the unit, p = POST_CHARS.
pub(crate) fn add(exception: &Value, worry: bool) {
    let exception = exception.deref_container();
    let file = crate::unit_source_file::current()
        .map(|file| file.resolve().to_string())
        .unwrap_or_default();
    UNITS.with(|units| {
        let mut units = units.borrow_mut();
        let Some(unit) = active_index(&units)
            .and_then(|i| units.get_mut(i))
            .filter(|unit| !unit.compiled)
        else {
            return;
        };
        let (line, post) = first_token(&unit.source);
        if let ValueView::Instance { attributes, .. } = exception.view() {
            attributes.store_through_container("filename", Value::str(file));
            attributes.store_through_container("line", Value::int(line));
            attributes.store_through_container("pre", Value::str("<BOL>".to_string()));
            attributes.store_through_container("post", Value::str(post));
            attributes.store_through_container("is-compile-time", Value::TRUE);
        }
        if worry {
            unit.worries.push(exception);
        } else {
            unit.sorries.push(exception);
        }
    });
}

/// The innermost unit's queued `(sorries, worries)`; both queues are emptied.
// Cost: O(d), d = queued diagnostics.
pub(crate) fn take() -> (Vec<Value>, Vec<Value>) {
    UNITS.with(|units| {
        units
            .borrow_mut()
            .last_mut()
            .map(|unit| {
                (
                    std::mem::take(&mut unit.sorries),
                    std::mem::take(&mut unit.worries),
                )
            })
            .unwrap_or_default()
    })
}

/// The innermost unit's queued `(sorries, worries)`, left in place
/// (`$*CU.sorries` / `$*CU.worries`).
// Cost: O(d), d = queued diagnostics.
pub(crate) fn peek() -> (Vec<Value>, Vec<Value>) {
    UNITS.with(|units| {
        let units = units.borrow();
        active_index(&units)
            .and_then(|i| units.get(i))
            .map(|unit| (unit.sorries.clone(), unit.worries.clone()))
            .unwrap_or_default()
    })
}

/// Line number (1-based) of the first non-whitespace character of `source`
/// and the first `POST_CHARS` characters from there to the end of that line.
// Cost: O(w + p), w = leading whitespace, p = POST_CHARS.
fn first_token(source: &str) -> (i64, String) {
    let rest = source.trim_start();
    let skipped = &source[..source.len() - rest.len()];
    let line = 1 + skipped.matches('\n').count() as i64;
    let post = rest
        .lines()
        .next()
        .unwrap_or("")
        .chars()
        .take(POST_CHARS)
        .collect();
    (line, post)
}

#[cfg(test)]
mod tests {
    use super::first_token;

    #[test]
    fn position_is_the_first_token() {
        assert_eq!(first_token("use v6;\nsay 1"), (1, "use v6;".to_string()));
        assert_eq!(first_token("\n  # c\nx"), (2, "# c".to_string()));
        let long = "a".repeat(60);
        assert_eq!(first_token(&long), (1, "a".repeat(40)));
        assert_eq!(first_token(""), (1, String::new()));
    }
}
