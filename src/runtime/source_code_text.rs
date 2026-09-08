//! A prose-free view of a compilation unit's source text.
//!
//! Several builtin preludes are gated on the source *mentioning* a name --
//! `NativeCall`, `does IO::Socket`, `trait_mod:<does>`, `Metamodel::Naming`
//! (see [`crate::runtime::run_prelude`]). Those gates used to run against the
//! raw source, so a name that appeared only in a **comment** switched the
//! prelude on: `# use NativeCall registers GLOBAL::nativecast` alone made
//! `&nativecast` resolvable in a program that never asked for it, and it
//! silently masked the failure a `t/` pin existed to catch (GH #7611).
//!
//! A comment is not code, and neither is a Pod block. [`CodeText`] is the one
//! place that distinction is made: it holds the source with `#` comments and
//! Pod blocks removed, and it is the only thing the prelude gates can be asked
//! about, so a gate added later cannot go back to reading raw text by mistake.
//!
//! String literals are deliberately left alone. A string *is* code -- a
//! `require ::('NativeCall')` names its module in one -- and recognising every
//! Raku quoting construct (`q//`, `qq{}`, heredocs, `«»`) well enough to blank
//! them out would risk dropping real code from the view. Prose is the part
//! that is definitionally not code, and prose is what this removes.
//!
//! Every judgement call here is biased toward *keeping* text: when a line
//! cannot be resolved with confidence (an unterminated quote, i.e. a
//! multi-line string), the line is kept verbatim. Keeping too much only
//! preserves the previous, over-eager behaviour for that one line; dropping
//! too much would lose a prelude a real program needs.

use std::borrow::Cow;

/// A compilation unit's source with comments and Pod removed.
///
/// Construct it with [`CodeText::from_source`]; the inner text is not exposed
/// as a plain `&str` for gating, only through [`CodeText::contains`], so the
/// prelude gates cannot accidentally be handed raw source.
pub(crate) struct CodeText<'a>(Cow<'a, str>);

impl<'a> CodeText<'a> {
    /// Strip `#` comments and Pod blocks from `source`.
    pub(crate) fn from_source(source: &'a str) -> Self {
        if !has_prose_marker(source) {
            return CodeText(Cow::Borrowed(source));
        }
        CodeText(Cow::Owned(strip_prose(source)))
    }

    /// Whether the code -- as opposed to the prose around it -- contains
    /// `needle`.
    pub(crate) fn contains(&self, needle: &str) -> bool {
        self.0.contains(needle)
    }
}

/// Cheap pre-check: a source with no `#` and no line starting with `=` has no
/// comment and no Pod block, so it is its own code view.
fn has_prose_marker(source: &str) -> bool {
    source.as_bytes().contains(&b'#') || source.starts_with('=') || source.contains("\n=")
}

/// The prose-removal pass. Line-oriented: both `#` comments and Pod blocks are
/// line-terminated constructs, and working a line at a time keeps a
/// misjudgement contained to that line.
fn strip_prose(source: &str) -> String {
    let mut out = String::with_capacity(source.len());
    // `Some(name)` while inside `=begin NAME` ... `=end NAME`.
    let mut in_delimited_pod: Option<String> = None;
    // Inside an abbreviated/`=for` Pod block, which runs to the next blank line.
    let mut in_pod_paragraph = false;
    for line in source.lines() {
        let trimmed = line.trim_start();
        if let Some(name) = &in_delimited_pod {
            if let Some(rest) = trimmed.strip_prefix("=end")
                && rest.trim() == name.as_str()
            {
                in_delimited_pod = None;
            }
            out.push('\n');
            continue;
        }
        if in_pod_paragraph {
            if trimmed.is_empty() {
                in_pod_paragraph = false;
            }
            out.push('\n');
            continue;
        }
        if is_pod_directive(trimmed) {
            if let Some(rest) = trimmed.strip_prefix("=begin ") {
                in_delimited_pod = Some(rest.trim().to_string());
            } else if trimmed == "=finish" || trimmed.starts_with("=finish ") {
                // Everything after `=finish` is the `$=finish` data section,
                // not code at all.
                break;
            } else if !trimmed.starts_with("=end ") {
                in_pod_paragraph = true;
            }
            out.push('\n');
            continue;
        }
        out.push_str(strip_line_comment(line));
        out.push('\n');
    }
    out
}

/// Whether a (left-trimmed) line opens or continues a Pod block: `=` followed
/// by an identifier character. Raku itself reads a line-initial `=word` as Pod,
/// so this cannot swallow a line that would have parsed as code.
fn is_pod_directive(trimmed: &str) -> bool {
    let mut chars = trimmed.chars();
    chars.next() == Some('=')
        && chars
            .next()
            .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
}

/// Return `line` up to a `#` that starts a comment, or the whole line when it
/// has none.
///
/// The scan tracks single- and double-quoted strings so a `#` inside one is not
/// mistaken for a comment (`say "#"; use NativeCall;` keeps its `use`). If the
/// line ends inside a quote it is part of a multi-line string this
/// line-oriented pass cannot see the end of, so the line is kept whole rather
/// than guessed at.
fn strip_line_comment(line: &str) -> &str {
    #[derive(PartialEq)]
    enum Quote {
        None,
        Single,
        Double,
    }
    let bytes = line.as_bytes();
    let mut quote = Quote::None;
    let mut i = 0;
    while i < bytes.len() {
        let b = bytes[i];
        match quote {
            Quote::None => match b {
                b'#' => return &line[..i],
                b'"' => quote = Quote::Double,
                // `'` is also an identifier character in Raku (`isn't-ok`),
                // but only between two word characters; anywhere else it opens
                // a string.
                b'\'' if !is_identifier_apostrophe(bytes, i) => quote = Quote::Single,
                _ => {}
            },
            Quote::Single => match b {
                b'\\' => i += 1,
                b'\'' => quote = Quote::None,
                _ => {}
            },
            Quote::Double => match b {
                b'\\' => i += 1,
                b'"' => quote = Quote::None,
                _ => {}
            },
        }
        i += 1;
    }
    // Unterminated quote: an in-flight multi-line string. Keep the line.
    line
}

/// Whether the `'` at `i` is the identifier-internal apostrophe Raku allows
/// (`don't`), which requires a word character on both sides.
fn is_identifier_apostrophe(bytes: &[u8], i: usize) -> bool {
    let word = |b: u8| b.is_ascii_alphanumeric() || b == b'_';
    i > 0 && word(bytes[i - 1]) && bytes.get(i + 1).copied().is_some_and(word)
}

#[cfg(test)]
mod tests {
    use super::CodeText;

    fn code(source: &str) -> String {
        // Round-trip through the public constructor so the tests exercise the
        // same path the prelude gates take.
        CodeText::from_source(source).0.into_owned()
    }

    #[test]
    fn line_comment_is_dropped() {
        assert!(!CodeText::from_source("# use NativeCall\nsay 1;\n").contains("NativeCall"));
        assert!(CodeText::from_source("use NativeCall;\nsay 1;\n").contains("NativeCall"));
    }

    #[test]
    fn trailing_comment_keeps_the_code_before_it() {
        let text = CodeText::from_source("use NativeCall; # NativeCall notes\n");
        assert!(text.contains("use NativeCall;"));
        assert_eq!(code("say 1; # hi\n"), "say 1; \n");
    }

    #[test]
    fn hash_inside_a_string_is_not_a_comment() {
        assert!(CodeText::from_source("say \"#\"; use NativeCall;\n").contains("NativeCall"));
        assert!(CodeText::from_source("say '#'; use NativeCall;\n").contains("NativeCall"));
        assert!(CodeText::from_source("say \"a\\\"#\"; use NativeCall;\n").contains("NativeCall"));
    }

    #[test]
    fn an_apostrophe_in_an_identifier_does_not_open_a_string() {
        // `don't` is one identifier; the `#` after it still starts a comment.
        assert!(!CodeText::from_source("don't-do(); # use NativeCall\n").contains("NativeCall"));
    }

    #[test]
    fn an_unterminated_quote_keeps_the_line() {
        // A multi-line string this line-oriented pass cannot resolve: keep it
        // rather than guess, which is the previous behaviour for that line only.
        assert_eq!(code("say 'a # b\n"), "say 'a # b\n");
    }

    #[test]
    fn delimited_pod_is_dropped() {
        let src = "=begin pod\nuse NativeCall in prose\n=end pod\nsay 1;\n";
        assert!(!CodeText::from_source(src).contains("NativeCall"));
        assert!(CodeText::from_source(src).contains("say 1;"));
    }

    #[test]
    fn abbreviated_pod_runs_to_the_blank_line() {
        let src = "=head1 use NativeCall\nstill NativeCall prose\n\nsay 1;\n";
        assert!(!CodeText::from_source(src).contains("NativeCall"));
        assert!(CodeText::from_source(src).contains("say 1;"));
    }

    #[test]
    fn finish_section_is_not_code() {
        let src = "say 1;\n=finish\nuse NativeCall\n";
        assert!(!CodeText::from_source(src).contains("NativeCall"));
    }

    #[test]
    fn source_without_prose_is_borrowed_unchanged() {
        let src = "use NativeCall;\nsay nativesizeof(int32);\n";
        assert_eq!(code(src), src);
    }
}
