//! The formatting codes of a Pod paragraph (`B<bold>`, `L<text|url>`), read
//! as rakudo's RakuDoc reader reads them, and their `Doc::Markup` nodes.

use crate::rakuast::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::Value;

/// A formatting code of a paragraph: `B<bold>`.
pub(super) struct Markup {
    letter: char,
    /// A run of `<` (`<`, `<<`, `<<<`, ...) or a `«`.
    opener: String,
    /// The matching run of `>`, or `»`.
    closer: String,
    atoms: Vec<Atom>,
    /// The text after the first `|` of a code that takes metadata (`L`, `X`, ...).
    meta: Option<String>,
    /// The pairs of an entity code (`E<gt>`), which has no atoms of its own.
    entities: Option<Vec<(String, String)>>,
}

/// The formatting codes that take metadata after a `|`.
const META_LETTERS: &str = "ADFLMPX";

pub(super) enum Atom {
    Text(String),
    Markup(Markup),
}

/// Why the reader declined a paragraph's text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::rakuast) enum Declined {
    /// A code whose closer never comes: its letter and closer, the innermost
    /// such code (rakudo's "RakuDoc markup code B missing endtag '>'").
    /// `None` only while the code being read has not yet named itself.
    Unterminated(Option<(char, String)>),
}

impl Declined {
    /// The error rakudo raises for it.
    // Cost: O(1).
    pub(in crate::rakuast) fn message(&self) -> String {
        match self {
            Declined::Unterminated(Some((letter, closer))) => {
                format!("RakuDoc markup code {letter} missing endtag '{closer}'.")
            }
            Declined::Unterminated(None) => "RakuDoc markup code missing its endtag.".to_string(),
        }
    }
}

/// The offset in `tail` of `closer` outside every balanced `<...>` / `«...»`,
/// or `None` when it never comes.
// Cost: O(n), n = length of the tail.
fn find_closer_nested(tail: &str, closer: &str) -> Option<usize> {
    let mut nest = 0usize;
    for (i, ch) in tail.char_indices() {
        if nest == 0 && tail[i..].starts_with(closer) {
            return Some(i);
        }
        match ch {
            '<' | '«' => nest += 1,
            '>' | '»' => nest = nest.saturating_sub(1),
            _ => {}
        }
    }
    None
}

/// The atoms of `text` up to `closer` (or its end), the metadata after a `|`
/// when `meta_ok`, and the length consumed with the closer. [`Declined`] for
/// text the reader does not take: an unterminated code.
// Cost: O(n), n = length of the text.
pub(super) fn parse_atoms(
    text: &str,
    closer: Option<&str>,
    meta_ok: bool,
) -> Result<(Vec<Atom>, usize, Option<String>), Declined> {
    let mut meta = None;
    let mut atoms = Vec::new();
    let mut buf = String::new();
    let mut at = 0;
    // Unclosed `<` / `«` of the code's text read so far.
    let mut nest = 0usize;
    loop {
        let rest = &text[at..];
        let Some(ch) = rest.chars().next() else {
            if closer.is_some() {
                return Err(Declined::Unterminated(None));
            }
            break;
        };
        if let Some(closer) = closer
            && nest == 0
            && rest.starts_with(closer)
        {
            at += closer.len();
            break;
        }
        if let Some(closer) = closer
            && meta_ok
            && ch == '|'
        {
            let tail = &rest[1..];
            let end = find_closer_nested(tail, closer).ok_or(Declined::Unterminated(None))?;
            // Balanced brackets are the metadata's text (`L<x|y<z>>`).
            meta = Some(tail[..end].to_string());
            at += 1 + end + closer.len();
            break;
        }
        if ch.is_ascii_uppercase() {
            let after = &rest[1..];
            // The opener is a `«` or a run of `<`, closed by `»` or the same
            // number of `>` (`B<<<x>>>`, `B<<<<x>>>>`), as in rakudo's reader.
            let opener: Option<(String, String)> = if after.starts_with('«') {
                Some(("«".to_string(), "»".to_string()))
            } else {
                let run = after.bytes().take_while(|&b| b == b'<').count();
                (run > 0).then(|| ("<".repeat(run), ">".repeat(run)))
            };
            if let Some((opener, closer)) = opener {
                // A code letter right after a word character still starts a
                // code, as in rakudo's reader: `aB<b>` is `a` then `B<b>`
                // (#19068).
                if !buf.is_empty() {
                    atoms.push(Atom::Text(std::mem::take(&mut buf)));
                }
                let body = &after[opener.len()..];
                if ch == 'E' {
                    let end = find_closer_nested(body, &closer)
                        .ok_or_else(|| Declined::Unterminated(Some((ch, closer.clone()))))?;
                    let content = &body[..end];
                    // Text holding brackets names no entity: it stays the
                    // code's text (`E<lt<x>>`), as in rakudo's reader.
                    let (shown, list) = match content.split_once('|') {
                        Some((shown, list)) => {
                            (shown, crate::rakuast::pod_entities::pairs_after_bar(list))
                        }
                        None if content.contains(['<', '>', '«', '»']) => (content, Vec::new()),
                        None => ("", crate::rakuast::pod_entities::pairs(content)),
                    };
                    at += 1 + opener.len() + end + closer.len();
                    atoms.push(Atom::Markup(Markup {
                        letter: 'E',
                        opener,
                        closer,
                        atoms: if shown.is_empty() {
                            Vec::new()
                        } else {
                            vec![Atom::Text(shown.to_string())]
                        },
                        meta: None,
                        entities: Some(list),
                    }));
                    continue;
                }
                // An empty code (`B<>`) is a `Markup` with no atoms (#19091).
                let (inner, used, inner_meta) =
                    parse_atoms(body, Some(&closer), META_LETTERS.contains(ch)).map_err(|why| {
                        match why {
                            // The code being read names itself; a deeper one
                            // already did.
                            Declined::Unterminated(None) => {
                                Declined::Unterminated(Some((ch, closer.clone())))
                            }
                            other => other,
                        }
                    })?;
                at += 1 + opener.len() + used;
                atoms.push(Atom::Markup(Markup {
                    letter: ch,
                    opener,
                    closer,
                    atoms: inner,
                    meta: inner_meta,
                    entities: None,
                }));
                continue;
            }
        }
        // A balanced `<...>` / `«...»` inside a code is its text, codes in it
        // still read (`C< infix:<+> >`, `C<< < B<foo> > >>`).
        if closer.is_some() && matches!(ch, '<' | '«') {
            nest += 1;
        } else if closer.is_some() && matches!(ch, '>' | '»') {
            // A `>` that does not close the code (`B<<< x > y >>>`) is text.
            nest = nest.saturating_sub(1);
        }
        buf.push(ch);
        at += ch.len_utf8();
    }
    if !buf.is_empty() {
        atoms.push(Atom::Text(buf));
    }
    Ok((atoms, at, meta))
}

/// The `Doc::Paragraph` of `atoms`.
// Cost: O(size of the atoms).
pub(super) fn paragraph_node(atoms: &[Atom]) -> RakuAstNode {
    let fields = atoms
        .iter()
        .map(|atom| RakuAstField {
            name: None,
            value: RakuAstFieldValue::Node(match atom {
                Atom::Text(text) => Value::str(text.clone()),
                Atom::Markup(markup) => Value::rakuast(Box::new(markup_node(markup))),
            }),
        })
        .collect();
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::DocParagraph,
        fields,
    }
}

/// The `Doc::Markup` of a formatting code.
// Cost: O(size of the code).
fn markup_node(markup: &Markup) -> RakuAstNode {
    let field = |name: &'static str, value: Value| RakuAstField {
        name: Some(name),
        value: RakuAstFieldValue::Node(value),
    };
    let atoms = markup
        .atoms
        .iter()
        .map(|atom| match atom {
            Atom::Text(text) => Value::str(text.clone()),
            Atom::Markup(inner) => Value::rakuast(Box::new(markup_node(inner))),
        })
        .collect::<Vec<_>>();
    let mut fields = vec![
        field("letter", Value::str(markup.letter.to_string())),
        field("opener", Value::str(markup.opener.to_string())),
        field("closer", Value::str(markup.closer.to_string())),
    ];
    if !atoms.is_empty() {
        fields.push(RakuAstField {
            name: Some("atoms"),
            value: RakuAstFieldValue::List(atoms),
        });
    }
    if let Some(meta) = &markup.meta {
        fields.push(RakuAstField {
            name: Some("meta"),
            value: RakuAstFieldValue::List(vec![Value::str(meta.clone())]),
        });
    }
    if let Some(entities) = markup.entities.as_ref().filter(|e| !e.is_empty()) {
        fields.push(RakuAstField {
            name: Some("meta"),
            value: RakuAstFieldValue::List(
                entities
                    .iter()
                    .map(|(key, value)| Value::pair(key.clone(), Value::str(value.clone())))
                    .collect(),
            ),
        });
    }
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::DocMarkup,
        fields,
    }
}
