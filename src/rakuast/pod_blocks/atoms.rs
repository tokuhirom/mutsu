//! The formatting codes of a Pod paragraph (`B<bold>`, `L<text|url>`), read
//! as rakudo's RakuDoc reader reads them, and their `Doc::Markup` nodes.

use crate::rakuast::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::Value;

/// A formatting code of a paragraph: `B<bold>`.
pub(super) struct Markup {
    letter: char,
    opener: &'static str,
    closer: &'static str,
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::rakuast) enum Declined {
    /// A code whose closer never comes: its letter and closer, the innermost
    /// such code (rakudo's "RakuDoc markup code B missing endtag '>'").
    /// `None` only while the code being read has not yet named itself.
    Unterminated(Option<(char, &'static str)>),
    /// Text the plain reader does not take: an ambiguous code, an `E<...>`
    /// it cannot read.
    Other,
}

impl Declined {
    /// The error rakudo raises for an unterminated code, `None` otherwise.
    // Cost: O(1).
    pub(in crate::rakuast) fn unterminated_message(self) -> Option<String> {
        match self {
            Declined::Unterminated(Some((letter, closer))) => Some(format!(
                "RakuDoc markup code {letter} missing endtag '{closer}'."
            )),
            _ => None,
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
/// text the plain reader does not take: an `E<...>`, an unterminated or
/// ambiguous code.
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
    let mut previous: Option<char> = None;
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
            let text = &tail[..end];
            if text.chars().any(|c| matches!(c, '<' | '>' | '«' | '»')) {
                return Err(Declined::Other);
            }
            meta = Some(text.to_string());
            at += 1 + end + closer.len();
            break;
        }
        if ch.is_ascii_uppercase() {
            let after = &rest[1..];
            let opener = ["<<<", "<<", "<", "«"]
                .into_iter()
                .find(|o| after.starts_with(o));
            if let Some(opener) = opener {
                // `E<...>` is an entity, and a letter after a word character
                // is not told apart from its text here.
                if previous.is_some_and(char::is_alphanumeric) {
                    return Err(Declined::Other);
                }
                let closer = match opener {
                    "<<<" => ">>>",
                    "<<" => ">>",
                    "<" => ">",
                    _ => "»",
                };
                if !buf.is_empty() {
                    atoms.push(Atom::Text(std::mem::take(&mut buf)));
                }
                if opener == "<<<" {
                    // A triple-delimited code holds its content as one raw
                    // string: no nested codes and no metadata are read in it.
                    let body = &after[3..];
                    let end = body
                        .find(">>>")
                        .ok_or(Declined::Unterminated(Some((ch, ">>>"))))?;
                    let content = &body[..end];
                    if body.starts_with('<')
                        || content.is_empty()
                        || ch == 'E'
                        || content.contains(['|', '«', '»'])
                    {
                        return Err(Declined::Other);
                    }
                    atoms.push(Atom::Markup(Markup {
                        letter: ch,
                        opener: "<<<",
                        closer: ">>>",
                        atoms: vec![Atom::Text(content.to_string())],
                        meta: None,
                        entities: None,
                    }));
                    at += 1 + 3 + end + 3;
                    previous = Some('>');
                    continue;
                }
                if ch == 'E' {
                    let body = &after[opener.len()..];
                    let end = body
                        .find(closer)
                        .ok_or(Declined::Unterminated(Some((ch, closer))))?;
                    let content = &body[..end];
                    if content.chars().any(|c| matches!(c, '<' | '>' | '«' | '»')) {
                        return Err(Declined::Other);
                    }
                    let (shown, list) = match content.split_once('|') {
                        Some((shown, list)) => {
                            (shown, crate::rakuast::pod_entities::pairs_after_bar(list))
                        }
                        None => ("", crate::rakuast::pod_entities::pairs(content)),
                    };
                    atoms.push(Atom::Markup(Markup {
                        letter: 'E',
                        opener: if opener == "<<" {
                            "<<"
                        } else if opener == "<" {
                            "<"
                        } else {
                            "«"
                        },
                        closer: if closer == ">>" {
                            ">>"
                        } else if closer == ">" {
                            ">"
                        } else {
                            "»"
                        },
                        atoms: if shown.is_empty() {
                            Vec::new()
                        } else {
                            vec![Atom::Text(shown.to_string())]
                        },
                        meta: None,
                        entities: Some(list),
                    }));
                    at += 1 + opener.len() + end + closer.len();
                    previous = closer.chars().last();
                    continue;
                }
                let (inner, used, inner_meta) = parse_atoms(
                    &after[opener.len()..],
                    Some(closer),
                    META_LETTERS.contains(ch),
                )
                .map_err(|why| match why {
                    // The code being read names itself; a deeper one already did.
                    Declined::Unterminated(None) => Declined::Unterminated(Some((ch, closer))),
                    other => other,
                })?;
                if inner.is_empty() && inner_meta.is_none() {
                    return Err(Declined::Other);
                }
                atoms.push(Atom::Markup(Markup {
                    letter: ch,
                    opener: if opener == "<<" {
                        "<<"
                    } else if opener == "<" {
                        "<"
                    } else {
                        "«"
                    },
                    closer: if closer == ">>" {
                        ">>"
                    } else if closer == ">" {
                        ">"
                    } else {
                        "»"
                    },
                    atoms: inner,
                    meta: inner_meta,
                    entities: None,
                }));
                at += 1 + opener.len() + used;
                previous = closer.chars().last();
                continue;
            }
        }
        // A balanced `<...>` / `«...»` inside a code is its text, codes in it
        // still read (`C< infix:<+> >`, `C<< < B<foo> > >>`).
        if closer.is_some() && matches!(ch, '<' | '«') {
            nest += 1;
        } else if closer.is_some() && matches!(ch, '>' | '»') {
            nest = nest.checked_sub(1).ok_or(Declined::Other)?;
        }
        buf.push(ch);
        previous = Some(ch);
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
