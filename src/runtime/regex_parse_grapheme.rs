//! Re-joining the tokenizer's per-codepoint literals into grapheme atoms.
//!
//! `parse_regex_structural` scans a pattern one `char` at a time, so a cluster
//! written in the source as several codepoints — `क्ष` (`क` U+0915, virama
//! U+094D, `ष` U+0937), or a base character followed by combining marks —
//! arrives as several [`RegexAtom::Literal`] tokens. Rakudo's regex grammar
//! works on graphemes instead: `/क्ष/` is *one* atom, and matching it must
//! consume the whole cluster. This pass restores that view before the pattern
//! is handed to the matcher.

use super::regex_types::{RegexAtom, RegexQuant, RegexToken};
use unicode_normalization::UnicodeNormalization;
use unicode_segmentation::UnicodeSegmentation;

/// Split `text` into one atom per grapheme cluster: a cluster of a single
/// codepoint stays a [`RegexAtom::Literal`], a longer one becomes a
/// [`RegexAtom::LiteralGrapheme`]. Every lowering that turns literal *text*
/// into literal *atoms* goes through here, so `/क्ष/`, `/'क्ष'/` and the
/// source-tree fast path all agree that the cluster is one atom.
pub(super) fn literal_grapheme_atoms(text: &str) -> Vec<RegexAtom> {
    text.graphemes(true)
        .map(|g| {
            let mut chars = g.chars();
            match (chars.next(), chars.next()) {
                (Some(c), None) => RegexAtom::Literal(c),
                // Raku strings are NFG, so the subject holds the composed
                // spelling: `"o\x[328]\x[304]"` *is* `"\x[1ED]"`. Compose the
                // pattern's cluster the same way, or the two spellings of one
                // grapheme would fail to match each other.
                _ => {
                    let composed: String = g.nfc().collect();
                    let mut composed_chars = composed.chars();
                    match (composed_chars.next(), composed_chars.next()) {
                        (Some(c), None) => RegexAtom::Literal(c),
                        _ => RegexAtom::LiteralGrapheme(composed.into()),
                    }
                }
            }
        })
        .collect()
}

/// True for a codepoint that a following one can join into a single grapheme:
/// any base character before a combining mark, and `\r` before `\n`. Every
/// other codepoint is its own cluster whatever follows it, so a run without one
/// of these needs no re-joining at all.
pub(super) fn can_start_a_longer_grapheme(c: char) -> bool {
    c == '\r' || unicode_normalization::char::is_combining_mark(c)
}

/// A token this pass may fold into a neighbouring one: a bare literal with no
/// capture, alias or separator attached. Its quantifier is checked separately —
/// only the last token of a run is allowed to carry one.
fn plain_literal_char(token: &RegexToken) -> Option<char> {
    let RegexAtom::Literal(ch) = token.atom else {
        return None;
    };
    (token.named_capture.is_none()
        && token.secondary_named_capture.is_none()
        && token.hash_capture.is_none()
        && !token.force_list_capture
        && token.separator.is_none())
    .then_some(ch)
}

fn same_flags(a: &RegexToken, b: &RegexToken) -> bool {
    a.ratchet == b.ratchet
        && a.frugal == b.frugal
        && a.from_runtime_interpolation == b.from_runtime_interpolation
}

/// True for a bare, uncaptured `\n` escape (compiled to [`RegexAtom::Newline`]
/// rather than `RegexAtom::Literal('\n')` — see `regex_parse_core.rs`, where
/// `\n` matches any Unicode logical newline, not just a literal LF).
fn plain_newline_token(token: &RegexToken) -> bool {
    matches!(token.atom, RegexAtom::Newline)
        && token.named_capture.is_none()
        && token.secondary_named_capture.is_none()
        && token.hash_capture.is_none()
        && !token.force_list_capture
        && token.separator.is_none()
        && matches!(token.quant, RegexQuant::One)
}

/// Re-join runs of adjacent plain literal tokens whose text spans fewer
/// graphemes than codepoints. A run that is already one grapheme per codepoint
/// — every ASCII pattern — is passed through untouched, so this costs one
/// segmentation pass and changes nothing for the overwhelming majority of
/// patterns.
pub(super) fn merge_grapheme_literal_tokens(tokens: Vec<RegexToken>) -> Vec<RegexToken> {
    if !tokens
        .iter()
        .any(|t| matches!(t.atom, RegexAtom::Literal(ch) if can_start_a_longer_grapheme(ch)))
    {
        return tokens;
    }

    let mut out: Vec<RegexToken> = Vec::with_capacity(tokens.len());
    let mut rest = tokens.into_iter().peekable();
    while let Some(first) = rest.next() {
        let Some(ch) = plain_literal_char(&first) else {
            out.push(first);
            continue;
        };
        if !matches!(first.quant, RegexQuant::One) {
            out.push(first);
            continue;
        }
        // Extend the run while the next token is another unquantified plain
        // literal with the same flags. One trailing quantified literal may
        // join: `/क्ष+/` quantifies the whole grapheme, because the grapheme
        // is the atom the `+` applies to.
        let mut text = String::from(ch);
        let mut last = first;
        while let Some(next_ch) = rest
            .peek()
            .filter(|next| same_flags(&last, next))
            .and_then(plain_literal_char)
        {
            let Some(next) = rest.next() else { break };
            let quantified = !matches!(next.quant, RegexQuant::One);
            text.push(next_ch);
            last = next;
            if quantified {
                break;
            }
        }
        // A run ending in `\r` may still be followed by a bare `\n` escape:
        // `\n` compiles to `RegexAtom::Newline` (it matches any logical-newline
        // sequence), not `RegexAtom::Literal('\n')`, so the loop above — which
        // only extends through `Literal` tokens — cannot see it and stops
        // right after the `\r`. Left unpaired, that `\r`'s own atomicity check
        // then rejects it outright: a `Literal` atom may never match only half
        // of the grapheme cluster `grapheme_end` reports ("\r" immediately
        // before "\n" is one CRLF cluster) — so `/\r\n/` failed to match even
        // a literal CRLF (LWP::Simple's ecosystem-parity hang: `\r\n` in
        // `parse_response`'s header-end split silently matched nothing, so the
        // header/body split never found the terminator). Absorbing the `\n`
        // into the same run, exactly as another `Literal` would be, sidesteps
        // that check instead of loosening it for every other cluster it
        // protects.
        if text.ends_with('\r')
            && let Some(next) = rest.peek()
            && plain_newline_token(next)
            && same_flags(&last, next)
        {
            let next = rest.next().expect("peeked");
            text.push('\n');
            last = next;
        }

        let atoms = literal_grapheme_atoms(&text);
        if atoms.len() == text.chars().count() {
            // Nothing merged: emit the run back as it came in. Every token in
            // it was unquantified except possibly the last, which keeps its
            // own quantifier.
            let mut chars = text.chars();
            let tail = chars.next_back();
            for c in chars {
                out.push(RegexToken {
                    atom: RegexAtom::Literal(c),
                    quant: RegexQuant::One,
                    ..last.clone()
                });
            }
            if let Some(c) = tail {
                out.push(RegexToken {
                    atom: RegexAtom::Literal(c),
                    ..last
                });
            }
            continue;
        }

        let count = atoms.len();
        for (i, atom) in atoms.into_iter().enumerate() {
            let quant = if i + 1 == count {
                last.quant.clone()
            } else {
                RegexQuant::One
            };
            out.push(RegexToken {
                atom,
                quant,
                ..last.clone()
            });
        }
    }
    out
}
