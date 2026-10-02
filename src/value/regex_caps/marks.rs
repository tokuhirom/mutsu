//! The `:ignoremark` view of a subject: the text with combining marks (and
//! grapheme-internal prepend/format characters) removed, plus the position
//! map back to the original. `MatchTarget` builds it once per subject and the
//! regex engine's `:ignoremark` path reads it, so it lives here below both
//! (#10779).

use unicode_normalization::UnicodeNormalization;
use unicode_normalization::char::is_combining_mark;
use unicode_segmentation::UnicodeSegmentation;

/// Strip combining marks (and prepend characters) from text, working by
/// grapheme cluster.  Returns stripped base chars and a position map from
/// stripped index to original char index.  The sentinel for one-past-end is
/// also appended.
// Cost: O(n), n = chars in `orig_chars`.
pub(crate) fn strip_marks_text(orig_chars: &[char]) -> (Vec<char>, Vec<usize>) {
    let text: String = orig_chars.iter().collect();
    let mut stripped_chars: Vec<char> = Vec::new();
    let mut pos_map: Vec<usize> = Vec::new(); // stripped idx -> original idx

    // Track the char-index offset as we iterate over grapheme clusters.
    let mut char_offset: usize = 0;
    for grapheme in text.graphemes(true) {
        let grapheme_start = char_offset;
        let grapheme_char_count = grapheme.chars().count();
        // NFD-decompose the entire grapheme and keep only non-combining-mark chars
        let bases: Vec<char> = grapheme.nfd().filter(|c| !is_combining_mark(*c)).collect();
        // Among the bases, drop Prepend characters and format characters (Cf)
        // that form part of multi-char grapheme clusters (e.g., ZWJ U+200D).
        // These are not the "base" letter of the grapheme.
        let is_multi_char = grapheme_char_count > 1;
        let filtered: Vec<char> = bases
            .into_iter()
            .filter(|c| !(is_prepend_char(*c) || is_multi_char && is_format_char(*c)))
            .collect();
        if filtered.is_empty() {
            // The entire grapheme is marks/format/prepends with no base — keep
            // the first non-combining char so the grapheme is not silently lost.
            for ch in grapheme.nfd() {
                if !is_combining_mark(ch) {
                    stripped_chars.push(ch);
                    pos_map.push(grapheme_start);
                    break;
                }
            }
        } else {
            for ch in filtered {
                stripped_chars.push(ch);
                pos_map.push(grapheme_start);
            }
        }
        char_offset += grapheme_char_count;
    }
    // sentinel for one-past-end
    pos_map.push(orig_chars.len());
    (stripped_chars, pos_map)
}

/// Check whether a character is a Unicode Prepend character (GCB=Prepend).
/// These are format characters that attach to the following character in a
/// grapheme cluster.
fn is_prepend_char(c: char) -> bool {
    matches!(c,
        '\u{0600}'..='\u{0605}'
        | '\u{06DD}'
        | '\u{070F}'
        | '\u{0890}'..='\u{0891}'
        | '\u{08E2}'
        | '\u{0D4E}'
        | '\u{110BD}'
        | '\u{110CD}'
        | '\u{111C2}'..='\u{111C3}'
        | '\u{1193F}'
        | '\u{11941}'
        | '\u{11A3A}'
        | '\u{11A84}'..='\u{11A89}'
        | '\u{11D46}'
    )
}

/// Check whether a character is a Unicode Format character (General_Category=Cf)
/// that commonly appears within grapheme clusters as a non-base element.
fn is_format_char(c: char) -> bool {
    matches!(c,
        '\u{00AD}'           // SOFT HYPHEN
        | '\u{200B}'         // ZERO WIDTH SPACE
        | '\u{200C}'         // ZERO WIDTH NON-JOINER
        | '\u{200D}'         // ZERO WIDTH JOINER
        | '\u{200E}'..='\u{200F}' // LRM, RLM
        | '\u{2060}'..='\u{2064}' // WORD JOINER, etc.
        | '\u{2066}'..='\u{2069}' // directional isolates
        | '\u{206A}'..='\u{206F}' // deprecated format chars
        | '\u{FEFF}'         // BOM / ZWNBSP
        | '\u{FE00}'..='\u{FE0F}' // Variation Selectors 1-16
        | '\u{E0100}'..='\u{E01EF}' // Variation Selectors 17-256
    )
}
