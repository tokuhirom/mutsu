//! Unicode character names: `char` -> `Name` (`.uniname`, `uninames`) and
//! `Name` -> `char` (`\c[NAME]`, `uniparse`).
//!
//! The data is the committed, generated [`super::unicode_name_data`] table
//! (see `unicode_name_gen.rs` for how it is derived and verified). Two families
//! are computed rather than stored, as the UCD itself does: `CJK UNIFIED
//! IDEOGRAPH-<hex>` over [`data::CJK_RANGES`], and `HANGUL SYLLABLE <jamo>`
//! over U+AC00..U+D7A3.
//!
//! Name lookup uses the UAX #44 LM2 loose-matching rule: ignore case,
//! whitespace, `_` and medial hyphens, except the hyphen of U+1180 HANGUL
//! JUNGSEONG O-E.
//!
//! Multi-word aliases (`NameAliases.txt`) and the derived names Rakudo also accepts
//! (`TANGUT IDEOGRAPH-17000`) are layered on top of this by the caller,
//! `token_kind::lookup_unicode_char_by_name`.

use super::unicode_name_data as data;

const HANGUL_PREFIX: &str = "HANGUL SYLLABLE ";
const NORMALIZED_HANGUL_PREFIX: &[u8] = b"HANGULSYLLABLE";
const CJK_PREFIX: &str = "CJK UNIFIED IDEOGRAPH-";
const NORMALIZED_CJK_PREFIX: &[u8] = b"CJKUNIFIEDIDEOGRAPH";
const HANGUL_BASE: u32 = 0xAC00;
const HANGUL_LAST: u32 = 0xD7A3;
/// U+1180 HANGUL JUNGSEONG O-E: the one name whose medial hyphen LM2 keeps,
/// because without it the name collides with U+116C HANGUL JUNGSEONG OE.
const JUNGSEONG_O_E: u32 = 0x1180;
const JUNGSEONG_OE: u32 = 0x116C;

// Jamo short names (Jamo.txt), in syllable-decomposition order.
const CHOSEONG: [&str; 19] = [
    "G", "GG", "N", "D", "DD", "R", "M", "B", "BB", "S", "SS", "", "J", "JJ", "C", "K", "T", "P",
    "H",
];
const JUNGSEONG: [&str; 21] = [
    "A", "AE", "YA", "YAE", "EO", "E", "YEO", "YE", "O", "WA", "WAE", "OE", "YO", "U", "WEO", "WE",
    "WI", "YU", "EU", "YI", "I",
];
const JONGSEONG: [&str; 28] = [
    "", "G", "GG", "GS", "N", "NJ", "NH", "D", "L", "LG", "LM", "LB", "LS", "LT", "LP", "LH", "M",
    "B", "BS", "S", "SS", "NG", "J", "C", "K", "T", "P", "H",
];

fn is_cjk_unified_ideograph(cp: u32) -> bool {
    data::CJK_RANGES
        .iter()
        .any(|&(lo, hi)| (lo..=hi).contains(&cp))
}

/// Append the stored name of table entry `i` to `out`.
fn push_stored_name(i: usize, out: &mut String) {
    let tokens = &data::TOKENS[data::NAME_START[i] as usize..data::NAME_START[i + 1] as usize];
    for (k, &tok) in tokens.iter().enumerate() {
        let w = (tok & 0x7FFF) as usize;
        out.push_str(
            &data::WORD_TEXT[data::WORD_START[w] as usize..data::WORD_START[w + 1] as usize],
        );
        if k + 1 < tokens.len() {
            out.push(if tok & 0x8000 != 0 { '-' } else { ' ' });
        }
    }
}

/// The Unicode `Name` of `c`, or `None` when it has none (controls,
/// unassigned codepoints, and the derived-name families other than CJK
/// unified ideographs and Hangul syllables).
// Cost: O(log N + m), N = stored names, m = name length.
pub(crate) fn char_name(c: char) -> Option<String> {
    let cp = c as u32;
    if let Ok(i) = data::CODES.binary_search(&cp) {
        let mut out = String::new();
        push_stored_name(i, &mut out);
        return Some(out);
    }
    if is_cjk_unified_ideograph(cp) {
        return Some(format!("{CJK_PREFIX}{cp:X}"));
    }
    if (HANGUL_BASE..=HANGUL_LAST).contains(&cp) {
        let n = cp - HANGUL_BASE;
        return Some(format!(
            "{HANGUL_PREFIX}{}{}{}",
            CHOSEONG[(n / (28 * 21)) as usize],
            JUNGSEONG[((n / 28) % 21) as usize],
            JONGSEONG[(n % 28) as usize]
        ));
    }
    None
}

/// LM2-normalize `name` into `out`: upper-case it and drop whitespace, `_`
/// and medial hyphens (a `-` between two ASCII alphanumerics), unless
/// `keep_medial_hyphens`. Returns `false` when `name` holds a character no
/// Unicode name can contain.
pub(super) fn normalize_into(name: &str, keep_medial_hyphens: bool, out: &mut Vec<u8>) -> bool {
    out.clear();
    let bytes = name.as_bytes();
    for (i, &b) in bytes.iter().enumerate() {
        let c = b.to_ascii_uppercase();
        if c.is_ascii_whitespace() || c == b'_' {
            continue;
        }
        if c == b'-' {
            let medial = i > 0
                && bytes[i - 1].is_ascii_alphanumeric()
                && bytes.get(i + 1).is_some_and(u8::is_ascii_alphanumeric);
            if medial && !keep_medial_hyphens {
                continue;
            }
        } else if !c.is_ascii_alphanumeric() {
            return false;
        }
        out.push(c);
    }
    true
}

/// The LM2-normalized key of stored entry `i`, written into `out`.
fn stored_key(i: usize, name_buf: &mut String, out: &mut Vec<u8>) {
    name_buf.clear();
    push_stored_name(i, name_buf);
    normalize_into(name_buf, data::CODES[i] == JUNGSEONG_O_E, out);
}

/// Strip the longest entry of `table` that prefixes `s`, returning its index.
fn shift_longest<'a>(table: &[&str], s: &'a [u8]) -> Option<(u32, &'a [u8])> {
    table
        .iter()
        .enumerate()
        .filter(|(_, part)| s.starts_with(part.as_bytes()))
        .max_by_key(|(_, part)| part.len())
        .map(|(i, part)| (i as u32, &s[part.len()..]))
}

fn hangul_from_normalized(rest: &[u8]) -> Option<char> {
    let (l, rest) = shift_longest(&CHOSEONG, rest)?;
    let (v, rest) = shift_longest(&JUNGSEONG, rest)?;
    let (t, rest) = shift_longest(&JONGSEONG, rest)?;
    if !rest.is_empty() {
        return None;
    }
    char::from_u32(HANGUL_BASE + (l * 21 + v) * 28 + t)
}

fn cjk_from_normalized(hex: &[u8]) -> Option<char> {
    if hex.is_empty() || hex.len() > 5 || !hex.iter().all(u8::is_ascii_hexdigit) {
        return None;
    }
    // Normalized input is upper-case, so lower-case hex never reaches here.
    let cp = u32::from_str_radix(std::str::from_utf8(hex).ok()?, 16).ok()?;
    is_cjk_unified_ideograph(cp)
        .then(|| char::from_u32(cp))
        .flatten()
}

/// The character whose Unicode `Name` loosely (UAX #44 LM2) matches `name`.
// Cost: O(m log N), m = name length, N = stored names.
pub(crate) fn char_by_name(name: &str) -> Option<char> {
    let mut key = Vec::with_capacity(name.len());
    if !normalize_into(name, false, &mut key) || key.is_empty() {
        return None;
    }
    if let Some(rest) = key.strip_prefix(NORMALIZED_HANGUL_PREFIX) {
        return hangul_from_normalized(rest);
    }
    if let Some(hex) = key.strip_prefix(NORMALIZED_CJK_PREFIX) {
        return cjk_from_normalized(hex);
    }
    let mut name_buf = String::new();
    let mut stored = Vec::new();
    let Ok(pos) = data::BY_NAME.binary_search_by(|&i| {
        stored_key(i as usize, &mut name_buf, &mut stored);
        stored.as_slice().cmp(key.as_slice())
    }) else {
        // A one-word alias (`BACKSPACE`, `NBSP`, `VS17`) spelled loosely: the
        // normalized key equals the alias itself. Multi-word aliases are
        // matched exactly by the caller.
        return std::str::from_utf8(&key)
            .ok()
            .and_then(super::unicode_name_alias_table::lookup_name_alias);
    };
    let cp = data::CODES[data::BY_NAME[pos] as usize];
    // "HANGUL JUNGSEONG O-E" normalizes like "...OE" (its hyphen is medial in
    // the query), so the only spelling that tells them apart is a hyphen
    // right before the final letter.
    if cp == JUNGSEONG_OE
        && name
            .trim_end_matches(|c: char| c.is_ascii_whitespace() || c == '_')
            .bytes()
            .nth_back(1)
            == Some(b'-')
    {
        return char::from_u32(JUNGSEONG_O_E);
    }
    char::from_u32(cp)
}
