//! Unicode normalization of program text and decoded strings.

/// NFC-normalize a string. Raku's `Str` is NFG, so any string built from bytes
/// -- or read out of program source -- is normalized at creation: `Buf.new(0xE2,0x84,0xA6).decode('utf-8')`
/// composes U+2126 OHM SIGN to U+03A9 GREEK CAPITAL LETTER OMEGA, and the result
/// `eq`s a literal `"Ω"`. mutsu normalizes string *literals* at parse time
/// (`parser/primary/string/escapes.rs`) but did not normalize decode output, so
/// the two compared unequal even though `.ords` — which normalizes on read —
/// reported the same code points. Cro's percent-decoding of a query key hit
/// exactly this: `%E2%84%A6%E2%84%A6` decoded to a key no lookup could find.
pub(crate) fn nfc(s: String) -> String {
    use unicode_normalization::{IsNormalized, UnicodeNormalization, is_nfc_quick};
    // `is_nfc_quick` answers `Yes` without allocating for the overwhelmingly
    // common already-normalized case (all-ASCII text answers immediately).
    match is_nfc_quick(s.chars()) {
        IsNormalized::Yes => s,
        _ => s.nfc().collect(),
    }
}
