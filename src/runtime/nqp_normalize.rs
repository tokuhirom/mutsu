//! Unicode normalization for `nqp::strtocodes` / `nqp::strfromcodes`.

pub(crate) enum Normalization {
    Nfc,
    Nfd,
    Nfkc,
    Nfkd,
}

/// `text` in normalization form `form`. ASCII text is returned borrowed:
/// no ASCII codepoint has a decomposition, a compatibility mapping or a
/// nonzero combining class, and no canonical composition has an all-ASCII
/// source pair, so all four forms map an ASCII string to itself.
/// (ADR-0116 D2.3: the normalizer was 5.2% of a JSON::Fast record, every
/// string of which is ASCII.)
// Cost: O(n), n = bytes of text (one ASCII scan; the normalizer only for non-ASCII input).
pub(crate) fn normalize(text: &str, form: Normalization) -> std::borrow::Cow<'_, str> {
    use unicode_normalization::UnicodeNormalization;
    if text.is_ascii() {
        return std::borrow::Cow::Borrowed(text);
    }
    std::borrow::Cow::Owned(match form {
        Normalization::Nfc => text.nfc().collect(),
        Normalization::Nfd => text.nfd().collect(),
        Normalization::Nfkc => text.nfkc().collect(),
        Normalization::Nfkd => text.nfkd().collect(),
    })
}
