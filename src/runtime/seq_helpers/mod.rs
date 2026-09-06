mod range_helpers;
mod regex_captures;
mod regex_interpolation;
/// Perl 5 pattern rewriting, used only by the PCRE2-backed `:P5` path.
#[cfg(feature = "pcre2")]
mod regex_transform;
mod role_type;
mod seq_arithmetic;
mod signature_helpers;
mod smart_match;
