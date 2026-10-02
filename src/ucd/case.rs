//! Unicode case mappings that are tables rather than runtime logic.

/// Unicode titlecase for the first character of a string.
/// Titlecase differs from uppercase for certain characters:
/// - 'ss' -> "Ss" (not "SS")
/// - digraph ligatures: special titlecase forms
pub(crate) fn unicode_titlecase_first(ch: char) -> String {
    match ch {
        // Latin digraph titlecase triples (lower | upper | title) → title form.
        // The title form titlecases to ITSELF, not to the full uppercase digraph.
        '\u{01C6}' | '\u{01C4}' | '\u{01C5}' => "\u{01C5}".to_string(), // Dž
        '\u{01C9}' | '\u{01C7}' | '\u{01C8}' => "\u{01C8}".to_string(), // Lj
        '\u{01CC}' | '\u{01CA}' | '\u{01CB}' => "\u{01CB}".to_string(), // Nj
        '\u{01F3}' | '\u{01F1}' | '\u{01F2}' => "\u{01F2}".to_string(), // Dz
        // Sharp S: titlecase is "Ss", not "SS"
        '\u{00DF}' => "Ss".to_string(),
        // Latin ligature titlecase mappings
        '\u{FB00}' => "Ff".to_string(),  // ff
        '\u{FB01}' => "Fi".to_string(),  // fi
        '\u{FB02}' => "Fl".to_string(),  // fl
        '\u{FB03}' => "Ffi".to_string(), // ffi
        '\u{FB04}' => "Ffl".to_string(), // ffl
        '\u{FB05}' => "St".to_string(),  // long st
        '\u{FB06}' => "St".to_string(),  // st
        // Default: use uppercase
        _ => ch.to_uppercase().to_string(),
    }
}
