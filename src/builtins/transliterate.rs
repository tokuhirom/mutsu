//! Pure transliteration (tr///) implementation.
//!
//! This module provides character-mapping transliteration without requiring
//! access to the interpreter. It handles the `from`/`to` spec expansion
//! (including `a..z` ranges) and the `:delete`, `:squeeze`, `:complement`
//! modifiers.

/// Expand a tr-style spec string: `a..z` becomes all chars from 'a' to 'z'.
/// Handles ambiguous ranges like `A..H..Z` (= `A..Z`) and leading/trailing `..`.
fn expand_trans_spec(spec: &str) -> Vec<char> {
    let chars: Vec<char> = spec.chars().collect();
    let mut result = Vec::new();
    let mut i = 0;
    while i < chars.len() {
        // Check for `X..Y` range pattern
        if i + 3 < chars.len() && chars[i + 1] == '.' && chars[i + 2] == '.' {
            let start = chars[i] as u32;
            let end = chars[i + 3] as u32;
            if start <= end {
                for c in start..=end {
                    if let Some(ch) = char::from_u32(c) {
                        result.push(ch);
                    }
                }
            }
            i += 4;
            // Handle continuation ranges: `A..H..Z` means A..H then H..Z
            while i + 1 < chars.len() && chars[i] == '.' && chars[i + 1] == '.' {
                if i + 2 < chars.len() {
                    let prev_end = result.last().copied().unwrap_or('\0') as u32;
                    let new_end = chars[i + 2] as u32;
                    if prev_end < new_end {
                        for c in (prev_end + 1)..=new_end {
                            if let Some(ch) = char::from_u32(c) {
                                result.push(ch);
                            }
                        }
                    }
                    i += 3;
                } else {
                    // Trailing `..` -- add as literal dots
                    result.push('.');
                    result.push('.');
                    i += 2;
                }
            }
            continue;
        }
        result.push(chars[i]);
        i += 1;
    }
    result
}

/// Apply character-based transliteration to `text`.
///
/// `from_spec` and `to_spec` are tr-style spec strings (e.g. `"a..z"`, `"A..Z"`).
/// Modifiers:
/// - `delete`: unmapped characters in the `from` set are deleted
/// - `squeeze`: consecutive identical replacements are collapsed to one
/// - `complement`: characters NOT in `from` are replaced instead
pub fn transliterate(
    text: &str,
    from_spec: &str,
    to_spec: &str,
    delete: bool,
    squeeze: bool,
    complement: bool,
) -> String {
    let from_chars = expand_trans_spec(from_spec);
    let to_chars = expand_trans_spec(to_spec);

    if complement {
        return apply_complement(text, &from_chars, &to_chars, delete, squeeze);
    }

    let chars: Vec<char> = text.chars().collect();
    let mut result = String::new();
    let mut last_replacement: Option<char> = None;

    for &ch in &chars {
        if let Some(pos) = from_chars.iter().position(|&fc| fc == ch) {
            let replacement_char = if pos < to_chars.len() {
                Some(to_chars[pos])
            } else if delete {
                None
            } else if !to_chars.is_empty() {
                Some(*to_chars.last().unwrap())
            } else {
                None
            };

            if let Some(rc) = replacement_char {
                if squeeze {
                    if last_replacement != Some(rc) {
                        result.push(rc);
                        last_replacement = Some(rc);
                    }
                } else {
                    result.push(rc);
                    last_replacement = Some(rc);
                }
            }
            // If replacement_char is None (delete mode), skip the character
        } else {
            result.push(ch);
            last_replacement = None;
        }
    }

    result
}

/// Apply transliteration with `:complement` modifier.
/// Characters NOT in `from_chars` are replaced with the replacement character
/// (first char of `to_chars`), while characters in `from_chars` pass through unchanged.
fn apply_complement(
    text: &str,
    from_chars: &[char],
    to_chars: &[char],
    delete: bool,
    squeeze: bool,
) -> String {
    let complement_replacement = to_chars.first().copied();
    let chars: Vec<char> = text.chars().collect();
    let mut result = String::new();
    let mut last_was_complement = false;

    for &ch in &chars {
        if from_chars.contains(&ch) {
            // Character is in the from set -- pass through unchanged
            result.push(ch);
            last_was_complement = false;
        } else {
            // Character is NOT in the from set -- apply complement replacement
            if delete {
                // Skip character
            } else if squeeze {
                if !last_was_complement && let Some(rc) = complement_replacement {
                    result.push(rc);
                }
            } else if let Some(rc) = complement_replacement {
                result.push(rc);
            }
            last_was_complement = true;
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_transliterate() {
        assert_eq!(
            transliterate("hello", "aeiou", "AEIOU", false, false, false),
            "hEllO"
        );
    }

    #[test]
    fn test_range_expansion() {
        assert_eq!(
            transliterate("abc", "a..c", "A..C", false, false, false),
            "ABC"
        );
    }

    #[test]
    fn test_delete() {
        assert_eq!(transliterate("hello", "l", "", true, false, false), "heo");
    }

    #[test]
    fn test_squeeze() {
        // aa->xx squeezed to x, bb->yy squeezed to y, cc->zz squeezed to z
        assert_eq!(
            transliterate("aabbcc", "abc", "xyz", false, true, false),
            "xyz"
        );
        // Squeeze only collapses consecutive identical replacements
        assert_eq!(transliterate("aaaa", "a", "x", false, true, false), "x");
    }

    #[test]
    fn test_complement() {
        // Characters NOT in from set get replaced: h->x, e->x, l->l, l->l, o->x
        assert_eq!(
            transliterate("hello", "l", "x", false, false, true),
            "xxllx"
        );
    }

    #[test]
    fn test_expand_trans_spec() {
        assert_eq!(expand_trans_spec("a..e"), vec!['a', 'b', 'c', 'd', 'e']);
        assert_eq!(expand_trans_spec("x"), vec!['x']);
    }
}
