//! The entity codes of a Pod paragraph, `E<gt>` / `E<0x41>` / `E<amp;lt>`:
//! rakudo reads them into the `meta` of a `Doc::Markup` as one pair per
//! `;`-separated item, the entity's character as its value. A name is looked
//! up in the HTML5 table (first code point only, as rakudo keeps it); a
//! number is a code point; anything else keeps its own spelling.

use std::collections::HashMap;
use std::sync::OnceLock;

/// The HTML5 entity names with the first code point of their characters
/// (`html_entities.txt`: name, TAB, hexadecimal code point).
static TABLE: &str = include_str!("html_entities.txt");

// Cost: O(1) after the first call (O(t) to build the index once), t = names.
fn table() -> &'static HashMap<&'static str, char> {
    static INDEX: OnceLock<HashMap<&'static str, char>> = OnceLock::new();
    INDEX.get_or_init(|| {
        TABLE
            .lines()
            .filter_map(|line| {
                let (name, code) = line.split_once('\t')?;
                Some((name, char::from_u32(u32::from_str_radix(code, 16).ok()?)?))
            })
            .collect()
    })
}

/// The code point a numeric item spells: decimal, `0x`, `0o` or `0b`.
// Cost: O(n), n = length of the item.
fn number(item: &str) -> Option<char> {
    let (digits, radix) = if let Some(d) = item.strip_prefix("0x") {
        (d, 16)
    } else if let Some(d) = item.strip_prefix("0o") {
        (d, 8)
    } else if let Some(d) = item.strip_prefix("0b") {
        (d, 2)
    } else {
        (item, 10)
    };
    if digits.is_empty() || !digits.chars().all(|c| c.is_digit(radix)) {
        return None;
    }
    char::from_u32(u32::from_str_radix(digits, radix).ok()?)
}

/// The key and value of the pair an entity `item` becomes.
// Cost: O(n), n = length of the item.
pub(super) fn pair(item: &str) -> (String, String) {
    let value = if item.is_empty() {
        '\0'.to_string()
    } else if let Some(c) = number(item) {
        c.to_string()
    } else if let Some(c) = table().get(item) {
        c.to_string()
    } else {
        format!("E<{item}>")
    };
    (item.to_string(), value)
}

/// The pairs of the `;`-separated entity list `text`; none for no text.
// Cost: O(n), n = length of the text.
pub(super) fn pairs(text: &str) -> Vec<(String, String)> {
    if text.is_empty() {
        return Vec::new();
    }
    text.split(';').map(pair).collect()
}

/// The pairs of the list after the `|` of `E<text|LIST>`: an empty list is
/// still one empty item.
// Cost: O(n), n = length of the text.
pub(super) fn pairs_after_bar(text: &str) -> Vec<(String, String)> {
    text.split(';').map(pair).collect()
}
