use crate::value::{RuntimeError, Value, ValueMap};

/// Levenshtein edit distance between two strings.
pub(crate) fn levenshtein_distance(a: &str, b: &str) -> usize {
    let a_len = a.chars().count();
    let b_len = b.chars().count();
    if a_len == 0 {
        return b_len;
    }
    if b_len == 0 {
        return a_len;
    }

    let mut prev_row: Vec<usize> = (0..=b_len).collect();
    let mut curr_row = vec![0; b_len + 1];

    for (i, a_ch) in a.chars().enumerate() {
        curr_row[0] = i + 1;
        for (j, b_ch) in b.chars().enumerate() {
            let cost = if a_ch == b_ch { 0 } else { 1 };
            curr_row[j + 1] = (prev_row[j + 1] + 1)
                .min(curr_row[j] + 1)
                .min(prev_row[j] + cost);
        }
        std::mem::swap(&mut prev_row, &mut curr_row);
    }

    prev_row[b_len]
}

/// `owner -> names` of the methods Rakudo declares directly on each built-in
/// owner (the `declared` lines of the committed Rakudo oracle snapshot).
fn declared_by_owner() -> &'static std::collections::HashMap<&'static str, Vec<&'static str>> {
    static TABLE: std::sync::OnceLock<std::collections::HashMap<&'static str, Vec<&'static str>>> =
        std::sync::OnceLock::new();
    TABLE.get_or_init(|| {
        include_str!("../builtins/rakudo_method_tables.txt")
            .lines()
            .filter_map(|line| {
                let mut fields = line.split('\t');
                (fields.next() == Some("declared")).then_some(())?;
                let owner = fields.next()?;
                Some((owner, fields.collect()))
            })
            .collect()
    })
}

/// Every public method name a built-in `type_name` answers to: what Rakudo
/// declares on each class of its MRO and on the roles those classes compose
/// (its `.^methods(:all)`). A type the catalog does not list (a user class)
/// is offered the `Any`/`Mu` names.
// Cost: O(m + n), m = MRO levels and composed roles, n = names collected (a dedup scan per name; error path only).
fn candidate_names(type_name: &str) -> Vec<&'static str> {
    use crate::builtin_types::catalog::{builtin_type_mro_strs, builtin_type_roles_strs};
    let mro = builtin_type_mro_strs(type_name)
        .or_else(|| builtin_type_mro_strs("Any"))
        .unwrap_or(&[]);
    let table = declared_by_owner();
    let mut names: Vec<&'static str> = Vec::new();
    let mut push_owner = |owner: &str| {
        for &name in table.get(owner).into_iter().flatten() {
            if !name.starts_with('!') && !names.contains(&name) {
                names.push(name);
            }
        }
    };
    for class in mro {
        push_owner(class);
        for role in builtin_type_roles_strs(class) {
            push_owner(role.split_once('[').map_or(*role, |(base, _)| base));
        }
    }
    names
}

/// Most distinct `(type, method)` pairs the suggestion memo holds before it
/// starts over: the keys come from the program, so the memo is bounded.
const SUGGESTION_MEMO_LIMIT: usize = 1024;

type SuggestionMemo =
    std::collections::HashMap<Box<str>, std::collections::HashMap<Box<str>, Vec<String>>>;

thread_local! {
    /// `type -> method -> suggestions`, with the number of entries held.
    static SUGGESTION_MEMO: std::cell::RefCell<(SuggestionMemo, usize)> =
        std::cell::RefCell::new((SuggestionMemo::new(), 0));
}

/// Rakudo's `X::Method::NotFound` suggestions: every candidate within
/// `max_length` edits of `method` (a long name also matches any candidate
/// that contains it), ordered by distance then name, cut at the distance of
/// the fourth entry.
///
/// The answer is a pure function of the two names (the candidates come from
/// the static Rakudo method catalog), so it is memoized: `.?name` and `.*name`
/// on a type without the method build and discard this error on every call,
/// and the edit-distance walk over every candidate was ~1.3M instructions of
/// it (Red's `$attr.type.?inflator` paid that per column).
// Cost: O(1) expected on a memo hit; O(c * l^2 + c log c) on a miss, c = candidates, l = longest name.
fn suggestions(method: &str, type_name: &str) -> Vec<String> {
    let hit = SUGGESTION_MEMO.with(|memo| {
        memo.borrow()
            .0
            .get(type_name)
            .and_then(|by_method| by_method.get(method))
            .cloned()
    });
    if let Some(found) = hit {
        return found;
    }
    let found = compute_suggestions(method, type_name);
    SUGGESTION_MEMO.with(|memo| {
        let (table, held) = &mut *memo.borrow_mut();
        if *held >= SUGGESTION_MEMO_LIMIT {
            table.clear();
            *held = 0;
        }
        table
            .entry(type_name.into())
            .or_default()
            .insert(method.into(), found.clone());
        *held += 1;
    });
    found
}

// Cost: O(c * l^2 + c log c), c = candidates, l = longest name.
fn compute_suggestions(method: &str, type_name: &str) -> Vec<String> {
    let max_length = match method.chars().count() {
        0..=3 => 1,
        4..=8 => 2,
        _ => 3,
    };
    let mut found: std::collections::BTreeMap<&str, usize> = std::collections::BTreeMap::new();
    match method {
        "length" => {
            let mro = crate::builtin_types::catalog::builtin_type_mro_strs(type_name).unwrap_or(&[]);
            let names: &[&str] = if mro.contains(&"List") {
                &["elems"]
            } else if mro.contains(&"Cool") {
                &["chars", "codes"]
            } else {
                &["elems", "chars", "codes"]
            };
            found.extend(names.iter().map(|n| (*n, 0)));
        }
        "bytes" => {
            found.insert("encode($encoding).bytes", 0);
        }
        "ceil" => {
            found.insert("ceiling", 0);
        }
        "last" => {
            found.insert("tail", 0);
        }
        "Uni" if type_name == "Str" => {
            found.extend(["NFC", "NFD", "NFKC", "NFKD"].iter().map(|n| (*n, 0)));
        }
        _ => {}
    }
    let folded = method.to_lowercase();
    let long = method.chars().count() > 5;
    let candidates = candidate_names(type_name);
    for name in candidates
        .iter()
        .copied()
        .chain(["HOW", "WHAT", "WHO"])
    {
        if name == method {
            continue;
        }
        let name_folded = name.to_lowercase();
        if long && name_folded.contains(&folded) {
            found.insert(name, 0);
            continue;
        }
        let dist = levenshtein_distance(&folded, &name_folded);
        if dist <= max_length {
            found.insert(name, dist);
        }
    }
    let mut sorted: Vec<(&str, usize)> = found.into_iter().collect();
    sorted.sort_by(|a, b| a.1.cmp(&b.1).then_with(|| a.0.cmp(b.0)));
    if sorted.is_empty() {
        return Vec::new();
    }
    let boundary = sorted[(sorted.len() - 1).min(3)].1;
    sorted
        .into_iter()
        .filter(|(_, d)| *d <= boundary)
        .map(|(n, _)| n.to_string())
        .collect()
}

/// Rakudo's `Str.naive-word-wrapper`: words are joined with single spaces and
/// a line is broken once adding a word would reach `max` columns; a word too
/// long for a line sits on a line of its own.
// Cost: O(n), n = chars in `text`.
pub(crate) fn naive_word_wrap(text: &str, max: usize) -> String {
    let mut lines: Vec<String> = Vec::new();
    let mut line: Vec<&str> = Vec::new();
    let mut width = 0usize;
    for word in text.split_whitespace() {
        let visible = word.chars().count();
        if width + visible >= max {
            if line.is_empty() {
                lines.push(word.to_string());
                width = 0;
            } else {
                lines.push(line.join(" "));
                line = vec![word];
                width = visible;
            }
        } else {
            line.push(word);
            width += 1 + visible;
        }
    }
    if !line.is_empty() {
        lines.push(line.join(" "));
    }
    lines.join("\n")
}

/// The text of Rakudo's `X::Method::NotFound.message`: the sentence naming
/// the invocant, the "Did you mean" tip appended to it, wrapped at 72 columns.
/// `string_invocant` is the stringified invocant when it is a `Str` (Rakudo
/// then says `for string 'abc'`).
// Cost: see `suggestions`.
pub(crate) fn method_not_found_message(
    method: &str,
    type_name: &str,
    display_type: &str,
    string_invocant: Option<&str>,
) -> String {
    let mut msg = match string_invocant {
        Some(text) => format!("No such method '{method}' for string '{text}'"),
        None => format!("No such method '{method}' for invocant of type '{display_type}'"),
    };
    let found = suggestions(method, type_name);
    match found.as_slice() {
        [] => {}
        [one] => msg.push_str(&format!(". Did you mean '{one}'?")),
        many => {
            let quoted: Vec<String> = many.iter().map(|n| format!("'{n}'")).collect();
            msg.push_str(&format!(
                ". Did you mean any of these: {}?",
                quoted.join(", ")
            ));
        }
    }
    naive_word_wrap(&msg, 72)
}

/// X::Method::NotFound - No such method
pub(crate) fn method_not_found(method: &str, typename: &str) -> RuntimeError {
    let msg = method_not_found_message(method, typename, typename, None);

    let mut attrs = ValueMap::default();
    attrs.insert("method".to_string(), Value::str(method.to_string()));
    attrs.insert("typename".to_string(), Value::str(typename.to_string()));
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    RuntimeError::typed("X::Method::NotFound", attrs)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_levenshtein_distance() {
        assert_eq!(levenshtein_distance("", ""), 0);
        assert_eq!(levenshtein_distance("abc", "abc"), 0);
        assert_eq!(levenshtein_distance("abc", "ab"), 1);
        assert_eq!(levenshtein_distance("abc", "abcd"), 1);
        assert_eq!(levenshtein_distance("kitten", "sitting"), 3);
        assert_eq!(levenshtein_distance("uppercase", "upercase"), 1);
        assert_eq!(levenshtein_distance("push", "pus"), 1);
    }

    #[test]
    fn test_suggestions_follow_rakudo() {
        assert_eq!(suggestions("lazy", "Int"), vec!["any", "say"]);
        assert_eq!(suggestions("elemz", "Int"), vec!["elems"]);
        assert!(suggestions("sya", "Int").is_empty());
        assert!(suggestions("xyz", "Str").is_empty());
    }

    #[test]
    fn test_naive_word_wrap() {
        assert_eq!(
            naive_word_wrap(
                "No such method 'lazy' for invocant of type 'Int'. Did you mean any of these: 'any', 'say'?",
                72
            ),
            "No such method 'lazy' for invocant of type 'Int'. Did you mean any of\nthese: 'any', 'say'?"
        );
    }
}
