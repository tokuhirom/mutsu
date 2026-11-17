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

/// [`levenshtein_distance`] when the answer only matters up to `max`: the
/// exact distance when it is at most `max`, otherwise some value above `max`.
/// A row's minimum never decreases down the table, so once a whole row
/// exceeds `max` the distance does too.
// Cost: O(a * b), a and b = chars of the two strings; stops at the first row
// whose minimum exceeds `max`.
pub(crate) fn bounded_levenshtein_distance(a: &str, b: &str, max: usize) -> usize {
    let a_len = a.chars().count();
    let b_len = b.chars().count();
    if a_len.abs_diff(b_len) > max {
        return max + 1;
    }
    if a_len == 0 || b_len == 0 {
        return a_len.max(b_len);
    }
    let mut prev_row: Vec<usize> = (0..=b_len).collect();
    let mut curr_row = vec![0; b_len + 1];
    for (i, a_ch) in a.chars().enumerate() {
        curr_row[0] = i + 1;
        let mut row_min = curr_row[0];
        for (j, b_ch) in b.chars().enumerate() {
            let cost = usize::from(a_ch != b_ch);
            curr_row[j + 1] = (prev_row[j + 1] + 1)
                .min(curr_row[j] + 1)
                .min(prev_row[j] + cost);
            row_min = row_min.min(curr_row[j + 1]);
        }
        if row_min > max {
            return max + 1;
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

/// Catalog-MRO address -> its candidate names (see [`candidate_names`]).
type CandidateMemo = std::collections::HashMap<usize, std::sync::Arc<[&'static str]>>;

/// Every public method name a built-in `type_name` answers to: what Rakudo
/// declares on each class of its MRO and on the roles those classes compose
/// (its `.^methods(:all)`). A type the catalog does not list (a user class)
/// is offered the `Any`/`Mu` names.
///
/// The answer depends only on the catalog MRO the name selects, a `'static`
/// slice, so it is built once per MRO: every type the catalog does not list
/// shares the `Any`/`Mu` one. A program that probes a missing method of a user
/// class with `.?name` builds a `X::Method::NotFound` per probe, and this
/// table was rebuilt for each (~126k instructions, #17693).
// Cost: O(1) expected after the first call for the MRO; the first call is
// O(m + n^2), m = MRO levels and composed roles, n = names collected.
pub(crate) fn candidate_names(type_name: &str) -> std::sync::Arc<[&'static str]> {
    use crate::builtin_types::catalog::builtin_type_mro_strs;
    // A type the catalog does not list (a user class, `Any`, `Mu`) answers to
    // what every object does: the `Any` and `Mu` methods.
    static ANY_MU: [&str; 2] = ["Any", "Mu"];
    let mro: &'static [&'static str] = builtin_type_mro_strs(type_name).unwrap_or(&ANY_MU);
    static MEMO: std::sync::OnceLock<std::sync::Mutex<CandidateMemo>> = std::sync::OnceLock::new();
    let memo = MEMO.get_or_init(Default::default);
    // The catalog's slices are distinct statics, so the address names the MRO.
    let key = mro.as_ptr() as usize;
    if let Some(names) = memo.lock().unwrap_or_else(|e| e.into_inner()).get(&key) {
        return names.clone();
    }
    let names: std::sync::Arc<[&'static str]> = collect_candidate_names(mro).into();
    memo.lock()
        .unwrap_or_else(|e| e.into_inner())
        .insert(key, names.clone());
    names
}

/// [`candidate_names`]' table for one catalog MRO.
// Cost: O(m + n^2), m = MRO levels and composed roles, n = names collected.
fn collect_candidate_names(mro: &'static [&'static str]) -> Vec<&'static str> {
    use crate::builtin_types::catalog::builtin_type_roles_strs;
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
fn suggestions(method: &str, type_name: &str, extra: &[String]) -> Vec<String> {
    // A user class's own method names are not part of the static catalog the
    // memo is a pure function of: compute those directly (error path only).
    if !extra.is_empty() {
        return compute_suggestions(method, type_name, extra);
    }
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
    let found = compute_suggestions(method, type_name, &[]);
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
fn compute_suggestions(method: &str, type_name: &str, extra: &[String]) -> Vec<String> {
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
    let folded_len = folded.chars().count();
    let long = method.chars().count() > 5;
    let candidates = candidate_names(type_name);
    for name in candidates
        .iter()
        .copied()
        .chain(["HOW", "WHAT", "WHO"])
        .chain(extra.iter().map(String::as_str))
    {
        if name == method {
            continue;
        }
        // The edit distance is at least the difference in length, and only a
        // name at least as long can contain `folded`: a candidate failing both
        // can match neither way, so it is skipped before the O(l^2) distance
        // (most of a user class's ~200 candidates are, #17693). Lengths are
        // of the lowercased spellings, which the matches below compare.
        let name_len = if name.is_ascii() {
            name.len()
        } else {
            name.chars().flat_map(char::to_lowercase).count()
        };
        if name_len.abs_diff(folded_len) > max_length && !(long && name_len >= folded_len) {
            continue;
        }
        let name_folded = name.to_lowercase();
        if long && name_folded.contains(&folded) {
            found.insert(name, 0);
            continue;
        }
        let dist = bounded_levenshtein_distance(&folded, &name_folded, max_length);
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
/// then says `for string 'abc'`). `extra` holds the method names a user class
/// (and its parents and roles) declares, which Rakudo offers beside the
/// built-in ones (`.^methods(:all)`).
// Cost: see `suggestions`.
pub(crate) fn method_not_found_message_with(
    method: &str,
    type_name: &str,
    display_type: &str,
    string_invocant: Option<&str>,
    extra: &[String],
) -> String {
    let mut msg = match string_invocant {
        Some(text) => format!("No such method '{method}' for string '{text}'"),
        None => format!("No such method '{method}' for invocant of type '{display_type}'"),
    };
    let found = suggestions(method, type_name, extra);
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
    method_not_found_with(method, typename, &[])
}

/// [`method_not_found`] for a user class: `extra` holds the method names it
/// declares (see [`method_not_found_message_with`]).
pub(crate) fn method_not_found_with(method: &str, typename: &str, extra: &[String]) -> RuntimeError {
    let msg = method_not_found_message_with(method, typename, typename, None, extra);

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
    fn bounded_distance_agrees_with_the_exact_one_up_to_the_bound() {
        let words = [
            "", "a", "push", "pus", "kitten", "sitting", "uppercase", "upercase",
            "xml-namespace-prefix", "namespace", "elems", "elemz", "Ünïcode", "unicode",
        ];
        for a in words {
            for b in words {
                for max in 0..4 {
                    let exact = levenshtein_distance(a, b);
                    let bounded = bounded_levenshtein_distance(a, b, max);
                    if exact <= max {
                        assert_eq!(bounded, exact, "{a:?} {b:?} max {max}");
                    } else {
                        assert!(bounded > max, "{a:?} {b:?} max {max}");
                    }
                }
            }
        }
    }

    #[test]
    fn candidate_names_are_shared_per_mro() {
        let a = candidate_names("SomeUserClass");
        let b = candidate_names("AnotherUserClass");
        assert!(std::sync::Arc::ptr_eq(&a, &b), "unlisted types share the Any/Mu table");
        assert!(a.contains(&"say"));
        assert_eq!(&*candidate_names("Int"), &*collect_candidate_names(
            crate::builtin_types::catalog::builtin_type_mro_strs("Int").unwrap()
        ));
    }

    #[test]
    fn test_suggestions_follow_rakudo() {
        assert_eq!(suggestions("lazy", "Int", &[]), vec!["any", "say"]);
        assert_eq!(suggestions("elemz", "Int", &[]), vec!["elems"]);
        assert!(suggestions("sya", "Int", &[]).is_empty());
        assert!(suggestions("xyz", "Str", &[]).is_empty());
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
