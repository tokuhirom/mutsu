/// Levenshtein edit distance between two strings.
pub(crate) fn levenshtein_distance(a: &str, b: &str) -> usize {
    let a_len = a.len();
    let b_len = b.len();
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

/// Find the closest match to `name` from `candidates`.
/// Returns `Some(suggestion)` if a close enough match exists.
pub(crate) fn suggest_method(name: &str, candidates: &[&str]) -> Option<String> {
    let max_distance = if name.len() <= 3 {
        1
    } else if name.len() <= 6 {
        2
    } else {
        3
    };

    let mut best: Option<(&str, usize)> = None;
    for &candidate in candidates {
        let dist = levenshtein_distance(name, candidate);
        if dist > 0 && dist <= max_distance {
            if let Some((_, best_dist)) = best {
                if dist < best_dist {
                    best = Some((candidate, dist));
                }
            } else {
                best = Some((candidate, dist));
            }
        }
    }
    best.map(|(s, _)| s.to_string())
}

/// Return a list of commonly available methods for a given type name.
pub(crate) fn known_methods_for_type(type_name: &str) -> &'static [&'static str] {
    match type_name {
        "Str" => &[
            "chars",
            "chomp",
            "chop",
            "comb",
            "contains",
            "encode",
            "ends-with",
            "fc",
            "flip",
            "gist",
            "index",
            "indices",
            "lc",
            "lines",
            "match",
            "ord",
            "perl",
            "raku",
            "pred",
            "rindex",
            "samemark",
            "split",
            "starts-with",
            "substr",
            "succ",
            "tc",
            "tclc",
            "trim",
            "trim-leading",
            "trim-trailing",
            "uc",
            "uniname",
            "uninames",
            "unival",
            "uppercase",
            "lowercase",
            "wordcase",
            "words",
            "IO",
            "Int",
            "Num",
            "Numeric",
            "Rat",
            "Bool",
            "Str",
            "codes",
            "elems",
            "fmt",
            "substr-eq",
            "substr-rw",
            "chrs",
            "parse-base",
            "base",
            "NFC",
            "NFD",
            "NFKC",
            "NFKD",
            "WHICH",
            "WHY",
            "say",
            "put",
            "print",
            "note",
            "defined",
            "so",
            "not",
            "self",
            "clone",
            "new",
            "WHAT",
            "WHO",
            "HOW",
        ],
        "Int" | "Num" | "Rat" | "FatRat" | "Complex" => &[
            "abs",
            "base",
            "ceiling",
            "chr",
            "denominator",
            "exp",
            "floor",
            "gist",
            "is-prime",
            "log",
            "log2",
            "log10",
            "msb",
            "lsb",
            "narrow",
            "nude",
            "numerator",
            "perl",
            "raku",
            "polymod",
            "pred",
            "rand",
            "round",
            "sign",
            "sqrt",
            "succ",
            "truncate",
            "Bool",
            "Int",
            "Num",
            "Numeric",
            "Rat",
            "Str",
            "defined",
            "so",
            "not",
            "WHAT",
            "WHICH",
            "new",
            "clone",
            "gcd",
            "lcm",
            "is-int",
        ],
        "Array" | "List" => &[
            "append",
            "antipairs",
            "AT-POS",
            "Bool",
            "classify",
            "combinations",
            "duckmap",
            "deepmap",
            "elems",
            "end",
            "first",
            "flat",
            "fmt",
            "gist",
            "grep",
            "head",
            "join",
            "keys",
            "kv",
            "map",
            "max",
            "maxpairs",
            "min",
            "minpairs",
            "minmax",
            "mix",
            "new",
            "pairs",
            "perl",
            "permutations",
            "pick",
            "pop",
            "prepend",
            "produce",
            "push",
            "raku",
            "race",
            "reduce",
            "repeated",
            "reverse",
            "roll",
            "rotate",
            "shift",
            "shuffle",
            "skip",
            "slice",
            "sort",
            "splice",
            "squish",
            "supply",
            "tail",
            "unique",
            "unshift",
            "values",
            "Str",
            "defined",
            "so",
            "not",
            "WHAT",
            "WHICH",
            "clone",
            "categorize",
            "batch",
            "rotor",
        ],
        "Hash" | "Map" => &[
            "antipairs",
            "AT-KEY",
            "Bool",
            "classify-list",
            "defined",
            "elems",
            "EXISTS-KEY",
            "DELETE-KEY",
            "first",
            "flat",
            "fmt",
            "gist",
            "grep",
            "invert",
            "keys",
            "kv",
            "map",
            "new",
            "pairs",
            "perl",
            "push",
            "raku",
            "sort",
            "values",
            "Str",
            "so",
            "not",
            "WHAT",
            "WHICH",
            "clone",
        ],
        "Bool" => &[
            "defined", "gist", "perl", "raku", "so", "not", "Bool", "Int", "Num", "Numeric", "Str",
            "succ", "pred", "pick", "roll", "WHAT", "WHICH",
        ],
        "Range" => &[
            "bounds",
            "elems",
            "excludes-max",
            "excludes-min",
            "first",
            "flat",
            "fmt",
            "gist",
            "grep",
            "head",
            "infinite",
            "int-bounds",
            "is-int",
            "keys",
            "kv",
            "list",
            "map",
            "max",
            "min",
            "minmax",
            "new",
            "pairs",
            "perl",
            "pick",
            "raku",
            "rand",
            "reverse",
            "roll",
            "tail",
            "values",
            "Bool",
            "defined",
            "so",
            "not",
            "Str",
            "WHAT",
            "WHICH",
        ],
        "Match" => &[
            "ast",
            "Bool",
            "caps",
            "chunks",
            "defined",
            "elems",
            "from",
            "gist",
            "hash",
            "keys",
            "kv",
            "list",
            "made",
            "make",
            "orig",
            "pairs",
            "perl",
            "pos",
            "postmatch",
            "prematch",
            "raku",
            "Str",
            "to",
            "values",
            "WHAT",
            "WHICH",
        ],
        "Regex" => &[
            "Bool", "defined", "gist", "ACCEPTS", "perl", "raku", "Str", "WHAT", "WHICH",
        ],
        _ => &[
            // Universal methods available on all types
            "defined", "gist", "perl", "raku", "Str", "Bool", "Int", "Num", "Numeric", "WHAT",
            "WHICH", "WHO", "HOW", "WHY", "so", "not", "clone", "new", "say", "put", "print",
            "note", "self", "elems", "keys", "values", "kv", "pairs", "map", "grep", "first",
            "sort", "flat", "head", "tail",
        ],
    }
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
    fn test_suggest_method() {
        let candidates = &["uppercase", "lowercase", "trim", "split"];
        assert_eq!(
            suggest_method("upercase", candidates),
            Some("uppercase".to_string())
        );
        assert_eq!(
            suggest_method("uupercase", candidates),
            Some("uppercase".to_string())
        );
        assert_eq!(suggest_method("xyz", candidates), None);
    }

    #[test]
    fn test_suggest_method_for_type() {
        let methods = known_methods_for_type("Str");
        assert_eq!(
            suggest_method("upercase", methods),
            Some("uppercase".to_string())
        );
    }
}
