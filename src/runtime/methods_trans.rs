use super::*;

mod apply;
mod rules;

/// A single translation rule used by `.trans`.
enum TransRule {
    /// Character-by-character mapping (from tr///‐style string pairs).
    CharMap {
        from_chars: Vec<char>,
        to_chars: Vec<char>,
        /// When the replacement is shorter than the key: `true` cycles the
        /// replacement (the Str=>Str first-multi form, `'123' => 'þð'` gives
        /// `þðþ`), `false` repeats the last replacement char (the list form).
        cycle: bool,
    },
    /// Multi-character token mapping (from array pairs).
    TokenMap {
        from_tokens: Vec<String>,
        to_tokens: Vec<String>,
    },
    /// Regex-based replacement (static string).
    Regex {
        pattern: String,
        replacement: String,
    },
    /// Character-based mapping with closure replacement.
    CharClosure {
        from_chars: Vec<char>,
        closure: Value,
    },
    /// Regex-based replacement with closure.
    RegexClosure { pattern: String, closure: Value },
    /// Multi-character token mapping where some (or all) replacements are closures.
    /// Each entry in `to_values` is either a static string or a closure.
    TokenClosureMap {
        from_tokens: Vec<String>,
        to_values: Vec<TokenReplacement>,
    },
}

/// A replacement value for a token: either a static string or a closure to call.
enum TokenReplacement {
    Static(String),
    Closure(Value),
}

/// A Seq/Slip key or value in a `.trans` pair is list-like; materialize it to
/// an Array so the list-form dispatch recognizes it (`"abc".comb => 1..2`).
/// Any other value is returned unchanged (Arc-cheap clone).
fn normalize_trans_operand(v: &Value) -> Value {
    // Decontainerize first: `"...".trans(%matcher.pairs)` feeds `trans` pairs
    // whose value is the hash element's own `Scalar` container (ADR-0036 slice
    // 3), and every test below asks what the operand *is* — `is_closure`, the
    // Regex/Array/Range shape match — which a `ContainerRef` would answer `no`
    // to, silently turning a closure replacement into a stringified one
    // (roast/S05-transliteration/with-closure.t).
    let v = v.deref_container().deitemize_element();
    match v.view() {
        ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => {
            Value::array(items.to_vec())
        }
        ValueView::Slip(items) => Value::array(items.to_vec()),
        _ => v.clone(),
    }
}

/// A grapheme unit's single codepoint, or `None` when it is empty or a
/// multi-codepoint cluster (e.g. a combining-mark sequence) — such a unit is
/// not eligible for a `..` range endpoint.
fn single_char(unit: &str) -> Option<char> {
    let mut it = unit.chars();
    let c = it.next()?;
    if it.next().is_some() { None } else { Some(c) }
}

/// Expand a tr-style spec string: `a..z` becomes all chars from 'a' to 'z'.
/// Handles ambiguous ranges like `A..H..Z` (= `A..Z`) and leading/trailing `..`.
///
/// Splits `spec` into Raku characters (extended grapheme clusters), not raw
/// Unicode codepoints, so a from/to pair whose alphabets each contain
/// combining-mark graphemes (e.g. `ſ̣`) stays index-aligned between the two
/// sides even when the marks fall at different string offsets in each
/// (`Acme::Text::UpsideDown`'s `$up`/`$down` alphabets do exactly this —
/// splitting by codepoint desynced every position after the first mark).
fn expand_trans_spec(spec: &str) -> Vec<String> {
    let units = crate::builtins::string_pos::grapheme_units(spec);
    let mut result: Vec<String> = Vec::new();
    let mut i = 0;
    while i < units.len() {
        // Check for `X..Y` range pattern (only meaningful when both endpoints
        // are a single codepoint).
        if i + 3 < units.len()
            && units[i + 1] == "."
            && units[i + 2] == "."
            && let (Some(start), Some(end)) = (single_char(units[i]), single_char(units[i + 3]))
        {
            let start = start as u32;
            let end = end as u32;
            if start <= end {
                for c in start..=end {
                    if let Some(ch) = char::from_u32(c) {
                        result.push(ch.to_string());
                    }
                }
            }
            i += 4;
            // Handle continuation ranges: `A..H..Z` means A..H then H..Z
            while i + 1 < units.len() && units[i] == "." && units[i + 1] == "." {
                if i + 2 < units.len()
                    && let Some(new_end) = single_char(units[i + 2])
                {
                    let prev_end = result
                        .last()
                        .and_then(|s| single_char(s))
                        .map(|c| c as u32)
                        .unwrap_or(0);
                    let new_end = new_end as u32;
                    // Skip the range start since it was already added
                    if prev_end < new_end {
                        for c in (prev_end + 1)..=new_end {
                            if let Some(ch) = char::from_u32(c) {
                                result.push(ch.to_string());
                            }
                        }
                    }
                    i += 3;
                } else {
                    // Trailing `..` — add as literal dots
                    result.push(".".to_string());
                    result.push(".".to_string());
                    i += 2;
                }
            }
            continue;
        }
        result.push(units[i].to_string());
        i += 1;
    }
    result
}

/// Convert a Value to a list of strings (for collection-based trans pairs).
/// Nested list-like values are flattened, Ranges are iterated, and strings are
/// expanded via `expand_trans_spec`. `Str.trans` accepts a Pair whose key and
/// value can be nested collections, as in the Rosetta Code Rot-13 example.
fn value_to_string_list(v: &Value) -> Vec<String> {
    match v.view() {
        ValueView::Array(items, ..) => items
            .iter()
            .flat_map(trans_collection_item_to_strings)
            .collect(),
        ValueView::Seq(items) | ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => items
            .iter()
            .flat_map(trans_collection_item_to_strings)
            .collect(),
        ValueView::Slip(items) => items
            .iter()
            .flat_map(trans_collection_item_to_strings)
            .collect(),
        // `expand_trans_spec` already splits by grapheme (`grapheme_units`),
        // which treats `\r\n` as one unit the same way the regex engine's
        // `can_start_a_longer_grapheme` does — so a literal `"\r\n"`
        // replacement value survives here whole, rather than being split into
        // `['\r', '\n']` and truncated to just `"\r"` when zipped against a
        // single-character key (LWP::Simple's `q:to/END/.trans: ["\n" =>
        // "\r\n"]` built its whole HTTP response with exactly that idiom).
        ValueView::Str(s) => expand_trans_spec(&s),
        // Handle Range types by iterating their elements
        ValueView::Range(..)
        | ValueView::RangeExcl(..)
        | ValueView::RangeExclStart(..)
        | ValueView::RangeExclBoth(..)
        | ValueView::GenericRange { .. } => {
            let items = crate::runtime::utils::value_to_list(v);
            items.iter().map(|i| i.to_string_value()).collect()
        }
        _ => {
            let s = v.to_string_value();
            expand_trans_spec(&s)
        }
    }
}

/// Expand one item from a collection-valued trans operand. Strings inside a
/// collection are tokens (so `['el'] => ['ip']` remains a two-character token
/// mapping); nested collections and ranges are expanded structurally.
fn trans_collection_item_to_strings(item: &Value) -> Vec<String> {
    let item = item.clone().deitemize_element();
    match item.view() {
        ValueView::Array(..)
        | ValueView::Seq(..)
        | ValueView::HyperSeq(..)
        | ValueView::RaceSeq(..)
        | ValueView::Slip(..) => value_to_string_list(&item),
        ValueView::Range(..)
        | ValueView::RangeExcl(..)
        | ValueView::RangeExclStart(..)
        | ValueView::RangeExclBoth(..)
        | ValueView::GenericRange { .. } => crate::runtime::utils::value_to_list(&item)
            .iter()
            .map(|value| value.to_string_value())
            .collect(),
        _ => vec![item.to_string_value()],
    }
}

/// Check if a value is a callable closure (Sub/Block).
fn is_closure(v: &Value) -> bool {
    matches!(
        v.view(),
        ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
    )
}

/// Check if a Pair key is a stringified regex pattern (e.g. `/ \s+ /`).
/// Returns the inner pattern if so.
fn extract_regex_pattern(key: &str) -> Option<&str> {
    if key.starts_with('/') && key.ends_with('/') && key.len() >= 3 {
        Some(&key[1..key.len() - 1])
    } else {
        None
    }
}

fn regex_value_pattern(v: &Value) -> Option<String> {
    match v.view() {
        ValueView::Regex(pattern) => Some(pattern.to_string()),
        ValueView::RegexWithAdverbs(adverbs) => Some(adverbs.pattern.to_string()),
        _ => None,
    }
}

impl Interpreter {
    // Cost: O(k + t) to build the rules (k = expanded from-chars, t = token chars),
    // then `apply_trans_rules` / `apply_trans_complement` (see there).
    pub(crate) fn dispatch_trans(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let text = target.to_string_value();

        let mut rules: Vec<TransRule> = Vec::new();
        let mut squash = false;
        let mut delete = false;
        let mut complement = false;

        // Flatten top-level array args that contain pairs.
        // e.g. .trans: (/\@/ => "-",), :c  passes (pair,) as an Array arg.
        let mut flat_args: Vec<Value> = Vec::new();
        for arg in args {
            // `.pairs`/`.list` now yield a Seq, so accept Array/Seq/Slip alike.
            if let Some(items) = arg.as_list_items() {
                let all_pairs = items
                    .iter()
                    .all(|v| matches!(v.view(), ValueView::Pair(..) | ValueView::ValuePair(..)));
                if all_pairs && !items.is_empty() {
                    for item in items.iter() {
                        flat_args.push(item.clone());
                    }
                    continue;
                }
            }
            flat_args.push(arg.clone());
        }

        for arg in &flat_args {
            if let ValueView::Pair(key, value) = arg.view() {
                // Same decontainerization as the `ValuePair` arm below: the
                // value may be an element's own container (ADR-0036 slice 3).
                let value = &value.deref_container();
                match key.as_str() {
                    "s" | "squash" => {
                        squash = value.truthy();
                        continue;
                    }
                    "d" | "delete" => {
                        delete = value.truthy();
                        continue;
                    }
                    "c" | "complement" => {
                        complement = value.truthy();
                        continue;
                    }
                    _ => {}
                }
                // Check if the value is a closure
                if is_closure(value) {
                    if let Some(pattern) = extract_regex_pattern(key) {
                        rules.push(TransRule::RegexClosure {
                            pattern: pattern.to_string(),
                            closure: value.clone(),
                        });
                    } else {
                        let from_chars: Vec<char> = expand_trans_spec(key)
                            .iter()
                            .filter_map(|s| s.chars().next())
                            .collect();
                        rules.push(TransRule::CharClosure {
                            from_chars,
                            closure: value.clone(),
                        });
                    }
                } else {
                    rules.push(self.parse_trans_pair(key, value));
                }
            } else if let ValueView::ValuePair(key, value) = arg.view() {
                // A Seq/Slip key or value (e.g. from `"abc".comb`) is list-like;
                // materialize it to an Array so the list-form dispatch below
                // recognizes it instead of stringifying the whole sequence.
                let key_norm = normalize_trans_operand(key);
                let value_norm = normalize_trans_operand(value);
                let key = &key_norm;
                let value = &value_norm;
                // For ValuePair, the key preserves its original type
                if let Some(pattern) = regex_value_pattern(key) {
                    if is_closure(value) {
                        rules.push(TransRule::RegexClosure {
                            pattern,
                            closure: value.clone(),
                        });
                    } else {
                        rules.push(TransRule::Regex {
                            pattern,
                            replacement: value.to_string_value(),
                        });
                    }
                } else if matches!(
                    key.view(),
                    ValueView::Array(..)
                        | ValueView::Range(..)
                        | ValueView::RangeExcl(..)
                        | ValueView::RangeExclStart(..)
                        | ValueView::RangeExclBoth(..)
                        | ValueView::GenericRange { .. }
                ) {
                    // A from-side element that is neither a Str nor a Regex
                    // (e.g. an `Any` object via `[Any.new]`) is an illegal
                    // substitution key.
                    if let ValueView::Array(items, ..) = key.view()
                        && let Some(bad) = items.iter().find(|v| {
                            matches!(v.view(), ValueView::Instance { .. } | ValueView::Package(_))
                        })
                    {
                        return Err(self.str_trans_illegal_key_error(bad));
                    }
                    // Check if the to-side array contains any closures.
                    let has_closures = if let ValueView::Array(items, ..) = value.view() {
                        items.iter().any(is_closure)
                    } else {
                        false
                    };
                    // Check if the from-side array contains any Regex values.
                    let has_regex = if let ValueView::Array(items, ..) = key.view() {
                        items.iter().any(|v| regex_value_pattern(v).is_some())
                    } else {
                        false
                    };
                    if has_closures {
                        // Build from_tokens and to_values with closure support
                        let from_list = value_to_string_list(key);
                        let to_values: Vec<TokenReplacement> =
                            if let ValueView::Array(items, ..) = value.view() {
                                items
                                    .iter()
                                    .map(|v| {
                                        if is_closure(v) {
                                            TokenReplacement::Closure(v.clone())
                                        } else {
                                            TokenReplacement::Static(v.to_string_value())
                                        }
                                    })
                                    .collect()
                            } else {
                                vec![TokenReplacement::Static(value.to_string_value())]
                            };
                        rules.push(TransRule::TokenClosureMap {
                            from_tokens: from_list,
                            to_values,
                        });
                    } else if has_regex {
                        let to_list = value_to_string_list(value);
                        if let ValueView::Array(items, ..) = key.view() {
                            for (idx, item) in items.iter().enumerate() {
                                let replacement = to_list.get(idx).cloned().unwrap_or_default();
                                if let Some(pattern) = regex_value_pattern(item) {
                                    rules.push(TransRule::Regex {
                                        pattern,
                                        replacement,
                                    });
                                } else {
                                    let s = item.to_string_value();
                                    // Treat multi-char strings as tokens, not individual chars
                                    if s.chars().count() > 1 {
                                        rules.push(TransRule::TokenMap {
                                            from_tokens: vec![s],
                                            to_tokens: vec![replacement],
                                        });
                                    } else {
                                        let from_chars: Vec<char> = expand_trans_spec(&s)
                                            .iter()
                                            .filter_map(|u| u.chars().next())
                                            .collect();
                                        let to_chars: Vec<char> = expand_trans_spec(&replacement)
                                            .iter()
                                            .filter_map(|u| u.chars().next())
                                            .collect();
                                        rules.push(TransRule::CharMap {
                                            from_chars,
                                            to_chars,
                                            cycle: false,
                                        });
                                    }
                                }
                            }
                        }
                    } else {
                        let from_list = value_to_string_list(key);
                        let to_list = value_to_string_list(value);
                        // Decide CharMap vs TokenMap based on element lengths
                        let has_multichar = from_list.iter().any(|s| s.chars().count() > 1)
                            || to_list.iter().any(|s| s.chars().count() > 1);
                        if has_multichar {
                            rules.push(TransRule::TokenMap {
                                from_tokens: from_list,
                                to_tokens: to_list,
                            });
                        } else {
                            let from_chars: Vec<char> =
                                from_list.iter().filter_map(|s| s.chars().next()).collect();
                            let to_chars: Vec<char> =
                                to_list.iter().filter_map(|s| s.chars().next()).collect();
                            rules.push(TransRule::CharMap {
                                from_chars,
                                to_chars,
                                cycle: false,
                            });
                        }
                    }
                } else if is_closure(value) {
                    // String key + closure value
                    let key_str = key.to_string_value();
                    if let Some(pattern) = extract_regex_pattern(&key_str) {
                        rules.push(TransRule::RegexClosure {
                            pattern: pattern.to_string(),
                            closure: value.clone(),
                        });
                    } else {
                        let from_chars: Vec<char> = expand_trans_spec(&key_str)
                            .iter()
                            .filter_map(|s| s.chars().next())
                            .collect();
                        rules.push(TransRule::CharClosure {
                            from_chars,
                            closure: value.clone(),
                        });
                    }
                } else {
                    let key_str = key.to_string_value();
                    rules.push(self.parse_trans_pair(&key_str, value));
                }
            } else {
                // Only Pair objects are valid positional arguments to .trans
                // (e.g. `"a".trans(rx/a/)` passes a bare Regex).
                return Err(self.str_trans_invalid_arg_error(arg));
            }
        }

        if rules.is_empty() {
            return Ok(Value::str(text));
        }

        let result = self.apply_trans_rules(&text, &rules, squash, delete, complement)?;
        Ok(Value::str(result))
    }
}
