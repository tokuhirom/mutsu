use super::super::*;

/// Unicode case-fold a single character. Returns the folded chars.
/// Uses the "uppercase then lowercase" approximation of full Unicode case folding.
/// Examples: 'ß' -> ['s','s'], 'ﬁ' -> ['f','i'], 'A' -> ['a']
fn casefold_char(c: char) -> Vec<char> {
    c.to_uppercase()
        .collect::<String>()
        .to_lowercase()
        .chars()
        .collect()
}

/// Returns true if a character case-folds to more than one character.
fn has_multichar_fold(c: char) -> bool {
    casefold_char(c).len() > 1
}

/// Case-fold text characters, expanding multi-char folds (e.g., 'ﬁ' -> 'f','i').
/// Returns folded chars and a position map from folded index to original char index.
/// The sentinel for one-past-end is also appended to the position map.
pub(super) fn casefold_text(orig_chars: &[char]) -> (Vec<char>, Vec<usize>) {
    let mut folded_chars: Vec<char> = Vec::new();
    let mut pos_map: Vec<usize> = Vec::new();

    for (idx, &c) in orig_chars.iter().enumerate() {
        let folded = casefold_char(c);
        for fc in folded {
            folded_chars.push(fc);
            pos_map.push(idx);
        }
    }
    // sentinel for one-past-end
    pos_map.push(orig_chars.len());
    (folded_chars, pos_map)
}

/// Check whether text or pattern contains any characters with multi-char case folds.
pub(super) fn needs_casefold_expansion(chars: &[char], pattern: &RegexPattern) -> bool {
    // Check text
    for &c in chars {
        if has_multichar_fold(c) {
            return true;
        }
    }
    // Check pattern literals
    pattern_has_multichar_fold(pattern)
}

fn pattern_has_multichar_fold(pattern: &RegexPattern) -> bool {
    for token in &pattern.tokens {
        if atom_has_multichar_fold(&token.atom) {
            return true;
        }
    }
    false
}

fn atom_has_multichar_fold(atom: &RegexAtom) -> bool {
    match atom {
        RegexAtom::Literal(ch) => has_multichar_fold(*ch),
        RegexAtom::Group(p) | RegexAtom::CaptureGroup(p) => pattern_has_multichar_fold(p),
        RegexAtom::Alternation(alts)
        | RegexAtom::SequentialAlternation(alts)
        | RegexAtom::Conjunction(alts) => alts.iter().any(pattern_has_multichar_fold),
        RegexAtom::GoalMatch { goal, inner, .. } => {
            pattern_has_multichar_fold(goal) || pattern_has_multichar_fold(inner)
        }
        RegexAtom::Lookaround { pattern, .. } => pattern_has_multichar_fold(pattern),
        _ => false,
    }
}

/// Case-fold all literal atoms in a RegexPattern (recursively).
/// Multi-char folds (e.g., 'ß' -> 'ss') are expanded into Group patterns
/// containing multiple Literal tokens.
pub(super) fn casefold_pattern(pattern: &RegexPattern) -> RegexPattern {
    RegexPattern {
        tokens: pattern.tokens.iter().flat_map(casefold_token).collect(),
        anchor_start: pattern.anchor_start,
        anchor_end: pattern.anchor_end,
        ignore_case: pattern.ignore_case,
        ignore_mark: pattern.ignore_mark,
    }
}

/// Case-fold the sub-pattern of a `%` / `%%` quantifier separator, if present.
fn casefold_separator(sep: &Option<Box<RegexSeparatorSpec>>) -> Option<Box<RegexSeparatorSpec>> {
    sep.as_ref().map(|s| {
        Box::new(RegexSeparatorSpec {
            pattern: casefold_pattern(&s.pattern),
            allow_trailing: s.allow_trailing,
        })
    })
}

fn casefold_token(token: &RegexToken) -> Vec<RegexToken> {
    let folded_atom = casefold_atom(&token.atom);
    match folded_atom {
        CasefoldedAtom::Single(atom) => vec![RegexToken {
            atom,
            quant: token.quant.clone(),
            named_capture: token.named_capture.clone(),
            secondary_named_capture: token.secondary_named_capture.clone(),
            hash_capture: token.hash_capture.clone(),
            ratchet: token.ratchet,
            frugal: token.frugal,
            separator: casefold_separator(&token.separator),
        }],
        CasefoldedAtom::Multiple(chars) => {
            // A literal that expanded to multiple chars (e.g., 'ß' -> 's','s').
            // Wrap them in a Group so they're matched as a sequence.
            let inner_tokens: Vec<RegexToken> = chars
                .into_iter()
                .map(|c| RegexToken {
                    atom: RegexAtom::Literal(c),
                    quant: RegexQuant::One,
                    named_capture: None,
                    secondary_named_capture: None,
                    hash_capture: None,
                    ratchet: token.ratchet,
                    frugal: false,
                    separator: None,
                })
                .collect();
            let group = RegexPattern {
                tokens: inner_tokens,
                anchor_start: false,
                anchor_end: false,
                ignore_case: false,
                ignore_mark: false,
            };
            vec![RegexToken {
                atom: RegexAtom::Group(group),
                quant: token.quant.clone(),
                named_capture: token.named_capture.clone(),
                secondary_named_capture: token.secondary_named_capture.clone(),
                hash_capture: token.hash_capture.clone(),
                ratchet: token.ratchet,
                frugal: token.frugal,
                separator: casefold_separator(&token.separator),
            }]
        }
    }
}

enum CasefoldedAtom {
    Single(RegexAtom),
    Multiple(Vec<char>),
}

fn casefold_atom(atom: &RegexAtom) -> CasefoldedAtom {
    match atom {
        RegexAtom::Literal(ch) => {
            let folded = casefold_char(*ch);
            if folded.len() == 1 {
                CasefoldedAtom::Single(RegexAtom::Literal(folded[0]))
            } else {
                CasefoldedAtom::Multiple(folded)
            }
        }
        RegexAtom::Named(name) => {
            // Case-fold the literal name text
            let folded: String = name.chars().flat_map(casefold_char).collect();
            CasefoldedAtom::Single(RegexAtom::Named(folded))
        }
        RegexAtom::Group(p) => CasefoldedAtom::Single(RegexAtom::Group(casefold_pattern(p))),
        RegexAtom::CaptureGroup(p) => {
            CasefoldedAtom::Single(RegexAtom::CaptureGroup(casefold_pattern(p)))
        }
        RegexAtom::Alternation(alts) => CasefoldedAtom::Single(RegexAtom::Alternation(
            alts.iter().map(casefold_pattern).collect(),
        )),
        RegexAtom::SequentialAlternation(alts) => CasefoldedAtom::Single(
            RegexAtom::SequentialAlternation(alts.iter().map(casefold_pattern).collect()),
        ),
        RegexAtom::Conjunction(branches) => CasefoldedAtom::Single(RegexAtom::Conjunction(
            branches.iter().map(casefold_pattern).collect(),
        )),
        RegexAtom::GoalMatch {
            goal,
            inner,
            goal_text,
        } => CasefoldedAtom::Single(RegexAtom::GoalMatch {
            goal: casefold_pattern(goal),
            inner: casefold_pattern(inner),
            goal_text: goal_text.clone(),
        }),
        RegexAtom::Lookaround {
            pattern,
            negated,
            is_behind,
        } => CasefoldedAtom::Single(RegexAtom::Lookaround {
            pattern: casefold_pattern(pattern),
            negated: *negated,
            is_behind: *is_behind,
        }),
        other => CasefoldedAtom::Single(other.clone()),
    }
}
