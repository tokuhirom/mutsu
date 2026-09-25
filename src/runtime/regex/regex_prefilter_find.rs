//! Literal search over a `[char]` subject, shared by the `Literal` and `Inner`
//! arms of [`super::regex_prefilter_scan::ScanPositions`].
//!
//! Both arms ask the same question — "where is the next occurrence of this
//! literal?" — and used to answer it with a slice compare at every position.
//! A `[char]` slice compare is a `bcmp` call, so a failing search over a long
//! subject paid a function call per character (#9250). Here the needle's first
//! character is located a block at a time with a branch-free comparison LLVM
//! vectorizes, and the rest of the needle is compared only where the first
//! character matched.
//!
//! The answer is exactly the one the per-position compare gave: the smallest
//! `i` in range with `haystack[i..i + needle.len()] == needle`. Nothing here
//! looks at bytes or encodings, so a `from_chars` target built for `:i`/`:m`
//! is searched the same way as any other.

/// Block size of the first-character sweep. Wide enough that the per-block
/// branch is amortized, small enough that a hit near the start of the block
/// does not re-scan much.
const BLOCK: usize = 32;

/// The first index in `haystack` holding `c`.
///
// Cost: O(n), n = haystack.len(); the block test is a branch-free compare the
// compiler vectorizes, so the constant is a fraction of a per-char loop.
fn find_char(haystack: &[char], c: char) -> Option<usize> {
    let mut blocks = haystack.chunks_exact(BLOCK);
    let mut base = 0;
    for block in &mut blocks {
        if block.iter().fold(false, |hit, &x| hit | (x == c)) {
            return block.iter().position(|&x| x == c).map(|i| base + i);
        }
        base += BLOCK;
    }
    blocks
        .remainder()
        .iter()
        .position(|&x| x == c)
        .map(|i| base + i)
}

/// The smallest `i` with `from <= i <= last` where `needle` occurs at `i` in
/// `haystack`. `last` is an inclusive start bound; starts past
/// `haystack.len() - needle.len()` are never returned, whatever `last` says.
///
/// An empty needle occurs everywhere, so it answers `from` when in range.
///
// Cost: O(n * m), n = last - from, m = needle.len(); O(n) with a vectorized
// constant when the needle's first character is rare in the haystack.
pub(super) fn find_literal(
    haystack: &[char],
    needle: &[char],
    from: usize,
    last: usize,
) -> Option<usize> {
    let last = last.min(haystack.len().checked_sub(needle.len())?);
    if from > last {
        return None;
    }
    let Some((&first, rest)) = needle.split_first() else {
        return Some(from);
    };
    let mut pos = from;
    while pos <= last {
        let at = pos + find_char(&haystack[pos..=last], first)?;
        if haystack[at + 1..at + needle.len()] == *rest {
            return Some(at);
        }
        pos = at + 1;
    }
    None
}

#[cfg(test)]
mod tests {
    use super::find_literal;

    /// The per-position compare this module replaced — the definition
    /// `find_literal` must agree with.
    fn reference(h: &[char], n: &[char], from: usize, last: usize) -> Option<usize> {
        let last = last.min(h.len().checked_sub(n.len())?);
        (from..=last).find(|&i| h[i..i + n.len()] == *n)
    }

    #[test]
    fn agrees_with_per_position_compare() {
        let subjects = [
            String::new(),
            "a".repeat(100),
            format!("{}b", "a".repeat(70)),
            format!("{}ab{}", "a".repeat(31), "a".repeat(40)),
            "abababababababababababababababababababab".to_string(),
            "a\u{e9}b\u{1f600}c\u{e9}b".repeat(9),
        ];
        let needles = [
            "",
            "a",
            "b",
            "ab",
            "ba",
            "aab",
            "\u{e9}b",
            "\u{1f600}c",
            "zz",
        ];
        for s in &subjects {
            let h: Vec<char> = s.chars().collect();
            for n in &needles {
                let n: Vec<char> = n.chars().collect();
                for from in 0..=h.len() + 1 {
                    for last in [0, from, from + 1, h.len() / 2, h.len(), usize::MAX] {
                        assert_eq!(
                            find_literal(&h, &n, from, last),
                            reference(&h, &n, from, last),
                            "subject {s:?} needle {n:?} from {from} last {last}"
                        );
                    }
                }
            }
        }
    }
}
