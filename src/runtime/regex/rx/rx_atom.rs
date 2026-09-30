//! One-grapheme atom tests for the compiled engine, with an ASCII fast path
//! that is *derived from*, never a restatement of, `match_consuming_atom`
//! (ADR-0135 D4).
//!
//! On first use a program asks `match_consuming_atom` itself, once per
//! printable ASCII character, whether each of its atoms accepts that
//! character as a whole grapheme, and keeps the answers as a 128-bit set per
//! atom. At a position where the grapheme is provably that one character, the
//! set answers; anywhere else the full function runs.
//!
//! "Provably one character": `c` is printable ASCII (not a control, so never
//! half of a CR LF pair), the character before it is ASCII or absent (so no
//! Prepend or other cluster reaches into it), and the character after it is
//! ASCII or absent (so no Extend, SpacingMark or ZWJ extends it). Under those
//! three conditions `grapheme_end(pos) == pos + 1` and `pos` is a grapheme
//! boundary, which are exactly the two facts `match_consuming_atom` checks
//! before testing the character; the probe below reproduces them by testing
//! `c` at the start of `[c, ' ']`.

use super::RxProgram;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;

/// Printable ASCII, the range the fast path covers.
const PRINTABLE: std::ops::Range<u32> = 0x20..0x7f;

impl RxProgram {
    /// The per-atom ASCII acceptance sets, probed on first use.
    // Cost: O(1) after the first call; O(a * 95) on it, a = atoms.
    fn ascii_sets(&self, interp: &mut Interpreter, pkg: Symbol) -> &[u128] {
        self.ascii.get_or_init(|| {
            self.atoms
                .iter()
                .map(|atom| {
                    let mut set = 0u128;
                    // The table is shared with the zero-width assertions,
                    // which never reach `rx_atom_at`.
                    if !super::rx_compile::is_consuming(atom) {
                        return set;
                    }
                    for c in PRINTABLE.filter_map(char::from_u32) {
                        if interp.match_consuming_atom(atom, &[c, ' '], 0, pkg, false) == Some(1) {
                            set |= 1u128 << (c as u32);
                        }
                    }
                    set
                })
                .collect()
        })
    }
}

/// Is the grapheme at `pos` provably the single character `chars[pos]`, and
/// is that character printable ASCII? (See the module doc.)
#[inline]
fn single_ascii(chars: &[char], pos: usize) -> Option<u32> {
    let c = *chars.get(pos)? as u32;
    if !PRINTABLE.contains(&c) {
        return None;
    }
    if pos > 0 && !chars[pos - 1].is_ascii() {
        return None;
    }
    if chars.get(pos + 1).is_some_and(|n| !n.is_ascii()) {
        return None;
    }
    Some(c)
}

impl Interpreter {
    /// Where `program.atoms[i]` ends when matched at `pos`, if it matches.
    // Cost: O(1) on the ASCII fast path; O(g) otherwise, g = the grapheme's
    // length (plus the class's item count), as `match_consuming_atom`.
    #[inline]
    pub(super) fn rx_atom_at(
        &mut self,
        program: &RxProgram,
        i: usize,
        chars: &[char],
        pos: usize,
        pkg: Symbol,
    ) -> Option<usize> {
        if let Some(c) = single_ascii(chars, pos) {
            let sets = program.ascii_sets(self, pkg);
            return (sets[i] & (1u128 << c) != 0).then_some(pos + 1);
        }
        self.match_consuming_atom(&program.atoms[i], chars, pos, pkg, false)
    }

    /// Match `program.atoms[i]` repeatedly from `pos`, up to `max` times,
    /// pushing the end of every iteration onto `ends` (so `ends[k]` is where
    /// iteration `k + 1` stopped). Returns how many iterations matched.
    // Cost: O(k·g), k = the iterations matched, g as in `rx_atom_at`.
    pub(super) fn rx_atom_run(
        &mut self,
        program: &RxProgram,
        i: usize,
        chars: &[char],
        mut pos: usize,
        max: u32,
        pkg: Symbol,
        ends: &mut Vec<usize>,
    ) -> u32 {
        let mut n = 0u32;
        while n < max {
            match self.rx_atom_at(program, i, chars, pos, pkg) {
                Some(next) => {
                    pos = next;
                    ends.push(pos);
                    n += 1;
                }
                None => break,
            }
        }
        n
    }
}
