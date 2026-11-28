//! A set of `&'static str` names answered by a hashed probe, its table built
//! at compile time.
//!
//! A fixed name list written as `matches!(s, "Int" | "Str" | ...)` or
//! `LIST.contains(&s)` compiles to one comparison per candidate of the same
//! length, and the answers that matter most -- a user type name, which is in
//! none of them -- pay every one. `is_known_type_constraint` alone ran ~25
//! `memcmp`s per typed declaration of a user type (#12917). A
//! [`StaticStrSet`] hashes the name once and compares it against, at a load
//! factor of at most 1/2, about one candidate.
//!
//! The table is a `const` value, so a `static` of it lives in `.rodata` and
//! costs no start-up work.

/// FNV-1a over `bytes[start..end]`.
// Cost: O(end - start).
pub(crate) const fn fnv1a(bytes: &[u8], start: usize, end: usize) -> u64 {
    let mut hash: u64 = 0xcbf2_9ce4_8422_2325;
    let mut i = start;
    while i < end {
        hash ^= bytes[i] as u64;
        hash = hash.wrapping_mul(0x0000_0100_0000_01b3);
        i += 1;
    }
    hash
}

/// The slot count a [`StaticStrSet`] of `sections` needs: at least twice its
/// names, a power of two.
// Cost: O(s), s = sections; compile time only.
pub(crate) const fn slots_for(sections: &[&[&str]]) -> usize {
    let mut total = 0;
    let mut i = 0;
    while i < sections.len() {
        total += sections[i].len();
        i += 1;
    }
    (2 * total).next_power_of_two()
}

/// The union of several `&'static str` lists, as an open-addressing hash
/// table of `S` slots ([`slots_for`]).
pub(crate) struct StaticStrSet<const S: usize> {
    sections: &'static [&'static [&'static str]],
    /// `position + 1` of the name in the concatenated sections, `0` = empty.
    slots: [u16; S],
}

impl<const S: usize> StaticStrSet<S> {
    /// The set of every name in `sections`; a name listed twice is stored once.
    // Cost: O(n), n = bytes of all names; compile time only.
    pub(crate) const fn new(sections: &'static [&'static [&'static str]]) -> Self {
        assert!(S.is_power_of_two(), "slot count must be a power of two");
        let mut slots = [0u16; S];
        let mut position = 0;
        let mut section = 0;
        while section < sections.len() {
            let names = sections[section];
            let mut i = 0;
            while i < names.len() {
                let bytes = names[i].as_bytes();
                let mut slot = (fnv1a(bytes, 0, bytes.len()) as usize) & (S - 1);
                let mut duplicate = false;
                while slots[slot] != 0 {
                    let held = name_at(sections, (slots[slot] - 1) as usize).as_bytes();
                    if bytes_eq(held, bytes) {
                        duplicate = true;
                        break;
                    }
                    slot = (slot + 1) & (S - 1);
                }
                if !duplicate {
                    assert!(position + 1 < u16::MAX as usize, "too many names");
                    slots[slot] = (position + 1) as u16;
                }
                position += 1;
                i += 1;
            }
            section += 1;
        }
        Self { sections, slots }
    }

    /// Whether `name` is in the set.
    // Cost: O(|name|).
    #[inline]
    pub(crate) fn contains(&self, name: &str) -> bool {
        let bytes = name.as_bytes();
        let mut slot = (fnv1a(bytes, 0, bytes.len()) as usize) & (S - 1);
        loop {
            let entry = self.slots[slot];
            if entry == 0 {
                return false;
            }
            if name_at(self.sections, (entry - 1) as usize) == name {
                return true;
            }
            slot = (slot + 1) & (S - 1);
        }
    }
}

/// The name at `position` of the concatenated `sections`.
// Cost: O(s), s = sections.
const fn name_at(sections: &[&'static [&'static str]], mut position: usize) -> &'static str {
    let mut section = 0;
    loop {
        let names = sections[section];
        if position < names.len() {
            return names[position];
        }
        position -= names.len();
        section += 1;
    }
}

// Cost: O(len).
const fn bytes_eq(a: &[u8], b: &[u8]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    let mut i = 0;
    while i < a.len() {
        if a[i] != b[i] {
            return false;
        }
        i += 1;
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    const A: &[&str] = &["Int", "Str", "Num"];
    const B: &[&str] = &["int", "Str", "uint8"];
    const SECTIONS: &[&[&str]] = &[A, B];
    static SET: StaticStrSet<{ slots_for(SECTIONS) }> = StaticStrSet::new(SECTIONS);

    #[test]
    fn answers_membership_across_sections() {
        for name in A.iter().chain(B) {
            assert!(SET.contains(name), "{name}");
        }
        for name in ["", "In", "Intx", "str", "Uint8", "Tag"] {
            assert!(!SET.contains(name), "{name}");
        }
    }
}
