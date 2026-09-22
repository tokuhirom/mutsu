//! Unicode General_Category lookup.
//!
//! This used to be 28 `regex::Regex::is_match` probes against a
//! one-character string, tried in order until one hit, followed by a
//! `String` allocation for an answer that is a fixed `&'static str`
//! ([#8999](https://github.com/tokuhirom/mutsu/issues/8999)). It cost ~970
//! instructions per character and is reached once per character by
//! `nqp::findcclass`/`findnotcclass`, by `.uniprop`, by collation and by the
//! segmentation properties -- so a JSON decode paid it 23,520 times.
//!
//! It is now a table lookup against [`super::unicode_gc_data`], which is generated
//! from `regex-syntax`'s own Unicode tables (see that module's header) and so
//! answers exactly what the regexes did. Three tiers, cheapest first:
//!
//! | codepoint | structure | cost |
//! | --- | --- | --- |
//! | `< 0x80` | direct index into a 128-byte table | one load |
//! | `< 0x10000` | two-stage trie, 64-codepoint blocks | two loads |
//! | above | binary search over 1,200 runs | ~11 branches |
//!
//! Total static data is ~21 KB in `.rodata`: no heap, no lock, no lazy
//! initialisation, and nothing to memoize per process.

use super::unicode_gc_data as data;

/// A Unicode General_Category value.
///
/// `Cs` (surrogate) is deliberately absent. A Rust `char` can never be a
/// surrogate, and the ordered-regex probe this replaced had no `Cs` arm
/// either -- the `regex` crate does not support `\p{Cs}` -- so a surrogate
/// codepoint fell through to `Cn`. The tables keep that answer.
#[derive(Clone, Copy, PartialEq, Eq, Debug, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub(crate) enum GeneralCategory {
    Lu = 0,
    Ll,
    Lt,
    Lm,
    Lo,
    Mn,
    Mc,
    Me,
    Nd,
    Nl,
    No,
    Pc,
    Pd,
    Ps,
    Pe,
    Pi,
    Pf,
    Po,
    Sm,
    Sc,
    Sk,
    So,
    Zs,
    Zl,
    Zp,
    Cc,
    Cf,
    Co,
    Cn,
}

use GeneralCategory as Gc;

impl GeneralCategory {
    /// Every category, ordered by discriminant. The generated tables store an
    /// index into this array, so its order is load-bearing -- `unicode_gc_gen`
    /// pins it against the list it generates from.
    pub(crate) const ALL: [Self; 29] = [
        Gc::Lu,
        Gc::Ll,
        Gc::Lt,
        Gc::Lm,
        Gc::Lo,
        Gc::Mn,
        Gc::Mc,
        Gc::Me,
        Gc::Nd,
        Gc::Nl,
        Gc::No,
        Gc::Pc,
        Gc::Pd,
        Gc::Ps,
        Gc::Pe,
        Gc::Pi,
        Gc::Pf,
        Gc::Po,
        Gc::Sm,
        Gc::Sc,
        Gc::Sk,
        Gc::So,
        Gc::Zs,
        Gc::Zl,
        Gc::Zp,
        Gc::Cc,
        Gc::Cf,
        Gc::Co,
        Gc::Cn,
    ];

    const NAMES: [&'static str; 29] = [
        "Lu", "Ll", "Lt", "Lm", "Lo", "Mn", "Mc", "Me", "Nd", "Nl", "No", "Pc", "Pd", "Ps", "Pe",
        "Pi", "Pf", "Po", "Sm", "Sc", "Sk", "So", "Zs", "Zl", "Zp", "Cc", "Cf", "Co", "Cn",
    ];

    /// The two-letter abbreviation, as `.uniprop` reports it.
    pub(crate) const fn as_str(self) -> &'static str {
        Self::NAMES[self as usize]
    }

    /// This category as a one-bit set, for testing membership of a group with
    /// a single `&` instead of a chain of string comparisons.
    pub(crate) const fn mask(self) -> u32 {
        1u32 << (self as u32)
    }

    /// Is this category in `mask`, one of the group constants below?
    pub(crate) const fn in_mask(self, mask: u32) -> bool {
        mask & self.mask() != 0
    }

    /// The major category letter (`b'L'`, `b'P'`, ...), for the parent-category
    /// match that `<:L>` and `.uniprop-matches('L')` perform.
    pub(crate) const fn major(self) -> u8 {
        Self::NAMES[self as usize].as_bytes()[0]
    }

    /// Parse an abbreviation back into a category. `Cs` is not a value this
    /// module can ever produce, but it is a name callers may ask about, so it
    /// is not accepted here either -- see the type's doc comment.
    pub(crate) fn from_name(name: &str) -> Option<Self> {
        Self::NAMES
            .iter()
            .position(|n| *n == name)
            .map(|i| Self::ALL[i])
    }

    /// A code as stored in the generated tables. Out-of-range codes cannot
    /// occur (the generator only emits `0..=28`); `Cn` keeps the function
    /// total rather than panicking on a corrupt table.
    fn from_code(code: u8) -> Self {
        Self::ALL.get(code as usize).copied().unwrap_or(Self::Cn)
    }

    /// `L*` -- what Raku's `<:L>` and MoarVM's `CCLASS_ALPHABETIC` mean.
    pub(crate) const LETTER: u32 =
        Gc::Lu.mask() | Gc::Ll.mask() | Gc::Lt.mask() | Gc::Lm.mask() | Gc::Lo.mask();
    /// `LC` -- the cased letters, an alias Raku accepts for `Lu|Ll|Lt`.
    pub(crate) const CASED_LETTER: u32 = Gc::Lu.mask() | Gc::Ll.mask() | Gc::Lt.mask();
    /// `P*`
    pub(crate) const PUNCTUATION: u32 = Gc::Pc.mask()
        | Gc::Pd.mask()
        | Gc::Ps.mask()
        | Gc::Pe.mask()
        | Gc::Pi.mask()
        | Gc::Pf.mask()
        | Gc::Po.mask();
    /// `Z*`
    pub(crate) const SEPARATOR: u32 = Gc::Zs.mask() | Gc::Zl.mask() | Gc::Zp.mask();
}

/// The General_Category of `ch`.
pub(crate) fn general_category(ch: char) -> GeneralCategory {
    let cp = ch as u32;
    let code = if cp < 0x80 {
        data::ASCII_CATS[cp as usize]
    } else if cp < 0x10000 {
        let leaf = data::BMP_INDEX[(cp >> data::BMP_SHIFT) as usize] as usize;
        let offset = (cp as usize) & ((1 << data::BMP_SHIFT) - 1);
        data::BMP_LEAVES[(leaf << data::BMP_SHIFT) | offset]
    } else {
        // `ASTRAL_STARTS[0]` is exactly `0x10000`, which is `<= cp` here, so
        // the partition point is at least 1 and the subtraction cannot wrap.
        let i = data::ASTRAL_STARTS.partition_point(|&start| start <= cp) - 1;
        data::ASTRAL_CATS[i]
    };
    GeneralCategory::from_code(code)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn spot_checks_across_the_tiers() {
        // ASCII tier
        assert_eq!(general_category('A'), Gc::Lu);
        assert_eq!(general_category('z'), Gc::Ll);
        assert_eq!(general_category('7'), Gc::Nd);
        assert_eq!(general_category(' '), Gc::Zs);
        assert_eq!(general_category('_'), Gc::Pc);
        assert_eq!(general_category('+'), Gc::Sm);
        assert_eq!(general_category('$'), Gc::Sc);
        assert_eq!(general_category('\n'), Gc::Cc);
        // BMP tier
        assert_eq!(general_category('\u{3042}'), Gc::Lo); // HIRAGANA LETTER A
        assert_eq!(general_category('\u{4E00}'), Gc::Lo); // CJK IDEOGRAPH ONE
        assert_eq!(general_category('\u{0301}'), Gc::Mn); // COMBINING ACUTE
        assert_eq!(general_category('\u{01C5}'), Gc::Lt); // DZ WITH CARON
        assert_eq!(general_category('\u{2028}'), Gc::Zl);
        assert_eq!(general_category('\u{2029}'), Gc::Zp);
        assert_eq!(general_category('\u{00AD}'), Gc::Cf); // SOFT HYPHEN
        assert_eq!(general_category('\u{E000}'), Gc::Co); // private use
        assert_eq!(general_category('\u{0378}'), Gc::Cn); // unassigned
        // astral tier
        assert_eq!(general_category('\u{1D400}'), Gc::Lu); // MATHEMATICAL BOLD A
        assert_eq!(general_category('\u{1F600}'), Gc::So); // GRINNING FACE
        assert_eq!(general_category('\u{20000}'), Gc::Lo); // CJK ext B
        assert_eq!(general_category('\u{10FFFF}'), Gc::Cn);
    }

    #[test]
    fn masks_and_names_line_up() {
        for (i, cat) in GeneralCategory::ALL.iter().enumerate() {
            assert_eq!(*cat as usize, i, "discriminant matches ALL position");
            assert_eq!(GeneralCategory::from_code(i as u8), *cat);
            assert_eq!(GeneralCategory::from_name(cat.as_str()), Some(*cat));
            assert_eq!(cat.as_str().len(), 2);
            assert_eq!(cat.major(), cat.as_str().as_bytes()[0]);
        }
        assert!(Gc::Lt.in_mask(GeneralCategory::LETTER));
        assert!(Gc::Lt.in_mask(GeneralCategory::CASED_LETTER));
        assert!(!Gc::Lm.in_mask(GeneralCategory::CASED_LETTER));
        assert!(Gc::Po.in_mask(GeneralCategory::PUNCTUATION));
        assert!(!Gc::Sm.in_mask(GeneralCategory::PUNCTUATION));
        // The group masks partition the categories, and agree with `major()`.
        let groups = [
            (b'L', GeneralCategory::LETTER),
            (b'M', Gc::Mn.mask() | Gc::Mc.mask() | Gc::Me.mask()),
            (b'N', Gc::Nd.mask() | Gc::Nl.mask() | Gc::No.mask()),
            (b'P', GeneralCategory::PUNCTUATION),
            (
                b'S',
                Gc::Sm.mask() | Gc::Sc.mask() | Gc::Sk.mask() | Gc::So.mask(),
            ),
            (b'Z', GeneralCategory::SEPARATOR),
            (
                b'C',
                Gc::Cc.mask() | Gc::Cf.mask() | Gc::Co.mask() | Gc::Cn.mask(),
            ),
        ];
        for cat in GeneralCategory::ALL {
            let hits: Vec<u8> = groups
                .iter()
                .filter(|(_, g)| cat.in_mask(*g))
                .map(|(letter, _)| *letter)
                .collect();
            assert_eq!(
                hits,
                vec![cat.major()],
                "{} is in exactly its own major group",
                cat.as_str()
            );
        }
    }
}
