//! How many alternatives Rakudo compiles one `[...]` class enumeration into.
//!
//! The count decides whether a *negated* enumeration stays a declarative LTM
//! atom. Rakudo's `cclass_elem` action (NQP `P6Regex/Actions.nqp`) folds every
//! plain entry of `[...]` into ONE `enumcharlist` and gives each of the
//! following its own alternative: a range (`a..z`), a class escape (`\d`, `\w`,
//! `\s` and their negations), a negated escape (`\N`, `\H`, `\V`, `\X[..]`,
//! `\C[..]`), and `\n`, which in Raku also matches `\r\n` and so is not a plain
//! enumeration entry either. One alternative compiles to that alternative
//! (a single NFA edge, negated or not). Two or more in a NEGATED class compile
//! to `[<?conj-of-alternatives> .]`, and Rakudo's NFA has no method for `conj`,
//! so the class is a fate: it terminates the declarative prefix (issue #9053).
//! A positive class with several alternatives is a plain `alt`, which the NFA
//! does build, so only the negated form is affected.
//!
//! Measured against `raku`: `<-[Z \n]>`, `<-[a..c x]>`, `<-[\d x]>`,
//! `<-[\n \r]>` terminate; `<-[\n]>`, `<-[a..c]>`, `<-[\t \r x]>`,
//! `<-[\x0a x]>` (a hex escape is a plain entry, unlike `\n`) stay declarative.

use super::ClassItem;

/// Tally of the alternatives a `[...]` enumeration compiles to in Rakudo.
#[derive(Default)]
pub(super) struct CharClassAltTally {
    /// Entries that each become an alternative of their own.
    separate: usize,
    /// Whether any entry lands in the shared `enumcharlist` alternative.
    merged: bool,
}

impl CharClassAltTally {
    /// A plain entry, folded into the shared `enumcharlist`.
    pub(super) fn plain(&mut self) {
        self.merged = true;
    }

    /// An entry Rakudo compiles as its own alternative.
    pub(super) fn separate(&mut self) {
        self.separate += 1;
    }

    /// A class escape item: `\h` and `\v` are plain enumerations, every other
    /// escape item (`\d`, `\W`, `\N`, ...) is a separate alternative.
    pub(super) fn escape_item(&mut self, item: &ClassItem) {
        match item {
            ClassItem::HorizSpace | ClassItem::VertSpace => self.plain(),
            _ => self.separate(),
        }
    }

    /// True when a negated class with these entries terminates the
    /// declarative LTM prefix (two or more alternatives).
    pub(super) fn negation_terminates_ltm(&self) -> bool {
        self.separate + usize::from(self.merged) >= 2
    }
}
