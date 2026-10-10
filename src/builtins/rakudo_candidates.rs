//! Rakudo's core routine candidates (#12472): the signature and declaration
//! site of every candidate of a core `sub` or built-in-type method.
//!
//! mutsu implements its built-in routines in Rust, so there is no Raku
//! declaration to read a signature from. `&say.candidates`, `.cando`,
//! `.signature`, `.file` and `.line` of a built-in therefore answer from
//! `rakudo_candidates.txt`, the committed output of
//! `scripts/gen-rakudo-candidates.raku` (the same snapshot approach as
//! `rakudo_method_tables.txt`). It is metadata about what Rakudo declares, not
//! behaviour: no routine is executed from it.

use std::collections::HashMap;
use std::sync::OnceLock;

const SNAPSHOT: &str = include_str!("rakudo_candidates.txt");

/// One candidate of a core routine.
#[derive(Debug, Clone, Copy)]
pub(crate) struct CoreCandidate {
    /// Declared `multi` (its `Code.multi` is true).
    pub(crate) multi: bool,
    /// `SETTING::src/core.c/...rakumod`.
    pub(crate) file: &'static str,
    pub(crate) line: u32,
    /// `Signature.raku` as Rakudo prints it, e.g. `:(Str:D $:: *%_)`.
    pub(crate) signature: &'static str,
}

/// `owner -> routine name -> candidates`; owner `""` is the core subs.
type Table = HashMap<&'static str, HashMap<&'static str, Vec<CoreCandidate>>>;

// Cost: O(1) after the first call; that call is O(r), r = rows in the snapshot.
fn table() -> &'static Table {
    static TABLE: OnceLock<Table> = OnceLock::new();
    TABLE.get_or_init(|| {
        let mut table = Table::new();
        for line in SNAPSHOT.lines().filter(|l| !l.starts_with('#')) {
            let mut cols = line.split('\t');
            let owner = match cols.next() {
                Some("sub") => "",
                Some("method") => cols.next().unwrap_or(""),
                _ => continue,
            };
            let (Some(name), Some(multi), Some(file), Some(line_no), Some(signature)) = (
                cols.next(),
                cols.next(),
                cols.next(),
                cols.next(),
                cols.next(),
            ) else {
                continue;
            };
            table
                .entry(owner)
                .or_default()
                .entry(name)
                .or_default()
                .push(CoreCandidate {
                    multi: multi == "1",
                    file,
                    line: line_no.parse().unwrap_or(0),
                    signature,
                });
        }
        table
    })
}

/// The candidates of the core sub `name`, in Rakudo's declaration order.
// Cost: O(1) lookup (plus the one-time table build), c = candidates returned.
pub(crate) fn core_sub_candidates(name: &str) -> &'static [CoreCandidate] {
    lookup("", name)
}

/// The candidates of the method `name` declared on built-in type `owner`.
// Cost: O(1) lookup (plus the one-time table build).
pub(crate) fn core_method_candidates(owner: &str, name: &str) -> &'static [CoreCandidate] {
    if owner.is_empty() {
        return &[];
    }
    lookup(owner, name)
}

fn lookup(owner: &str, name: &str) -> &'static [CoreCandidate] {
    table()
        .get(owner)
        .and_then(|names| names.get(name))
        .map_or(&[], Vec::as_slice)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn say_has_three_candidates_with_setting_locations() {
        let say = core_sub_candidates("say");
        assert_eq!(say.len(), 3);
        assert!(say.iter().all(|c| c.multi));
        assert!(say[0].file.starts_with("SETTING::"));
        assert!(say[0].line > 0);
    }

    #[test]
    fn str_numeric_is_a_method_row() {
        assert!(!core_method_candidates("Str", "Numeric").is_empty());
        assert!(core_method_candidates("", "Numeric").is_empty());
    }
}
