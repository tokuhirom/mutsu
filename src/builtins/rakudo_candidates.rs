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

use rustc_hash::{FxHashMap, FxHashSet};
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

/// `routine name -> candidates`.
type Routines = FxHashMap<&'static str, Vec<CoreCandidate>>;

/// Where the method rows start: `scripts/gen-rakudo-candidates.raku` writes
/// every sub row before the first method row (pinned by the test
/// `sub_rows_precede_method_rows`), so each kind's rows are one contiguous
/// stretch of the snapshot.
// Cost: O(1) after the first call; that call is O(b), b = bytes of the sub
// rows (searched once).
fn method_rows_start() -> usize {
    static START: OnceLock<usize> = OnceLock::new();
    *START.get_or_init(|| {
        SNAPSHOT
            .find("\nmethod\t")
            .map_or(SNAPSHOT.len(), |at| at + 1)
    })
}

/// The snapshot rows of one kind, `"sub"` or `"method"`, each with the columns
/// after the kind.
///
/// Each kind is parsed only on its first use, and only its own stretch of the
/// snapshot is read. A program declaring a `sub` asks [`is_core_sub_name`] on
/// that path. Building the whole table there, methods included, was ~14M
/// instructions: five times the rest of the startup of `sub f() { 1 }; say 1`.
// Cost: O(b), b = bytes of that kind's stretch (a line scan).
fn rows(kind: &'static str) -> impl Iterator<Item = std::str::Split<'static, char>> {
    let stretch = if kind == "method" {
        &SNAPSHOT[method_rows_start()..]
    } else {
        &SNAPSHOT[..method_rows_start()]
    };
    stretch.lines().filter_map(move |line| {
        let rest = line.strip_prefix(kind)?.strip_prefix('\t')?;
        Some(rest.split('\t'))
    })
}

/// One candidate row's `multi, file, line, signature` columns.
// Cost: O(1) beyond the columns' own length.
fn candidate(mut cols: std::str::Split<'static, char>) -> Option<CoreCandidate> {
    let (Some(multi), Some(file), Some(line_no), Some(signature)) =
        (cols.next(), cols.next(), cols.next(), cols.next())
    else {
        return None;
    };
    Some(CoreCandidate {
        multi: multi == "1",
        file,
        line: line_no.parse().unwrap_or(0),
        signature,
    })
}

/// The core subs' candidates, by name.
// Cost: O(1) after the first call; that call is O(b + s), b = bytes of the
// snapshot (scanned), s = the sub rows (parsed).
fn sub_table() -> &'static Routines {
    static TABLE: OnceLock<Routines> = OnceLock::new();
    TABLE.get_or_init(|| {
        let mut table = Routines::default();
        for mut cols in rows("sub") {
            let Some(name) = cols.next() else { continue };
            if let Some(c) = candidate(cols) {
                table.entry(name).or_default().push(c);
            }
        }
        table
    })
}

/// The built-in types' method candidates, by owner, then name.
// Cost: O(1) after the first call; that call is O(b + m), b = bytes of the
// snapshot (scanned), m = the method rows (parsed).
fn method_table() -> &'static FxHashMap<&'static str, Routines> {
    static TABLE: OnceLock<FxHashMap<&'static str, Routines>> = OnceLock::new();
    TABLE.get_or_init(|| {
        let mut table = FxHashMap::<&'static str, Routines>::default();
        for mut cols in rows("method") {
            let (Some(owner), Some(name)) = (cols.next(), cols.next()) else {
                continue;
            };
            if let Some(c) = candidate(cols) {
                table
                    .entry(owner)
                    .or_default()
                    .entry(name)
                    .or_default()
                    .push(c);
            }
        }
        table
    })
}

/// The names of the core subs, without their candidates: the cheapest answer
/// to "does Rakudo declare a sub of this name", which every `sub` a program
/// declares asks (`note_main_unit_routine`).
// Cost: O(1) after the first call; that call is O(b + s), b = bytes of the
// snapshot (scanned), s = the sub rows (only their name column read).
fn sub_names() -> &'static FxHashSet<&'static str> {
    static NAMES: OnceLock<FxHashSet<&'static str>> = OnceLock::new();
    NAMES.get_or_init(|| rows("sub").filter_map(|mut cols| cols.next()).collect())
}

/// Whether Rakudo declares a core sub named `name` (the names
/// [`core_sub_candidates`] answers non-empty for), without building any
/// candidate table.
// Cost: O(n) (a hash probe; a second one for an operator name holding `<` or
// `>`), n = name length, plus the one-time name scan.
pub(crate) fn is_core_sub_name(name: &str) -> bool {
    sub_names().contains(name)
        || french_quoted_operator_name(name)
            .is_some_and(|french| sub_names().contains(french.as_str()))
}

/// The candidates of the core sub `name`, in Rakudo's declaration order.
// Cost: O(1) lookup (plus the one-time table build), c = candidates returned.
pub(crate) fn core_sub_candidates(name: &str) -> &'static [CoreCandidate] {
    let rows = sub_lookup(name);
    if !rows.is_empty() {
        return rows;
    }
    // Rakudo names an operator whose symbol holds `<` or `>` with French
    // quotes (`infix:«>»`), and the snapshot keeps that spelling; a code
    // handle names it `infix:<>>`. Without this, `&infix:«>».candidates` was
    // empty, and the `static` analyzer judged every `$x > 0` a mismatch.
    match french_quoted_operator_name(name) {
        Some(french) => sub_lookup(&french),
        None => rows,
    }
}

/// `infix:<>>` -> `infix:«>»`: the `«»` spelling Rakudo gives an operator
/// name whose symbol contains `<` or `>`, or `None` for any other name.
// Cost: O(n), n = name length.
fn french_quoted_operator_name(name: &str) -> Option<String> {
    let (category, rest) = name.split_once(":<")?;
    let symbol = rest.strip_suffix('>')?;
    (!category.is_empty() && symbol.contains(['<', '>'])).then(|| format!("{category}:«{symbol}»"))
}

/// The candidates of the method `name` declared on built-in type `owner`.
// Cost: O(1) lookup (plus the one-time table build).
pub(crate) fn core_method_candidates(owner: &str, name: &str) -> &'static [CoreCandidate] {
    if owner.is_empty() {
        return &[];
    }
    method_table()
        .get(owner)
        .and_then(|names| names.get(name))
        .map_or(&[], Vec::as_slice)
}

fn sub_lookup(name: &str) -> &'static [CoreCandidate] {
    sub_table().get(name).map_or(&[], Vec::as_slice)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sub_rows_precede_method_rows() {
        let first_method = SNAPSHOT
            .lines()
            .position(|l| l.starts_with("method\t"))
            .expect("the snapshot has method rows");
        assert!(
            SNAPSHOT
                .lines()
                .skip(first_method)
                .all(|l| !l.starts_with("sub\t")),
            "every sub row must precede the first method row (`rows` reads each kind's stretch)"
        );
    }

    #[test]
    fn core_sub_names_agree_with_candidates() {
        for name in ["say", "infix:<+>", "infix:<>>", "EVAL", "DEFINITE"] {
            assert!(is_core_sub_name(name), "{name}");
            assert!(!core_sub_candidates(name).is_empty(), "{name}");
        }
        for name in ["workload", "no-such-core-sub"] {
            assert!(!is_core_sub_name(name), "{name}");
            assert!(core_sub_candidates(name).is_empty(), "{name}");
        }
    }

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
