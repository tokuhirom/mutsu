//! The class-level helpers of `RakuAST::Pragma` (rakudo's
//! `src/Raku/ast/pragma.rakumod`): the pragma and `-ism` tables the compiler
//! consults, which tools such as `RakuAST::Deparse::Highlight` read to tell a
//! pragma from a module. A value of `KNOWN-PRAGMAS` is 1 when rakudo has not
//! implemented the pragma yet.

use crate::value::{Value, ValueMap};

/// The pragmas rakudo 2026.09 knows, with their not-yet-implemented flag.
const KNOWN_PRAGMAS: &[(&str, i64)] = &[
    ("MONKEY", 0),
    ("MONKEY-GUTS", 1),
    ("MONKEY-SEE-NO-EVAL", 1),
    ("MONKEY-TYPING", 1),
    ("attributes", 0),
    ("dynamic-scope", 0),
    ("fatal", 0),
    ("internals", 1),
    ("invocant", 0),
    ("isms", 0),
    ("lib", 0),
    ("nqp", 1),
    ("parameters", 0),
    ("precompilation", 0),
    ("soft", 0),
    ("strict", 1),
    ("trace", 1),
    ("variables", 0),
    ("worries", 0),
];

/// The `-ism` pragmas, by the language they name.
const KNOWN_ISMS: &[(&str, &str)] = &[("C++", "c++isms"), ("Perl5", "p5isms")];

/// Answer `RakuAST::Pragma.<method>(...)`, or `None` when it is no helper.
// Cost: O(1) (the tables are constant size).
pub(super) fn call(method: &str, args: &[Value]) -> Option<Value> {
    let name = || args.first().map(Value::to_string_value).unwrap_or_default();
    let flag = |known: bool| Value::int(i64::from(known));
    match method {
        "IS-PRAGMA" => Some(flag(KNOWN_PRAGMAS.iter().any(|(n, _)| *n == name()))),
        "IS-NYI" => Some(Value::int(
            KNOWN_PRAGMAS
                .iter()
                .find(|(n, _)| *n == name())
                .map_or(0, |(_, nyi)| *nyi),
        )),
        "IS-ISM" => Some(flag(KNOWN_ISMS.iter().any(|(n, _)| *n == name()))),
        "KNOWN-PRAGMAS" => Some(Value::hash(
            KNOWN_PRAGMAS
                .iter()
                .map(|(n, nyi)| ((*n).to_string(), Value::int(*nyi)))
                .collect::<ValueMap>(),
        )),
        "KNOWN-ISMS" => Some(Value::hash(
            KNOWN_ISMS
                .iter()
                .map(|(n, v)| ((*n).to_string(), Value::str((*v).to_string())))
                .collect::<ValueMap>(),
        )),
        _ => None,
    }
}
