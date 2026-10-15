//! The `Unicode` class's rows (ADR-11276 §9.111): `Unicode.version` and
//! `Unicode.NFG`, which describe the Unicode data this runtime ships.
//!
//! Rakudo calls them on the type object (`Unicode.version`), and mutsu has no
//! `Unicode` instance, so the shape is a type object only and every row takes
//! one.

use super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, VersionPart};

type Answer = Option<Result<Value, RuntimeError>>;

macro_rules! row {
    ($name:literal, $handler:ident) => {
        MethodRow {
            owner: "Unicode",
            name: $name,
            arity: 0,
            handler: Handler::Narrow($handler),
            flags: RowFlags::TYPE_OBJECT_OK,
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[row!("version", version), row!("NFG", nfg)];

/// `Unicode.version`: the version of the tables `unicode-normalization`
/// ships. Rakudo renders Unicode 17.0.0 as `v17.0`, so a patch component is
/// kept only when it is nonzero.
// Cost: O(1).
fn version(_target: &Value, _args: &[Value]) -> Answer {
    let (major, minor, patch) = unicode_normalization::UNICODE_VERSION;
    let mut parts = vec![
        VersionPart::Num(i64::from(major)),
        VersionPart::Num(i64::from(minor)),
    ];
    if patch != 0 {
        parts.push(VersionPart::Num(i64::from(patch)));
    }
    Some(Ok(Value::version(parts, false, false)))
}

/// `Unicode.NFG`: mutsu's strings are grapheme-aware throughout, so it gives
/// MoarVM's answer.
// Cost: O(1).
fn nfg(_target: &Value, _args: &[Value]) -> Answer {
    Some(Ok(Value::TRUE))
}
