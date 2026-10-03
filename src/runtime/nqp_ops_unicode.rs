//! The Unicode property and character-name `nqp::` ops (#11495):
//! `unipropcode`, `unipvalcode`, `getuniprop_int` / `_str` / `_bool`,
//! `matchuniprop`, `hasuniprop`, `getuniname`, `codepointfromname` and
//! `strfromname`.
//!
//! The property *values* come from the same tables `uniprop` / `unimatch`
//! read (`builtins::unicode_gc`, `builtins::unicode_script`,
//! `builtins::uniprop::try_binary_property`); only the integer handles are
//! nqp's own, and those are MoarVM's measured numbers
//! (`nqp_uniprop_data`). A property mutsu cannot answer is an error rather
//! than a handle that would later produce a confidently wrong integer.

use super::nqp_uniprop_data::{
    BINARY_PROPERTIES, GENERAL_CATEGORY_ALIASES, GENERAL_CATEGORY_VALUES, SCRIPT_VALUES,
};
use crate::value::{RuntimeError, Value};

/// MoarVM's property code for `General_Category`.
const PROP_GENERAL_CATEGORY: i64 = 20;
/// MoarVM's property code for `Script`.
const PROP_SCRIPT: i64 = 9;

fn iarg(args: &[Value], i: usize) -> i64 {
    args.get(i).map(crate::runtime::to_int).unwrap_or(0)
}

fn sarg(args: &[Value], i: usize) -> String {
    args.get(i).map(|v| v.to_string_value()).unwrap_or_default()
}

/// MoarVM's name rule for properties and property values: the exact
/// spelling, or that spelling lowercased (`latin` finds `Latin`, `LATIN` does
/// not -- measured).
// Cost: O(m), m = chars of `name`.
fn name_matches(input: &str, name: &str) -> bool {
    input == name
        || (input.len() == name.len()
            && input
                .bytes()
                .zip(name.bytes())
                .all(|(a, b)| a == b.to_ascii_lowercase()))
}

/// What a property code names, among the properties mutsu answers.
enum Prop {
    GeneralCategory,
    Script,
    /// A binary property, by its canonical name.
    Binary(&'static str),
}

fn prop_of(code: i64) -> Option<Prop> {
    match code {
        PROP_GENERAL_CATEGORY => Some(Prop::GeneralCategory),
        PROP_SCRIPT => Some(Prop::Script),
        _ => BINARY_PROPERTIES
            .binary_search_by_key(&code, |&(c, _)| c)
            .ok()
            .map(|i| Prop::Binary(BINARY_PROPERTIES[i].1[0])),
    }
}

fn unsupported_prop(op: &str, code: i64) -> RuntimeError {
    RuntimeError::new(format!(
        "nqp::{op}: unsupported property code {code} \
         (mutsu answers General_Category, Script and the binary properties)"
    ))
}

/// The property code `nqp::unipropcode($name)` answers.
// Cost: O(P * m), P = properties known (~60), m = chars of `name`.
fn prop_code(name: &str) -> Option<i64> {
    if name_matches(name, "General_Category") || name_matches(name, "gc") {
        return Some(PROP_GENERAL_CATEGORY);
    }
    if name_matches(name, "Script") || name_matches(name, "sc") {
        return Some(PROP_SCRIPT);
    }
    BINARY_PROPERTIES
        .iter()
        .find(|(_, names)| names.iter().any(|n| name_matches(name, n)))
        .map(|&(code, _)| code)
}

/// The General_Category abbreviation of a codepoint; a surrogate is `Cs` and
/// anything outside the codepoint space is `Cn`, as in MoarVM.
// Cost: O(1) (table lookup; astral codepoints O(log r), r = ranges).
fn gc_abbrev(cp: i64) -> &'static str {
    match u32::try_from(cp) {
        Ok(0xD800..=0xDFFF) => "Cs",
        Ok(c) => char::from_u32(c)
            .map(|ch| crate::builtins::unicode_gc::general_category(ch).as_str())
            .unwrap_or("Cn"),
        Err(_) => "Cn",
    }
}

fn char_of(cp: i64) -> Option<char> {
    u32::try_from(cp).ok().and_then(char::from_u32)
}

/// `nqp::getuniprop_int($cp, $prop)`: the property VALUE code of a codepoint.
// Cost: O(1) for General_Category and the binary properties (table lookups);
// O(S) for Script, S = scripts (~175), to turn the name into its code.
fn prop_value_code(op: &str, cp: i64, code: i64) -> Result<i64, RuntimeError> {
    Ok(
        match prop_of(code).ok_or_else(|| unsupported_prop(op, code))? {
            Prop::GeneralCategory => {
                let gc = gc_abbrev(cp);
                GENERAL_CATEGORY_VALUES
                    .iter()
                    .position(|&(abbrev, _)| abbrev == gc)
                    .unwrap_or(0) as i64
            }
            Prop::Script => char_of(cp)
                .map(crate::builtins::unicode::unicode_script_name)
                .and_then(|name| SCRIPT_VALUES.iter().position(|&(n, _)| n == name))
                .unwrap_or(0) as i64,
            Prop::Binary(name) => char_of(cp)
                .and_then(|ch| crate::builtins::uniprop::try_binary_property(ch, name))
                .map_or(0, i64::from),
        },
    )
}

/// `nqp::unipvalcode($prop, $name)`: the value code a property value name
/// stands for, 0 for a name the property does not have. A binary property
/// has no value names in MoarVM (`True`, `Y`, `1` all answer 0 -- measured).
// Cost: O(V * m), V = values of the property, m = chars of `name`.
fn value_code(op: &str, code: i64, name: &str) -> Result<i64, RuntimeError> {
    Ok(
        match prop_of(code).ok_or_else(|| unsupported_prop(op, code))? {
            Prop::GeneralCategory => GENERAL_CATEGORY_VALUES
                .iter()
                .position(|&(abbrev, long)| name_matches(name, abbrev) || name_matches(name, long))
                .map(|i| i as i64)
                .or_else(|| {
                    GENERAL_CATEGORY_ALIASES
                        .iter()
                        .find(|&&(alias, _)| name_matches(name, alias))
                        .map(|&(_, c)| c)
                })
                .unwrap_or(0),
            Prop::Script => SCRIPT_VALUES
                .iter()
                .position(|&(long, aliases)| {
                    name_matches(name, long) || aliases.iter().any(|a| name_matches(name, a))
                })
                .map_or(0, |i| i as i64),
            Prop::Binary(_) => 0,
        },
    )
}

/// MoarVM's `codepointfromname` spelling rule: the exact name or alias, in
/// capitals with single inner spaces. `uniparse`'s loose matching
/// (`latin small letter a`, `LATIN_SMALL_LETTER_A`) is `strfromname`'s, not
/// this op's (measured: those answer -1 here).
// Cost: O(m), m = chars of `name`.
fn is_strict_name_spelling(name: &str) -> bool {
    !name.is_empty()
        && !name.starts_with(' ')
        && !name.ends_with(' ')
        && !name.contains("  ")
        && !name.bytes().any(|b| b.is_ascii_lowercase() || b == b'_')
}

/// Try a Unicode property / character-name `nqp::` op. `None` means "not an
/// op this table knows".
pub(super) fn call_nqp_unicode_op(op: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    Some(match op {
        // nqp::unipropcode($name) -> the property handle getuniprop_* takes.
        // Cost: O(P * m), P = properties known (~60), m = chars of $name.
        "unipropcode" => {
            let name = sarg(args, 0);
            prop_code(&name).map(Value::int).ok_or_else(|| {
                RuntimeError::new(format!(
                    "nqp::unipropcode: unsupported property '{name}' \
                     (mutsu answers General_Category, Script and the binary properties)"
                ))
            })
        }
        // nqp::unipvalcode($prop, $name) -> the value handle matchuniprop takes.
        // Cost: O(V * m), V = values of the property (<= ~175), m = chars of $name.
        "unipvalcode" => value_code(op, iarg(args, 0), &sarg(args, 1)).map(Value::int),
        // nqp::getuniprop_int($cp, $prop) -> the property VALUE code;
        // getuniprop_bool whether it is non-zero (measured: Script answers 1
        // for any assigned script); getuniprop_str the value's name, which is
        // empty for a binary property (measured).
        // Cost: O(1), O(S) for Script, S = scripts (~175).
        "getuniprop_int" => prop_value_code(op, iarg(args, 0), iarg(args, 1)).map(Value::int),
        // Cost: as getuniprop_int.
        "getuniprop_bool" => {
            prop_value_code(op, iarg(args, 0), iarg(args, 1)).map(|c| Value::int(i64::from(c != 0)))
        }
        // Cost: as getuniprop_int.
        "getuniprop_str" => {
            let (cp, code) = (iarg(args, 0), iarg(args, 1));
            prop_value_code(op, cp, code).map(|value| {
                Value::str_from(match prop_of(code) {
                    Some(Prop::GeneralCategory) => GENERAL_CATEGORY_VALUES[value as usize].0,
                    Some(Prop::Script) => SCRIPT_VALUES[value as usize].0,
                    _ => "",
                })
            })
        }
        // nqp::matchuniprop($cp, $prop, $pvalcode) -> whether the codepoint's
        // value of $prop is that value code.
        // Cost: as getuniprop_int.
        "matchuniprop" => prop_value_code(op, iarg(args, 0), iarg(args, 1))
            .map(|c| Value::int(i64::from(c == iarg(args, 2)))),
        // nqp::hasuniprop($str, $pos, $prop, $pvalcode): matchuniprop on the
        // codepoint `nqp::ordat` reports at grapheme $pos; 0 outside the string.
        // Cost: O(1) amortized for a flat string, O(STRIDE) otherwise, plus as getuniprop_int.
        "hasuniprop" => {
            let cp = crate::builtins::str_prim::nqp_ordat(
                args.first().unwrap_or(&Value::NIL),
                iarg(args, 1),
            );
            if cp < 0 {
                Ok(Value::int(0))
            } else {
                prop_value_code(op, cp, iarg(args, 2))
                    .map(|c| Value::int(i64::from(c == iarg(args, 3))))
            }
        }

        // -- character names --
        // nqp::getuniname($cp): the routine `uniname` uses, with its
        // `<control-000A>` / `<illegal>` / `<unassigned>` sentinels.
        // Cost: O(1) (table lookup).
        "getuniname" => crate::builtins::unicode::uniname_from_int(iarg(args, 0)).map(Value::str),
        // nqp::codepointfromname($name) -> the codepoint, or -1. Only the
        // exact spelling resolves (see is_strict_name_spelling).
        // Cost: O(m), m = chars of $name (one name-table probe).
        "codepointfromname" => {
            let name = sarg(args, 0);
            let cp = is_strict_name_spelling(&name)
                .then(|| crate::token_kind::lookup_unicode_char_by_name(&name))
                .flatten()
                .map_or(-1, |c| c as i64);
            Ok(Value::int(cp))
        }
        // nqp::strfromname($name) -> the string `uniparse` resolves one name
        // to (a character, named sequence or emoji sequence), or "".
        // Cost: O(m), m = chars of $name; a name no table knows also walks the
        // CLDR emoji list (O(E), E = emoji count).
        "strfromname" => Ok(Value::str(
            crate::token_kind::lookup_unicode_name_string(&sarg(args, 0)).unwrap_or_default(),
        )),
        _ => return None,
    })
}
