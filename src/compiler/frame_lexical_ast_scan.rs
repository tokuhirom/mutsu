//! The AST half of the frame-lexical proof (ADR-0113): which mentions of a
//! candidate routine's name a routine body makes, read off its serialized
//! AST. See `frame_lexical_routines.rs`.

use serde_json::Value as Json;
use std::collections::{HashMap, HashSet};

/// String leaves that make the whole body ineligible: they reach a routine
/// by a name computed at run time, or observe the routine as a code object
/// or through the dispatcher.
const REJECT_ALL_STRINGS: &[&str] = &[
    "EVAL",
    "EVALFILE",
    "evalbytes",
    "callframe",
    "callframes",
    "samewith",
    "callsame",
    "nextsame",
    "callwith",
    "nextwith",
    "nextcallee",
    "lastcall",
    "?ROUTINE",
    "&?ROUTINE",
];

/// Pseudo-packages that can reach a lexical by name.
const REJECT_ALL_PREFIXES: &[&str] = &[
    "MY::",
    "OUTER::",
    "OUTERS::",
    "CALLER::",
    "CALLERS::",
    "LEXICAL::",
    "UNIT::",
    "DYNAMIC::",
];

/// AST node kinds that make the whole body ineligible. Symbolic and
/// indirect lookups reach a routine by a computed name; a lexical type
/// declaration makes a parameter's type constraint resolve differently per
/// call, which the once-per-interpreter derivation could not follow.
const REJECT_ALL_VARIANTS: &[&str] = &[
    "IndirectCodeLookup",
    "IndirectTypeLookup",
    "IndirectTypeLookupAssign",
    "SymbolicDeref",
    "SymbolicDerefAssign",
    "PseudoStash",
    "UserRoutineCall",
    "ClassDecl",
    "RoleDecl",
    "EnumDecl",
    "SubsetDecl",
    "Package",
    "AugmentClass",
];

/// Field names given to the elements of a tuple variant.
const TUPLE_FIELDS: &[&str] = &["0", "1", "2", "3", "4", "5", "6", "7"];

#[derive(Default)]
pub(super) struct AstScan {
    pub(super) names: HashSet<String>,
    pub(super) calls: HashMap<String, usize>,
    pub(super) decls: HashMap<String, usize>,
    /// Names read as a code object (`&name`, the `CodeVar` node).
    pub(super) values: HashSet<String>,
    pub(super) rejected: HashSet<String>,
    pub(super) reject_all: bool,
}

impl AstScan {
    pub(super) fn walk(&mut self, v: &Json, variant: Option<&str>, field: Option<&str>) {
        if self.reject_all {
            return;
        }
        match v {
            Json::String(s) => self.check_str(s, variant, field),
            Json::Array(items) => {
                for item in items {
                    self.walk(item, None, None);
                }
            }
            Json::Object(map) => {
                if map.len() == 1
                    && let Some((key, inner)) = map.iter().next()
                    && key.starts_with(|c: char| c.is_ascii_uppercase())
                {
                    if REJECT_ALL_VARIANTS.contains(&key.as_str()) {
                        self.reject_all = true;
                        return;
                    }
                    match inner {
                        Json::Object(fields)
                            if !(fields.len() == 1
                                && fields.keys().next().is_some_and(|k| {
                                    k.starts_with(|c: char| c.is_ascii_uppercase())
                                })) =>
                        {
                            for (f, fv) in fields {
                                self.walk(fv, Some(key), Some(f));
                            }
                        }
                        // A tuple variant: its elements keep the variant, and
                        // their position stands in for the field name.
                        Json::Array(items) => {
                            for (i, item) in items.iter().enumerate() {
                                self.walk(
                                    item,
                                    Some(key),
                                    Some(TUPLE_FIELDS.get(i).unwrap_or(&"")),
                                );
                            }
                        }
                        other => self.walk(other, Some(key), None),
                    }
                    return;
                }
                for (f, fv) in map {
                    self.walk(fv, None, Some(f));
                }
            }
            _ => {}
        }
    }

    fn check_str(&mut self, s: &str, variant: Option<&str>, field: Option<&str>) {
        if REJECT_ALL_STRINGS.contains(&s) || REJECT_ALL_PREFIXES.iter().any(|p| s.contains(p)) {
            self.reject_all = true;
            return;
        }
        if self.names.contains(s) {
            match (variant, field) {
                (Some("Call"), Some("name")) => *self.calls.entry(s.to_string()).or_default() += 1,
                (Some("SubDecl"), Some("name")) => {
                    *self.decls.entry(s.to_string()).or_default() += 1
                }
                (Some("CodeVar"), None) => {
                    self.values.insert(s.to_string());
                }
                // A scalar or sigilless variable of the same name (`$name`
                // is `name` in the AST): a different symbol than `&name`.
                (Some("Var" | "MarkBoundContainer"), None)
                | (Some("VarDecl" | "Assign" | "AssignExpr"), Some("name"))
                | (Some("MarkReadonly"), Some("0"))
                | (None, Some("name")) => {}
                _ => {
                    self.rejected.insert(s.to_string());
                }
            }
            return;
        }
        // `&name`, `Pkg::name`, `&Pkg::name`: the routine as a code object or
        // by a qualified name.
        let tail = s.rsplit("::").next().unwrap_or(s);
        let tail = tail.strip_prefix('&').unwrap_or(tail);
        if tail != s && self.names.contains(tail) {
            self.rejected.insert(tail.to_string());
        }
    }
}
