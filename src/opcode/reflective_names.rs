//! Which by-name reads of a chunk can reach another frame's lexicals: the
//! classification `CompiledCode::scan_reflective_name_access` latches the
//! process-wide `NON_EVAL_REFLECTIVE_SEEN` on (#17998).
use super::{CompiledCode, OpCode};
use crate::symbol::Symbol;

/// Whether the pseudo-stash `name` (`UNIT::`, `OUTER::OUTER::`, `Foo::Bar::`,
/// ...) can only reach a scope that lexically encloses the code reading it,
/// or a package: no component walks the dynamic chain (`CALLER`, `CALLERS`,
/// `DYNAMIC`, `CLIENT`) or a lexical view that crosses it (`LEXICAL`, which
/// includes dynamics). `PROCESS::` is answered before this is asked.
// Cost: O(n), n = bytes of `name`.
pub(super) fn pseudo_stash_names_enclosing_scope(name: &str) -> bool {
    !name.is_empty()
        && every_segment(Symbol::intern(name), |part| {
            !matches!(
                part,
                "CALLER" | "CALLERS" | "DYNAMIC" | "CLIENT" | "LEXICAL"
            )
        })
}

/// Whether `pred` holds for every `::` segment of `name` (`Foo::Bar::` has the
/// segments `Foo`, `Bar` and the empty last one).
// Cost: O(s) memo probes, s = segments of `name`.
fn every_segment(name: Symbol, pred: impl Fn(&str) -> bool) -> bool {
    let mut rest = name;
    loop {
        match crate::qualified::split_qualified(rest) {
            Some((head, tail)) => {
                if !pred(tail.as_str()) {
                    return false;
                }
                rest = head;
            }
            None => return pred(rest.as_str()),
        }
    }
}

/// Whether a computed indirect name that starts with the constant `prefix`
/// (`"VERS::"` of `::("VERS::$_")`) can only name a member of a package: the
/// prefix is a package qualifier (`Ident::`, possibly nested) whose components
/// are no pseudo-package that reaches a lexical scope (`MY`, `OUTER`,
/// `CALLER`, ...). A sigil or a bare word without `::` may name a lexical.
// Cost: O(n), n = bytes of `prefix`.
pub(super) fn names_package_member_prefix(prefix: &str) -> bool {
    let Some((qualifier, _)) = crate::qualified::split_qualified(Symbol::intern(prefix)) else {
        return false;
    };
    !qualifier.as_str().is_empty()
        && every_segment(qualifier, |part| {
            part.starts_with(|c: char| c.is_alphabetic() || c == '_')
                && part
                    .chars()
                    .all(|c| c.is_alphanumeric() || c == '_' || c == '-')
                && !matches!(
                    part,
                    "MY" | "OUR"
                        | "OUTER"
                        | "OUTERS"
                        | "UNIT"
                        | "LEXICAL"
                        | "CALLER"
                        | "CALLERS"
                        | "DYNAMIC"
                        | "CLIENT"
                        | "SETTING"
                        | "CORE"
                        | "COMPILING"
                        | "PROCESS"
                        | "GLOBAL"
                )
        })
}

impl CompiledCode {
    /// The constant the first operand of the `StringConcat(n)` at `ip` loads,
    /// when every operand is one value-producing op (`LoadConst`, a local,
    /// global or upvalue read), optionally decontainerized. `None` for any
    /// other operand shape, whose start cannot be told from the ops alone.
    // Cost: O(n), n = the concat's operands.
    pub(super) fn string_concat_first_const(&self, ip: usize, n: usize) -> Option<u32> {
        let mut remaining = n;
        let mut p = ip;
        let mut first = None;
        while remaining > 0 {
            p = p.checked_sub(1)?;
            match self.ops.get(p)? {
                // Consumes and pushes one value: part of the operand before it.
                OpCode::DerefContainer => continue,
                OpCode::LoadConst(idx) => first = Some(*idx),
                OpCode::GetLocal(_) | OpCode::GetGlobal(_) | OpCode::GetUpvalue { .. } => {
                    first = None
                }
                _ => return None,
            }
            remaining -= 1;
        }
        first
    }
}
