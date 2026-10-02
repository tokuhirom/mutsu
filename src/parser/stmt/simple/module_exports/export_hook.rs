//! Approximating the export set of a module that exports through a run-time
//! `sub EXPORT` hook (ADR-0087).
//!
//! The surrounding static scan looks for `is export` traits. A module that
//! computes its exports in `sub EXPORT` carries none, so it contributes nothing
//! to the importer's parse-time knowledge and every call shape that needs the
//! parser to know a name is a routine — a listop call above all — fails to
//! parse. This module supplies the approximation the scan falls back to.

use super::InlineModuleExport;
use crate::ast::Stmt;
use std::collections::HashMap;

/// Does this module install its imports at run time through a `sub EXPORT`
/// hook, rather than through `is export` traits?
///
/// `sub EXPORT` is Raku's escape hatch for computing an export set at load
/// time: whatever `Map` it returns becomes the importer's lexical import. The
/// static scan in the parent module cannot evaluate it — mutsu loads modules at run time, so
/// the set genuinely does not exist while the importing file is being parsed —
/// which is why its presence switches on the unit-scope approximation here
/// (ADR-0087).
///
/// Only a *unit-scope* `EXPORT` counts: that is the only place rakudo looks for
/// the hook, so a `sub EXPORT` nested inside a class body is an ordinary
/// routine and says nothing about how the module exports.
pub(super) fn declares_export_sub(stmts: &[Stmt]) -> bool {
    stmts.iter().any(|stmt| match stmt {
        Stmt::SubDecl { name, .. } | Stmt::ProtoDecl { name, .. } => name.resolve() == "EXPORT",
        // `unit module Foo;` wraps the rest of the file, but its declarations
        // are still the compunit's own unit scope.
        Stmt::Package {
            body,
            is_unit: true,
            ..
        } => declares_export_sub(body),
        _ => false,
    })
}

/// Approximate the export set of a `sub EXPORT` module with the routines it
/// declares in its own unit scope (ADR-0087).
///
/// The dominant idiom exports exactly that set — `UNIT::.grep: { .key.starts-with('&') }`
/// (String::Utils, and most of lizmat's ecosystem) — and every other shape
/// draws its `Pair` values from the same pool of unit-scope routines. The
/// approximation is therefore a superset in practice: it can name a routine the
/// module keeps to itself, and it misses a name the hook synthesises out of
/// thin air.
///
/// That asymmetry is deliberate. The only thing the parse phase does with this
/// set is decide that an identifier is a routine, which is what lets
/// `root <abcd abce>` parse as a listop call instead of an infix `<` comparison.
/// A name that is *not* really exported still resolves against the run-time
/// import set, which is computed by actually running `sub EXPORT`, so a
/// superset costs a worse diagnostic at worst and never changes the meaning of
/// a program that runs. Registering nothing — today's behavior — makes every
/// such call shape a hard parse error.
pub(super) fn collect_unit_scope_routines(
    stmts: &[Stmt],
    out: &mut HashMap<String, InlineModuleExport>,
) {
    for stmt in stmts {
        match stmt {
            Stmt::SubDecl { name, .. } | Stmt::ProtoDecl { name, .. } => {
                let resolved = name.resolve();
                // `EXPORT` is the hook itself; rakudo never exports it, and the
                // `UNIT::` idiom excludes it by name. A qualified `sub Foo::bar`
                // is installed in a package, not in the unit's lexical scope.
                if resolved.is_empty() || resolved == "EXPORT" || resolved.contains("::") {
                    continue;
                }
                out.entry(resolved.clone()).or_insert(InlineModuleExport {
                    name: resolved,
                    precedence: None,
                    associativity: None,
                    is_test_assertion: false,
                });
            }
            Stmt::Package {
                body,
                is_unit: true,
                ..
            } => collect_unit_scope_routines(body, out),
            _ => {}
        }
    }
}

/// The unit-scope `sub EXPORT`'s own body, if this module declares one —
/// same descent through a `unit module Foo;` wrapper as [`declares_export_sub`].
fn find_export_sub_body(stmts: &[Stmt]) -> Option<&[Stmt]> {
    stmts.iter().find_map(|stmt| match stmt {
        Stmt::SubDecl { name, body, .. } if name.resolve() == "EXPORT" => Some(body.as_slice()),
        Stmt::Package {
            body,
            is_unit: true,
            ..
        } => find_export_sub_body(body),
        _ => None,
    })
}

/// A second idiom `sub EXPORT` modules use, distinct from the `UNIT::`-grep
/// [`collect_unit_scope_routines`] approximates: building the exported names as
/// LOCAL declarations inside the hook's own body and hand-assembling the
/// returned `Map` from them (French's `my &infix:<et> = sub (...) {...}`,
/// `my \vrai = True;`, ...; lizmat's ecosystem leans on `UNIT::` instead, but
/// not every `sub EXPORT` module does).
///
/// The operator-slot spelling `my &infix:<op> = ...` is collected by
/// [`collect_export_hook_operator_subs`]: the custom-infix-word matcher takes a
/// word only when it is a declared (or CORE) infix, so the importer's parse has
/// to learn "et" is one (#9918).
///
/// A plain VALUE term (`my \vrai = True;`) needs this walk: an unknown
/// bareword defaults to a listop-call head, so `vrai et 2` misparsed as
/// `vrai(et, 2)` and died evaluating `et` as if it were a zero-arg call
/// (`Unknown function: et`) — the infix guess never even got a chance,
/// because the term guess ran first and swallowed it as an argument.
///
/// `my \x = ...` compiles to a `VarDecl` immediately followed by a sibling
/// `MarkSigillessReadonly` naming the same variable (the parser's marker for
/// a sigilless declaration); this walks the hook's body — including into the
/// `SyntheticBlock`/`Block` wrapper such a pair is nested in — collecting
/// every one it finds, so the importer's parse learns `vrai` is a term with
/// no arguments to swallow, the same way an exported `constant` already does.
fn collect_export_body_value_terms(stmts: &[Stmt], out: &mut Vec<String>) {
    for (i, stmt) in stmts.iter().enumerate() {
        match stmt {
            Stmt::VarDecl { name, .. } => {
                if let Some(Stmt::MarkSigillessReadonly(marked)) = stmts.get(i + 1)
                    && marked == name
                {
                    out.push(name.clone());
                }
            }
            Stmt::SyntheticBlock(inner) | Stmt::Block(inner) => {
                collect_export_body_value_terms(inner, out);
            }
            _ => {}
        }
    }
}

/// Extend `out` with the value terms an `EXPORT` hook declares locally in its
/// own body — see [`collect_export_body_value_terms`]. A no-op unless this
/// module actually declares the hook (checked again here rather than trusting
/// the caller, since the AST walk and the `declares_export_sub` regex
/// fallback can disagree on best-effort-parsed sources).
pub(super) fn collect_export_hook_value_terms(stmts: &[Stmt], out: &mut Vec<String>) {
    if let Some(body) = find_export_sub_body(stmts) {
        collect_export_body_value_terms(body, out);
    }
}

/// A fourth idiom: an operator declared as an ordinary LOCAL routine inside the
/// hook's own body and handed out through the returned `Map` (Understitch's
/// `sub infix:<_> (...) is equiv(&infix:<~>) { ... }` followed by
/// `Map.new: '&infix:<_>' => &infix:<_>`). It carries no `is export`, so the
/// precise walker skips it.
///
/// Without it every form that asks whether an operator is DECLARED — plain
/// word-infix use (#9918) and the reduction metaop `[_]` — rejects it.
/// A categorical routine declared in the hook has no purpose other than being
/// exported (the hook's body is a private lexical scope that ends when it
/// returns), so it is collected, with the precedence/associativity traits the
/// importer's parse needs.
pub(super) fn collect_export_hook_operator_subs(
    stmts: &[Stmt],
    exports: &mut HashMap<String, InlineModuleExport>,
) {
    if let Some(body) = find_export_sub_body(stmts) {
        collect_operator_subs_in(body, exports);
    }
}

fn collect_operator_subs_in(stmts: &[Stmt], exports: &mut HashMap<String, InlineModuleExport>) {
    for stmt in stmts {
        match stmt {
            Stmt::SubDecl {
                name,
                associativity,
                precedence_trait,
                ..
            } => {
                let resolved = name.resolve();
                if !is_operator_routine_name(&resolved) {
                    continue;
                }
                exports.entry(resolved.clone()).or_insert_with(|| {
                    super::sub_export_entry(
                        resolved,
                        precedence_trait.as_ref(),
                        associativity.clone(),
                        false,
                    )
                });
            }
            // The operator-slot variable spelling, `my &infix:<et> = sub {...}`
            // (French). It is exported the same way, and the importer's parse
            // must know it: an undeclared word is not an infix (#9918).
            Stmt::VarDecl { name, .. } => {
                let Some(resolved) = name.strip_prefix('&') else {
                    continue;
                };
                if !is_operator_routine_name(resolved) {
                    continue;
                }
                exports.entry(resolved.to_string()).or_insert_with(|| {
                    super::sub_export_entry(resolved.to_string(), None, None, false)
                });
            }
            Stmt::SyntheticBlock(inner) | Stmt::Block(inner) => {
                collect_operator_subs_in(inner, exports);
            }
            _ => {}
        }
    }
}

fn is_operator_routine_name(name: &str) -> bool {
    [
        "infix:<",
        "prefix:<",
        "postfix:<",
        "circumfix:<",
        "postcircumfix:<",
    ]
    .iter()
    .any(|category| name.starts_with(category))
}
