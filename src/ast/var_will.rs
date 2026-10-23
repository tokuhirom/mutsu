//! `my $x will begin { ... }`: a declaration with `will PHASE` traits.
//!
//! The parser expands each trait into a phaser after the declaration whose body
//! runs the written block with `$_` bound to the container
//! (`parser::wrap_with_will_phasers`). Rakudo's tree keeps them as the
//! declaration's `Trait::Will(phase => "begin", block => Block)` entries
//! (measured on 2026.09); [`recognize`] reads the expansion back.

use super::{Expr, PhaserKind, Stmt};

/// The `will` spelling of a phaser kind (`begin`, `leave`, ...).
// Cost: O(1).
pub(crate) fn phase_name(kind: &PhaserKind) -> &'static str {
    match kind {
        PhaserKind::Begin => "begin",
        PhaserKind::Check => "check",
        PhaserKind::Init => "init",
        PhaserKind::End => "end",
        PhaserKind::Enter => "enter",
        PhaserKind::Leave => "leave",
        PhaserKind::Keep => "keep",
        PhaserKind::Undo => "undo",
        PhaserKind::First => "first",
        PhaserKind::Next => "next",
        PhaserKind::Last => "last",
        PhaserKind::Pre => "pre",
        PhaserKind::Post => "post",
        PhaserKind::Quit => "quit",
        PhaserKind::Close => "close",
    }
}

/// The phaser kind a `will NAME` trait names, if it is one.
// Cost: O(1).
pub(crate) fn phase_kind(name: &str) -> Option<PhaserKind> {
    Some(match name {
        "begin" => PhaserKind::Begin,
        "check" => PhaserKind::Check,
        "init" => PhaserKind::Init,
        "end" => PhaserKind::End,
        "enter" => PhaserKind::Enter,
        "leave" => PhaserKind::Leave,
        "keep" => PhaserKind::Keep,
        "undo" => PhaserKind::Undo,
        "first" => PhaserKind::First,
        "next" => PhaserKind::Next,
        "last" => PhaserKind::Last,
        "pre" => PhaserKind::Pre,
        "post" => PhaserKind::Post,
        "quit" => PhaserKind::Quit,
        "close" => PhaserKind::Close,
        _ => return None,
    })
}

/// One `will` phaser of an expansion: its kind and the written body.
pub(crate) type WillPhaser<'a> = (&'a PhaserKind, &'a [Stmt]);

/// The declaration and its `will` phasers (kind and written body) of an
/// expansion the parser built, or `None` for any other statement.
// Cost: O(p), p = number of phasers.
pub(crate) fn recognize(stmt: &Stmt) -> Option<(&Stmt, Vec<WillPhaser<'_>>)> {
    let Stmt::SyntheticBlock(stmts) = stmt else {
        return None;
    };
    let [decl @ Stmt::VarDecl { name, .. }, phasers @ ..] = stmts.as_slice() else {
        return None;
    };
    if phasers.is_empty() {
        return None;
    }
    let mut out = Vec::with_capacity(phasers.len());
    for phaser in phasers {
        let Stmt::Phaser {
            spans: _,
            kind,
            body,
            condition: None,
            end_index: None,
        } = phaser
        else {
            return None;
        };
        let [
            Stmt::Given {
                topic,
                body,
                is_statement_modifier: false,
                with_kind: None,
            },
        ] = body.as_slice()
        else {
            return None;
        };
        // A run-time phaser's topic is the variable; a compile-time one's is
        // the declaration's initial value.
        let compile_time = matches!(
            kind,
            PhaserKind::Begin | PhaserKind::Check | PhaserKind::Init
        );
        if !compile_time && !matches!(topic, Expr::Var(v) if v == name) {
            return None;
        }
        out.push((kind, body.as_slice()));
    }
    Some((decl, out))
}
