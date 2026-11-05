//! `Block.phasers` and `Block.callable_for_phaser` (#15375).
//!
//! Rakudo keeps a block's loop and scope phasers in a table keyed by the
//! phaser's name; the parallel-iteration modules (ParaSeq) pull the `FIRST`,
//! `NEXT` and `LAST` callables out of a block they run themselves. mutsu keeps
//! a phaser as a top-level `Stmt::Phaser` of the block's body, so both methods
//! read it from there and answer a fresh `Block` that runs the phaser body in
//! the block's captured scope.

use super::*;
use crate::ast::{PhaserKind, Stmt};

/// The phaser kind a block keeps a table entry for, spelled as rakudo's
/// `phasers` key (`"NEXT"`); `None` for any other name, including the
/// compile-time phasers (`BEGIN`, `CHECK`, `INIT`, `END`), which a block does
/// not carry.
// Cost: O(1).
fn block_phaser_kind(name: &str) -> Option<PhaserKind> {
    let kind = match name {
        "ENTER" => PhaserKind::Enter,
        "LEAVE" => PhaserKind::Leave,
        "KEEP" => PhaserKind::Keep,
        "UNDO" => PhaserKind::Undo,
        "FIRST" => PhaserKind::First,
        "NEXT" => PhaserKind::Next,
        "LAST" => PhaserKind::Last,
        "PRE" => PhaserKind::Pre,
        "POST" => PhaserKind::Post,
        "QUIT" => PhaserKind::Quit,
        "CLOSE" => PhaserKind::Close,
        _ => return None,
    };
    Some(kind)
}

/// Whether rakudo appends a phaser of `kind` to its table entry (`ENTER`,
/// `FIRST`, `PRE` run in declaration order) rather than prepending it (the
/// rest run last-declared first).
// Cost: O(1).
fn runs_in_declaration_order(kind: &PhaserKind) -> bool {
    matches!(kind, PhaserKind::Enter | PhaserKind::First | PhaserKind::Pre)
}

impl Interpreter {
    /// A `Block` that runs `body` in `data`'s captured scope, with no
    /// signature: rakudo's phaser blocks take no parameters.
    // The compiler forces the capture of the enclosing variables a phaser body
    // names (`Stmt::Phaser` in `compile_stmt`), so a write reaches them.
    // Cost: O(1) plus the clone of the captured `SubData` fields.
    fn phaser_block(data: &crate::value::SubData, body: Vec<Stmt>) -> Value {
        let mut phaser = data.clone();
        phaser.params = std::sync::Arc::new(Vec::new());
        phaser.param_defs = std::sync::Arc::new(Vec::new());
        Self::compile_phaser_body(&mut phaser, body);
        Value::sub_value(crate::gc::Gc::new(phaser))
    }

    /// Make `phaser` (a copy of its block's `SubData`) run `body`, a phaser
    /// statement's body, as the shared compiled chunk for that text and
    /// signature instead of an AST body the tree-walk carrier recompiles.
    // Cost: O(1) after the first request for this body and signature; O(b)
    // once, b = size of the body.
    pub(crate) fn compile_phaser_body(phaser: &mut crate::value::SubData, body: Vec<Stmt>) {
        phaser.body = std::sync::Arc::new(body);
        phaser.is_bare_block = true;
        phaser.compiled_routine = None;
        let key = {
            use std::hash::{Hash, Hasher};
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            format!("{:?}{:?}{:?}", phaser.body, phaser.params, phaser.param_defs)
                .hash(&mut hasher);
            format!("phaser:{:x}", hasher.finish())
        };
        let (code, fns) = crate::runtime::synthesized_closure_code(
            &key,
            &phaser.params,
            &phaser.param_defs,
            &phaser.body,
        );
        phaser.compiled_code = Some(code);
        phaser.compiled_fns = Some(fns);
    }

    /// The `name` phasers of the block `data`, in the order rakudo runs them;
    /// empty for a name that is not a block phaser.
    // Cost: O(s) over the block's s top-level statements.
    pub(crate) fn block_phasers(data: &crate::value::SubData, name: &str) -> Vec<Value> {
        let Some(kind) = block_phaser_kind(name) else {
            return Vec::new();
        };
        let mut found: Vec<Value> = data
            .body
            .iter()
            .filter_map(|stmt| match stmt {
                Stmt::Phaser {
                    kind: stmt_kind,
                    body,
                    ..
                } if *stmt_kind == kind => Some(Self::phaser_block(data, body.clone())),
                _ => None,
            })
            .collect();
        if !runs_in_declaration_order(&kind) {
            found.reverse();
        }
        found
    }

    /// `Block.callable_for_phaser`: the block that runs every `name` phaser in
    /// order, or `Nil` when the block has none.
    // Cost: O(s) over the block's s top-level statements.
    pub(crate) fn block_callable_for_phaser(data: &crate::value::SubData, name: &str) -> Value {
        let Some(kind) = block_phaser_kind(name) else {
            return Value::NIL;
        };
        let mut bodies: Vec<Vec<Stmt>> = data
            .body
            .iter()
            .filter_map(|stmt| match stmt {
                Stmt::Phaser {
                    kind: stmt_kind,
                    body,
                    ..
                } if *stmt_kind == kind => Some(body.clone()),
                _ => None,
            })
            .collect();
        if !runs_in_declaration_order(&kind) {
            bodies.reverse();
        }
        match bodies.len() {
            0 => Value::NIL,
            1 => Self::phaser_block(data, bodies.remove(0)),
            // Each phaser keeps its own lexical scope.
            _ => Self::phaser_block(data, bodies.into_iter().map(Stmt::Block).collect()),
        }
    }
}
