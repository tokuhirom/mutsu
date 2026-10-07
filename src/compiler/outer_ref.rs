//! Which binding a write through `OUTER::` (or a read past a shadow) targets,
//! and how the compiler reaches it (#10676, #10827).
//!
//! `OUTER::` names one lexical scope, so the binding it denotes is settled by
//! the scope chain alone. When no scope between here and the target shadows the
//! name, `$OUTER::x` is exactly the plain `$x` and compiles as a write to it
//! (#10676). Otherwise the plain name denotes the shadow -- in its slot, its env
//! entry and every by-name write path alike -- so the target is reached through
//! a key of its own: the target's slot is given a shared cell, published in the
//! env under `__mutsu_outer::<scope>:<name>` (`OpCode::BoxOuterRef`, see
//! `src/vm/vm_outer_capture.rs`), and the write is an ordinary by-name store of
//! that key, which lands in the cell (a `:=` reseats a binding cell, ADR-0097
//! §14.1).
//!
//! Where the cell is published depends on where the target lives:
//!
//! - **In this compilation frame** (a shadowing inner block): right at the
//!   write site, since the target slot is addressable here.
//! - **In an enclosing frame** (`sub s { my $x; $OUTER::x = 1 }`): by the frame
//!   that declares it, right before it creates the closure or sub. The nested
//!   body records the binding in
//!   [`CompiledCode::outer_captures`](crate::opcode::CompiledCode::outer_captures)
//!   and [`Compiler::absorb_outer_captures`] turns each entry into that op
//!   (or passes it on outward); the closure capture then carries the key like
//!   every other `__mutsu_*` key.
//!
//! An `@`/`%` binding takes the same route with an `@`/`%` key
//! (`@__mutsu_outer::<scope>:<name>`), so a whole-container store keeps its
//! list-assignment semantics, and a mutating method call writes its result
//! back through that key. An element store (`@OUTER::a[0] = v`) needs no key:
//! it stores into the container the lexical read yields (#10857).

use std::collections::HashMap;

use super::{Compiler, lex_scope};
use crate::ast::{AssignOp, Expr, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_stmt};
use crate::meta_ns::MetaNs;
use crate::opcode::{CompiledCode, OpCode, OuterCapture};
use crate::token_kind::TokenKind;
use crate::value::Value;

/// Collects the names a tree writes through `OUTER::` -- a scalar assigned,
/// bound or stepped, or any mention of an `@`/`%` container -- each with
/// whether one of the writes is a `:=` (see `Compiler::outer_write_names`).
#[derive(Default)]
struct OuterWriteScan {
    names: HashMap<String, bool>,
}

impl OuterWriteScan {
    fn record(&mut self, target: &str, rebinds: bool) {
        if let Some((name, _)) = Compiler::split_outer_name(target)
            && !name.starts_with('&')
        {
            *self.names.entry(name).or_default() |= rebinds;
        }
    }
}

impl<'ast> Visit<'ast> for OuterWriteScan {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if let Stmt::Assign { name, op, .. } = stmt {
            self.record(name, matches!(op, AssignOp::Bind));
        }
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        match expr {
            Expr::AssignExpr { name, is_bind, .. } => self.record(name, *is_bind),
            // An Array/Hash is mutable through every path that reaches it --
            // a method call, an element store -- so any mention of one through
            // `OUTER::` counts as a write (#10857).
            Expr::ArrayVar(name) => self.record(&format!("@{name}"), false),
            Expr::HashVar(name) => self.record(&format!("%{name}"), false),
            Expr::PostfixOp {
                op: TokenKind::PlusPlus | TokenKind::MinusMinus,
                expr: target,
            }
            | Expr::Unary {
                op: TokenKind::PlusPlus | TokenKind::MinusMinus,
                expr: target,
                ..
            } => {
                if let Expr::Var(name) = target.peel_parens() {
                    self.record(name, false);
                }
            }
            _ => {}
        }
        walk_expr(self, expr);
    }
}

impl Compiler {
    /// Record the names `stmts` write through `OUTER::` (see
    /// [`Compiler::outer_write_names`]).
    // Cost: O(n), n = size of the tree.
    pub(super) fn seed_outer_write_names(&mut self, stmts: &[Stmt]) {
        let mut scan = OuterWriteScan::default();
        for stmt in stmts {
            scan.visit_stmt(stmt);
        }
        if scan.names.is_empty() {
            return;
        }
        let mut names = (*self.outer_write_names).clone();
        for (name, rebinds) in scan.names {
            *names.entry(name).or_default() |= rebinds;
        }
        self.outer_write_names = std::sync::Arc::new(names);
    }

    /// After `stmt`, if it is a `my $x` declaration: when the unit writes `$x` through
    /// `OUTER::` somewhere, share the new binding in a cell right away, while
    /// the name still denotes it. A write that later reaches it past a shadow
    /// then lands in the cell the slot and the env entry both hold -- so every
    /// by-name reader, and the block/loop exits that re-seed an enclosing slot
    /// from its env entry, see it too. The cell is also published under the
    /// binding's `OUTER::` key, which is how that write finds it.
    pub(super) fn share_outer_written_decl(&mut self, stmt: &Stmt) {
        if self.outer_write_names.is_empty() {
            return;
        }
        let Stmt::VarDecl {
            name,
            is_state: false,
            is_our: false,
            ..
        } = stmt
        else {
            return;
        };
        let Some(&rebinds) = self.outer_write_names.get(name) else {
            return;
        };
        let Some(&slot) = self.local_map.get(name) else {
            return;
        };
        let scope = self.enclosing_scopes.len() + self.local_scopes.len() - 1;
        let key_idx = self
            .code
            .add_constant(Value::str(Self::outer_cell_key(name, scope)));
        self.code.note_rebound_slot(rebinds.then_some(slot));
        self.code.emit(OpCode::BoxOuterRef {
            slot,
            key_idx,
            rebinds,
            visible: true,
        });
    }

    /// Split a `[sigil]OUTER::...::name` spelling into the name a scope frame
    /// records (the sigil kept for `@`/`%`/`&`) and the `OUTER::` depth.
    fn split_outer_name(name: &str) -> Option<(String, usize)> {
        let (sigil, rest) = match name.as_bytes().first() {
            Some(b'@' | b'%' | b'&') => name.split_at(1),
            _ => ("", name),
        };
        let (bare, depth) = Self::parse_outer_prefix(rest)?;
        Some((format!("{sigil}{bare}"), depth))
    }

    /// The name a write through `OUTER::` (`$OUTER::x := $y`, `$OUTER::x = 5`,
    /// `$OUTER::x++`) stores under: the plain name when it denotes the target
    /// binding here, else the key the target's shared cell is published under
    /// (emitting the publication when the target is in this frame). `None`
    /// when `name` is not such a spelling or names no binding this compilation
    /// can see; the write then keeps its literal by-name store. `rebinds` is
    /// whether the write is a `:=`.
    pub(super) fn outer_write_target(&mut self, name: &str, rebinds: bool) -> Option<String> {
        let (key, depth) = Self::split_outer_name(name)?;
        let chain = self.full_scope_chain();
        if lex_scope::outer_is_visible_binding(&chain, &key, depth) {
            return Some(key);
        }
        let scope = lex_scope::outer_target_index(&chain, &key, depth)?;
        // A `&` binding has no shadowed-write form worth a key of its own.
        if key.starts_with('&') {
            return None;
        }
        if scope < self.enclosing_scopes.len() {
            return Some(self.outer_capture_key(&key, scope, rebinds));
        }
        let slot = lex_scope::slot_at_index(&chain, &self.local_map, &key, scope)?;
        let cell_key = Self::outer_cell_key(&key, scope);
        let key_idx = self.code.add_constant(Value::str(cell_key.clone()));
        self.code.note_rebound_slot(rebinds.then_some(slot));
        self.code.emit(OpCode::BoxOuterRef {
            slot,
            key_idx,
            rebinds,
            visible: false,
        });
        Some(cell_key)
    }

    /// The env key the shared cell of `name`, declared in the scope at index
    /// `scope` of the full scope chain, is published under. An `@`/`%` name
    /// keeps its sigil in front (`@__mutsu_outer::1:a`): a whole-container
    /// store takes its list-assignment semantics from the sigil of the name it
    /// stores under, and the Array/Hash itself is what the key holds -- it is
    /// reference-shared, so storing into it reaches the declaring slot (#10857).
    fn outer_cell_key(name: &str, scope: usize) -> String {
        match name.as_bytes().first() {
            Some(b'@' | b'%') => {
                let (sigil, bare) = name.split_at(1);
                format!(
                    "{sigil}{}",
                    MetaNs::Outer.owned_key_for_str(format!("{scope}:{bare}"))
                )
            }
            _ => MetaNs::Outer.owned_key_for_str(format!("{scope}:{name}")),
        }
    }

    /// Record that this code reaches `name` of the enclosing-frame scope
    /// `scope` through a capture, and return the capture's env key.
    fn outer_capture_key(&mut self, name: &str, scope: usize, rebinds: bool) -> String {
        let key = Self::outer_cell_key(name, scope);
        match self.code.outer_captures.iter_mut().find(|c| c.key == key) {
            Some(capture) => capture.rebinds |= rebinds,
            None => self.code.outer_captures.push(OuterCapture {
                key: key.clone(),
                name: name.to_string(),
                scope,
                rebinds,
            }),
        }
        key
    }

    /// Emit a read of `bare` (`$OUTER::x`, `OUTER::<$x>`) through a capture
    /// when it names a binding of an enclosing compilation frame that a scope
    /// in between shadows. Returns `false` (nothing emitted) for every other
    /// shape, which `GetOuterVar` resolves.
    pub(super) fn try_emit_outer_capture_read(&mut self, bare: &str, depth: usize) -> bool {
        let chain = self.full_scope_chain();
        if lex_scope::outer_is_visible_binding(&chain, bare, depth) {
            return false;
        }
        let Some(scope) = lex_scope::outer_target_index(&chain, bare, depth) else {
            return false;
        };
        if scope >= self.enclosing_scopes.len() {
            return false;
        }
        let key = self.outer_capture_key(bare, scope, false);
        let key_idx = self.code.add_constant(Value::str(key));
        let name_idx = self.code.add_constant(Value::str(bare.to_string()));
        self.code.emit(OpCode::GetOuterCapture {
            key_idx,
            name_idx,
            depth: depth as u32,
        });
        true
    }

    /// Take over the captures of a body nested in this one, just compiled and
    /// about to be turned into a closure or sub: a binding declared in this
    /// frame is boxed and published right here, before the creation op; one
    /// declared further out becomes this code's own capture, so the frame that
    /// creates *this* code publishes it.
    pub(super) fn absorb_outer_captures(&mut self, nested: &CompiledCode) {
        if nested.outer_captures.is_empty() {
            return;
        }
        let chain = self.full_scope_chain();
        let base = self.enclosing_scopes.len();
        for capture in &nested.outer_captures {
            if capture.scope < base {
                self.outer_capture_key(&capture.name, capture.scope, capture.rebinds);
                continue;
            }
            let Some(slot) =
                lex_scope::slot_at_index(&chain, &self.local_map, &capture.name, capture.scope)
            else {
                continue;
            };
            let visible = self.local_map.get(&capture.name) == Some(&slot);
            self.code.note_rebound_slot(capture.rebinds.then_some(slot));
            let key_idx = self.code.add_constant(Value::str(capture.key.clone()));
            self.code.emit(OpCode::BoxOuterRef {
                slot,
                key_idx,
                rebinds: capture.rebinds,
                visible,
            });
        }
    }
}
