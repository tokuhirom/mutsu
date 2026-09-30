//! Nested and value-form BEGIN in the unit prologue (ADR-0134, slice 2).
//!
//! A `BEGIN` written inside a routine, closure, loop or block runs once, while
//! the enclosing unit is being compiled. It does not wait for the enclosing
//! code to run, and it does not run again each time that code runs. A
//! value-form `BEGIN` (`my $v = BEGIN expr`) behaves the same way, and its
//! value is a constant of its site. This module moves each such effect into the
//! unit's prologue, at the position of the top-level statement that contains
//! it.
//!
//! The effect sees the lexicals of its enclosing scopes in their static state.
//! A unit-level lexical is shared directly, because the prologue runs in the
//! unit's frame. A lexical of an inner scope does not exist yet when the
//! prologue runs, so it gets a **static cell**, a unit-level slot that stands
//! for the lexical's static value:
//!
//! - The lifted body runs in a block that declares the inner name, initialized
//!   from the cell. After the body, the block copies the value back to the cell.
//! - The inner declaration moves to the head of its scope and starts from the
//!   cell on every entry. Its initializer, if any, stays at the declaration's
//!   position as an assignment.
//!
//! The resulting value per entry is what rakudo's pad clone gives. One case
//! differs: an `@`/`%` lexical is copied from the cell on each entry, where
//! rakudo shares the same object (ADR-0134 §7).
//!
//! A parameter of an enclosing routine or block is unbound at BEGIN time. The
//! body sees a fresh, empty declaration of that name, as it does on rakudo.
//! An `our` variable is re-declared in the body's block, which binds the same
//! package variable.
//!
//! A routine an enclosing inner scope declares ahead of the BEGIN does not
//! exist in the prologue either. The body gets a copy of each one it calls (see
//! [`routines`]).
//!
//! A BEGIN is not lifted in these cases, and keeps its pre-ADR handling:
//!
//! - an enclosing inner scope declares, ahead of it, a type, a package, an
//!   import, a code variable, or a routine that is not a plain `sub` (a
//!   `multi`, an `our sub`, an operator, an exported one), which the prologue
//!   cannot reproduce yet;
//! - it can reach a name dynamically (`EVAL`, `CALLER::`, symbolic lookup), or
//!   calls a routine that is neither one of the scope's nor a core one (which
//!   may evaluate a string where it was called from), in a scope that declares
//!   a routine, since it cannot say which one it needs;
//! - it sits in a package body;
//! - it reads a name that resolves to nothing the unit declares (for example
//!   an EVAL's caller lexical);
//! - it reads a `state`, `constant` or group-declared inner lexical.

mod routines;

use crate::ast::{Expr, PhaserKind, Stmt};
use routines::{Access, FrameBlock, Routine, Scan};
use std::collections::{BTreeMap, HashSet};
use std::sync::atomic::{AtomicUsize, Ordering};

static SLOT_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn next_slot(prefix: &str) -> String {
    format!("{prefix}{}", SLOT_COUNTER.fetch_add(1, Ordering::Relaxed))
}

/// The prologue contributions of the BEGINs nested in one top-level statement.
#[derive(Default)]
pub(super) struct Lifted {
    /// The static declarations of the cells and value slots. They head the
    /// prologue, so every effect can reach them.
    pub(super) decls: Vec<Stmt>,
    /// One statement-form `BEGIN` per lifted effect, in source order.
    pub(super) effects: Vec<Stmt>,
    /// A BEGIN-time effect could not be lifted. It keeps its pre-ADR handling,
    /// which runs it later than the prologue, so no later effect is lifted
    /// either: that would run it ahead of an effect that precedes it in the
    /// source.
    halted: bool,
}

/// Lift every liftable BEGIN nested in `stmt`, a top-level statement of a
/// unit whose top-level lexical names are `unit_names`.
pub(super) fn lift_in_stmt(stmt: &mut Stmt, unit_names: &HashSet<String>, lifted: &mut Lifted) {
    let mut walker = Walker {
        unit_names,
        frames: Vec::new(),
        lifted,
    };
    walker.walk_stmt(stmt, None);
}

struct Walker<'a> {
    unit_names: &'a HashSet<String>,
    frames: Vec<Frame>,
    lifted: &'a mut Lifted,
}

#[derive(Default)]
struct Frame {
    bindings: Vec<Binding>,
    /// A type, package, import, operator or other routine the prologue cannot
    /// reproduce was declared in this scope ahead of the current statement.
    blocked: bool,
    /// The plain routines declared in this scope ahead of the current
    /// statement. A lifted BEGIN that calls one gets its own copy
    /// ([`Routine`]).
    routines: Vec<Routine>,
    /// Edits to this scope's statement list, applied once it has been walked.
    edits: Vec<(usize, Edit)>,
}

struct Binding {
    name: String,
    kind: BindingKind,
}

enum BindingKind {
    Param,
    /// An `our` declaration. Re-declaring it binds the same package variable.
    Our(Box<Stmt>),
    Local {
        index: usize,
        static_decl: Box<Stmt>,
        assign: Option<Box<Stmt>>,
        cell: Option<String>,
    },
    Opaque,
}

enum Edit {
    Remove,
    Replace(Box<Stmt>),
    /// Move the declaration to the head of its scope, starting from its cell.
    /// The initializer's assignment stays in place.
    Split {
        head: Box<Stmt>,
        assign: Option<Box<Stmt>>,
    },
}

impl Walker<'_> {
    fn walk_list(&mut self, list: &mut Vec<Stmt>, frame: Frame) {
        self.frames.push(frame);
        let len = list.len();
        for (i, stmt) in list.iter_mut().enumerate() {
            self.walk_stmt(stmt, Some((i, i + 1 == len)));
        }
        let frame = self.frames.pop().expect("frame pushed above");
        if frame.edits.is_empty() {
            return;
        }
        let mut edits: Vec<Option<Edit>> = (0..len).map(|_| None).collect();
        for (i, edit) in frame.edits {
            edits[i] = Some(edit);
        }
        let mut head = Vec::new();
        let mut out = Vec::with_capacity(len);
        for (stmt, edit) in std::mem::take(list).into_iter().zip(edits) {
            match edit {
                None => out.push(stmt),
                Some(Edit::Remove) => {}
                Some(Edit::Replace(replacement)) => out.push(*replacement),
                Some(Edit::Split { head: decl, assign }) => {
                    head.push(*decl);
                    out.extend(assign.map(|a| *a));
                }
            }
        }
        head.append(&mut out);
        *list = head;
    }

    fn params_frame(names: impl IntoIterator<Item = String>) -> Frame {
        Frame {
            bindings: names
                .into_iter()
                .map(|name| Binding {
                    name,
                    kind: BindingKind::Param,
                })
                .collect(),
            ..Frame::default()
        }
    }

    /// `loc` is the statement's index in the list being walked and whether it
    /// is that list's last statement, or `None` for a statement embedded in an
    /// expression.
    fn walk_stmt(&mut self, stmt: &mut Stmt, loc: Option<(usize, bool)>) {
        match stmt {
            Stmt::Phaser {
                kind: PhaserKind::Begin,
                body,
                ..
            } => {
                let (Some((index, is_tail)), false) = (loc, self.frames.is_empty()) else {
                    return;
                };
                // A BEGIN that ends its block is the block's value.
                let slot = is_tail.then(|| next_slot("__begin_value_"));
                if self.lift(body, slot.as_deref()) {
                    let edit = match slot {
                        Some(slot) => Edit::Replace(Box::new(Stmt::Expr(slot_read(slot)))),
                        None => Edit::Remove,
                    };
                    self.current_frame().edits.push((index, edit));
                }
            }
            Stmt::VarDecl {
                expr,
                custom_traits,
                ..
            } => {
                if custom_traits.iter().any(|(t, _)| t == "__constant") {
                    self.lift_constant_initializer(expr);
                } else {
                    self.walk_expr(expr);
                }
                self.bind_decl(stmt, loc.map(|(i, _)| i));
            }
            Stmt::SyntheticBlock(inner) => {
                for member in inner.iter() {
                    match member {
                        Stmt::VarDecl { name, .. } => self.bind_opaque(name.clone()),
                        // A `will begin` trait is a BEGIN-time effect this
                        // slice does not lift.
                        Stmt::Phaser {
                            kind: PhaserKind::Begin,
                            ..
                        } => self.lifted.halted = true,
                        _ => {}
                    }
                }
            }
            Stmt::Assign { expr, .. }
            | Stmt::Expr(expr)
            | Stmt::Return(expr)
            | Stmt::Die(expr)
            | Stmt::Fail(expr)
            | Stmt::Take(expr, _) => self.walk_expr(expr),
            Stmt::Say(exprs) | Stmt::Put(exprs) | Stmt::Print(exprs) | Stmt::Note(exprs) => {
                for e in exprs.iter_mut() {
                    self.walk_expr(e);
                }
            }
            Stmt::Call { args, .. } => {
                for arg in args.iter_mut() {
                    match arg {
                        crate::ast::CallArg::Positional(e)
                        | crate::ast::CallArg::Slip(e)
                        | crate::ast::CallArg::Invocant(e) => self.walk_expr(e),
                        crate::ast::CallArg::Named { value, .. } => {
                            if let Some(e) = value {
                                self.walk_expr(e);
                            }
                        }
                    }
                }
            }
            Stmt::Block(body)
            | Stmt::Default(body)
            | Stmt::Catch(body)
            | Stmt::Control(body)
            | Stmt::React { body } => self.walk_list(body, Frame::default()),
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                ..
            } => {
                self.walk_expr(cond);
                self.walk_list(then_branch, Frame::default());
                self.walk_list(else_branch, Frame::default());
            }
            Stmt::While { cond, body, .. } | Stmt::When { cond, body, .. } => {
                self.walk_expr(cond);
                self.walk_list(body, Frame::default());
            }
            Stmt::Loop { body, .. } => self.walk_list(body, Frame::default()),
            Stmt::For {
                iterable,
                param,
                params,
                body,
                ..
            } => {
                self.walk_expr(iterable);
                let names = param.iter().chain(params.iter()).cloned();
                self.walk_list(body, Self::params_frame(names));
            }
            Stmt::Given { topic, body, .. } => {
                self.walk_expr(topic);
                self.walk_list(body, Frame::default());
            }
            Stmt::Whenever { supply, body, .. } => {
                self.walk_expr(supply);
                self.walk_list(body, Frame::default());
            }
            Stmt::Label { stmt: inner, .. } => self.walk_stmt(inner, loc),
            Stmt::SubDecl { params, body, .. } => {
                self.walk_list(body, Self::params_frame(params.iter().cloned()));
                self.declare_routine(stmt);
            }
            Stmt::ProtoDecl { .. }
            | Stmt::MethodDecl { .. }
            | Stmt::TokenDecl { .. }
            | Stmt::RuleDecl { .. }
            | Stmt::ProtoToken { .. }
            | Stmt::ClassDecl { .. }
            | Stmt::RoleDecl { .. }
            | Stmt::EnumDecl { .. }
            | Stmt::SubsetDecl { .. }
            | Stmt::Package { .. }
            | Stmt::AugmentClass { .. }
            | Stmt::Use { .. }
            | Stmt::No { .. }
            | Stmt::Need { .. }
            | Stmt::Import { .. } => self.block_current_frame(),
            _ => {}
        }
    }

    fn walk_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::PhaserExpr {
                kind: PhaserKind::Begin,
                body,
            } => {
                let slot = next_slot("__begin_value_");
                if self.lift(body, Some(&slot)) {
                    *expr = slot_read(slot);
                }
            }
            Expr::Block(stmts) | Expr::Gather(stmts) | Expr::DoBlock { body: stmts, .. } => {
                self.walk_list(stmts, Frame::default())
            }
            Expr::AnonSub { body, .. } => self.walk_list(body, Frame::default()),
            Expr::AnonSubParams { params, body, .. } => {
                self.walk_list(body, Self::params_frame(params.iter().cloned()))
            }
            Expr::Lambda { param, body, .. } => {
                self.walk_list(body, Self::params_frame([param.clone()]))
            }
            Expr::Try { body, catch } => {
                self.walk_list(body, Frame::default());
                if let Some(c) = catch {
                    self.walk_list(c, Frame::default());
                }
            }
            Expr::DoStmt(inner) => self.walk_stmt(inner, None),
            Expr::WhateverCurry(inner)
            | Expr::Grouped(inner)
            | Expr::Unary { expr: inner, .. }
            | Expr::PostfixOp { expr: inner, .. }
            | Expr::AssignExpr { expr: inner, .. }
            | Expr::PositionalPair(inner)
            | Expr::ZenSlice(inner)
            | Expr::Eager(inner)
            | Expr::Itemize(inner) => self.walk_expr(inner),
            Expr::Binary { left, right, .. } => {
                self.walk_expr(left);
                self.walk_expr(right);
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                self.walk_expr(cond);
                self.walk_expr(then_expr);
                self.walk_expr(else_expr);
            }
            Expr::MethodCall { target, args, .. } => {
                self.walk_expr(target);
                for a in args.iter_mut() {
                    self.walk_expr(a);
                }
            }
            Expr::CallOn { target, args } => {
                self.walk_expr(target);
                for a in args.iter_mut() {
                    self.walk_expr(a);
                }
            }
            Expr::Call { args, .. } | Expr::UserRoutineCall { args, .. } => {
                for a in args.iter_mut() {
                    self.walk_expr(a);
                }
            }
            Expr::Index { target, index, .. } => {
                self.walk_expr(target);
                self.walk_expr(index);
            }
            Expr::ArrayLiteral(es)
            | Expr::BracketArray(es, _)
            | Expr::StringInterpolation(es)
            | Expr::CaptureLiteral(es) => {
                for e in es.iter_mut() {
                    self.walk_expr(e);
                }
            }
            Expr::Hash(pairs) => {
                for (_, v) in pairs.iter_mut() {
                    if let Some(e) = v {
                        self.walk_expr(e);
                    }
                }
            }
            _ => {}
        }
    }

    fn current_frame(&mut self) -> &mut Frame {
        self.frames
            .last_mut()
            .expect("a statement list is being walked")
    }

    fn block_current_frame(&mut self) {
        if let Some(frame) = self.frames.last_mut() {
            frame.blocked = true;
        }
    }

    fn bind_opaque(&mut self, name: String) {
        if let Some(frame) = self.frames.last_mut() {
            frame.bindings.push(Binding {
                name,
                kind: BindingKind::Opaque,
            });
        }
    }

    fn bind_decl(&mut self, stmt: &Stmt, index: Option<usize>) {
        let Stmt::VarDecl {
            name,
            is_state,
            is_our,
            ..
        } = stmt
        else {
            return;
        };
        if self.frames.is_empty() {
            return;
        }
        let split = crate::runtime::phasers::split_var_decl(stmt);
        let kind = match (split, index) {
            (Some((static_decl, _)), _) if *is_our => BindingKind::Our(Box::new(static_decl)),
            (Some((static_decl, assign)), Some(index)) if !*is_state => BindingKind::Local {
                index,
                static_decl: Box::new(static_decl),
                assign: assign.map(Box::new),
                cell: None,
            },
            _ => BindingKind::Opaque,
        };
        // A code variable can declare an operator (`my &infix:<plus>`) and be
        // reached by symbolic lookup, so it counts as a routine declaration.
        if name.starts_with('&') {
            self.block_current_frame();
        }
        let name = name.clone();
        self.current_frame().bindings.push(Binding { name, kind });
    }

    /// A `constant` initializer is a BEGIN-time effect. One nested in a scope
    /// is lifted when it reads an inner lexical that a lifted BEGIN gave a
    /// static cell, since only the cell holds the value it must see. Any other
    /// nested constant is left to the constant handling of slice 3.
    fn lift_constant_initializer(&mut self, expr: &mut Expr) {
        if self.frames.is_empty() {
            return;
        }
        let body = [Stmt::Expr(expr.clone())];
        let reads_cell = Scan::of(&body).free().iter().any(|sym| {
            self.find_binding(&sym.resolve(), None)
                .is_some_and(|(f, b)| {
                    matches!(
                        self.frames[f].bindings[b].kind,
                        BindingKind::Local { cell: Some(_), .. }
                    )
                })
        });
        if !reads_cell {
            return;
        }
        let slot = next_slot("__begin_value_");
        if self.lift(&body, Some(&slot)) {
            *expr = slot_read(slot);
        }
    }

    /// Lift `body` into the prologue when every name it reads can be supplied
    /// there. With `slot`, the body's value is stored in that unit-level slot.
    /// Returns whether the body was lifted.
    fn lift(&mut self, body: &[Stmt], slot: Option<&str>) -> bool {
        if self.lifted.halted {
            return false;
        }
        // A blockless `BEGIN my %h = ...` declares into the enclosing scope,
        // which the lifted body's block would hide. Its body is that one
        // declaration; `BEGIN { my $x ... }` keeps its `my` to itself.
        let declares = matches!(body, [Stmt::VarDecl { .. } | Stmt::SyntheticBlock(_)]);
        // A placeholder makes the body an error (`X::Placeholder::Block`), which
        // the in-place path reports and the lifted body would not.
        let has_placeholder = !crate::ast::collect_unattached_placeholders(body).is_empty();
        let deps = if declares || has_placeholder || self.frames.iter().any(|f| f.blocked) {
            None
        } else {
            self.resolve_dependencies(body)
        };
        let Some(deps) = deps else {
            self.lifted.halted = true;
            return false;
        };
        let mut blocks: BTreeMap<usize, FrameBlock> = BTreeMap::new();
        self.add_routines(&deps, &mut blocks);
        for ((frame, binding), access) in deps.bindings {
            let block = blocks.entry(frame).or_default();
            match access {
                Access::CopyIn(decl) => block.copy_in.push(*decl),
                Access::Cell => {
                    let (decl, back) = self.cell_access(frame, binding);
                    block.copy_in.push(decl);
                    block.copy_out.push(back);
                }
            }
        }
        let mut inner = Vec::new();
        match slot {
            Some(slot) => {
                self.lifted.decls.push(static_scalar(slot));
                inner.push(Stmt::Assign {
                    name: slot.to_string(),
                    expr: Expr::DoBlock {
                        body: body.to_vec(),
                        label: None,
                        origin: crate::ast::DoBlockOrigin::Desugar,
                    },
                    op: crate::ast::AssignOp::Assign,
                    target_is_sigilless: false,
                });
            }
            None => inner.extend_from_slice(body),
        }
        let inner = FrameBlock::nest(blocks, inner);
        self.lifted.effects.push(Stmt::Phaser {
            kind: PhaserKind::Begin,
            body: vec![Stmt::Block(inner)],
            condition: None,
            end_index: None,
        });
        true
    }

    /// The copy-in declaration and copy-out assignment for an inner lexical,
    /// allocating its static cell on first use.
    fn cell_access(&mut self, frame: usize, binding: usize) -> (Stmt, Stmt) {
        let name = self.frames[frame].bindings[binding].name.clone();
        let BindingKind::Local {
            index,
            static_decl,
            assign,
            cell,
        } = &mut self.frames[frame].bindings[binding].kind
        else {
            unreachable!("only a Local binding gets a cell")
        };
        let cell_name = match cell {
            Some(cell_name) => cell_name.clone(),
            None => {
                let cell_name = format!("{}{}", sigil_of(&name), next_slot("__begin_cell_"));
                self.lifted
                    .decls
                    .push(renamed_static_decl(static_decl, &cell_name));
                let head = decl_from_cell(static_decl, &cell_name);
                let edit = Edit::Split {
                    head: Box::new(head),
                    assign: assign.take(),
                };
                let index = *index;
                *cell = Some(cell_name.clone());
                self.frames[frame].edits.push((index, edit));
                cell_name
            }
        };
        let BindingKind::Local { static_decl, .. } = &self.frames[frame].bindings[binding].kind
        else {
            unreachable!()
        };
        let copy_in = decl_from_cell(static_decl, &cell_name);
        let copy_out = Stmt::Assign {
            name: cell_name,
            expr: read_var(&name),
            op: crate::ast::AssignOp::Assign,
            target_is_sigilless: false,
        };
        (copy_in, copy_out)
    }
}

fn sigil_of(name: &str) -> &str {
    match name.as_bytes().first() {
        Some(b'@') => "@",
        Some(b'%') => "%",
        Some(b'&') => "&",
        _ => "",
    }
}

/// The expression reading variable `name` (in `VarDecl` naming).
fn read_var(name: &str) -> Expr {
    match name.as_bytes().first() {
        Some(b'@') => Expr::ArrayVar(name[1..].to_string()),
        Some(b'%') => Expr::HashVar(name[1..].to_string()),
        Some(b'&') => Expr::CodeVar(name[1..].to_string()),
        _ => Expr::Var(name.to_string()),
    }
}

fn static_scalar(name: &str) -> Stmt {
    Stmt::VarDecl {
        name: name.to_string(),
        expr: Expr::Literal(crate::value::Value::NIL),
        type_constraint: None,
        is_state: false,
        is_our: false,
        is_dynamic: false,
        is_export: false,
        export_tags: vec![],
        custom_traits: vec![],
        where_constraint: None,
    }
}

/// A fresh declaration of `name` as an unbound parameter looks at BEGIN time.
fn unbound_decl(name: &str) -> Stmt {
    let decl = static_scalar(name);
    crate::runtime::phasers::split_var_decl(&decl)
        .map(|(static_decl, _)| static_decl)
        .unwrap_or(decl)
}

fn without_initializer_markers(traits: &[(String, Option<Expr>)]) -> Vec<(String, Option<Expr>)> {
    traits
        .iter()
        .filter(|(t, _)| t != "__has_initializer" && t != "__scalar_bind")
        .cloned()
        .collect()
}

/// The cell's own static declaration: the variable's, under the cell's name.
fn renamed_static_decl(static_decl: &Stmt, cell_name: &str) -> Stmt {
    let mut decl = static_decl.clone();
    if let Stmt::VarDecl {
        name,
        is_export,
        export_tags,
        custom_traits,
        ..
    } = &mut decl
    {
        *name = cell_name.to_string();
        *is_export = false;
        export_tags.clear();
        *custom_traits = without_initializer_markers(custom_traits);
    }
    decl
}

/// The variable's declaration, initialized from its cell.
fn decl_from_cell(static_decl: &Stmt, cell_name: &str) -> Stmt {
    let mut decl = static_decl.clone();
    if let Stmt::VarDecl {
        expr,
        custom_traits,
        ..
    } = &mut decl
    {
        *expr = read_var(cell_name);
        let mut traits = without_initializer_markers(custom_traits);
        traits.push(("__has_initializer".to_string(), None));
        traits.push((
            crate::runtime::phasers::BEGIN_STATIC_TRAIT.to_string(),
            None,
        ));
        *custom_traits = traits;
    }
    decl
}

/// Reads a value slot the way the BEGIN's own value would be read: the slot is
/// a scalar, so it is decontainerized (`$slot<>`). Otherwise
/// `my str @hex = BEGIN (^256)>>.fmt("%02x")` would assign one itemized list.
fn slot_read(slot: String) -> Expr {
    Expr::MethodCall {
        target: Box::new(Expr::Var(slot)),
        name: crate::symbol::Symbol::intern("__mutsu_zen_angle"),
        args: vec![],
        modifier: None,
        quoted: false,
    }
}
