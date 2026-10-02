//! The traversal of the nested-BEGIN lift: a [`VisitMut`] (ADR-10499) that
//! keeps one [`Frame`] per statement list, so a lifted BEGIN can resolve the
//! lexicals of every scope around it and edit that scope's list.
//!
//! The walk reaches every child, except at the deliberate stops below: a
//! package or type body (a BEGIN there "sits in a package body"), a method,
//! token or proto declaration (which blocks the frame instead), and the
//! grouped declarations of a `SyntheticBlock`.

use super::cell_ast::slot_read;
use super::{Binding, BindingKind, Edit, Frame, Walker, next_slot};
use crate::ast::{Expr, ParamDef, PhaserKind, Stmt};
use crate::ast_visit::{VisitMut, walk_expr_mut, walk_stmt_mut};

impl VisitMut for Walker<'_> {
    fn visit_stmts_mut(&mut self, body: &mut Vec<Stmt>) {
        self.walk_list(body, Frame::default(), |_| {});
    }

    fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
        self.walk_stmt(stmt, None);
    }

    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        self.walk_expr(expr);
    }

    // TODO: lift from a regex code block too. The regex a match runs is not
    // always this tree (a code block closing over a lexical runs from the
    // copy the literal's value carries), so a BEGIN lifted from the tree
    // could run twice; it keeps its pre-ADR handling. See #10550.
    fn visit_regex_node_mut(&mut self, _node: &mut crate::regex_tree::RegexNode) {}
}

impl Walker<'_> {
    /// Walks `list` in a new scope `frame`, after `pre` has walked what is
    /// in that scope ahead of the list (a signature's defaults), then applies
    /// the edits the scope collected.
    pub(super) fn walk_list(
        &mut self,
        list: &mut Vec<Stmt>,
        frame: Frame,
        pre: impl FnOnce(&mut Self),
    ) {
        self.frames.push(frame);
        pre(self);
        let len = list.len();
        for (i, stmt) in list.iter_mut().enumerate() {
            self.walk_stmt(unlabel(stmt), Some((i, i + 1 == len)));
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

    /// Record that the phaser statement at `index` was lifted out: it leaves
    /// its scope, or, when it ends the scope, stands for its value there.
    fn edit_lifted(&mut self, index: usize, slot: Option<String>) {
        let edit = match slot {
            Some(slot) => Edit::Replace(Box::new(Stmt::Expr(slot_read(slot)))),
            None => Edit::Remove,
        };
        self.current_frame().edits.push((index, edit));
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
    pub(super) fn walk_stmt(&mut self, stmt: &mut Stmt, loc: Option<(usize, bool)>) {
        match stmt {
            Stmt::Phaser {
                kind: PhaserKind::Begin,
                body,
                ..
            } => {
                let (Some((index, is_tail)), false) = (loc, self.frames.is_empty()) else {
                    return;
                };
                // A BEGIN in a package body is not lifted.
                if self.in_package() {
                    return;
                }
                // A BEGIN that ends its block is the block's value.
                let slot = is_tail.then(|| next_slot("__begin_value_"));
                if self.lift(body, slot.as_deref(), &PhaserKind::Begin) {
                    self.edit_lifted(index, slot);
                }
            }
            // An INIT or CHECK that reads a lexical of the scope it is written
            // in is lifted to the unit's own sequence of them (#10562).
            Stmt::Phaser {
                kind: kind @ (PhaserKind::Init | PhaserKind::Check),
                body,
                ..
            } => {
                if let Some((index, is_tail)) = loc {
                    let slot = is_tail.then(|| next_slot("__init_value_"));
                    if self.lift(body, slot.as_deref(), &kind.clone()) {
                        self.edit_lifted(index, slot);
                        return;
                    }
                }
                walk_stmt_mut(self, stmt);
            }
            Stmt::VarDecl {
                expr,
                custom_traits,
                where_constraint,
                ..
            } => {
                if custom_traits.iter().any(|(t, _)| t == "__constant") && !self.in_package() {
                    self.lift_constant_initializer(expr);
                } else {
                    self.visit_expr_mut(expr);
                }
                for e in custom_traits.iter_mut().filter_map(|(_, a)| a.as_mut()) {
                    self.visit_expr_mut(e);
                }
                if let Some(e) = where_constraint {
                    self.visit_expr_mut(e);
                }
                self.bind_decl(stmt, loc.map(|(i, _)| i));
            }
            // The members of a grouped declaration are bound opaquely and not
            // walked: their initializers belong to the destructuring.
            Stmt::SyntheticBlock(inner) => {
                for member in crate::ast::scope_members_mut(inner) {
                    match member {
                        Stmt::VarDecl { name, .. } => self.bind_opaque(name.clone()),
                        // An exported type is its declaration plus a marker.
                        Stmt::ClassDecl { .. } | Stmt::Package { .. } if self.frames.is_empty() => {
                            self.walk_package(member)
                        }
                        Stmt::RoleDecl { .. } if self.frames.is_empty() => {
                            self.walk_detached(member)
                        }
                        // A nested `will begin` trait is a BEGIN-time effect
                        // this slice does not lift. A top-level one is split
                        // by the unit partition itself.
                        Stmt::Phaser {
                            kind: PhaserKind::Begin,
                            ..
                        } if !self.frames.is_empty() => self.lifted.halted = true,
                        _ => {}
                    }
                }
            }
            // A C-style loop's `init` declares into the loop's own scope.
            Stmt::Loop { .. } => {
                self.frames.push(Frame::default());
                walk_stmt_mut(self, stmt);
                self.frames.pop();
            }
            Stmt::For {
                iterable,
                param,
                param_def,
                params,
                params_def,
                body,
                ..
            } => {
                self.visit_expr_mut(iterable);
                let names = param.iter().chain(params.iter()).cloned();
                self.walk_list(body, Self::params_frame(names), |w| {
                    if let Some(p) = param_def.as_mut() {
                        w.visit_param_mut(p);
                    }
                    w.visit_params(params_def);
                });
            }
            Stmt::SubDecl {
                name_expr,
                params,
                param_defs,
                signature_alternates,
                custom_traits,
                body,
                ..
            } => {
                let frame = Self::params_frame(params.iter().cloned());
                self.walk_list(body, frame, |w| {
                    if let Some(e) = name_expr {
                        w.visit_expr_mut(e);
                    }
                    w.visit_params(param_defs);
                    for (_, defs) in signature_alternates.iter_mut() {
                        w.visit_params(defs);
                    }
                    for e in custom_traits.iter_mut().filter_map(|(_, a)| a.as_mut()) {
                        w.visit_expr_mut(e);
                    }
                });
                // A package's routines are reached through the package.
                if !self.in_package() {
                    self.declare_routine(stmt);
                }
            }
            // A method of a package body: only an INIT or CHECK in it is
            // lifted, to the unit's own sequence of them, which re-enters the
            // package (#10562).
            Stmt::MethodDecl {
                name_expr,
                params,
                param_defs,
                custom_traits,
                body,
                ..
            } if self.directly_in_package() => {
                let frame = Self::params_frame(params.iter().cloned());
                self.walk_list(body, frame, |w| {
                    if let Some(e) = name_expr {
                        w.visit_expr_mut(e);
                    }
                    w.visit_params(param_defs);
                    for e in custom_traits.iter_mut().filter_map(|(_, a)| a.as_mut()) {
                        w.visit_expr_mut(e);
                    }
                });
            }
            Stmt::Use {
                module,
                arg: Some(_),
                condition: None,
                ..
            } if module == "lib" && !self.frames.is_empty() && !self.in_package() => {
                self.lift_use_lib(stmt, loc)
            }
            // The routines of a class or package at the unit's level are
            // walked for their INIT and CHECK phasers; its BEGINs are not
            // lifted.
            Stmt::ClassDecl { .. } | Stmt::Package { .. }
                if self.frames.iter().all(|f| f.package.is_some()) =>
            {
                self.walk_package(stmt)
            }
            // A role, and a class declared in code, are walked without a
            // package to re-enter, and are recorded as a type of their scope
            // as before.
            Stmt::RoleDecl { .. } | Stmt::ClassDecl { .. } => {
                self.walk_detached(stmt);
                self.declare_type_or_import(stmt);
            }
            // A BEGIN in a package or type body is not lifted: the declaration
            // is recorded and its body left alone.
            Stmt::EnumDecl { .. }
            | Stmt::SubsetDecl { .. }
            | Stmt::Package { .. }
            | Stmt::Use { .. }
            | Stmt::No { .. }
            | Stmt::Need { .. }
            | Stmt::Import { .. } => self.declare_type_or_import(stmt),
            Stmt::ProtoDecl { .. }
            | Stmt::MethodDecl { .. }
            | Stmt::TokenDecl { .. }
            | Stmt::RuleDecl { .. }
            | Stmt::ProtoToken { .. }
            | Stmt::AugmentClass { .. } => self.block_current_frame(),
            // Package bodies and the body copy of a nested method (whose
            // declaration is hoisted into its package) are package code too.
            Stmt::PackageRuntimeBody { .. }
            | Stmt::NestedMethodCapture { .. }
            | Stmt::HasDecl { .. }
            | Stmt::DoesDecl { .. }
            | Stmt::TrustsDecl { .. } => {}
            _ => walk_stmt_mut(self, stmt),
        }
    }

    pub(super) fn walk_expr(&mut self, expr: &mut Expr) {
        match expr {
            Expr::PhaserExpr {
                kind: PhaserKind::Begin,
                body,
            } => {
                if self.in_package() {
                    return;
                }
                let slot = next_slot("__begin_value_");
                if self.lift(body, Some(&slot), &PhaserKind::Begin) {
                    *expr = slot_read(slot);
                }
            }
            Expr::PhaserExpr {
                kind: kind @ (PhaserKind::Init | PhaserKind::Check),
                body,
            } => {
                let slot = next_slot("__init_value_");
                if self.lift(body, Some(&slot), &kind.clone()) {
                    *expr = slot_read(slot);
                } else {
                    walk_expr_mut(self, expr);
                }
            }
            Expr::AnonSubParams {
                params,
                param_defs,
                custom_traits,
                body,
                ..
            } => {
                let frame = Self::params_frame(params.iter().cloned());
                self.walk_list(body, frame, |w| {
                    w.visit_params(param_defs);
                    for e in custom_traits
                        .as_mut_slice()
                        .iter_mut()
                        .filter_map(|(_, a)| a.as_mut())
                    {
                        w.visit_expr_mut(e);
                    }
                });
            }
            // `target`/`rhs` are a model-layer copy of the `expanded` form the
            // compiler runs (RakuAST); lifting from both would run the BEGIN
            // twice.
            Expr::CompoundAssign { expanded, .. } => self.visit_expr_mut(expanded),
            Expr::Lambda { param, body, .. } => {
                self.walk_list(body, Self::params_frame([param.clone()]), |_| {})
            }
            _ => walk_expr_mut(self, expr),
        }
    }

    fn visit_params(&mut self, defs: &mut [ParamDef]) {
        for p in defs {
            self.visit_param_mut(p);
        }
    }
}

/// The statement a (possibly labelled) list member stands for: a label is
/// transparent to the member's position in its list.
fn unlabel(mut stmt: &mut Stmt) -> &mut Stmt {
    while let Stmt::Label { stmt: inner, .. } = stmt {
        stmt = inner;
    }
    stmt
}
