//! Compile-time questions about a block body, answered by walking the typed
//! AST visitor (ADR-0137). Each one decides what the compiler emits around a
//! block: a `let`/`temp` save frame, a `state` reset, a succeed barrier, a
//! block-local scope, a per-iteration topic scope.
//!
//! Where a scan stops is part of its answer; [`crate::ast::scope_scan`] holds the
//! boundaries the scans share, and every other stop is an explicit hook arm
//! with the reason next to it.

use crate::ast::scope_scan::{
    is_code_object, is_scope_declaration, opens_own_scope, walk_control_header, walk_stmt_own_scope,
};
use crate::ast::{AssignOp, Expr, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_stmt, walk_stmts};
use crate::regex_tree::RegexNode;

/// Finds a `let`/`temp` save whose resolution belongs to the scanned block.
///
/// Nested bare blocks, `if` branches, `given`/`when` bodies, `do {}` and
/// `try {}` are entered: their saves are resolved no later than at this
/// block's own exit, so this block needs the save frame. Without one nothing
/// resolves the save at all and the speculative value becomes permanent
/// (GH-7645).
struct LetScan {
    /// Count only a real `let` (not `temp`): whether the frame needs the
    /// block's value to decide success.
    real_only: bool,
    found: bool,
}

impl<'ast> Visit<'ast> for LetScan {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.found {
            return;
        }
        match stmt {
            Stmt::Let { is_temp, .. } if !self.real_only || !*is_temp => self.found = true,
            Stmt::TempMethodAssign { .. } if !self.real_only => self.found = true,
            // A loop body owns a save frame per iteration
            // (`Compiler::loop_body_let_frame`); only the loop's header
            // runs in this block.
            // A statement-modifier `for`/`while` opens no block, so its saves
            // resolve at THIS block's exit.
            Stmt::For {
                is_statement_modifier: true,
                ..
            }
            | Stmt::While {
                is_statement_modifier: true,
                ..
            } => walk_stmt(self, stmt),
            Stmt::For { .. } | Stmt::While { .. } | Stmt::Loop { .. } | Stmt::Whenever { .. } => {
                walk_control_header(self, stmt)
            }
            // A routine, package or phaser body resolves its own saves.
            Stmt::Phaser { .. } | Stmt::DocPhaser(_) => {}
            s if is_scope_declaration(s) => {}
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.found {
            return;
        }
        match expr {
            // A code object, a lazy `gather` and a once-only or phaser value
            // run their body under a frame of their own.
            e if is_code_object(e) => {}
            Expr::Gather(_) | Expr::Once { .. } | Expr::PhaserExpr { .. } => {}
            // `undefine temp $var`: the compiler expands it to a save plus an
            // assignment, so the block needs the save frame.
            Expr::Call { name, args, .. }
                if !self.real_only
                    && name.resolve() == "undefine"
                    && args.len() == 1
                    && matches!(&args[0], Expr::Call { name: inner, .. } if inner.resolve() == "temp") =>
            {
                self.found = true
            }
            _ => walk_expr(self, expr),
        }
    }

    // A regex code block runs inside the matcher, as a closure of its own.
    fn visit_regex_node(&mut self, _node: &'ast RegexNode) {}
}

/// Whether `stmts` holds a `let`/`temp` this block's save frame must resolve
/// (`real_only`: a real `let`, not just a `temp`). See [`LetScan`].
// Cost: O(n), n = size of `stmts` up to the first nested code object.
pub(super) fn has_let(stmts: &[Stmt], real_only: bool) -> bool {
    let mut scan = LetScan {
        real_only,
        found: false,
    };
    walk_stmts(&mut scan, stmts);
    scan.found
}

/// Finds a `state` declaration at the scanned block's own level. A `state`
/// in a nested block or closure belongs to that construct's clone and is reset
/// at its entry.
#[derive(Default)]
struct StateScan {
    found: bool,
}

impl<'ast> Visit<'ast> for StateScan {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.found {
            return;
        }
        match stmt {
            Stmt::VarDecl { is_state: true, .. } => self.found = true,
            _ => walk_stmt_own_scope(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if !self.found && !opens_own_scope(expr) {
            walk_expr(self, expr);
        }
    }

    // A regex code block is a closure of its own.
    fn visit_regex_node(&mut self, _node: &'ast RegexNode) {}
}

/// Whether `stmts` declares a `state` variable at its own block level.
// Cost: O(n), n = size of the part of `stmts` in the block's own scope.
pub(super) fn stmts_declare_state(stmts: &[Stmt]) -> bool {
    let mut scan = StateScan::default();
    walk_stmts(&mut scan, stmts);
    scan.found
}

/// Whether `expr` declares a `state` variable at its own block level.
// Cost: O(n), n = size of the part of `expr` in the block's own scope.
pub(super) fn expr_declares_state(expr: &Expr) -> bool {
    let mut scan = StateScan::default();
    scan.visit_expr(expr);
    scan.found
}

/// Finds a `when`/`default` (or `do when`/`do default`) whose succeed unwinds
/// to the scanned block, stopping at anything that introduces its own scope
/// or absorbs a succeed unconditionally (a nested block, a loop or `if` body,
/// a closure, `do {}`, `gather`, `try`).
#[derive(Default)]
struct WhenScan {
    found: bool,
}

impl<'ast> Visit<'ast> for WhenScan {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.found {
            return;
        }
        match stmt {
            Stmt::When { .. } | Stmt::Default(_) => self.found = true,
            // A topicalizer absorbs a succeed itself, and a loop body catches
            // one per iteration — statement modifier or not.
            Stmt::Given { .. } | Stmt::While { .. } | Stmt::For { .. } => {
                walk_control_header(self, stmt)
            }
            _ => walk_stmt_own_scope(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if !self.found && !opens_own_scope(expr) {
            walk_expr(self, expr);
        }
    }

    // A regex code block is a closure of its own.
    fn visit_regex_node(&mut self, _node: &'ast RegexNode) {}
}

/// Whether a `when`/`default` in `stmts` can reach this block's own succeed
/// barrier. See [`WhenScan`].
// Cost: O(n), n = size of the part of `stmts` in the block's own scope.
pub(super) fn reaches_when(stmts: &[Stmt]) -> bool {
    let mut scan = WhenScan::default();
    walk_stmts(&mut scan, stmts);
    scan.found
}

/// Finds a `$_ :=` rebind anywhere in a loop body, nested blocks included,
/// but not inside a closure or routine (which binds its own `$_`).
#[derive(Default)]
struct TopicRebindScan {
    found: bool,
}

impl<'ast> Visit<'ast> for TopicRebindScan {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.found {
            return;
        }
        match stmt {
            Stmt::Assign {
                name,
                op: AssignOp::Bind,
                ..
            } if name == "_" => self.found = true,
            // A routine, package or phaser body has a `$_` of its own.
            Stmt::Phaser { .. } | Stmt::DocPhaser(_) => {}
            s if is_scope_declaration(s) => {}
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if self.found {
            return;
        }
        match expr {
            Expr::AssignExpr {
                name,
                is_bind: true,
                ..
            } if name == "_" => self.found = true,
            // A code object and a lazy `gather` bind their own `$_`.
            e if is_code_object(e) => {}
            Expr::Gather(_) => {}
            _ => walk_expr(self, expr),
        }
    }

    // A regex code block is a closure of its own.
    fn visit_regex_node(&mut self, _node: &'ast RegexNode) {}
}

/// Whether `stmts` rebinds the topic (`$_ := ...`). See [`TopicRebindScan`].
// Cost: O(n), n = size of `stmts` outside nested code objects.
pub(super) fn rebinds_topic(stmts: &[Stmt]) -> bool {
    let mut scan = TopicRebindScan::default();
    walk_stmts(&mut scan, stmts);
    scan.found
}

/// Finds a plain lexical `my` declared in the scanned branch's own scope — at
/// statement level or inside an expression (`foo(my $x = 5)`) — or a lexically
/// scoped type (`my class`, `my package`, `my role`). `state`, `our` and
/// dynamic declarations are not plain lexical shadows.
#[derive(Default)]
struct BlockLocalDeclScan {
    found: bool,
    /// Look for the lexically scoped types only, not the `my` variables.
    types_only: bool,
}

impl<'ast> Visit<'ast> for BlockLocalDeclScan {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.found {
            return;
        }
        match stmt {
            Stmt::VarDecl {
                is_state: false,
                is_our: false,
                is_dynamic: false,
                ..
            } if !self.types_only => self.found = true,
            // A lexical type name is bound in the declaring scope's env just
            // like a `my` variable, so a branch that declares one owes the same
            // scope exit (#10594).
            Stmt::ClassDecl {
                is_lexical: true, ..
            }
            | Stmt::Package { is_my: true, .. } => self.found = true,
            Stmt::RoleDecl { custom_traits, .. }
                if custom_traits.iter().any(|(t, _)| t == "__my_scoped") =>
            {
                self.found = true
            }
            _ => walk_stmt_own_scope(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if !self.found && !opens_own_scope(expr) {
            walk_expr(self, expr);
        }
    }

    // A regex code block is a closure of its own.
    fn visit_regex_node(&mut self, _node: &'ast RegexNode) {}
}

/// Whether a branch body declares a block-local `my` (or a lexically scoped
/// type, see [`BlockLocalDeclScan`]) in its own scope.
// Cost: O(n), n = size of the part of `stmts` in the branch's own scope.
pub(super) fn declares_block_local(stmts: &[Stmt]) -> bool {
    let mut scan = BlockLocalDeclScan::default();
    walk_stmts(&mut scan, stmts);
    scan.found
}

/// Whether a body declares a lexically scoped type (`my class`, `my package`,
/// `my role`) in its own scope, ignoring `my` variables.
// Cost: O(n), n = size of the part of `stmts` in the body's own scope.
pub(super) fn declares_lexical_type(stmts: &[Stmt]) -> bool {
    let mut scan = BlockLocalDeclScan {
        types_only: true,
        ..Default::default()
    };
    walk_stmts(&mut scan, stmts);
    scan.found
}

/// Collects the `our sub` declarations nested in plain blocks (not at the top
/// level, which `hoist_sub_decls` registers itself).
struct NestedOurSubScan<'ast> {
    depth: usize,
    out: Vec<&'ast Stmt>,
}

impl<'ast> Visit<'ast> for NestedOurSubScan<'ast> {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        match stmt {
            Stmt::SubDecl { custom_traits, .. }
                if self.depth > 0 && custom_traits.iter().any(|(t, _)| t == "__our_scoped") =>
            {
                self.out.push(stmt);
            }
            Stmt::Block(body) | Stmt::SyntheticBlock(body) => {
                self.depth += 1;
                walk_stmts(self, body);
                self.depth -= 1;
            }
            // Only plain block nesting is hoisted: an `our sub` inside a
            // routine, class, loop or branch body closes over that body's
            // frame, so registering it at the unit head would be wrong.
            _ => {}
        }
    }
}

/// The `our sub` declarations nested in plain blocks of `stmts`, for early
/// registration.
// Cost: O(n), n = statements reachable through plain block nesting.
pub(super) fn nested_our_subs(stmts: &[Stmt]) -> Vec<&Stmt> {
    let mut scan = NestedOurSubScan {
        depth: 0,
        out: Vec::new(),
    };
    walk_stmts(&mut scan, stmts);
    scan.out
}

/// Collects `constant Name = Type` aliases anywhere in the unit.
#[derive(Default)]
struct TypeAliasScan {
    out: Vec<(String, String)>,
}

impl<'ast> Visit<'ast> for TypeAliasScan {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        // A user subset spelled like a native type (`subset int8 of Int ...`)
        // shadows it: recorded as a self-alias, which
        // `native_default_expr_for_constraint` reads as "not native here".
        if let Stmt::SubsetDecl { name, .. } = stmt {
            let n = name.resolve();
            if crate::runtime::native_types::is_native_int_type(&n)
                || matches!(n.as_str(), "num" | "num32" | "num64" | "str")
            {
                self.out.push((n.clone(), n));
            }
        }
        if let Stmt::VarDecl {
            name,
            expr: Expr::BareWord(target),
            custom_traits,
            ..
        } = stmt
            && custom_traits.iter().any(|(t, _)| t == "__constant")
            && !name.starts_with(['$', '@', '%', '&'])
        {
            self.out.push((name.clone(), target.clone()));
        }
        walk_stmt(self, stmt);
    }
}

/// Every sigilless `constant Name = Type` alias declared in `stmts`, at any
/// depth.
// Cost: O(n), n = size of `stmts`.
pub(super) fn type_aliases(stmts: &[Stmt]) -> Vec<(String, String)> {
    let mut scan = TypeAliasScan::default();
    walk_stmts(&mut scan, stmts);
    scan.out
}
