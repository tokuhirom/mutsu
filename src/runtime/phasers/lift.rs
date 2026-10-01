//! The two tree walks of the phaser reordering, on the mutable AST visitor
//! (ADR-10499):
//!
//! - [`lift_phasers`] takes the `BEGIN`/`CHECK`/`INIT` phasers out of the
//!   children of one statement list, so they run with that level's phasers;
//! - [`recurse_into_stmt`] reorders every statement list below a statement.
//!
//! Both reach every child. A `CHECK`/`INIT` runs once, at check/init time,
//! wherever it is written (rakudo runs one in a method body, a phaser body, a
//! parameter default or any operand before the mainline), so the lift
//! descends everywhere except into the bodies that are reordered as a level
//! of their own (types and packages) and into the BEGIN/CHECK/INIT phasers
//! the level itself reorders.

use super::{next_temp_name, reorder_recursive};
use crate::ast::{AssignOp, Expr, PhaserKind, Stmt};
use crate::ast_visit::{VisitMut, walk_expr_mut, walk_stmt_mut, walk_stmts_mut};

/// Lift BEGIN/INIT/CHECK phasers from the children of `stmts` (the current
/// level) into the three buckets. A statement-level BEGIN of the current
/// level, or a BEGIN that is a current-level statement's whole expression,
/// stays in source order so the compiler has already recorded preceding
/// constants; one in a child closure body is lifted.
// Cost: O(n), n = size of `stmts`' subtree.
pub(super) fn lift_phasers(
    stmts: &mut [Stmt],
    begin: &mut Vec<Stmt>,
    check: &mut Vec<Stmt>,
    init: &mut Vec<Stmt>,
) {
    let mut lifter = Lifter {
        begin,
        check,
        init,
        closure: false,
        begin_ok: false,
    };
    for stmt in stmts {
        lifter.visit_stmt_mut(stmt);
    }
}

/// Reorder every statement list below `stmt` as a level of its own.
// Cost: O(n), n = size of `stmt`'s subtree.
pub(super) fn recurse_into_stmt(stmt: &mut Stmt) {
    Reorder.visit_stmt_mut(stmt);
}

struct Reorder;

impl VisitMut for Reorder {
    fn visit_stmts_mut(&mut self, body: &mut Vec<Stmt>) {
        reorder_recursive(body, false);
    }

    fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
        // A statement-form loop phaser is marked as `[SyntheticBlock([stmt])]`
        // (see phaser_stmt): it shares the enclosing block's lexical scope,
        // and `expand_loop_phasers` reads the marker to splice it scope-less.
        // Flattening the marker here would silently downgrade it to the
        // scoped block form, so reorder the inner statements instead.
        if let Stmt::Phaser {
            kind: PhaserKind::First | PhaserKind::Next | PhaserKind::Last,
            body,
            ..
        } = stmt
            && let [Stmt::SyntheticBlock(inner)] = body.as_mut_slice()
        {
            reorder_recursive(inner, false);
            return;
        }
        walk_stmt_mut(self, stmt);
    }
}

struct Lifter<'a> {
    begin: &'a mut Vec<Stmt>,
    check: &'a mut Vec<Stmt>,
    init: &'a mut Vec<Stmt>,
    /// Inside a child closure body rather than at the current level.
    closure: bool,
    /// A `BEGIN` expression at this position is lifted: false only for the
    /// whole expression of a current-level statement.
    begin_ok: bool,
}

impl Lifter<'_> {
    fn in_mode(&mut self, closure: bool, f: impl FnOnce(&mut Self)) {
        let saved = (self.closure, self.begin_ok);
        self.closure = closure;
        self.begin_ok = closure;
        f(self);
        (self.closure, self.begin_ok) = saved;
    }

    /// Replace the phaser expression `expr` with a temp variable, and push the
    /// temp's declaration and the assignment of the phaser's value to its
    /// bucket.
    fn lift_phaser_expr(&mut self, expr: &mut Expr) {
        let temp_name = next_temp_name();
        let old = std::mem::replace(expr, Expr::Var(temp_name.clone()));
        let Expr::PhaserExpr { kind, body } = old else {
            unreachable!("only a phaser expression is lifted")
        };
        let bucket = match kind {
            PhaserKind::Begin => &mut *self.begin,
            PhaserKind::Check => &mut *self.check,
            PhaserKind::Init => &mut *self.init,
            _ => unreachable!("only BEGIN/CHECK/INIT are lifted"),
        };
        push_temp_phaser(bucket, temp_name, phaser_value(body, kind));
    }
}

impl VisitMut for Lifter<'_> {
    // TODO: lift from a parameter default too, as rakudo does. A default is
    // compiled into a standalone chunk that resolves names through the env
    // (ADR-0133), where the lifted phaser's temp, a level-local slot, is not
    // visible, so the default would read `Any`. See #10551.
    fn visit_param_mut(&mut self, _param: &mut crate::ast::ParamDef) {}

    // TODO: lift from a regex code block too, as rakudo does. The regex a
    // match runs is not always this tree: a code block that closes over a
    // lexical runs from the copy the literal's value carries (and its source
    // text), so lifting from the tree would run the phaser twice. See #10550.
    fn visit_regex_node_mut(&mut self, _node: &mut crate::regex_tree::RegexNode) {}

    /// Every statement list reached below the current level is a child
    /// closure body: its statement-level CHECK/INIT phasers are extracted.
    fn visit_stmts_mut(&mut self, body: &mut Vec<Stmt>) {
        self.in_mode(true, |l| {
            extract_phasers_from_stmts(body, l.check, l.init);
            walk_stmts_mut(l, body);
        });
    }

    fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
        match stmt {
            // Type and package bodies are reordered as a level of their own
            // (`recurse_into_stmt`), which lifts their phasers there.
            Stmt::ClassDecl { .. }
            | Stmt::RoleDecl { .. }
            | Stmt::Package { .. }
            | Stmt::PackageRuntimeBody { .. }
            | Stmt::AugmentClass { .. } => {}
            // `source_regex` is a copy of the regex the body's literal value
            // carries and runs; lifting a phaser out of the copy would leave
            // the running one in place, so the phaser would run twice.
            Stmt::TokenDecl { .. } | Stmt::RuleDecl { .. } => {}
            // The level's own BEGIN/CHECK/INIT phasers are reordered by
            // `reorder_at_level`; their bodies are a level of their own.
            Stmt::Phaser {
                kind: PhaserKind::Begin | PhaserKind::Check | PhaserKind::Init,
                ..
            } => {}
            // A bare block at the current level is transparent when it
            // declares no variables: its phasers lift to this level and its
            // statements follow the current-level rules. A block that
            // declares variables is a scope of its own; it is reordered as
            // its own level.
            Stmt::Block(body) if !self.closure => {
                if !body.iter().any(super::stmt_declares_var) {
                    extract_phasers_from_stmts(body, self.check, self.init);
                    walk_stmts_mut(self, body);
                }
            }
            _ => {
                if !self.closure {
                    self.begin_ok = false;
                }
                walk_stmt_mut(self, stmt);
            }
        }
    }

    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        if let Expr::PhaserExpr { kind, .. } = expr
            && (matches!(kind, PhaserKind::Check | PhaserKind::Init)
                || (self.begin_ok && *kind == PhaserKind::Begin))
        {
            self.lift_phaser_expr(expr);
            return;
        }
        match expr {
            // Parentheses group, so a phaser written *inside* a parenthesized
            // expression lifts just as it would unparenthesized -- that is
            // what makes `(gather for ... { INIT take ... })` run its `INIT`
            // at initialisation time rather than per-iteration. A phaser that
            // IS the parenthesized expression is left alone: `is (BEGIN A +
            // 1), 4` evaluates in place, so it still sees the `constant A`
            // declared above it.
            Expr::Grouped(inner) if matches!(inner.as_ref(), Expr::PhaserExpr { .. }) => {}
            // `target`/`rhs` are a model-layer copy of the `expanded` form the
            // compiler runs (RakuAST); lifting from both would run the phaser
            // twice.
            // (`expanded` is an assignment, never itself a phaser.)
            Expr::CompoundAssign { expanded, .. } => {
                let saved = self.begin_ok;
                self.begin_ok = true;
                walk_expr_mut(self, expanded);
                self.begin_ok = saved;
            }
            // A `do` statement (e.g. a string interpolation block) is a simple
            // expression wrapper: a BEGIN in its block is extracted too, and
            // the statement follows the current-level rules.
            Expr::DoStmt(inner) => {
                if let Stmt::Block(body) = inner.as_mut() {
                    extract_begin_from_stmts(body, self.begin);
                }
                self.in_mode(false, |l| l.visit_stmt_mut(inner));
            }
            _ => {
                let saved = self.begin_ok;
                self.begin_ok = true;
                walk_expr_mut(self, expr);
                self.begin_ok = saved;
            }
        }
    }
}

/// The value of a lifted phaser: its body as a `do` block. A CHECK's block
/// carries a sentinel label, so the compiler emits CheckPhaserStart /
/// CheckPhaserEnd and its errors are wrapped in X::Comp::BeginTime.
fn phaser_value(body: Vec<Stmt>, kind: PhaserKind) -> Expr {
    let label = (kind == PhaserKind::Check).then(|| "__mutsu_check_phaser__".to_string());
    // The phaser body's braces are real, but this node is only the vehicle of
    // the `my $tmp; $tmp = do{BODY}` hoist -- the save resolution measured
    // against Rakudo already matches without claiming block identity here
    // (GH-7635).
    Expr::DoBlock {
        body,
        label,
        origin: crate::ast::DoBlockOrigin::Desugar,
    }
}

/// Push `my $temp; $temp = VALUE` for a lifted phaser to `bucket`.
fn push_temp_phaser(bucket: &mut Vec<Stmt>, temp_name: String, value: Expr) {
    bucket.push(Stmt::VarDecl {
        name: temp_name.clone(),
        expr: Expr::Literal(crate::value::Value::NIL),
        type_constraint: None,
        is_state: false,
        is_our: false,
        is_dynamic: false,
        is_export: false,
        export_tags: vec![],
        custom_traits: vec![],
        where_constraint: None,
    });
    bucket.push(Stmt::Assign {
        name: temp_name,
        expr: value,
        op: AssignOp::Assign,
        target_is_sigilless: false,
    });
}

/// Extract INIT/CHECK statement-level phasers from a stmt list.
/// Replaces extracted phasers with a temp variable expression so the phaser's
/// return value is available in expression context (e.g. string interpolation).
///
/// BEGIN is NOT extracted here because transparent blocks may contain local
/// sub declarations that the BEGIN body references (see begin.t test case
/// with `sub my-uc`). BEGIN is only extracted from `do` statement blocks via
/// [`extract_begin_from_stmts`] and as a phaser expression.
fn extract_phasers_from_stmts(stmts: &mut [Stmt], check: &mut Vec<Stmt>, init: &mut Vec<Stmt>) {
    for stmt in stmts.iter_mut() {
        let Stmt::Phaser {
            kind: kind @ (PhaserKind::Check | PhaserKind::Init),
            ..
        } = stmt
        else {
            continue;
        };
        let kind = kind.clone();
        let temp_name = next_temp_name();
        let old = std::mem::replace(stmt, Stmt::Expr(Expr::Var(temp_name.clone())));
        let Stmt::Phaser { body, .. } = old else {
            unreachable!()
        };
        let bucket = if kind == PhaserKind::Check {
            &mut *check
        } else {
            &mut *init
        };
        push_temp_phaser(bucket, temp_name, phaser_value(body, kind));
    }
}

/// Extract BEGIN statement-level phasers from the block of a `do` statement.
/// This is separate from [`extract_phasers_from_stmts`] because BEGIN should
/// only be extracted from such blocks (e.g. string interpolation), not from
/// general blocks which may contain local sub declarations needed by the
/// BEGIN body.
pub(super) fn extract_begin_from_stmts(stmts: &mut [Stmt], begin: &mut Vec<Stmt>) {
    for stmt in stmts.iter_mut() {
        if !matches!(
            stmt,
            Stmt::Phaser {
                kind: PhaserKind::Begin,
                ..
            }
        ) {
            continue;
        }
        let temp_name = next_temp_name();
        let old = std::mem::replace(stmt, Stmt::Expr(Expr::Var(temp_name.clone())));
        let Stmt::Phaser { body, .. } = old else {
            unreachable!()
        };
        push_temp_phaser(begin, temp_name, Expr::desugar_block(body));
    }
}
