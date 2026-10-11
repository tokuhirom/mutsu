//! `nqp::p6stateinit()`, lowered onto mutsu's own `state` variables
//! (ADR-11505 §2.4).
//!
//! In Rakudo the op reads a flag MoarVM sets on a frame when the frame's code
//! declares `state` variables and this is the first run of that closure clone:
//! the moment its `state` variables get initialised. mutsu keeps no such
//! frame flag; it initialises each `state` variable on the first run of its
//! declaration, per closure clone. So the op is lowered to a hidden pair of
//! variables in the frame's body, prepended once per frame that reads it:
//!
//! ```text
//! state $__P6STATEINIT_SEEN__;
//! my $__P6STATEINIT__ = $__P6STATEINIT_SEEN__ ?? 0 !! ($__P6STATEINIT_SEEN__ = 1);
//! ```
//!
//! and each `nqp::p6stateinit()` of that frame reads `$__P6STATEINIT__`. The
//! hidden `state` variable is cloned and initialised exactly as the frame's
//! own `state` variables are, so the answer agrees with them by construction.
//!
//! Which frame an op belongs to follows Rakudo after its optimizer: a code
//! object (a routine, method, closure or block value, a phaser, a `CATCH`) is
//! a frame; a statement-level body (`if`, `for`, `while`, a bare block, `do`,
//! `try`) is inlined into the enclosing frame unless it declares `state`
//! variables itself, because Rakudo never inlines a block that does. A frame
//! with no `state` declaration answers 0.
//!
//! Run once over a parsed unit, only when its source mentions the op.

use crate::ast::{Expr, Stmt};
use crate::ast_visit::{VisitMut, walk_expr_mut, walk_stmt_mut, walk_stmts_mut};
use crate::symbol::Symbol;
use crate::value::Value;

const OP_NAME: &str = "nqp::p6stateinit";
const SEEN_VAR: &str = "__P6STATEINIT_SEEN__";
const FLAG_VAR: &str = "__P6STATEINIT__";

/// Lower every `nqp::p6stateinit()` of a parsed unit.
// Cost: O(n), n = AST nodes of the unit.
pub(crate) fn lower_program(stmts: &mut Vec<Stmt>) {
    let mut pass = Lower {
        frames: vec![Frame::default()],
    };
    pass.visit_stmts_mut(stmts);
}

#[derive(Default)]
struct Frame {
    /// The frame's body declares `state` variables directly.
    has_state: bool,
    /// An op of this frame read the hidden flag.
    used: bool,
}

struct Lower {
    frames: Vec<Frame>,
}

impl Lower {
    fn in_code_frame(&mut self, walk: impl FnOnce(&mut Self)) {
        self.frames.push(Frame::default());
        walk(self);
        self.frames.pop();
    }
}

/// Whether a statement is a code object of its own, never inlined.
// Cost: O(1).
fn stmt_is_code_frame(stmt: &Stmt) -> bool {
    matches!(
        stmt,
        Stmt::SubDecl { .. }
            | Stmt::TokenDecl { .. }
            | Stmt::RuleDecl { .. }
            | Stmt::ProtoToken { .. }
            | Stmt::ProtoDecl { .. }
            | Stmt::MethodDecl { .. }
            | Stmt::Package { .. }
            | Stmt::PackageRuntimeBody { .. }
            | Stmt::ClassDecl { .. }
            | Stmt::RoleDecl { .. }
            | Stmt::AugmentClass { .. }
            | Stmt::Phaser { .. }
            | Stmt::Whenever { .. }
            | Stmt::Catch(..)
            | Stmt::Control(..)
    )
}

/// Whether an expression is a code object of its own, never inlined.
// Cost: O(1).
fn expr_is_code_frame(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Block(..)
            | Expr::AnonSub { .. }
            | Expr::AnonSubParams { .. }
            | Expr::Lambda { .. }
            | Expr::Gather(..)
            | Expr::PhaserExpr { .. }
    )
}

/// Whether `args` holds only the parser's internal call-site marker.
// Cost: O(a), a = arguments.
fn no_user_args(args: &[Expr]) -> bool {
    args.iter().all(|arg| {
        matches!(arg, Expr::Binary { left, .. }
            if matches!(left.as_ref(), Expr::Literal(v)
                if v.to_string_value() == crate::parser::TEST_CALLSITE_LINE_KEY))
    })
}

fn hidden_decls() -> [Stmt; 2] {
    let decl = |name: &str, expr: Expr, is_state: bool, custom_traits| Stmt::VarDecl {
        name: name.to_string(),
        expr,
        type_constraint: None,
        is_state,
        is_our: false,
        is_dynamic: false,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits,
        where_constraint: None,
    };
    let first_run = Expr::Ternary {
        cond: Box::new(Expr::Var(SEEN_VAR.to_string())),
        then_expr: Box::new(Expr::Literal(Value::int(0))),
        else_expr: Box::new(Expr::Grouped(Box::new(Expr::AssignExpr {
            name: SEEN_VAR.to_string(),
            expr: Box::new(Expr::Literal(Value::int(1))),
            is_bind: false,
        }))),
    };
    [
        decl(SEEN_VAR, Expr::Literal(Value::NIL), true, Vec::new()),
        decl(
            FLAG_VAR,
            first_run,
            false,
            vec![("__has_initializer".to_string(), None)],
        ),
    ]
}

impl VisitMut for Lower {
    fn visit_stmts_mut(&mut self, body: &mut Vec<Stmt>) {
        let has_state = body
            .iter()
            .any(|stmt| matches!(stmt, Stmt::VarDecl { is_state: true, .. }));
        if !has_state {
            walk_stmts_mut(self, body);
            return;
        }
        self.frames.push(Frame {
            has_state: true,
            used: false,
        });
        walk_stmts_mut(self, body);
        let frame = self.frames.pop().unwrap_or_default();
        if frame.used {
            body.splice(0..0, hidden_decls());
        }
    }

    fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
        if stmt_is_code_frame(stmt) {
            self.in_code_frame(|pass| walk_stmt_mut(pass, stmt));
        } else {
            walk_stmt_mut(self, stmt);
        }
    }

    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        if let Expr::Call { name, args, .. } = expr
            && *name == Symbol::intern(OP_NAME)
            && no_user_args(args)
        {
            let frame = self.frames.last_mut();
            *expr = match frame {
                Some(frame) if frame.has_state => {
                    frame.used = true;
                    Expr::Var(FLAG_VAR.to_string())
                }
                _ => Expr::Literal(Value::int(0)),
            };
            return;
        }
        if expr_is_code_frame(expr) {
            self.in_code_frame(|pass| walk_expr_mut(pass, expr));
        } else {
            walk_expr_mut(self, expr);
        }
    }
}
