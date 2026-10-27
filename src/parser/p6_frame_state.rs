//! The Rakudo `p6*` ops that read the running frame, placed at parse time
//! (ADR-11505 §2.4, §2.5): `nqp::p6stateinit()` and `nqp::p6takefirstflag()`.
//!
//! Both ask about "the frame the op runs in", and which frame that is follows
//! Rakudo after its optimizer: a code object (a routine, method, closure or
//! block value, a phaser, a `CATCH`) is a frame; a statement-level body (`if`,
//! `for`, `while`, a bare block, `do`, `try`) is inlined into the enclosing
//! frame unless it declares `state` variables itself, because Rakudo never
//! inlines a block that does. Only the whole tree can say which frame an op is
//! in, so this pass runs once over a parsed unit, and only when its source
//! mentions one of the ops.
//!
//! `nqp::p6stateinit()` reads a flag MoarVM sets on a frame whose code declares
//! `state` variables, on the first run of that closure clone. mutsu keeps no
//! such frame flag; it initialises each `state` variable on the first run of
//! its declaration, per closure clone. So the op is lowered to a hidden pair
//! of variables in the frame's body, prepended once per frame that reads it:
//!
//! ```text
//! state $__P6STATEINIT_SEEN__;
//! my $__P6STATEINIT__ = $__P6STATEINIT_SEEN__ ?? 0 !! ($__P6STATEINIT_SEEN__ = 1);
//! ```
//!
//! and each `nqp::p6stateinit()` of that frame reads `$__P6STATEINIT__`. The
//! hidden `state` variable is cloned and initialised exactly as the frame's
//! own `state` variables are, so the answer agrees with them by construction.
//! A frame with no `state` declaration answers 0.
//!
//! `nqp::p6takefirstflag()` reads and clears the `FIRST` flag of the frame's
//! code object. The op is given that code object as an operand: the routine
//! itself (`&name`) in a named sub, `&?ROUTINE` in a method or a multi, and
//! `&?BLOCK` in any other code object. The mainline has no code object a
//! `p6setfirstflag` could reach, so there it is 0.

use crate::ast::{Expr, Stmt};
use crate::ast_visit::{VisitMut, walk_expr_mut, walk_stmt_mut, walk_stmts_mut};
use crate::symbol::Symbol;
use crate::value::Value;

const STATEINIT_OP: &str = "nqp::p6stateinit";
const TAKEFIRSTFLAG_OP: &str = "nqp::p6takefirstflag";
const SEEN_VAR: &str = "__P6STATEINIT_SEEN__";
const FLAG_VAR: &str = "__P6STATEINIT__";

/// Whether `source` mentions an op this pass lowers.
// Cost: O(n), n = bytes of the source.
pub(crate) fn mentions_op(source: &str) -> bool {
    source.contains("p6stateinit") || source.contains("p6takefirstflag")
}

/// Lower every frame-reading op of a parsed unit.
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
    /// A `p6stateinit` of this frame read the hidden flag.
    used: bool,
    /// The expression that reads the frame's code object; `None` for the
    /// mainline.
    code: Option<Expr>,
    /// A code frame whose body statement list has not been visited yet.
    awaiting_body: bool,
}

struct Lower {
    frames: Vec<Frame>,
}

impl Lower {
    fn in_code_frame(&mut self, code: Expr, walk: impl FnOnce(&mut Self)) {
        self.frames.push(Frame {
            code: Some(code),
            awaiting_body: true,
            ..Frame::default()
        });
        walk(self);
        self.frames.pop();
    }
}

fn code_var(name: &str) -> Expr {
    Expr::CodeVar(name.to_string())
}

/// The expression for the code object a statement declares, when it is a
/// code object of its own (never inlined).
// Cost: O(1).
fn stmt_code_frame(stmt: &Stmt) -> Option<Expr> {
    match stmt {
        Stmt::SubDecl {
            name,
            name_expr: None,
            multi: false,
            ..
        } if !name.as_str().is_empty() => Some(code_var(name.as_str())),
        Stmt::SubDecl { .. } | Stmt::MethodDecl { .. } => Some(code_var("?ROUTINE")),
        Stmt::TokenDecl { .. }
        | Stmt::RuleDecl { .. }
        | Stmt::ProtoToken { .. }
        | Stmt::ProtoDecl { .. }
        | Stmt::Package { .. }
        | Stmt::PackageRuntimeBody { .. }
        | Stmt::ClassDecl { .. }
        | Stmt::RoleDecl { .. }
        | Stmt::AugmentClass { .. }
        | Stmt::Phaser { .. }
        | Stmt::Whenever { .. }
        | Stmt::Catch(..)
        | Stmt::Control(..) => Some(code_var("?BLOCK")),
        _ => None,
    }
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
    let decl = |name: &str, expr: Expr, is_state: bool, has_initializer| Stmt::VarDecl {
        anonymous: false,
        name: name.to_string(),
        expr,
        type_constraint: None,
        is_state,
        is_our: false,
        is_dynamic: false,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits: Vec::new(),
        has_initializer,
        constant: None,
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
        decl(SEEN_VAR, Expr::Literal(Value::NIL), true, false),
        decl(FLAG_VAR, first_run, false, true),
    ]
}

impl VisitMut for Lower {
    fn visit_stmts_mut(&mut self, body: &mut Vec<Stmt>) {
        let has_state = body
            .iter()
            .any(|stmt| matches!(stmt, Stmt::VarDecl { is_state: true, .. }));
        // The body of the code frame just entered is that frame; any other
        // list is a frame of its own only when it declares `state` variables.
        let is_body = self.frames.last().is_some_and(|f| f.awaiting_body);
        if is_body {
            if let Some(frame) = self.frames.last_mut() {
                frame.awaiting_body = false;
                frame.has_state = has_state;
            }
        } else if has_state {
            self.frames.push(Frame {
                has_state: true,
                code: Some(code_var("?BLOCK")),
                ..Frame::default()
            });
        } else {
            walk_stmts_mut(self, body);
            return;
        }
        walk_stmts_mut(self, body);
        let used = self.frames.last().is_some_and(|f| f.used);
        if !is_body {
            self.frames.pop();
        }
        if used {
            body.splice(0..0, hidden_decls());
        }
    }

    fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
        if let Some(code) = stmt_code_frame(stmt) {
            self.in_code_frame(code, |pass| walk_stmt_mut(pass, stmt));
        } else {
            walk_stmt_mut(self, stmt);
        }
    }

    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        if let Expr::Call { name, args, .. } = expr
            && no_user_args(args)
        {
            if *name == Symbol::intern(STATEINIT_OP) {
                *expr = match self.frames.last_mut() {
                    Some(frame) if frame.has_state => {
                        frame.used = true;
                        Expr::Var(FLAG_VAR.to_string())
                    }
                    _ => Expr::Literal(Value::int(0)),
                };
                return;
            }
            if *name == Symbol::intern(TAKEFIRSTFLAG_OP) {
                match self.frames.last().and_then(|f| f.code.clone()) {
                    Some(code) => *args = vec![code],
                    None => *expr = Expr::Literal(Value::int(0)),
                }
                return;
            }
        }
        if expr_is_code_frame(expr) {
            self.in_code_frame(code_var("?BLOCK"), |pass| walk_expr_mut(pass, expr));
        } else {
            walk_expr_mut(self, expr);
        }
    }
}
