//! The lean shape of a list-assigning declaration, `my ($a, $b) = EXPR`.
//!
//! The parser lowers such a declaration (`rakuast::signature_expand`) to a
//! synthetic block: the targets declared with their defaults, the staging
//! array `@__destructure_tmp__ = __mutsu_list_assign_rhs(EXPR)`, one
//! declaration per target reading its element, and finally the list of the
//! targets as the block's value. Two parts of that cost more than the work
//! they stand for:
//!
//! - the helper call went through the general by-name function dispatch
//!   (~11k instructions) to deitemize one value; a single positional argument
//!   now compiles to [`OpCode::ListAssignRhs`];
//! - in statement position the block's value (`($a, $b)`) was built and then
//!   sunk, which observes nothing: it is a List of variables just declared.
//!   [`Compiler::destructure_result_is_unobservable`] lets the sunk form skip
//!   it.

use super::Compiler;
use crate::ast::{Expr, Stmt};
use crate::opcode::OpCode;
use crate::symbol::Symbol;

impl Compiler {
    /// Compile `__mutsu_list_assign_rhs(arg)` as `arg` followed by
    /// [`OpCode::ListAssignRhs`] when the call has one argument whose value
    /// the general call path would hand the helper unchanged: a call or a
    /// method call (not an accessor-shaped one, whose argument compile marks
    /// a container for `is rw` binding), or an array variable. Returns
    /// `false`, emitting nothing, for every other shape (a slip, a pair
    /// argument, an element or attribute read), which keep the call.
    // Cost: O(1).
    pub(super) fn try_compile_list_assign_rhs_op(&mut self, name: Symbol, args: &[Expr]) -> bool {
        let [arg] = args else {
            return false;
        };
        if !name.with_str(|n| n == "__mutsu_list_assign_rhs")
            || Self::is_named_arg_expr(arg)
            || Self::is_accessor_shaped_arg(arg)
        {
            return false;
        }
        let lean = match arg {
            Expr::Call { name, .. } => !name.with_str(|n| n.starts_with("__mutsu_")),
            Expr::MethodCall { .. } | Expr::ArrayVar(_) => true,
            _ => false,
        };
        if !lean {
            return false;
        }
        self.compile_expr(arg);
        // The call path turns a Pair argument into a positional one; the
        // helper then received (and returned) that positional pair.
        self.code.emit(OpCode::ContainerizePair);
        self.code.emit(OpCode::ListAssignRhs);
        true
    }

    /// Whether `stmt`, the last statement of a list-assigning declaration's
    /// synthetic block (`stmts`), is the block's result list made of plain
    /// reads of the declared variables, so that evaluating and sinking it is
    /// unobservable and the sunk form may omit it.
    // Cost: O(n), n = statements of the block plus the list's elements.
    pub(super) fn destructure_result_is_unobservable(stmts: &[Stmt], stmt: &Stmt) -> bool {
        if !crate::ast::signature_decl::is_destructure_block(stmts)
            || !std::ptr::eq(
                stmts.last().map_or(std::ptr::null(), |s| s as *const Stmt),
                stmt,
            )
        {
            return false;
        }
        let Stmt::Expr(Expr::ArrayLiteral(items)) = stmt else {
            return false;
        };
        items.iter().all(|item| match item {
            Expr::Var(name) => crate::value::attr_twigil_base(name).is_none(),
            Expr::ArrayVar(_) | Expr::HashVar(_) => true,
            _ => false,
        })
    }
}
