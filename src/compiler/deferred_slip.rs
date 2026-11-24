//! `|EXPR` in an argument or list, flattened when the capture or list is
//! built (#19135).
//!
//! Rakudo flattens a slip when it assembles the capture, after every argument
//! has been evaluated: `f(|@p, :t(@p.pop))` binds `[1 2]` and `3` for
//! `@p = 1, 2, 3`, and `(|@p, @p.shift)` is `(2 3 1)`. Emitting `MakeSlip`
//! where the slip is written copied the elements before the later arguments
//! ran. When a later argument can have an effect, the slip's operand is
//! compiled alone -- the argument stack then holds the container itself, which
//! shares its storage with the variable -- and `MakeSlipAt` turns it into the
//! slip in place once the last argument is on the stack.

use super::Compiler;
use crate::ast::Expr;
use crate::opcode::OpCode;
use crate::token_kind::TokenKind;

impl Compiler {
    /// For each of `args`, whether it is a `|EXPR` slip to flatten after the
    /// arguments that follow it: some later argument is not a plain variable,
    /// literal or word, whose evaluation changes nothing. Empty when none is.
    // Cost: O(a), a = arguments.
    pub(super) fn deferred_slips(args: &[Expr]) -> Vec<bool> {
        let mut deferred = vec![false; args.len()];
        let mut effect_after = false;
        for (i, arg) in args.iter().enumerate().rev() {
            if effect_after && slip_operand(arg).is_some() {
                deferred[i] = true;
            }
            effect_after |= !Self::effect_free_arg(arg);
        }
        if deferred.contains(&true) {
            deferred
        } else {
            Vec::new()
        }
    }

    /// Whether evaluating `arg` changes nothing: a plain term, a pair of
    /// plain terms, or the slip of one.
    // Cost: O(1).
    fn effect_free_arg(arg: &Expr) -> bool {
        match arg {
            Expr::Binary {
                op: TokenKind::FatArrow,
                left,
                right,
                ..
            } => plain_term(left) && plain_term(right),
            Expr::Unary {
                op: TokenKind::Pipe,
                expr,
                ..
            } => plain_term(expr),
            _ => plain_term(arg),
        }
    }

    /// Compile the argument `deferred` marks as a slip flattened later: only
    /// its operand. `false` when it is not one (the caller compiles it as
    /// usual).
    // Cost: O(1) plus compiling the operand.
    pub(super) fn compile_deferred_slip(
        &mut self,
        deferred: &[bool],
        i: usize,
        arg: &Expr,
    ) -> bool {
        let Some(operand) = deferred
            .get(i)
            .copied()
            .unwrap_or(false)
            .then(|| slip_operand(arg))
            .flatten()
        else {
            return false;
        };
        self.compile_expr(operand);
        true
    }

    /// After the last of the arguments is on the stack, flatten each deferred
    /// slip in its place.
    // Cost: O(a), a = arguments.
    pub(super) fn emit_deferred_slips(&mut self, deferred: &[bool]) {
        let last = deferred.len().saturating_sub(1);
        for (i, _) in deferred.iter().enumerate().filter(|(_, d)| **d) {
            self.code.emit(OpCode::MakeSlipAt((last - i) as u32));
        }
    }
}

/// A variable, literal or word: reading it changes nothing.
// Cost: O(1).
fn plain_term(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Literal(_)
            | Expr::Var(_)
            | Expr::ArrayVar(_)
            | Expr::HashVar(_)
            | Expr::CodeVar(_)
            | Expr::BareWord(_)
    )
}

/// The operand of a `|EXPR` argument.
// Cost: O(1).
fn slip_operand(arg: &Expr) -> Option<&Expr> {
    match arg {
        Expr::Unary {
            op: TokenKind::Pipe,
            expr,
            ..
        } => Some(expr),
        _ => None,
    }
}
