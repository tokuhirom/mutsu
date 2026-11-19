//! The constant a declaration argument evaluates to, when it needs no run.
//!
//! A literal (`has $.n = 0`) is one. So is a CORE term keyword (`= False`,
//! `= Nil`) in a compunit that imported through a `sub EXPORT` hook: the
//! parser defers such a keyword to `OpCode::GetShadowableTerm` because the
//! hook may install the name (#9047), so the default compiles to a chunk.
//! Unless a hook did install it, that chunk can only push the folded constant.
//! Treating it as the literal it folds to keeps the constructor from running
//! the chunk and, in the constructor lane, from collapsing the caller's env
//! for it (#16897: `Token.new` in MCP::Server::Tool::Web's tokenizer, once
//! per token).

use super::*;

impl Interpreter {
    /// `arg`'s value when evaluating it could not observe anything: a literal,
    /// or a term keyword no `sub EXPORT` hook installed. `None` when it has to
    /// run.
    ///
    /// The hook-installed names (`ModuleState::export_term_override_names`)
    /// are only ever added to, so a `None` here never turns into a stale
    /// `Some` later in the run; a name installed after this answer was given
    /// is read by the next call.
    // Cost: O(1), one set probe for a term keyword.
    pub(crate) fn decl_arg_constant<'a>(
        &self,
        arg: &'a crate::opcode::DeclTraitArg,
    ) -> Option<&'a Value> {
        if let Some(value) = arg.literal() {
            return Some(value);
        }
        // A class attribute's default is still kept as its expression (the
        // registration walks the source declaration), any other declaration
        // argument as a compiled chunk.
        let (name, fallback) = match arg {
            crate::opcode::DeclTraitArg::Ast(expr) => match &**expr {
                crate::ast::Expr::ShadowableTermKeyword { name, value } => (*name, value),
                _ => return None,
            },
            crate::opcode::DeclTraitArg::Compiled(chunk) => chunk.shadowable_term()?,
            crate::opcode::DeclTraitArg::Literal(_) => return None,
        };
        (!self.module.export_term_override_names.contains(&name)).then_some(fallback)
    }
}
