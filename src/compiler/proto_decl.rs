//! Proto declaration arguments use the same compiled chunks as other traits.

use super::Compiler;
use crate::ast::Expr;

impl Compiler {
    // Cost: O(t + a), t = custom traits, a = size of their argument expressions.
    pub(super) fn compile_proto_trait_args(
        &self,
        custom_traits: &[String],
        trait_args: &[(String, Option<Expr>)],
    ) -> Vec<Option<crate::opcode::DeclTraitArg>> {
        custom_traits
            .iter()
            .enumerate()
            .map(|(index, name)| {
                trait_args
                    .get(index)
                    .filter(|(candidate, _)| candidate == name)
                    .and_then(|(_, argument)| argument.as_ref())
                    .map(|argument| self.compile_decl_trait_arg(argument))
            })
            .collect()
    }
}
