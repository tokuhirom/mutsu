//! Compile attribute defaults, constraints and custom trait arguments once.

use super::Compiler;
use crate::ast::Stmt;
use crate::opcode::AttrDeclChunks;

impl Compiler {
    pub(super) fn compile_attr_decl_chunks(&self, stmt: &Stmt) -> AttrDeclChunks {
        let Stmt::HasDecl {
            default,
            where_constraint,
            is_default,
            unknown_traits,
            is_built,
            ..
        } = stmt
        else {
            unreachable!("compile_attr_decl_chunks called on a non-HasDecl statement");
        };
        AttrDeclChunks {
            is_default: is_default.as_ref().map(|e| self.compile_decl_trait_arg(e)),
            default: default.as_ref().map(|expr| {
                if *is_built == Some(crate::ast::BuiltTrait::Bind) {
                    self.compile_bound_attribute_default(expr)
                } else {
                    self.compile_decl_trait_arg(expr)
                }
            }),
            where_constraint: where_constraint
                .as_deref()
                .map(|e| self.compile_decl_trait_arg(e)),
            unknown_traits: Some(
                unknown_traits
                    .iter()
                    .map(|(kind, name, argument)| {
                        (
                            kind.clone(),
                            name.clone(),
                            argument.as_ref().map(|e| self.compile_decl_trait_arg(e)),
                        )
                    })
                    .collect(),
            ),
        }
    }

    /// A bound default returns the location it evaluates to, just as an
    /// `is raw` routine tail does; the constructor decides whether to bind it.
    // Cost: O(n), n = expression size.
    fn compile_bound_attribute_default(
        &self,
        expr: &crate::ast::Expr,
    ) -> crate::opcode::DeclTraitArg {
        if let crate::ast::Expr::Literal(value) = expr {
            return crate::opcode::DeclTraitArg::Literal(value.clone());
        }
        let mut compiler = self.new_decl_chunk_compiler();
        compiler.rw_tail = true;
        crate::opcode::DeclTraitArg::Compiled(Self::compile_decl_chunk(
            compiler,
            &[Stmt::Expr(expr.clone())],
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::opcode::DeclTraitArg;

    #[test]
    fn custom_attribute_arguments_are_compiled_for_classes_and_roles() {
        let (stmts, _) = crate::parser::parse_program(
            "class C { has $.x is index[1 + 2] }; role R { has $.y is index[3] }",
        )
        .unwrap();
        let (code, _) = Compiler::new().compile(&stmts);
        let classes = code
            .class_decl_plans
            .iter()
            .flat_map(|plan| &plan.attr_decls);
        let roles = code
            .role_decl_plans
            .iter()
            .flat_map(|plan| &plan.attr_decls);
        let mut arguments = 0;
        for (_, attribute) in classes.chain(roles) {
            for (_, _, argument) in &attribute.unknown_traits {
                let Some(argument) = argument else { continue };
                assert!(
                    matches!(argument, DeclTraitArg::Compiled(_)),
                    "bracketed trait argument must execute bytecode, got {argument:?}"
                );
                arguments += 1;
            }
        }
        assert!(
            arguments >= 2,
            "both declaration families must carry compiled arguments"
        );
    }
}
