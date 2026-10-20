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
            ..
        } = stmt
        else {
            unreachable!("compile_attr_decl_chunks called on a non-HasDecl statement");
        };
        AttrDeclChunks {
            is_default: is_default.as_ref().map(|e| self.compile_decl_trait_arg(e)),
            default: default.as_ref().map(|e| self.compile_decl_trait_arg(e)),
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
