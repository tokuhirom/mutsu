//! A closure literal's TRIR body (#8335).
//!
//! `-> $x { ... }` whose body TRIR proves (`trir::compile::compile_block`)
//! keeps that chunk on its `CompiledCode`, and a value call runs it
//! (`Interpreter::try_call_closure_trir`) instead of the untyped closure entry.

use super::*;

impl Compiler {
    /// The TRIR chunk of the single-parameter pointy block `-> $param { body }`,
    /// or `None` when TRIR declines the body.
    ///
    /// The parameter has no `ParamDef` on the untyped side (it binds through
    /// the legacy `params` path, see `compile_expr_lambda`), so the TRIR compile
    /// is handed the one the signature means: an untyped, read-only, mandatory
    /// block parameter.
    pub(super) fn compile_pointy_trir(
        &self,
        param: &str,
        params: &[String],
        body: &[Stmt],
    ) -> Option<std::sync::Arc<crate::trir::TrChunk>> {
        let pd = crate::ast::ParamDef {
            type_capture: None,
            name: param.to_string(),
            default: None,
            multi_invocant: true,
            required: true,
            named: false,
            named_alias: false,
            slurpy: false,
            sigilless: false,
            type_constraint: None,
            literal_value: None,
            sub_signature: None,
            where_constraint: None,
            traits: Vec::new(),
            double_slurpy: false,
            onearg: false,
            optional_marker: false,
            outer_sub_signature: None,
            code_signature: None,
            is_invocant: false,
            shape_constraints: None,
            block_param: true,
            code: Default::default(),
            trait_args: Vec::new(),
        };
        self.compile_block_trir(std::slice::from_ref(&pd), params, body)
    }

    /// The TRIR chunk of a pointy block with the signature `param_defs`, or
    /// `None` when TRIR declines the body.
    pub(super) fn compile_block_trir(
        &self,
        param_defs: &[crate::ast::ParamDef],
        params: &[String],
        body: &[Stmt],
    ) -> Option<std::sync::Arc<crate::trir::TrChunk>> {
        crate::trir::compile::compile_block(param_defs, params, body, self.fatal_pragma_active)
    }
}
