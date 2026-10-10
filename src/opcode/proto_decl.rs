//! Compiled proto declaration plan construction.
use super::*;

impl CompiledCode {
    // Cost: O(p + b + t), p = signature size, b = body size, t = traits.
    pub(crate) fn add_proto_decl_plan(
        &mut self,
        stmt: &Stmt,
        trait_args: Vec<Option<DeclTraitArg>>,
    ) -> u32 {
        let Stmt::ProtoDecl {
            name,
            params,
            param_defs,
            return_type,
            body,
            is_export,
            export_tags,
            custom_traits,
            trait_args: _,
            is_method,
            is_our,
        } = stmt
        else {
            panic!("add_proto_decl_plan expects ProtoDecl");
        };
        let plan_idx = self.proto_decl_plans.len() as u32;
        self.proto_decl_plans.push(CompiledProtoDeclPlan {
            name: *name,
            params: params.clone(),
            param_defs: param_defs.clone(),
            return_type: return_type.clone(),
            is_export: *is_export,
            export_tags: export_tags.clone(),
            custom_traits: custom_traits.clone(),
            trait_args,
            is_method: *is_method,
            is_our: *is_our,
            legacy_body: body.clone(),
            compiled_routine_key: None,
        });
        let idx = self.decl_plans.len() as u32;
        self.decl_plans.push(CompiledDeclPlanRef::Proto(plan_idx));
        idx
    }
}
