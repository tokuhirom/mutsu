//! The codec of `CompiledCode`: every field, in declaration order (see the parent module).

use super::{
    decode_map, decode_serde, decode_set, encode_map, encode_string_set, encode_sym_set,
    encode_u32_set, require_empty,
};
use crate::opcode::CompiledCode;
use crate::precomp_codec::DecodeCtx;
use bincode::de::Decoder;
use bincode::enc::Encoder;
use bincode::error::{DecodeError, EncodeError};
use bincode::serde::Compat;
use bincode::{Decode, Encode};

impl Encode for CompiledCode {
    // Cost: O(n), n = size of the chunk.
    fn encode<E: Encoder>(&self, encoder: &mut E) -> Result<(), EncodeError> {
        let CompiledCode {
            ops,
            op_lines,
            source_file,
            emit_line,
            constants,
            const_index,
            handler_snapshot: _,
            stmt_pool,
            sub_decl_plans,
            class_decl_plans,
            role_decl_plans,
            proto_decl_plans,
            token_decl_plans,
            proto_token_plans,
            decl_plans,
            trir_call_sites,
            reduction_specs,
            locals,
            locals_sym,
            binding_descs,
            pending_declared_constraints,
            state_locals,
            our_locals,
            param_bind_names,
            scalar_bind_locals,
            param_local_slots,
            param_locals,
            lex_scopes,
            closure_compiled_codes,
            compiled_fns,
            atomic_env_sync_locals,
            atomic_target_syms,
            rw_arg_env_sync_syms,
            named_arg_specs,
            closure_escapes,
            is_routine,
            lexically_in_routine,
            method_fatal_pragma,
            succeed_passes_through,
            reads_topic,
            mentions_native_scalar_type_name,
            source_line,
            is_pointy_block,
            pointy_alias_param,
            immutable_topic,
            declarator_doc,
            writes_topic,
            declared_in_routine,
            reads_args_array,
            reads_args_hash,
            has_env_writes,
            may_capture_outer_vars,
            needs_env_sync,
            env_consumer_slots,
            dup_named_locals,
            has_dup_named_locals,
            multi_scope_slots,
            block_scope_protected_slots,
            my_declared_sym,
            dynamic_declared_sym,
            my_declared_enum_sym,
            for_loop_param_syms,
            expr_declared_syms,
            free_var_syms,
            free_var_parent_slots,
            upvalue_parent_slots,
            outer_ref_names,
            free_var_writes,
            forced_free_var_syms,
            forced_free_var_writes,
            free_var_container_writes,
            named_sub_captures,
            lexical_routines,
            amp_shadowed_calls,
            lexical_subtree,
            nested_routine_free_reads,
            bounded_lazy_decl_plans,
            lazy_body_env_sync_slots,
            lazy_decl_reads,
            nested_routine_free_writes,
            nested_sub_written_free,
            needs_cell_named_sub,
            needs_cell_ref_capture_slots,
            container_ref_capture_syms,
            needs_cell_named_sub_free,
            escaping_our_sub_captures,
            needs_cell_escaping_our_sub,
            needs_cell_escaping_our_sub_free,
            escaping_our_env_params,
            captured_mutated_locals,
            needs_cell_locals,
            needs_cell_unvouched_locals,
            needs_cell_unvouched_containers,
            needs_cell_regex,
            type_body_written_lexicals,
            authoritative_free_vars,
            self_capture_decl_locals,
            captures_own_declaration,
            outer_code_var_names,
            shadowed_sigilless_reads,
            unscoped_amp_reads,
            needs_cell_free_vars,
            has_calls,
            has_once,
            uses_callframe,
            uses_samewith,
            needs_reflective_capture,
            uses_dispatcher,
            may_observe_named_slurpy,
            is_supply_block_body,
            eval_context_target_callable_id,
            supply_emitter_sym,
            inherited_owned_lexicals,
            upvalue_syms,
            env_only_decls,
            const_syms,
            local_attr_keys: _,
            attr_sites: _,
            bareword_sites: _,
            method_sites: _,
            type_decl_sites: _,
            local_read_plain: _,
            rebound_slots,
            rebound_free_names,
            outer_captures,
            numeric_operand_names,
            free_var_rebinds,
            free_var_sym_set: _,
            local_sym_set: _,
            capture_hidden_set: _,
            capture_probe_keys: _,
            free_enum_bare_keys: _,
            local_slot_index: _,
            op_scan_index: _,
            stmt_pool_bodies: _,
            stmt_pool_signatures: _,
            jit: _,
        } = self;
        ops.encode(encoder)?;
        op_lines.encode(encoder)?;
        source_file.encode(encoder)?;
        emit_line.encode(encoder)?;
        constants.encode(encoder)?;
        require_empty(const_index.is_empty(), "CompiledCode::const_index")?;
        Compat(stmt_pool).encode(encoder)?;
        sub_decl_plans.encode(encoder)?;
        class_decl_plans.encode(encoder)?;
        role_decl_plans.encode(encoder)?;
        proto_decl_plans.encode(encoder)?;
        token_decl_plans.encode(encoder)?;
        Compat(proto_token_plans).encode(encoder)?;
        decl_plans.encode(encoder)?;
        trir_call_sites.encode(encoder)?;
        reduction_specs.encode(encoder)?;
        locals.encode(encoder)?;
        locals_sym.encode(encoder)?;
        binding_descs.encode(encoder)?;
        require_empty(
            pending_declared_constraints.is_empty(),
            "CompiledCode::pending_declared_constraints",
        )?;
        state_locals.encode(encoder)?;
        our_locals.encode(encoder)?;
        param_bind_names.encode(encoder)?;
        scalar_bind_locals.encode(encoder)?;
        param_local_slots.encode(encoder)?;
        encode_sym_set(param_locals, encoder)?;
        lex_scopes.encode(encoder)?;
        closure_compiled_codes.encode(encoder)?;
        compiled_fns.encode(encoder)?;
        atomic_env_sync_locals.encode(encoder)?;
        encode_sym_set(atomic_target_syms, encoder)?;
        encode_sym_set(rw_arg_env_sync_syms, encoder)?;
        named_arg_specs.encode(encoder)?;
        closure_escapes.encode(encoder)?;
        is_routine.encode(encoder)?;
        lexically_in_routine.encode(encoder)?;
        method_fatal_pragma.encode(encoder)?;
        succeed_passes_through.encode(encoder)?;
        reads_topic.encode(encoder)?;
        mentions_native_scalar_type_name.encode(encoder)?;
        source_line.encode(encoder)?;
        is_pointy_block.encode(encoder)?;
        pointy_alias_param.encode(encoder)?;
        immutable_topic.encode(encoder)?;
        Compat(declarator_doc.as_deref()).encode(encoder)?;
        writes_topic.encode(encoder)?;
        declared_in_routine.encode(encoder)?;
        reads_args_array.encode(encoder)?;
        reads_args_hash.encode(encoder)?;
        has_env_writes.encode(encoder)?;
        may_capture_outer_vars.encode(encoder)?;
        needs_env_sync.encode(encoder)?;
        env_consumer_slots.encode(encoder)?;
        dup_named_locals.encode(encoder)?;
        has_dup_named_locals.encode(encoder)?;
        encode_u32_set(multi_scope_slots, encoder)?;
        encode_map(block_scope_protected_slots, encoder)?;
        encode_sym_set(my_declared_sym, encoder)?;
        encode_sym_set(dynamic_declared_sym, encoder)?;
        encode_sym_set(my_declared_enum_sym, encoder)?;
        encode_sym_set(for_loop_param_syms, encoder)?;
        encode_sym_set(expr_declared_syms, encoder)?;
        free_var_syms.encode(encoder)?;
        free_var_parent_slots.encode(encoder)?;
        upvalue_parent_slots.encode(encoder)?;
        outer_ref_names.encode(encoder)?;
        free_var_writes.encode(encoder)?;
        forced_free_var_syms.encode(encoder)?;
        forced_free_var_writes.encode(encoder)?;
        free_var_container_writes.encode(encoder)?;
        named_sub_captures.encode(encoder)?;
        lexical_routines.encode(encoder)?;
        amp_shadowed_calls.encode(encoder)?;
        lexical_subtree.encode(encoder)?;
        nested_routine_free_reads.encode(encoder)?;
        bounded_lazy_decl_plans.encode(encoder)?;
        lazy_body_env_sync_slots.encode(encoder)?;
        lazy_decl_reads.encode(encoder)?;
        nested_routine_free_writes.encode(encoder)?;
        nested_sub_written_free.encode(encoder)?;
        needs_cell_named_sub.encode(encoder)?;
        needs_cell_ref_capture_slots.encode(encoder)?;
        container_ref_capture_syms.encode(encoder)?;
        needs_cell_named_sub_free.encode(encoder)?;
        escaping_our_sub_captures.encode(encoder)?;
        needs_cell_escaping_our_sub.encode(encoder)?;
        needs_cell_escaping_our_sub_free.encode(encoder)?;
        escaping_our_env_params.encode(encoder)?;
        captured_mutated_locals.encode(encoder)?;
        needs_cell_locals.encode(encoder)?;
        needs_cell_unvouched_locals.encode(encoder)?;
        needs_cell_unvouched_containers.encode(encoder)?;
        needs_cell_regex.encode(encoder)?;
        type_body_written_lexicals.encode(encoder)?;
        authoritative_free_vars.encode(encoder)?;
        self_capture_decl_locals.encode(encoder)?;
        captures_own_declaration.encode(encoder)?;
        encode_string_set(outer_code_var_names, encoder)?;
        shadowed_sigilless_reads.encode(encoder)?;
        unscoped_amp_reads.encode(encoder)?;
        needs_cell_free_vars.encode(encoder)?;
        has_calls.encode(encoder)?;
        has_once.encode(encoder)?;
        uses_callframe.encode(encoder)?;
        uses_samewith.encode(encoder)?;
        needs_reflective_capture.encode(encoder)?;
        uses_dispatcher.encode(encoder)?;
        may_observe_named_slurpy.encode(encoder)?;
        is_supply_block_body.encode(encoder)?;
        eval_context_target_callable_id.encode(encoder)?;
        supply_emitter_sym.encode(encoder)?;
        inherited_owned_lexicals.encode(encoder)?;
        upvalue_syms.encode(encoder)?;
        env_only_decls.encode(encoder)?;
        const_syms.encode(encoder)?;
        rebound_slots.encode(encoder)?;
        rebound_free_names.encode(encoder)?;
        outer_captures.encode(encoder)?;
        numeric_operand_names.encode(encoder)?;
        free_var_rebinds.encode(encoder)?;
        Ok(())
    }
}

impl Decode<DecodeCtx> for CompiledCode {
    // Cost: O(n), n = size of the chunk.
    fn decode<D: Decoder<Context = DecodeCtx>>(decoder: &mut D) -> Result<Self, DecodeError> {
        Ok(CompiledCode {
            ops: Decode::decode(decoder)?,
            op_lines: Decode::decode(decoder)?,
            source_file: Decode::decode(decoder)?,
            emit_line: Decode::decode(decoder)?,
            constants: Decode::decode(decoder)?,
            const_index: Default::default(),
            handler_snapshot: Default::default(),
            stmt_pool: decode_serde(decoder)?,
            sub_decl_plans: Decode::decode(decoder)?,
            class_decl_plans: Decode::decode(decoder)?,
            role_decl_plans: Decode::decode(decoder)?,
            proto_decl_plans: Decode::decode(decoder)?,
            token_decl_plans: Decode::decode(decoder)?,
            proto_token_plans: decode_serde(decoder)?,
            decl_plans: Decode::decode(decoder)?,
            trir_call_sites: Decode::decode(decoder)?,
            reduction_specs: Decode::decode(decoder)?,
            locals: Decode::decode(decoder)?,
            locals_sym: Decode::decode(decoder)?,
            binding_descs: Decode::decode(decoder)?,
            pending_declared_constraints: Default::default(),
            state_locals: Decode::decode(decoder)?,
            our_locals: Decode::decode(decoder)?,
            param_bind_names: Decode::decode(decoder)?,
            scalar_bind_locals: Decode::decode(decoder)?,
            param_local_slots: Decode::decode(decoder)?,
            param_locals: decode_set(decoder)?,
            lex_scopes: Decode::decode(decoder)?,
            closure_compiled_codes: Decode::decode(decoder)?,
            compiled_fns: Decode::decode(decoder)?,
            atomic_env_sync_locals: Decode::decode(decoder)?,
            atomic_target_syms: decode_set(decoder)?,
            rw_arg_env_sync_syms: decode_set(decoder)?,
            named_arg_specs: Decode::decode(decoder)?,
            closure_escapes: Decode::decode(decoder)?,
            is_routine: Decode::decode(decoder)?,
            lexically_in_routine: Decode::decode(decoder)?,
            method_fatal_pragma: Decode::decode(decoder)?,
            succeed_passes_through: Decode::decode(decoder)?,
            reads_topic: Decode::decode(decoder)?,
            mentions_native_scalar_type_name: Decode::decode(decoder)?,
            source_line: Decode::decode(decoder)?,
            is_pointy_block: Decode::decode(decoder)?,
            pointy_alias_param: Decode::decode(decoder)?,
            immutable_topic: Decode::decode(decoder)?,
            declarator_doc: decode_serde::<Option<crate::decl_doc::DeclDoc>, _>(decoder)?
                .map(std::sync::Arc::new),
            writes_topic: Decode::decode(decoder)?,
            declared_in_routine: Decode::decode(decoder)?,
            reads_args_array: Decode::decode(decoder)?,
            reads_args_hash: Decode::decode(decoder)?,
            has_env_writes: Decode::decode(decoder)?,
            may_capture_outer_vars: Decode::decode(decoder)?,
            needs_env_sync: Decode::decode(decoder)?,
            env_consumer_slots: Decode::decode(decoder)?,
            dup_named_locals: Decode::decode(decoder)?,
            has_dup_named_locals: Decode::decode(decoder)?,
            multi_scope_slots: decode_set(decoder)?,
            block_scope_protected_slots: decode_map(decoder)?,
            my_declared_sym: decode_set(decoder)?,
            dynamic_declared_sym: decode_set(decoder)?,
            my_declared_enum_sym: decode_set(decoder)?,
            for_loop_param_syms: decode_set(decoder)?,
            expr_declared_syms: decode_set(decoder)?,
            free_var_syms: Decode::decode(decoder)?,
            free_var_parent_slots: Decode::decode(decoder)?,
            upvalue_parent_slots: Decode::decode(decoder)?,
            outer_ref_names: Decode::decode(decoder)?,
            free_var_writes: Decode::decode(decoder)?,
            forced_free_var_syms: Decode::decode(decoder)?,
            forced_free_var_writes: Decode::decode(decoder)?,
            free_var_container_writes: Decode::decode(decoder)?,
            named_sub_captures: Decode::decode(decoder)?,
            lexical_routines: Decode::decode(decoder)?,
            amp_shadowed_calls: Decode::decode(decoder)?,
            lexical_subtree: Decode::decode(decoder)?,
            nested_routine_free_reads: Decode::decode(decoder)?,
            bounded_lazy_decl_plans: Decode::decode(decoder)?,
            lazy_body_env_sync_slots: Decode::decode(decoder)?,
            lazy_decl_reads: Decode::decode(decoder)?,
            nested_routine_free_writes: Decode::decode(decoder)?,
            nested_sub_written_free: Decode::decode(decoder)?,
            needs_cell_named_sub: Decode::decode(decoder)?,
            needs_cell_ref_capture_slots: Decode::decode(decoder)?,
            container_ref_capture_syms: Decode::decode(decoder)?,
            needs_cell_named_sub_free: Decode::decode(decoder)?,
            escaping_our_sub_captures: Decode::decode(decoder)?,
            needs_cell_escaping_our_sub: Decode::decode(decoder)?,
            needs_cell_escaping_our_sub_free: Decode::decode(decoder)?,
            escaping_our_env_params: Decode::decode(decoder)?,
            captured_mutated_locals: Decode::decode(decoder)?,
            needs_cell_locals: Decode::decode(decoder)?,
            needs_cell_unvouched_locals: Decode::decode(decoder)?,
            needs_cell_unvouched_containers: Decode::decode(decoder)?,
            needs_cell_regex: Decode::decode(decoder)?,
            type_body_written_lexicals: Decode::decode(decoder)?,
            authoritative_free_vars: Decode::decode(decoder)?,
            self_capture_decl_locals: Decode::decode(decoder)?,
            captures_own_declaration: Decode::decode(decoder)?,
            outer_code_var_names: decode_set(decoder)?,
            shadowed_sigilless_reads: Decode::decode(decoder)?,
            unscoped_amp_reads: Decode::decode(decoder)?,
            needs_cell_free_vars: Decode::decode(decoder)?,
            has_calls: Decode::decode(decoder)?,
            has_once: Decode::decode(decoder)?,
            uses_callframe: Decode::decode(decoder)?,
            uses_samewith: Decode::decode(decoder)?,
            needs_reflective_capture: Decode::decode(decoder)?,
            uses_dispatcher: Decode::decode(decoder)?,
            may_observe_named_slurpy: Decode::decode(decoder)?,
            is_supply_block_body: Decode::decode(decoder)?,
            eval_context_target_callable_id: Decode::decode(decoder)?,
            supply_emitter_sym: Decode::decode(decoder)?,
            inherited_owned_lexicals: Decode::decode(decoder)?,
            upvalue_syms: Decode::decode(decoder)?,
            env_only_decls: Decode::decode(decoder)?,
            const_syms: Decode::decode(decoder)?,
            local_attr_keys: Default::default(),
            attr_sites: Default::default(),
            bareword_sites: Default::default(),
            method_sites: Default::default(),
            type_decl_sites: Default::default(),
            local_read_plain: Default::default(),
            rebound_slots: Decode::decode(decoder)?,
            rebound_free_names: Decode::decode(decoder)?,
            outer_captures: Decode::decode(decoder)?,
            numeric_operand_names: Decode::decode(decoder)?,
            free_var_rebinds: Decode::decode(decoder)?,
            free_var_sym_set: Default::default(),
            local_sym_set: Default::default(),
            capture_hidden_set: Default::default(),
            capture_probe_keys: Default::default(),
            free_enum_bare_keys: Default::default(),
            local_slot_index: Default::default(),
            op_scan_index: Default::default(),
            stmt_pool_bodies: Default::default(),
            stmt_pool_signatures: Default::default(),
            jit: Default::default(),
        })
    }
}
bincode::impl_borrow_decode_with_context!(CompiledCode, DecodeCtx);
