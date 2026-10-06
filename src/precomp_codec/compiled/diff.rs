//! Field-by-field comparison of two `CompiledCode`s, for the verify mode's
//! diagnostic: which field of which routine differs, not just which byte.

use crate::opcode::{CompiledCode, CompiledFns};

/// The first field that differs between `a` and `b` (compared by `Debug`), as
/// `field: <a> != <b>` with both sides truncated.
// Cost: O(n), n = size of the chunks (each field rendered once per side).
pub(crate) fn first_code_difference(a: &CompiledCode, b: &CompiledCode) -> Option<String> {
    macro_rules! cmp_serde {
        ($($field:ident),* $(,)?) => {
            $(
                let enc = |c: &CompiledCode| crate::precomp_codec::encode(&bincode::serde::Compat(&c.$field)).ok();
                if enc(a) != enc(b) {
                    return Some(format!("{} differs", stringify!($field)));
                }
            )*
        };
    }
    macro_rules! cmp_sorted {
        ($($field:ident),* $(,)?) => {
            $(
                let render = |c: &CompiledCode| {
                    let mut items: Vec<String> = c.$field.iter().map(|e| format!("{e:?}")).collect();
                    items.sort();
                    items
                };
                let (ra, rb) = (render(a), render(b));
                if ra != rb {
                    return Some(format!("{}: {:?} != {:?}", stringify!($field), ra, rb));
                }
            )*
        };
    }
    macro_rules! cmp {
        ($($field:ident),* $(,)?) => {
            $(
                // Compared by encoding (sorted where the field holds hash
                // maps), shown by `Debug`.
                let same = match (crate::precomp_codec::encode(&a.$field), crate::precomp_codec::encode(&b.$field)) {
                    (Ok(x), Ok(y)) => x == y,
                    _ => format!("{:?}", a.$field) == format!("{:?}", b.$field),
                };
                if !same {
                    let (da, db) = (format!("{:?}", a.$field), format!("{:?}", b.$field));
                    let at = da.bytes().zip(db.bytes()).position(|(x, y)| x != y).unwrap_or(0);
                    let lo = at.saturating_sub(80);
                    let hi = |s: &str| (at + 160).min(s.len());
                    return Some(format!(
                        "{}: {} != {}",
                        stringify!($field),
                        da.get(lo..hi(&da)).unwrap_or(&da),
                        db.get(lo..hi(&db)).unwrap_or(&db)
                    ));
                }
            )*
        };
    }
    // Nested chunks are compared field by field too, so the report names the
    // field that differs rather than the first `Debug` divergence (a hash set
    // renders in iteration order).
    if a.closure_compiled_codes.len() != b.closure_compiled_codes.len() {
        return Some("closure_compiled_codes differ in length".to_string());
    }
    for (i, (ca, cb)) in a
        .closure_compiled_codes
        .iter()
        .zip(&b.closure_compiled_codes)
        .enumerate()
    {
        if let Some(diff) = first_code_difference(ca, cb) {
            return Some(format!("closure_compiled_codes[{i}] {diff}"));
        }
    }
    cmp!(
        ops,
        op_lines,
        source_file,
        emit_line,
        constants,
        sub_decl_plans,
        class_decl_plans,
        role_decl_plans,
        proto_decl_plans,
        token_decl_plans,
        decl_plans,
        trir_call_sites,
        reduction_specs,
        locals,
        locals_sym,
        binding_descs,
        state_locals,
        our_locals,
        param_bind_names,
        scalar_bind_locals,
        param_local_slots,
        lex_scopes,
        compiled_fns,
        atomic_env_sync_locals,
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
        free_var_syms,
        free_var_parent_slots,
        upvalue_parent_slots,
        outer_ref_names,
        free_var_writes,
        forced_free_var_syms,
        forced_free_var_writes,
        free_var_container_writes,
        free_var_call_arg_syms,
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
        rebound_slots,
        rebound_free_names,
        outer_captures,
        numeric_operand_names,
        free_var_rebinds,
    );
    cmp_serde!(stmt_pool, proto_token_plans,);
    if format!("{:?}", a.declarator_doc) != format!("{:?}", b.declarator_doc) {
        return Some("declarator_doc differs".to_string());
    }
    // Hash sets and maps compare as sorted element lists: their encoding is
    // sorted, their iteration order is not.
    cmp_sorted!(
        param_locals,
        atomic_target_syms,
        rw_arg_env_sync_syms,
        multi_scope_slots,
        my_declared_sym,
        dynamic_declared_sym,
        my_declared_enum_sym,
        for_loop_param_syms,
        expr_declared_syms,
        outer_code_var_names,
        block_scope_protected_slots,
    );
    None
}

/// The first difference between two compiled units: the mainline, then each
/// routine (by key) and its nested routines.
// Cost: O(n), n = size of the compiled units.
pub(crate) fn first_difference(
    a: &(CompiledCode, CompiledFns),
    b: &(CompiledCode, CompiledFns),
) -> Option<String> {
    if let Some(diff) = first_code_difference(&a.0, &b.0) {
        return Some(format!("mainline {diff}"));
    }
    fns_difference(&a.1, &b.1)
}

fn fns_difference(a: &CompiledFns, b: &CompiledFns) -> Option<String> {
    let mut keys: Vec<_> = a.keys().map(|k| k.as_str()).collect();
    keys.sort();
    let b_keys: std::collections::BTreeSet<&str> = b.keys().map(|k| k.as_str()).collect();
    for key in keys {
        if !b_keys.contains(key) {
            return Some(format!("routine {key} missing from one side"));
        }
        let sym = crate::symbol::Symbol::intern(key);
        let (fa, fb) = (a.get(&sym)?, b.get(&sym)?);
        if let Some(diff) = first_code_difference(&fa.code, &fb.code) {
            return Some(format!("routine {key} {diff}"));
        }
        if let (Some(na), Some(nb)) = (&fa.compiled_fns, &fb.compiled_fns)
            && let Some(diff) = fns_difference(na, nb)
        {
            return Some(format!("in {key}: {diff}"));
        }
    }
    (a.len() != b.len())
        .then(|| format!("routine tables differ in size ({} vs {})", a.len(), b.len()))
}
