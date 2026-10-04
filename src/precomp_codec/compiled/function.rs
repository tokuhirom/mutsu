//! The codec of `CompiledFunction`: every field, in declaration order (see the parent module).

#[allow(unused_imports)]
use super::{
    decode_map, decode_serde, decode_set, encode_map, encode_string_set, encode_sym_set,
    encode_u32_set, require_empty,
};
use crate::opcode::CompiledFunction;
use crate::precomp_codec::DecodeCtx;
use bincode::de::Decoder;
use bincode::enc::Encoder;
use bincode::error::{DecodeError, EncodeError};
#[allow(unused_imports)]
use bincode::serde::Compat;
use bincode::{Decode, Encode};

impl Encode for CompiledFunction {
    // Cost: O(n), n = size of the chunk.
    fn encode<E: Encoder>(&self, encoder: &mut E) -> Result<(), EncodeError> {
        let CompiledFunction {
            code,
            source_file,
            params,
            param_defs,
            return_type,
            fingerprint,
            empty_sig,
            is_rw,
            is_cached,
            is_raw,
            uses_return_rw,
            has_rw_positional_param,
            param_local_slots,
            params_fill_frame,
            has_inner_subs,
            declares_inner_routines,
            captured_fatal_mode,
            named_call_plan,
            deprecated_info,
            trir,
            declared_locals,
            param_name_syms,
            param_fast_types,
            param_itemize_on_bind,
            param_const_fills,
            light_required_positionals,
            light_full_arity_only,
            return_fast_type,
            return_definite_const,
            package,
            compiled_fns,
            memo_cache: _,
            package_sym_cache: _,
            source_file_sym_cache: _,
            package_routine_scoped_cache: _,
        } = self;
        code.encode(encoder)?;
        source_file.encode(encoder)?;
        params.encode(encoder)?;
        Compat(param_defs).encode(encoder)?;
        return_type.encode(encoder)?;
        fingerprint.encode(encoder)?;
        empty_sig.encode(encoder)?;
        is_rw.encode(encoder)?;
        is_cached.encode(encoder)?;
        is_raw.encode(encoder)?;
        uses_return_rw.encode(encoder)?;
        has_rw_positional_param.encode(encoder)?;
        param_local_slots.encode(encoder)?;
        params_fill_frame.encode(encoder)?;
        has_inner_subs.encode(encoder)?;
        declares_inner_routines.encode(encoder)?;
        captured_fatal_mode.encode(encoder)?;
        named_call_plan.encode(encoder)?;
        deprecated_info.encode(encoder)?;
        trir.encode(encoder)?;
        match declared_locals {
            None => false.encode(encoder)?,
            Some(set) => {
                true.encode(encoder)?;
                encode_sym_set(set, encoder)?;
            }
        }
        param_name_syms.encode(encoder)?;
        param_fast_types.encode(encoder)?;
        param_itemize_on_bind.encode(encoder)?;
        param_const_fills.encode(encoder)?;
        light_required_positionals.encode(encoder)?;
        light_full_arity_only.encode(encoder)?;
        return_fast_type.encode(encoder)?;
        return_definite_const.encode(encoder)?;
        package.encode(encoder)?;
        compiled_fns.encode(encoder)?;
        Ok(())
    }
}

impl Decode<DecodeCtx> for CompiledFunction {
    // Cost: O(n), n = size of the chunk.
    fn decode<D: Decoder<Context = DecodeCtx>>(decoder: &mut D) -> Result<Self, DecodeError> {
        Ok(CompiledFunction {
            code: Decode::decode(decoder)?,
            source_file: Decode::decode(decoder)?,
            params: Decode::decode(decoder)?,
            param_defs: decode_serde(decoder)?,
            return_type: Decode::decode(decoder)?,
            fingerprint: Decode::decode(decoder)?,
            empty_sig: Decode::decode(decoder)?,
            is_rw: Decode::decode(decoder)?,
            is_cached: Decode::decode(decoder)?,
            is_raw: Decode::decode(decoder)?,
            uses_return_rw: Decode::decode(decoder)?,
            has_rw_positional_param: Decode::decode(decoder)?,
            param_local_slots: Decode::decode(decoder)?,
            params_fill_frame: Decode::decode(decoder)?,
            has_inner_subs: Decode::decode(decoder)?,
            declares_inner_routines: Decode::decode(decoder)?,
            captured_fatal_mode: Decode::decode(decoder)?,
            named_call_plan: Decode::decode(decoder)?,
            deprecated_info: Decode::decode(decoder)?,
            trir: Decode::decode(decoder)?,
            declared_locals: if bool::decode(decoder)? {
                Some(decode_set(decoder)?)
            } else {
                None
            },
            param_name_syms: Decode::decode(decoder)?,
            param_fast_types: Decode::decode(decoder)?,
            param_itemize_on_bind: Decode::decode(decoder)?,
            param_const_fills: Decode::decode(decoder)?,
            light_required_positionals: Decode::decode(decoder)?,
            light_full_arity_only: Decode::decode(decoder)?,
            return_fast_type: Decode::decode(decoder)?,
            return_definite_const: Decode::decode(decoder)?,
            package: Decode::decode(decoder)?,
            compiled_fns: Decode::decode(decoder)?,
            memo_cache: Default::default(),
            package_sym_cache: Default::default(),
            source_file_sym_cache: Default::default(),
            package_routine_scoped_cache: Default::default(),
        })
    }
}
bincode::impl_borrow_decode_with_context!(CompiledFunction, DecodeCtx);
