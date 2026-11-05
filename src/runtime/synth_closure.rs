//! Bytecode for the `Sub` values Rust code synthesizes (ADR-0055 section 8.3).
//!
//! A synthesized callable used to carry only an AST body, which
//! `call_sub_value`'s tree-walk carrier recompiled on every call. Its body is
//! now compiled once, here, and every callable built from the same `key`
//! shares the resulting chunk, so it runs through the ordinary compiled
//! closure path like any block. Values the body needs ride in the `SubData`'s
//! captured env under fixed names, never as literals in the AST, which is what
//! makes one chunk valid for all of them.

use crate::ast::{ParamDef, Stmt};
use crate::opcode::{CompiledCode, CompiledFns};
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::Arc;

type SynthChunk = (Arc<CompiledCode>, Arc<CompiledFns>);

thread_local! {
    static SYNTH_CODE: RefCell<HashMap<String, SynthChunk>> = RefCell::new(HashMap::new());
}

/// The compiled chunk for the synthesized closure `key`, compiling
/// `params`/`param_defs`/`body` on the first request. The caller guarantees
/// that one `key` always names the same body.
// Cost: O(1) after the first request for `key`; O(body) once.
pub(crate) fn synthesized_closure_code(
    key: &str,
    params: &[String],
    param_defs: &[ParamDef],
    body: &[Stmt],
) -> SynthChunk {
    if let Some(hit) = SYNTH_CODE.with(|c| c.borrow().get(key).cloned()) {
        return hit;
    }
    let mut compiler = crate::compiler::Compiler::new();
    let code = compiler.compile_closure_body(params, param_defs, body);
    let chunk = (Arc::new(code), Arc::new(CompiledFns::default()));
    SYNTH_CODE.with(|c| c.borrow_mut().insert(key.to_string(), chunk.clone()));
    chunk
}

/// Hand the value layer the compiler for the closures it synthesizes
/// (`Parameter.default`, `.constraints`).
// Cost: O(1).
pub(crate) fn install_value_layer_compiler() {
    crate::value::signature::install_synth_compile(synthesized_closure_code);
}
