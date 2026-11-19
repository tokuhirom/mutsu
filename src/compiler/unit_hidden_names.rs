//! Which of the unit's block-only lexicals a top-level routine can look up by
//! name (ADR-12529 phase 3, #17630).
//!
//! A routine declared at a compunit's top level has the unit's top level as
//! its lexical outer. While a mainline block calls it, the program scope its
//! frame links to also holds that block's own lexicals, which the frame hides
//! (`Env::set_unit_hidden`). A reflective routine hides every one of them; any
//! other routine only those its code can name, so a routine that names none
//! carries no set and its call pays nothing.

use crate::opcode::{CompiledCode, CompiledFunction};
use crate::symbol::Symbol;
use crate::value::ValueView;

/// Whether `cf`'s body, or a parameter's `where`, default or shape chunk, can
/// look `sym` up by name.
// Cost: O(f + c) per chunk, f = free variables, c = constants.
pub(super) fn routine_may_name(cf: &CompiledFunction, sym: Symbol) -> bool {
    code_may_name(&cf.code, sym)
        || cf.param_defs.iter().any(|pd| {
            pd.code.get().is_some_and(|chunks| {
                chunks
                    .where_chunk
                    .iter()
                    .chain(chunks.default_chunk.iter())
                    .chain(chunks.shape_chunks.iter().flatten())
                    .any(|chunk| code_may_name(&chunk.code, sym))
            })
        })
}

/// Whether `code` can look `sym` up by name past its own frame: a free
/// variable, a string constant an op names (a routine, type or bareword, and
/// its `&` form) or an `OUTER::` name. Its own parameters and locals are not
/// counted: the frame answers them before any link, so hiding them would only
/// cost the routine its light call (`vm_call_light`, which gives a frame with
/// a hidden set a root of its own). Code that names lexicals by string, or
/// creates closures (whose own constants it does not carry), can name
/// anything. `CompiledCode::mentions_name_memo` without the locals and the
/// memo: the chunk is still being finished here.
// Cost: O(f + c), f = free variables, c = constants.
fn code_may_name(code: &CompiledCode, sym: Symbol) -> bool {
    if code.needs_reflective_capture
        || code.static_name_lookup
        || !code.closure_compiled_codes.is_empty()
    {
        return true;
    }
    if code.free_var_syms.contains(&sym) {
        return true;
    }
    sym.with_str(|name| {
        // Its own declaration names it too (`SetVarDynamic` carries the name).
        if code.locals.iter().any(|local| local == name) {
            return false;
        }
        code.outer_ref_names.iter().any(|outer| outer == name)
            || code.constants.iter().any(|v| match v.view() {
                ValueView::Str(s) => {
                    let s = s.as_str();
                    s == name || name.strip_prefix('&') == Some(s)
                }
                _ => false,
            })
    })
}
