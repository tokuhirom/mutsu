//! A variable `:=`-bound to a missing element, passed to a closure's
//! container-binding parameter (#18186).
//!
//! `my $c := %h<a>` leaves `$c` holding the deferred entry token
//! (`HashEntryRef`): the element does not exist until something writes
//! through `$c`. An argument is read with `GetLocal`, which resolves the token
//! to the element's current value (`Any`), so an `is raw` / `is rw` / `\x`
//! parameter of a closure called through a value (`CallOnValue`) was bound to
//! that value, not to the location: a write through the parameter, or a bind
//! to an element below it (`my $z := $h<b>`), never reached `%h`.
//!
//! The call site knows which argument came from which caller variable
//! (`arg_sources`). For a positional argument that lands on a
//! container-binding parameter, this pass hands the callee the variable's
//! token itself, as a variable bound to an existing element hands over that
//! element's cell. A readonly parameter keeps receiving the value.

use super::*;

impl Interpreter {
    /// Replace each positional argument at `stack[start..]` that came from a
    /// caller variable holding a deferred entry token, and binds a
    /// container-binding parameter of `target`, by that token.
    // Cost: O(a + p), a = arguments, p = the callee's parameters; O(1) when
    // no argument source is a local holding a token.
    pub(super) fn pass_entry_tokens_to_raw_params(
        &mut self,
        code: &CompiledCode,
        target: &Value,
        start: usize,
        sources: &[Option<String>],
    ) {
        let ValueView::Sub(data) = target.view() else {
            return;
        };
        let token_args: Vec<(usize, Value)> = sources
            .iter()
            .enumerate()
            .filter_map(|(i, source)| {
                let slot = self.find_local_slot(code, source.as_deref()?)?;
                let held = self.locals.get(slot)?;
                matches!(held.view(), ValueView::HashEntryRef { .. }).then(|| (i, held.clone()))
            })
            .collect();
        if token_args.is_empty() {
            return;
        }
        // The arguments map onto the positional parameters in order only
        // while every one before them is a plain positional value.
        let positional: Vec<&crate::ast::ParamDef> = data
            .param_defs
            .iter()
            .filter(|pd| !pd.named && !pd.is_invocant)
            .collect();
        for (i, token) in token_args {
            let Some(pd) = positional.get(i) else {
                continue;
            };
            if pd.is_variadic() || !pd.binds_caller_container() {
                continue;
            }
            let all_plain_before = self.stack[start..start + i]
                .iter()
                .all(|arg| !matches!(arg.view(), ValueView::Pair(..) | ValueView::Slip(..)));
            if all_plain_before && let Some(arg) = self.stack.get_mut(start + i) {
                *arg = token;
            }
        }
    }
}
