//! The TRIR door for a `CallFunc` whose name had not been resolved yet.
//!
//! [`Interpreter::try_call_trir`] is reached from `exec_call_func_op`'s
//! resolution-cache hit. A call site's FIRST call misses that cache and takes
//! `dispatch_func_call_inner`, which resolved the callee and then ran its
//! untyped body. One untyped run per site sounds harmless. It is not for a
//! recursive-descent routine: the first `parse-array` of a JSON::Fast decode
//! is the top-level array, and its loop over every element ran untyped, with
//! the whole generic call protocol on each element's `parse-thing` call
//! (~55 untyped ops an element, `gen-links=0` for the whole first
//! `from-json`). This door runs the chunk on that first call too, under the
//! hit path's own admissions.

use crate::opcode::{CompiledCode, CompiledFns, CompiledFunction};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

impl Interpreter {
    /// Run `cf`'s chunk with an already-collected argument list, when every
    /// admission the resolution-cache hit path applies holds. `None` leaves
    /// the VM exactly as it was; the caller then takes its ordinary path.
    pub(crate) fn try_call_trir_values(
        &mut self,
        cf: &CompiledFunction,
        args: &[Value],
        caller_code: &CompiledCode,
        compiled_fns: &CompiledFns,
    ) -> Option<Result<Value, RuntimeError>> {
        cf.trir.as_ref()?;
        // The hit path's admissions (`exec_call_func_op`): a junction
        // autothreads, a callsite-line marker belongs to a test assertion, and
        // an aggregate handed to a `$` parameter shares its container.
        let declines = args.iter().any(|v| {
            let view = v.view();
            matches!(view, ValueView::Junction { .. })
                || Self::callsite_line_of_view(&view).is_some()
        }) || Self::call_shares_container_into_scalar_param(cf, args);
        if declines {
            return None;
        }
        let base = self.stack.len();
        self.stack.extend(args.iter().cloned());
        let out = self.try_call_trir(cf, base, Some(caller_code), compiled_fns);
        if out.is_none() {
            self.stack.truncate(base);
        }
        out
    }
}
