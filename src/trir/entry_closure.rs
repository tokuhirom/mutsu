//! The closure door into TRIR (#8335).
//!
//! A closure literal whose body TRIR proves (`compile::compile_block`) carries
//! its chunk on its `CompiledCode` (`closure_trir`). A value call of such a
//! closure runs the chunk instead of `call_compiled_closure_in_unit`, which
//! builds an env frame, installs the capture, binds through the env and runs
//! the caller-writeback scan on every call.
//!
//! None of that is needed, because the chunk reaches nothing past its own
//! slots and its free variables: the block compile declines a call, a
//! `return`, a dynamic variable and every write to a free variable. Its free
//! variables are read from the closure's own capture (`SubData::env`), and only
//! when the captured binding is a shared cell — the binding the untyped entry
//! force-installs over any same-named caller variable — so the read is the
//! lexical one. A plain captured value declines the call: the untyped entry
//! gives the caller chain precedence over it, which a TRIR read would not.
//!
//! Declining is free: every `None` leaves the VM state as it was, and the
//! caller takes the untyped closure entry.

use super::TrChunk;
use super::entry::{RwPlan, RwTarget};
use super::frame::TrFrame;
use crate::opcode::{CompiledCode, CompiledFns};
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, SubData, Value, ValueView};

/// Where a closure call's arguments are: borrowed from the caller, or still
/// on the VM stack at `stack[base..]`.
#[derive(Clone, Copy)]
enum ClosureArgs<'a> {
    Slice(&'a [Value]),
    Stack(usize),
}

impl Interpreter {
    /// Run `cc`'s TRIR chunk for a value call of the closure `data`, or answer
    /// `None` when the call has a shape the chunk does not cover.
    // Cost: O(p + o), p = parameters, o = free variables of the body.
    pub(crate) fn try_call_closure_trir(
        &mut self,
        data: &SubData,
        cc: &CompiledCode,
        args: &[Value],
        compiled_fns: &CompiledFns,
    ) -> Option<Result<Value, RuntimeError>> {
        let chunk = cc.closure_trir.as_ref()?;
        if !Self::closure_trir_args_admitted(data, chunk, args) {
            return None;
        }
        self.run_closure_trir(data, chunk, ClosureArgs::Slice(args), compiled_fns)
    }

    /// The `CallOnValue` door: the target is `stack[base - 1]` and the
    /// arguments `stack[base..]`. On `Some`, both are consumed. `None` leaves
    /// the stack untouched for the ordinary value call.
    ///
    /// The call site has nothing left to do for such a call: a TRIR closure
    /// binds no `is rw` parameter, so its argument sources would never be read,
    /// and it reads no topic, so the `TopicArgSite` is moot.
    // Cost: O(p + o + a), p, o as `try_call_closure_trir`, a = the call site's
    // argument descriptors scanned for a named or `|` argument.
    pub(crate) fn try_value_call_closure_trir(
        &mut self,
        code: &CompiledCode,
        base: usize,
        arg_sources_idx: Option<u32>,
        compiled_fns: &CompiledFns,
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Sub(data) = self.stack.get(base.checked_sub(1)?)?.view() else {
            return None;
        };
        let cc = data.compiled_code.as_ref()?;
        let chunk = cc.closure_trir.as_ref()?;
        // An assignment through the call (`$b($x) = 1`) wants the container
        // the untyped entry hands back.
        if self.in_lvalue_assignment {
            return None;
        }
        let kinds = Self::stack_arg_kinds(code, arg_sources_idx);
        if kinds.named || kinds.slip {
            return None;
        }
        // The checks `vm_call_on_value_at` makes before its compiled-closure
        // fast path: a wrapped closure runs its wrappers, and a multi's
        // dispatcher value dispatches.
        if self.has_wrap_chain(data.id)
            || data
                .env
                .contains_key_sym(crate::symbol::well_known::multi_dispatch_candidates())
            || (self.stack.len() > base && Self::sub_multi_method_dispatcher_name(&data).is_some())
        {
            return None;
        }
        if !Self::closure_trir_args_admitted(&data, chunk, &self.stack[base..]) {
            return None;
        }
        let (data, chunk) = (data.clone(), chunk.clone());
        let out = self.run_closure_trir(&data, &chunk, ClosureArgs::Stack(base), compiled_fns)?;
        self.stack.truncate(base - 1);
        Some(out)
    }

    /// The argument half of the gate both doors share.
    fn closure_trir_args_admitted(data: &SubData, chunk: &TrChunk, args: &[Value]) -> bool {
        // `use fatal` turns a returned Failure into a throw at the closure's
        // boundary, and `.assuming` reshapes the arguments: both are the
        // untyped entry's.
        if data.captured_fatal_mode
            || !data.assumed_positional.is_empty()
            || !data.assumed_named.is_empty()
            || args.len() != chunk.params.len()
        {
            return false;
        }
        // A `Pair` is a named argument (or the call site's line marker), and a
        // `Junction` autothreads through the body's operators, which TRIR's
        // typed ops do not.
        !args.iter().any(|a| {
            matches!(
                a.unwrap_varref().view(),
                ValueView::Pair(..) | ValueView::ValuePair(..) | ValueView::Junction { .. }
            )
        })
    }

    fn run_closure_trir(
        &mut self,
        data: &SubData,
        chunk: &TrChunk,
        args: ClosureArgs<'_>,
        compiled_fns: &CompiledFns,
    ) -> Option<Result<Value, RuntimeError>> {
        if let Some(file) = data.source_file_sym() {
            let _ = chunk.def_file.set(file);
        }
        // The body runs under the closure's declaring package, as the untyped
        // entry's `enter_package_guarded_sym` does.
        let pkg_str = data.package.as_str();
        let pkg = (!pkg_str.is_empty()
            && !crate::runtime::utils::has_routine_scope_marker(pkg_str))
        .then_some(data.package);
        self.run_trir_from_outside(
            chunk,
            pkg,
            compiled_fns,
            |me, frame| me.trir_bind_closure_args(chunk, frame, args),
            |me, chunk, frame| me.trir_seed_closure_outers(data, chunk, frame),
        )
    }

    /// Bind a closure call's arguments exactly as the light closure bind
    /// (`closure_light_bind`) does: each is an item binding of the argument's
    /// value.
    fn trir_bind_closure_args(
        &mut self,
        chunk: &TrChunk,
        frame: TrFrame,
        args: ClosureArgs<'_>,
    ) -> Option<RwPlan> {
        for (i, p) in chunk.params.iter().enumerate() {
            if p.sigilless || p.is_rw {
                return None;
            }
            let arg = match args {
                ClosureArgs::Slice(args) => args[i].clone(),
                ClosureArgs::Stack(base) => self.stack[base + i].clone(),
            };
            let val = Self::itemize_scalar_store_value(
                crate::runtime::types::unwrap_varref_value(arg).hllize_vm_collection(),
            );
            self.bind_ro_param(frame, p, &val)?;
        }
        Some(([(0u16, RwTarget::Slot(0)); 4], 0))
    }

    /// Push the closure's free variables from its capture, answering whether
    /// every one is a shared cell (see the module doc).
    fn trir_seed_closure_outers(&mut self, data: &SubData, chunk: &TrChunk, frame: TrFrame) -> bool {
        debug_assert_eq!(
            self.trir.outers.len(),
            frame.outer_base as usize,
            "a frame's free variables are seeded before anything else pushes any"
        );
        for o in &chunk.outers {
            match data.env.get_sym(o.name) {
                Some(v) if v.is_container_ref() => {
                    let v = super::entry::deref_cell(v);
                    self.trir.outers.push(v);
                }
                _ => {
                    self.trir.outers.truncate(frame.outer_base as usize);
                    return false;
                }
            }
        }
        true
    }
}
