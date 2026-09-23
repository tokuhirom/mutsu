//! A TRIR routine's entry on `routine_stack`.
//!
//! A TRIR frame used to be invisible to everything that reads
//! `routine_stack`, which counts routine frames: `CALLER::` (and its repeated
//! forms, validated against the stack's depth), backtraces, and every name
//! resolution that asks which compunit the running frame belongs to. An
//! untyped callee of a TRIR routine therefore saw one frame fewer than there
//! were, so `CALLER::CALLER::.BIND-KEY` from two calls down answered "frame is
//! gone". Each TRIR routine body now pushes the frame the untyped light path
//! pushes for the same call (`vm_call_light.rs`) and pops it on every exit.

use super::TrChunk;
use super::exec::TrOutcome;
use super::frame::TrFrame;
use crate::opcode::{CompiledFns, CompiledFunction};
use crate::runtime::Interpreter;
use crate::value::RuntimeError;

impl TrChunk {
    /// Record the file `cf`'s body was declared in, once it is known. A
    /// nested routine is compiled before its enclosing file is stamped
    /// (`CompiledFunction::stamp_source_file`), so this is set from wherever
    /// the stamped function is in hand; an unset file reads as the caller's.
    pub(crate) fn note_def_file(&self, cf: &CompiledFunction) {
        if let Some(file) = cf.source_file_sym() {
            let _ = self.def_file.set(file);
        }
    }
}

impl Interpreter {
    /// Run `chunk` as a routine body: under its own `routine_stack` frame,
    /// exactly as the light path runs an untyped body. The frame's package is
    /// the current one, which every door has already switched to the callee's
    /// (`trir_body_package`).
    pub(crate) fn run_trir_routine(
        &mut self,
        chunk: &TrChunk,
        frame: TrFrame,
        compiled_fns: &CompiledFns,
    ) -> Result<TrOutcome, RuntimeError> {
        self.push_routine_with_location(
            self.current_package_sym(),
            chunk.name,
            self.current_source_line(),
            self.executing_source_file_sym(),
            chunk.def_file.get().copied(),
        );
        let outcome = self.run_trir_chunk(chunk, frame, compiled_fns);
        self.pop_routine();
        outcome
    }
}
