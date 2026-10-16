//! A closure an `EVAL` creates over a lexical of the code that called it
//! (ADR-12529 §3, slice 28).
//!
//! ```raku
//! sub run(&c) { c() }
//! my $h;
//! my $handle = EVAL '{ $h ~= "x" }';
//! run { $handle() };
//! say $h;   # x
//! ```
//!
//! The `EVAL`'d unit is compiled on its own, so the caller's `$h` is not one
//! of its locals: the unit reads and writes it by name, on the env it runs
//! on, which is the caller's. A closure the unit creates captured the plain
//! value from there, and its write reached `$h` only when the call that ran
//! it returned and each frame's return merge copied it back. Now the
//! closure's capture promotes such a binding to a shared cell in the env
//! instead, and logs the name for the `EVAL` carrier, whose call site then
//! moves the cell into the caller's slot (`writeback_carrier_writes`): the
//! caller, the closure and its siblings share one container from then on.

use super::*;

impl Interpreter {
    /// Promote every name the closure `cc` writes that the creating `code`
    /// has no slot for, and that the running env binds to a plain user
    /// lexical, to a shared cell in that env. Only inside an `EVAL`'d unit,
    /// whose free names are the calling code's lexicals.
    // Cost: O(w), w = names `cc` writes; O(1) outside an `EVAL`.
    pub(super) fn cell_eval_captured_caller_lexicals(
        &mut self,
        code: &CompiledCode,
        cc: &CompiledCode,
    ) {
        if cc.free_var_writes.is_empty()
            || !self
                .env()
                .get("__mutsu_in_eval")
                .is_some_and(|v| v.truthy())
        {
            return;
        }
        for sym in &cc.free_var_writes {
            if code.locals_sym.contains(sym) || !sym.with_str(crate::env::is_plain_user_lexical) {
                continue;
            }
            let Some(cur) = self.env().get_sym(*sym) else {
                continue;
            };
            if cur.is_container_ref() || !Self::is_writeback_safe_scalar(cur) {
                continue;
            }
            let cell = cur.clone().into_container_ref();
            self.env_mut().insert_sym(*sym, cell);
            sym.with_str(|name| self.note_caller_env_write(name));
        }
    }
}
