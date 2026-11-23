//! A callee's bind of its caller's `$_`, `$/` or `$!` (`nqp::bindlexdyn`,
//! `CALLER::<$_> := $v`), seen by the caller once the call returns (#17453).
//!
//! Every routine frame has its own `$_`/`$/`/`$!`, kept in its env. A call
//! saves the caller's env on its `VmCallFrame` and every return path restores
//! it from there, so the bind is written into that saved env: the restore
//! hands the caller its new binding, whichever return path runs. The return
//! writeback (`pop_caller_env_with_writeback`) never copies the callee's own
//! `$_`/`$/`/`$!` back, so a callee's own topic still cannot leak.

use super::*;

impl Interpreter {
    /// Bind the calling frame's `key` (`_`, `/` or `!`) to `value`.
    // Cost: O(1) expected (one insert into each of the two caller env copies).
    pub(crate) fn bind_caller_implicit_lexical(&mut self, key: &str, value: Value) {
        if let Some(caller) = self.caller_env_stack.last_mut() {
            caller.insert(key.to_string(), value.clone());
        }
        if let Some(frame) = self.call_frames.last_mut() {
            frame.saved_env.insert(key.to_string(), value);
        }
    }

    /// Whether `name` is the caller's `$_` reached through `CALLER::` from
    /// code compiled under a language revision where `$_` is not dynamic
    /// (6.d on; 6.c declares it dynamic): a read or an assignment is
    /// `X::Caller::NotDynamic`, and only a bind reaches it (#18988). The
    /// revision is the one the accessing chunk was compiled under
    /// (`CompiledCode::caller_topic_dynamic`), which an `EVAL`'d unit's own
    /// `use v6.c` sets too.
    // Cost: O(1).
    pub(crate) fn caller_topic_is_not_dynamic(&self, name: &str) -> bool {
        if name != "_" {
            return false;
        }
        if self.current_code == 0 {
            return self.running_revision_letter() != 'c';
        }
        // SAFETY: `current_code` is the address of the live bytecode frame's
        // `CompiledCode` running the `CALLER::` access, kept alive for the
        // whole frame by `vm_call_*`; it is only read here, synchronously.
        let code = unsafe { &*(self.current_code as *const crate::opcode::CompiledCode) };
        !code.caller_topic_dynamic
    }
}
