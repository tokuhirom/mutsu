//! The slang host on the program interpreter (ADR-17558 §2.8, slice 4b).
//!
//! A slang module the program interpreter loaded at parse time (slice 4a) has
//! its productions run by that same interpreter: the parser's request reaches
//! it through the compile-time loader as a nested call on the parse's own
//! thread, instead of a message to the activation interpreter's thread
//! (`slang_host.rs`). The protocol keeps its shape:
//!
//! ```text
//! parser  -> run_in_process_slang_hook(rule, source)   the slang's token
//!            -> in_process_back_call(rule, pos)          a core rule (`<block>`)
//!               -> the parser's back-call answer
//!            <- (end, node)
//! parser  <- (end, node)
//! ```
//!
//! The unit's parser state is suspended while the hook's own code runs (it may
//! parse: a regex compiled at run time, an `EVAL`) and put back for each
//! back-call, which parses the unit's text in the unit's state. A production
//! runs in the interpreter that runs the unit, so what it builds (a class, a
//! registration an action makes) is already there: nothing is logged for the
//! unit to replay (ADR-13370, ADR-17243).

use super::*;
use crate::compile_time_loader::CompileTimeLoader;
use crate::slang_link::{BackCall, BackCallDynamics, HookRequest, HostAnswer};
use std::cell::RefCell;

/// One in-process hook that is running.
struct InlineHook {
    /// The parser's answer to a back-call, borrowed for the hook's extent.
    back_call: *mut BackCall<'static>,
    /// The unit's parser state, suspended while the hook's own code runs
    /// (`None` while a back-call has it in place).
    unit_parse: Option<crate::parser::unit_state::SuspendedUnit>,
}

thread_local! {
    /// The in-process hooks running on this thread, innermost last (a slang
    /// production in a block another one's back-call parsed).
    static INLINE_HOOKS: RefCell<Vec<InlineHook>> = const { RefCell::new(Vec::new()) };
}

impl Interpreter {
    /// Run the production `request` of a slang this interpreter hosts, with
    /// `back_call` answering the core productions its token reaches back for.
    // Cost: the slang's token and action, plus one back-call per core rule.
    pub(crate) fn run_in_process_slang_hook(
        &mut self,
        request: HookRequest,
        back_call: &mut BackCall<'_>,
    ) -> HostAnswer {
        let back_call: *mut BackCall<'_> = back_call;
        // SAFETY: only the lifetime is erased. The entry holding the pointer
        // is popped before this function returns, while `back_call`'s borrow
        // is still live, and `in_process_back_call` dereferences it only while
        // the entry is on the stack.
        let back_call: *mut BackCall<'static> = unsafe { std::mem::transmute(back_call) };
        let unit_parse = crate::parser::unit_state::suspend();
        INLINE_HOOKS.with(|hooks| {
            hooks.borrow_mut().push(InlineHook {
                back_call,
                unit_parse: Some(unit_parse),
            })
        });
        let HookRequest {
            rule,
            source,
            start,
            node,
            block,
            span,
            roles,
        } = request;
        let answer = self.hook_answer(&rule, &source, start, node, block, span, &roles, false);
        // Dropping the entry puts the unit's parser state back.
        let hook = INLINE_HOOKS.with(|hooks| hooks.borrow_mut().pop());
        drop(hook);
        answer
    }

    /// Answer a back-call of the innermost in-process hook: parse the core
    /// production `rule` at `pos` of the unit's text, in the unit's parser
    /// state. `None` when no in-process hook is running (the activation
    /// interpreter's host asks its parser over the channel).
    // Cost: the parser's own parse of the production.
    pub(super) fn in_process_back_call(
        &mut self,
        rule: &str,
        pos: usize,
        dynamics: BackCallDynamics,
    ) -> Option<HostAnswer> {
        let (back_call, unit_parse) = INLINE_HOOKS.with(|hooks| {
            let mut hooks = hooks.borrow_mut();
            let hook = hooks.last_mut()?;
            Some((hook.back_call, hook.unit_parse.take()))
        })?;
        // The hook's own parser state steps aside for the unit's.
        let host_parse = crate::parser::unit_state::suspend();
        drop(unit_parse);
        // A `use` in the production the parser reaches loads at parse time, in
        // this interpreter, as it would outside the hook.
        let loader: *mut dyn CompileTimeLoader = self as *mut Self;
        // SAFETY: `loader` comes from the live `&mut self`, which this function
        // does not use again until the guard is dropped below; the parse the
        // back-call runs reaches the interpreter only through the guard's
        // pointer (`compile_time_loader::with_loader`).
        let guard = unsafe { crate::compile_time_loader::LoaderGuard::push(loader) };
        // SAFETY: `back_call` is the live borrow of the innermost running
        // in-process hook (`run_in_process_slang_hook` pops its entry before
        // that borrow ends), and nothing else calls it meanwhile: a nested
        // hook pushes its own entry.
        let answer = unsafe { (*back_call)(rule, pos, dynamics) };
        drop(guard);
        let unit_parse = crate::parser::unit_state::suspend();
        INLINE_HOOKS.with(|hooks| {
            if let Some(hook) = hooks.borrow_mut().last_mut() {
                hook.unit_parse = Some(unit_parse);
            }
        });
        drop(host_parse);
        Some(answer)
    }
}
