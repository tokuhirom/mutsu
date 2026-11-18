//! The slang host on the program's own interpreter (ADR-17558 slice 4b).
//!
//! A slang module the compile-time loader loaded when the parser reached its
//! `use` registered its roles in this interpreter, so the productions its
//! grammar adds run here too: the parser's hook is a nested call through the
//! loader ([`crate::slang_link::SlangHost::Program`]), and a shim's request for
//! a core production (`<block>`) is a nested call back into the parser. Both
//! sides share one thread and one stack, where the activation interpreter's
//! host (`slang_host.rs`) alternates with the parser over two channels.
//!
//! The parser keeps its per-unit state in thread-locals, and the host
//! evaluates code (its entry, a slang method compiled on first use), which
//! parses. The unit's parse is therefore suspended for the hook and resumed
//! for each back-call, which parses part of that unit.
//!
//! A hook's effects now happen in the interpreter that runs the unit. The
//! replays that carry them from the activation interpreter into the unit
//! (ADR-13370, ADR-17243, ADR-17741) still travel in the produced statements,
//! because a precompiled unit does not run its hooks again:
//!
//! - a type the production minted is already built here, so its class replay
//!   binds the live type instead of minting another ([`LIVE_MINTED`]);
//! - the slang's own actions still re-run where the production stands. The
//!   class a `routine-declarator` production's method belongs to does not
//!   exist while the unit is parsed, so the live run's
//!   `$method.meta-object` is a detached method, and only the replay reaches
//!   the method the class registers (EEL's `pattern`).
//!   TODO: an action's effect on state that does exist at parse time (a
//!   module-level registry) happens twice, once live and once in the
//!   replay; the replay can be skipped only once a declaration's
//!   compile-time objects are the run-time ones, as on rakudo.

use super::*;
use crate::slang_link::{BackCall, BackCallDynamics, HookRequest, HostAnswer};
use std::cell::RefCell;
use std::collections::HashSet;

/// One running program hook: the parser's back-call, and the unit's parse
/// state while the hook runs (`None` while a back-call has resumed it).
struct RunningHook {
    back_call: *mut BackCall<'static>,
    suspended: Option<crate::parser::unit_state::SuspendedUnit>,
}

thread_local! {
    /// The program hooks running on this thread, innermost last.
    static HOOKS: RefCell<Vec<RunningHook>> = const { RefCell::new(Vec::new()) };
    /// The names of the types a production minted while it ran live on this
    /// thread's program host: their class replay binds the live type.
    static LIVE_MINTED: RefCell<HashSet<String>> = RefCell::new(HashSet::new());
}

/// Whether the type the host minted as `name` was minted by a production that
/// ran live in this process.
// Cost: O(1).
pub(super) fn minted_live(name: &str) -> bool {
    LIVE_MINTED.with(|m| m.borrow().contains(name))
}

/// Record what a production that ran live minted.
// Cost: O(m), m = the types it minted.
pub(super) fn note_live_hook(minted: impl IntoIterator<Item = String>) {
    LIVE_MINTED.with(|m| m.borrow_mut().extend(minted));
}

/// Whether a program hook is running on this thread, so a shim's back-call
/// goes to the parser directly rather than over a channel.
// Cost: O(1).
pub(super) fn program_hook_running() -> bool {
    HOOKS.with(|h| !h.borrow().is_empty())
}

impl Interpreter {
    /// Run one slang production on this interpreter (see the module docs).
    // Cost: the slang's own token and action.
    pub(crate) fn run_program_slang_hook(
        &mut self,
        request: &HookRequest<'_>,
        back_call: &mut BackCall<'_>,
    ) -> HostAnswer {
        // SAFETY: the pointer only erases the closure's lifetime. It is pushed
        // here and popped before this function returns, and only
        // `program_back_call` dereferences it, while the hook this frame runs
        // is the innermost one (the entry is the stack's last), i.e. while
        // `back_call` is still borrowed by this frame and not otherwise used.
        let back_call: *mut BackCall<'static> =
            unsafe { std::mem::transmute::<*mut BackCall<'_>, *mut BackCall<'static>>(back_call) };
        HOOKS.with(|h| {
            h.borrow_mut().push(RunningHook {
                back_call,
                suspended: Some(crate::parser::unit_state::suspend()),
            })
        });
        let saved_grammar = std::mem::replace(
            &mut self.regex_state.slang_grammar_roles,
            request.host.grammar_roles.clone(),
        );
        let saved_actions = std::mem::replace(
            &mut self.regex_state.slang_action_roles,
            request.host.action_roles.clone(),
        );
        let answer = self.answer_slang_hook(
            request.rule,
            request.source,
            request.start,
            request.node.clone(),
            request.block.clone(),
            request
                .span
                .as_ref()
                .map(|span| crate::slang_link::ActionSpan {
                    end: span.end,
                    file: span.file.clone(),
                }),
            true,
        );
        self.regex_state.slang_grammar_roles = saved_grammar;
        self.regex_state.slang_action_roles = saved_actions;
        // Dropping the entry resumes the unit's parse.
        let hook = HOOKS.with(|h| h.borrow_mut().pop());
        drop(hook);
        answer
    }

    /// A shim's request for the core production `rule` at `pos`, answered by
    /// the parser of the innermost program hook: the unit's parse resumes for
    /// it, with this interpreter as its loader again (a `use` or a nested
    /// slang production in the block it parses).
    // Cost: the parser's own parse of the production.
    pub(super) fn program_back_call(
        &mut self,
        rule: &str,
        pos: usize,
        dynamics: BackCallDynamics,
    ) -> HostAnswer {
        let Some((back_call, suspended)) = HOOKS.with(|h| {
            let mut hooks = h.borrow_mut();
            let hook = hooks.last_mut()?;
            Some((hook.back_call, hook.suspended.take()))
        }) else {
            return Err("no slang production is running".to_string());
        };
        // Resume the unit's parse.
        drop(suspended);
        let loader: *mut dyn crate::compile_time_loader::CompileTimeLoader = self as *mut Self;
        // SAFETY: `loader` comes from the live `&mut self`, which this frame
        // does not use while the guard lives; the parser reaches the
        // interpreter only through `compile_time_loader`'s re-borrow of it.
        let guard = unsafe { crate::compile_time_loader::LoaderGuard::push(loader) };
        // SAFETY: see `run_program_slang_hook`: the innermost hook's frame is
        // below this one on the stack and holds the closure's borrow.
        let answer = unsafe { (*back_call)(rule, pos, dynamics) };
        drop(guard);
        let suspended = crate::parser::unit_state::suspend();
        HOOKS.with(|h| {
            if let Some(hook) = h.borrow_mut().last_mut() {
                hook.suspended = Some(suspended);
            }
        });
        answer
    }
}
