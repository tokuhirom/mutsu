//! Rendering an *uncaught* exception the way rakudo renders one: `.gist`.
//!
//! rakudo's top-level handler prints `$exception.gist`, so any override of
//! that method decides what the user sees. mutsu used to print the
//! `RuntimeError`'s message plus its string backtrace, which is only ever the
//! *default* `Exception.gist` — every `method gist` override was therefore
//! ignored at the top level even though `say $ex` honoured it. That is not a
//! user-code curiosity: several core exception shapes are *defined* by a
//! `gist` override rather than a `message` one (`X::Promise::Broken`,
//! `X::Await::Died`), so their explanatory wrapper never reached stderr.
//!
//! Doing this properly means rendering inside the interpreter, because `.gist`
//! may run arbitrary user code (and may itself throw). This module is that
//! entry point; `main.rs` falls back to the pure `error_render::render_error`
//! when it declines.

use super::Interpreter;
use crate::value::RuntimeError;

/// The separator [`crate::Interpreter::attach_backtrace_to_error`] uses to join
/// a `Failure`'s fail-site backtrace to the site where *using* it finally
/// threw. It is a property of the uncaught throw, not of the exception —
/// rakudo's `$!.gist` for a caught one shows only the fail-site frames — so it
/// is re-attached here rather than being folded into `.gist`.
const ACTUALLY_THROWN_AT: &str = "\n\nActually thrown at:\n";

impl Interpreter {
    /// Render an uncaught error as rakudo's top-level handler would, or `None`
    /// when this error is not something `.gist` can speak for:
    ///
    /// * a parse diagnosis (`err.code()` is set) — the CLI renders those as
    ///   `===SORRY!===` with a source snippet, which `.gist` knows nothing of;
    /// * an error carrying no exception object at all (a native failure, a
    ///   stray control signal);
    /// * an exception whose `.gist` itself dies — rather than replacing the
    ///   user's error with the secondary one, fall back to the plain
    ///   message-and-backtrace rendering that cannot fail.
    pub fn render_uncaught(&mut self, err: &RuntimeError) -> Option<String> {
        if err.code().is_some() {
            return None;
        }
        let exception = (**err.exception.as_ref()?).clone();
        let mut text = self.render_gist_value(&exception).ok()?;
        // An error surfaced from an unhandled Failure renders both stacks.
        if let Some(bt) = err.backtrace()
            && let Some(at) = bt.find(ACTUALLY_THROWN_AT)
        {
            text.push_str(&bt[at..]);
        }
        Some(text)
    }
}
