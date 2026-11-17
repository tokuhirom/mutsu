//! The compile-time loader seam (ADR-17558 §2.1): the parser asks the
//! interpreter that is compiling the unit to load a `use`d module when the
//! parser reaches the `use`, as rakudo does at BEGIN time.
//!
//! A leaf module, like [`crate::slang_link`]: the parser names only the
//! [`CompileTimeLoader`] trait and the free functions here, never
//! `crate::runtime` (ADR-10779's upward-call rule). The interpreter implements
//! the trait and pushes itself with [`LoaderGuard::push`] for the extent of a
//! parse whose unit it is about to run: the program (`run`, `-c`) and an
//! `EVAL`. A module's own compile and a `require`d file do not push one yet
//! (their `use`s load in their prologue; ADR-17558 §7.6), so a parse nested in
//! a load finds the busy entry of the load and loads nothing.
//!
//! **Fail-closed** (ADR-17558 §2.5, §7.5): a parse with no loader on this
//! thread's stack — every analysis path (`--dump-*`, `src/analysis/`,
//! `crates/mutsu-lsp`), a module's static export scan, any thread nobody
//! pushed one on — loads nothing, and the parser falls back to the static
//! scan. A `NoExecuteGuard` held on the thread refuses too.

use crate::value::RuntimeError;
use std::cell::RefCell;

/// One `use` statement the parser finished, as the loader needs it.
pub(crate) struct UseRequest<'a> {
    /// The module name, dist selectors included (`Foo:ver<1.2>`).
    pub(crate) module: &'a str,
    /// The adverb tags of the `use` (`:from`, export tags).
    pub(crate) tags: &'a [String],
    /// Whether the `use` carries a positional argument list (for `sub EXPORT`).
    pub(crate) has_arg: bool,
    /// Whether the `use` carries a `:if(...)` condition.
    pub(crate) has_condition: bool,
    /// Whether the `use` is inside a block rather than at the unit's top
    /// level: what the module installs while it loads is then lexical to that
    /// block, as for the prologue's preload of a nested `use`.
    pub(crate) nested: bool,
    /// Whether the module imports through a `sub EXPORT` hook (its own or
    /// Slangify's), as the parser's scan of it found.
    pub(crate) through_hook: bool,
    /// The parser's module search path at the `use`: the interpreter's chain
    /// plus the unit's parse-time `use lib` additions, in order.
    pub(crate) lib_paths: &'a [String],
}

/// What the loader did with a [`UseRequest`].
pub(crate) enum LoadOutcome {
    /// The module is loaded (now, or by an earlier `use`); the run-time `use`
    /// only imports.
    Loaded,
    /// The loader did not take this `use` (no loader, a pragma, a module it
    /// cannot resolve at parse time, a shape it does not load yet); it loads
    /// at run time as before.
    NotTaken,
    /// The module died while loading: the importing unit's compile fails at
    /// the `use` with this error.
    Failed(RuntimeError),
}

/// Implemented by the interpreter that compiles and will run a unit.
pub(crate) trait CompileTimeLoader {
    /// Load the module `request` names, without importing it.
    fn load_use(&mut self, request: &UseRequest<'_>) -> LoadOutcome;
}

thread_local! {
    /// The loaders whose parse is in progress on this thread, innermost last.
    /// `None` marks a parse that must not load: a static scan nested in a
    /// loading parse, or the loader that is already running.
    static LOADERS: RefCell<Vec<Option<*mut dyn CompileTimeLoader>>> =
        const { RefCell::new(Vec::new()) };
    /// The load failure that made the parse in progress fail, for the parse's
    /// caller to report instead of the parser's own error text.
    static FAILURE: RefCell<Option<RuntimeError>> = const { RefCell::new(None) };
}

/// RAII registration of the loader for one parse.
#[must_use = "the loader is only reachable while the guard is alive"]
pub(crate) struct LoaderGuard {
    depth: usize,
}

impl LoaderGuard {
    /// Make `loader` the loader of the parse that runs while the guard lives.
    ///
    /// # Safety
    /// `loader` must stay valid for the lifetime of the guard, and the caller
    /// must not otherwise access it until the guard is dropped: callers derive
    /// the pointer from the live `&mut Interpreter` that is about to parse,
    /// and only call the parser (which does not hold the interpreter) until
    /// the parse returns. The loader is re-borrowed only inside [`load_use`],
    /// with its stack entry marked busy, so a nested parse can reach the same
    /// interpreter only through a pointer it pushes itself, derived from the
    /// re-borrow that is live at that point.
    pub(crate) unsafe fn push(loader: *mut dyn CompileTimeLoader) -> LoaderGuard {
        LOADERS.with(|stack| {
            let mut stack = stack.borrow_mut();
            stack.push(Some(loader));
            LoaderGuard { depth: stack.len() }
        })
    }

    /// Keep the parse that runs while the guard lives from loading anything
    /// (a module's static export scan).
    // Cost: O(1).
    pub(crate) fn suppress() -> LoaderGuard {
        LOADERS.with(|stack| {
            let mut stack = stack.borrow_mut();
            stack.push(None);
            LoaderGuard { depth: stack.len() }
        })
    }
}

impl Drop for LoaderGuard {
    fn drop(&mut self) {
        LOADERS.with(|stack| stack.borrow_mut().truncate(self.depth - 1));
    }
}

/// Ask this thread's current loader to load the module of a finished `use`.
// Cost: O(1) when there is no loader; the module load otherwise.
pub(crate) fn load_use(request: &UseRequest<'_>) -> LoadOutcome {
    if crate::parser::no_execute::no_execute() {
        return LoadOutcome::NotTaken;
    }
    // Take the innermost loader out for the extent of the call, so the
    // re-borrow below is the only path to it.
    let Some((index, loader)) = LOADERS.with(|stack| {
        let mut stack = stack.borrow_mut();
        let index = stack.len().checked_sub(1)?;
        stack[index].take().map(|loader| (index, loader))
    }) else {
        return LoadOutcome::NotTaken;
    };
    // SAFETY: the pointer was pushed by `LoaderGuard::push`, whose contract
    // keeps it valid and otherwise unused while its guard lives; the guard
    // outlives this call (it is dropped only after the parse that called us
    // returns). The entry is `None` until we put it back, so no other call
    // can re-borrow it while this `&mut` is live.
    let outcome = unsafe { (*loader).load_use(request) };
    LOADERS.with(|stack| {
        if let Some(slot) = stack.borrow_mut().get_mut(index) {
            *slot = Some(loader);
        }
    });
    outcome
}

/// Record the load failure that is failing the parse in progress.
// Cost: O(1).
pub(crate) fn note_failure(error: RuntimeError) {
    FAILURE.with(|slot| *slot.borrow_mut() = Some(error));
}

/// The load failure that failed the parse that just returned, if one did.
// Cost: O(1).
pub(crate) fn take_failure() -> Option<RuntimeError> {
    FAILURE.with(|slot| slot.borrow_mut().take())
}
