//! The compile-time loader seam (ADR-17558 §2.1): the parser asks the
//! interpreter that is compiling the unit to load a `use`d module when the
//! parser reaches the `use`, as rakudo does at BEGIN time.
//!
//! A leaf module, like [`crate::slang_link`]: the parser names only the
//! [`CompileTimeLoader`] trait and the free functions here, never
//! `crate::runtime` (ADR-10779's upward-call rule). The interpreter implements
//! the trait and pushes itself with [`LoaderGuard::push`] for the extent of a
//! parse whose unit it compiles: the program (`run`, `-c`), an `EVAL`, a
//! `Str.AST`, and a module's own compile. A `require`d file does not push one
//! yet (its `use`s load in its prologue; ADR-17558 §7.7), so a parse nested in
//! such a load finds the busy entry of the load and loads nothing.
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
    /// That argument list's values, when every element is a literal the
    /// parser can hand over as it stands (`use M <a b>`, `use M 'x', 2`);
    /// `None` for an argument list that has to be evaluated.
    pub(crate) literal_args: Option<Vec<crate::value::Value>>,
    /// Whether the `use` carries a `:if(...)` condition.
    pub(crate) has_condition: bool,
    /// Whether the `use` is inside a block rather than at the unit's top
    /// level: what the module installs while it loads is then lexical to that
    /// block, as for the prologue's preload of a nested `use`.
    pub(crate) nested: bool,
    /// Whether the module activates a slang (it `use`s Slangify), which the
    /// slang activation interpreter does at parse time (ADR-0026 §2.1).
    pub(crate) activates_slang: bool,
    /// The package the `use` stands in (`unit module Foo;` or a package
    /// block), `None` at the unit's top level outside any.
    pub(crate) package: Option<String>,
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

/// One routine a loaded module exports to a `use`: its name (sigil-less,
/// `postfix:<USD>`) and, for an operator declared with `is tighter` / `is
/// looser` / `is equiv` / `is assoc`, its `Routine.prec` record.
pub(crate) struct LoadedExport {
    pub(crate) name: String,
    pub(crate) op_prec: Option<crate::op_prec::OpPrec>,
}

/// What a loaded unit provides to the parse of the `use` that loaded it
/// (ADR-17558 §2.2): the routines it exports under the tags the `use`
/// imports, and the `(directive, declarator)` pairs of its `EXPORTHOW`
/// (`DECLARE`/`SUPERSEDE`), a BEGIN-time merge included.
#[derive(Default)]
pub(crate) struct LoadedFacts {
    pub(crate) exports: Vec<LoadedExport>,
    pub(crate) declarators: Vec<(String, String)>,
}

/// Implemented by the interpreter that compiles and will run a unit.
pub(crate) trait CompileTimeLoader {
    /// Load the module `request` names, without importing it.
    fn load_use(&mut self, request: &UseRequest<'_>) -> LoadOutcome;

    /// The facts of `module`, which this loader has loaded, that a `use` of
    /// it with `import_tags` imports (`None`: the `use`'s arguments go to a
    /// `sub EXPORT`, so every tag counts).
    fn loaded_facts(&mut self, module: &str, import_tags: Option<&[String]>) -> LoadedFacts;
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
    with_loader(|loader| loader.load_use(request)).unwrap_or(LoadOutcome::NotTaken)
}

/// The facts of `module`, which this thread's current loader loaded for the
/// `use` being parsed; `None` when no loader is reachable.
// Cost: O(1) when there is no loader; the loader's fact read otherwise.
pub(crate) fn loaded_facts(module: &str, import_tags: Option<&[String]>) -> Option<LoadedFacts> {
    with_loader(|loader| loader.loaded_facts(module, import_tags))
}

/// Run `call` on this thread's innermost loader, if one is reachable: none
/// while a `NoExecuteGuard` is held, none for a parse that pushed no loader or
/// a suppressing entry.
// Cost: O(1) besides `call`.
fn with_loader<T>(call: impl FnOnce(&mut dyn CompileTimeLoader) -> T) -> Option<T> {
    if crate::parser::no_execute::no_execute() {
        return None;
    }
    // Take the innermost loader out for the extent of the call, so the
    // re-borrow below is the only path to it.
    let (index, loader) = LOADERS.with(|stack| {
        let mut stack = stack.borrow_mut();
        let index = stack.len().checked_sub(1)?;
        stack[index].take().map(|loader| (index, loader))
    })?;
    // SAFETY: the pointer was pushed by `LoaderGuard::push`, whose contract
    // keeps it valid and otherwise unused while its guard lives; the guard
    // outlives this call (it is dropped only after the parse that called us
    // returns). The entry is `None` until we put it back, so no other call
    // can re-borrow it while this `&mut` is live.
    let outcome = call(unsafe { &mut *loader });
    LOADERS.with(|stack| {
        if let Some(slot) = stack.borrow_mut().get_mut(index) {
            *slot = Some(loader);
        }
    });
    Some(outcome)
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
