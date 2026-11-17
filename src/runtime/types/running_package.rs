//! The packages a lookup anchored on "whatever routine is running" tries, as
//! interned symbols, and the fingerprint of that anchoring that a per-site
//! memo of a package-relative answer is keyed on.

use crate::runtime::Interpreter;
use crate::symbol::Symbol;

/// What decides which package a bare name is resolved relative to while
/// some routine runs: the four [`Interpreter::running_package_candidate_syms`]
/// plus the running frame's own lexical package (the owner whose module-scope
/// terms a bareword consults first). Two executions with equal fingerprints
/// walk the same packages in the same order.
pub(crate) type RunningPackageContext = crate::value::SiteContext;

impl Interpreter {
    /// [`Interpreter::running_package_candidates`] as interned symbols, most
    /// specific first: the running method's class, the lexical package of the
    /// running scope (a candidate scope's, else the frame's), the running
    /// frame's package unless it is GLOBAL, and `current_package`.
    ///
    /// A called closure compiled in a compunit of its own anchors on that
    /// compunit, not on the routine that happens to be calling it: a block of
    /// the main script handed to a module routine (`cb({ array[..] })`) must not
    /// see the module's file-scope names.
    ///
    /// A module mainline being loaded (a `require` from inside a routine of
    /// another module) is a compunit of its own too: the frames below the depth
    /// its mainline started at belong to whoever triggered the load, so their
    /// packages -- and the aliases those packages imported -- are not the
    /// loaded code's. Rakudo compiles the required unit with only the setting
    /// as its outer scope.
    // Cost: O(f), f = routine frames above the module-mainline floor walked to
    // the nearest non-block frame (usually 1-2).
    pub(crate) fn running_package_candidate_syms(&self) -> [Option<Symbol>; 4] {
        let floor = self.module_mainline_routine_floor().unwrap_or(0);
        let frames = self.routine_stack.get(floor..).unwrap_or_default();
        let own_block = frames
            .last()
            .filter(|frame| frame.is_block && !frame.is_inlined_block && frame.def_file.is_some());
        let frame = own_block.or_else(|| {
            frames
                .iter()
                .rev()
                .find(|frame| !frame.is_block)
                .or_else(|| frames.last())
        });
        // The running method's class is the loader's too when no frame of the
        // loaded code has been pushed yet.
        let method_class = if floor > 0 && frames.is_empty() {
            None
        } else {
            self.method_class_stack_top_sym()
        };
        [
            method_class,
            self.candidate_scope_lexical_package()
                .or_else(|| frame.and_then(|frame| frame.lexical_package)),
            frame
                .map(|frame| frame.package)
                .filter(|pkg| !crate::qualified::is_global_package(*pkg)),
            Some(self.current_package_sym()),
        ]
    }

    /// The [`RunningPackageContext`] of this moment.
    // Cost: O(f), as `running_package_candidate_syms`.
    pub(crate) fn running_package_context(&self) -> RunningPackageContext {
        let [a, b, c, d] = self.running_package_candidate_syms();
        let frame_package = self
            .routine_stack()
            .last()
            .and_then(|frame| frame.lexical_package);
        [a, b, c, d, frame_package]
    }
}
