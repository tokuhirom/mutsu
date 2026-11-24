//! Pushing a method's routine frame.

use super::*;

impl Interpreter {
    /// Push a method routine frame. `line`/`file` record the call-site (as for
    /// `push_routine_with_location`); `def_file` is the file the method body was
    /// *declared* in (the class's `use`d module, or `None` for the main script
    /// or a synthetic/native method) — see `MethodDef::source_file`. Without
    /// this, `executing_source_file()`'s frame walk always fell through past a
    /// method frame to the dynamically-scoped `?FILE`, which had already
    /// reverted to the main script by the time the method ran.
    /// `callable_id` is the per-invocation `__mutsu_callable_id` the method
    /// body runs under (see `RoutineFrame::callable_id`). `fixtures` is the
    /// frame's invocant / `?CLASS` / `?ROLE` record (ADR-14633).
    /// `on_caller_env` marks a method that runs on its caller's env handle.
    // Cost: O(1) amortized.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn push_method_routine_with_location(
        &mut self,
        package: Symbol,
        lexical_package: Symbol,
        name: Symbol,
        line: Option<u32>,
        file: Option<Symbol>,
        def_file: Option<Symbol>,
        is_submethod: bool,
        is_hidden_from_backtrace: bool,
        callable_id: u64,
        fixtures: super::routine_stack::FrameFixtures,
        on_caller_env: bool,
    ) {
        // A method that runs on its caller's env handle (the slot method
        // frame, ADR-18393) has no env of its own to drop the caller's
        // typed-`$_` constraint from, and its body never names `$_`:
        // dropping it would strip the constraint from the caller (#19146).
        if !on_caller_env {
            self.drop_inherited_topic_constraint();
        }
        let invocation_id = self.take_invocation_id();
        let frame = super::RoutineFrame {
            package,
            lexical_package: Some(lexical_package),
            name,
            line,
            file,
            is_method: true,
            is_submethod,
            is_block: false,
            is_inlined_block: false,
            is_hidden_from_backtrace,
            def_file,
            invocation_id,
            callable_id,
            code: 0,
            fixtures: 0,
            unit_scoped: false,
            fixtures_floor: 0,
        };
        self.record_profile_routine_frame(&frame);
        self.routine_stack.push_with_fixtures(frame, fixtures);
    }
}
