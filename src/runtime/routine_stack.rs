//! The live routine/block frames, with the one aggregate question the hot
//! path asks kept as a count rather than recomputed by a scan.
//!
//! `running_module_bareword` (`src/vm/vm_var_get_ops.rs`) opens every bareword
//! read with "does any live frame carry a `lexical_package`?", and answered it
//! by walking the whole stack. In a ten-decode `JSON::Fast` profile that
//! function is entered **295,917 times**, and the walk grows with nesting
//! depth on a path that runs per name — `parse-thing` -> `parse-obj` ->
//! `parse-thing` nests as deep as the document does
//! ([#8918](https://github.com/tokuhirom/mutsu/issues/8918)).
//!
//! The count is maintained here rather than at the mutation sites, and that
//! is the whole point of the type existing: `push`/`pop`/`truncate` are
//! spread across nine modules, so a counter kept by hand at each of them goes
//! wrong the first time someone adds a tenth — silently, because nothing
//! would recompute it. With `frames` private, the compiler finds every
//! mutation instead.

use super::RoutineFrame;

/// The interpreter's routine/block frame stack.
#[derive(Debug, Default)]
pub(crate) struct RoutineStack {
    frames: Vec<RoutineFrame>,
    /// How many live frames have `lexical_package.is_some()`.
    ///
    /// Maintained by the three mutators below, which are the only ways the
    /// `Vec` can change — see the module comment.
    lexical_package_frames: usize,
}

impl RoutineStack {
    /// Whether any live frame carries a `lexical_package`.
    ///
    /// The scan this replaces was `self.routine_stack().iter().rev().any(|f|
    /// f.lexical_package.is_some())`.
    #[inline]
    pub(crate) fn any_lexical_package(&self) -> bool {
        self.lexical_package_frames != 0
    }

    pub(crate) fn push(&mut self, frame: RoutineFrame) {
        if frame.lexical_package.is_some() {
            self.lexical_package_frames += 1;
        }
        self.frames.push(frame);
    }

    pub(crate) fn pop(&mut self) -> Option<RoutineFrame> {
        let frame = self.frames.pop()?;
        if frame.lexical_package.is_some() {
            self.lexical_package_frames -= 1;
        }
        Some(frame)
    }

    /// Drop every frame above `len`.
    ///
    /// Counting the dropped tail is `O(dropped)`, not `O(depth)`: the frames
    /// being removed are exactly the ones whose contribution has to go.
    pub(crate) fn truncate(&mut self, len: usize) {
        if len >= self.frames.len() {
            return;
        }
        let dropped = self.frames[len..]
            .iter()
            .filter(|f| f.lexical_package.is_some())
            .count();
        self.lexical_package_frames -= dropped;
        self.frames.truncate(len);
    }
}

impl std::ops::Deref for RoutineStack {
    type Target = Vec<RoutineFrame>;

    /// Read-only access to the frames: `len`, `is_empty`, `last`, `iter`,
    /// indexing. Deliberately NOT `DerefMut` — a `&mut Vec` would hand out
    /// `push`/`pop`/`truncate` again, past the count.
    fn deref(&self) -> &Vec<RoutineFrame> {
        &self.frames
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbol::Symbol;

    fn frame(lexical: bool) -> RoutineFrame {
        RoutineFrame {
            package: Symbol::intern("GLOBAL"),
            lexical_package: lexical.then(|| Symbol::intern("M")),
            name: Symbol::intern("f"),
            line: None,
            file: None,
            is_method: false,
            is_submethod: false,
            is_block: false,
            is_hidden_from_backtrace: false,
            def_file: None,
            invocation_id: 0,
        }
    }

    /// The count must equal what the scan it replaces would answer, after
    /// every mutation — that equivalence is the only reason the fast answer
    /// is allowed to exist.
    fn assert_agrees(stack: &RoutineStack) {
        let by_scan = stack.iter().any(|f| f.lexical_package.is_some());
        assert_eq!(stack.any_lexical_package(), by_scan);
        let counted = stack.iter().filter(|f| f.lexical_package.is_some()).count();
        assert_eq!(stack.lexical_package_frames, counted);
    }

    #[test]
    fn the_count_tracks_push_and_pop() {
        let mut stack = RoutineStack::default();
        assert_agrees(&stack);
        for lexical in [false, true, false, true, true] {
            stack.push(frame(lexical));
            assert_agrees(&stack);
        }
        assert!(stack.any_lexical_package());
        while stack.pop().is_some() {
            assert_agrees(&stack);
        }
        assert!(!stack.any_lexical_package());
    }

    #[test]
    fn the_count_tracks_truncate_including_the_no_op_cases() {
        let mut stack = RoutineStack::default();
        for lexical in [true, false, true, false, true] {
            stack.push(frame(lexical));
        }
        // A `len` at or above the current depth removes nothing.
        stack.truncate(5);
        assert_eq!(stack.len(), 5);
        assert_agrees(&stack);
        stack.truncate(9);
        assert_eq!(stack.len(), 5);
        assert_agrees(&stack);
        // Dropping the tail drops exactly the tail's contribution.
        stack.truncate(2);
        assert_eq!(stack.len(), 2);
        assert_agrees(&stack);
        assert!(stack.any_lexical_package());
        stack.truncate(1);
        assert_agrees(&stack);
        assert!(stack.any_lexical_package());
        stack.truncate(0);
        assert_agrees(&stack);
        assert!(!stack.any_lexical_package());
        // ...and an empty stack truncates without underflowing.
        stack.truncate(0);
        assert_agrees(&stack);
    }
}
