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

//!
//! It also hands out O(1)-amortized immutable snapshots of itself
//! ([`RoutineStack::snapshot`]) for a lazily rendered backtrace: a `die`
//! captures the stack it was thrown from, but only a reader of `.backtrace`
//! pays for rendering it (#9172).

use super::RoutineFrame;
use std::cell::RefCell;
use std::sync::Arc;

/// One frame of an immutable stack snapshot, linked to the frame below it.
/// Snapshots taken at different depths share their common prefix.
#[derive(Debug)]
pub(crate) struct FrameNode {
    pub(crate) frame: RoutineFrame,
    pub(crate) parent: Option<Arc<FrameNode>>,
}

impl FrameNode {
    /// The frames of the snapshot ending at `top`, outermost first.
    // Cost: O(s), s = the snapshot's depth.
    pub(crate) fn frames(top: Option<&Arc<FrameNode>>) -> Vec<RoutineFrame> {
        let mut frames = Vec::new();
        let mut node = top;
        while let Some(n) = node {
            frames.push(n.frame);
            node = n.parent.as_ref();
        }
        frames.reverse();
        frames
    }
}

impl Drop for FrameNode {
    /// Unlink iteratively: dropping the last handle on a deep snapshot would
    /// otherwise recurse once per frame.
    fn drop(&mut self) {
        let mut parent = self.parent.take();
        while let Some(node) = parent {
            match Arc::try_unwrap(node) {
                Ok(mut owned) => parent = owned.parent.take(),
                Err(_) => break,
            }
        }
    }
}

/// The interpreter's routine/block frame stack.
#[derive(Debug, Default)]
pub(crate) struct RoutineStack {
    frames: Vec<RoutineFrame>,
    /// How many live frames have `lexical_package.is_some()`.
    ///
    /// Maintained by the three mutators below, which are the only ways the
    /// `Vec` can change — see the module comment.
    lexical_package_frames: usize,
    /// Snapshot nodes for a prefix of `frames`: `nodes[i]` is the node of
    /// `frames[i]`, linked to `nodes[i - 1]`. Built on demand by
    /// [`Self::snapshot`] and cut back by `pop`/`truncate`, so it never runs
    /// past `frames` and a node always describes the live frame at its index
    /// (a frame is never modified in place — there is no `DerefMut`).
    nodes: RefCell<Vec<Arc<FrameNode>>>,
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
        self.cut_nodes();
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
        self.cut_nodes();
    }

    /// Drop the snapshot nodes of frames that are no longer live.
    #[inline]
    fn cut_nodes(&mut self) {
        let nodes = self.nodes.get_mut();
        if nodes.len() > self.frames.len() {
            nodes.truncate(self.frames.len());
        }
    }

    /// An immutable snapshot of the live frames, as the innermost frame's
    /// node (`None` for an empty stack).
    ///
    /// Only the frames pushed since the last snapshot get a node: the prefix
    /// already built is shared, so each frame is copied at most once per
    /// lifetime and the cost is charged to its push.
    // Cost: O(1) amortized (O(k), k = frames pushed since the last snapshot
    // that are still live).
    pub(crate) fn snapshot(&self) -> Option<Arc<FrameNode>> {
        let mut nodes = self.nodes.borrow_mut();
        for frame in &self.frames[nodes.len()..] {
            let node = Arc::new(FrameNode {
                frame: *frame,
                parent: nodes.last().cloned(),
            });
            nodes.push(node);
        }
        nodes.last().cloned()
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

    fn named(name: &str) -> RoutineFrame {
        RoutineFrame {
            name: Symbol::intern(name),
            ..frame(false)
        }
    }

    fn names(top: Option<&Arc<FrameNode>>) -> Vec<String> {
        FrameNode::frames(top)
            .iter()
            .map(|f| f.name.resolve())
            .collect()
    }

    #[test]
    fn a_snapshot_is_the_stack_it_was_taken_from() {
        let mut stack = RoutineStack::default();
        assert!(stack.snapshot().is_none());
        stack.push(named("a"));
        stack.push(named("b"));
        let first = stack.snapshot();
        assert_eq!(names(first.as_ref()), ["a", "b"]);
        // Popping and pushing a different frame must not leak the old node.
        stack.pop();
        stack.push(named("c"));
        stack.push(named("d"));
        let second = stack.snapshot();
        assert_eq!(names(second.as_ref()), ["a", "c", "d"]);
        // The earlier snapshot is immutable.
        assert_eq!(names(first.as_ref()), ["a", "b"]);
        // The shared prefix is one node.
        let a_of = |top: &Option<Arc<FrameNode>>| {
            let mut n = top.clone().unwrap();
            while let Some(p) = n.parent.clone() {
                n = p;
            }
            n
        };
        assert!(Arc::ptr_eq(&a_of(&first), &a_of(&second)));
        stack.truncate(1);
        stack.push(named("e"));
        assert_eq!(names(stack.snapshot().as_ref()), ["a", "e"]);
        stack.truncate(0);
        assert!(stack.snapshot().is_none());
    }

    #[test]
    fn dropping_a_deep_snapshot_does_not_recurse() {
        let mut stack = RoutineStack::default();
        for _ in 0..200_000 {
            stack.push(frame(false));
        }
        let snap = stack.snapshot();
        stack.truncate(0);
        drop(snap);
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
