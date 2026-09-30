//! The items a `for` loop iterates, one `(index, item)` at a time: a list
//! materialized up front, a live Array read in place (#9158), or a streamed
//! `.map`/`.grep` pulled one iteration's worth at a time (#9936).

use super::vm_control_ops::ForLoopSpec;
use super::vm_for_loop_map_grep::MapGrepStream;
use super::*;

/// The items a `for` loop body iterates, as `(index, item)`.
pub(super) enum ForItemIter<'a> {
    /// A list materialized before the loop, skipping to the resume index.
    Owned(std::iter::Skip<std::iter::Enumerate<std::vec::IntoIter<Value>>>),
    /// A plain Array read in place, one element per iteration, re-reading its
    /// live length each time (#9158): `for @a { last }` copies nothing, and
    /// an element pushed or stored by the body is seen when the loop gets
    /// there, as Rakudo's Array iterator does.
    Live { array: Value, next: usize },
    /// A not-yet-run `.map`/`.grep` Seq, pulled one iteration's worth of
    /// elements at a time (#9936). `next` counts iterations, `pos` the
    /// stream elements read; `pending` holds the rest of an autothreaded
    /// Junction's eigenstates, and `ended` is set once the stream ran out or
    /// reached the `IterationEnd` sentinel.
    Pulled {
        stream: &'a mut MapGrepStream,
        next: usize,
        pos: usize,
        pending: std::collections::VecDeque<Value>,
        ended: bool,
    },
}

impl ForItemIter<'_> {
    /// The next item of a materialized or live source. Never called on
    /// [`ForItemIter::Pulled`], which needs the interpreter to pull.
    // Cost: O(1).
    pub(super) fn next_ready(&mut self) -> Option<(usize, Value)> {
        match self {
            ForItemIter::Pulled { .. } => None,
            ForItemIter::Owned(items) => items.next(),
            ForItemIter::Live { array, next } => {
                let ValueView::Array(items, _) = array.view() else {
                    return None;
                };
                let item = items.get(*next)?.clone();
                if item.is_iteration_end() {
                    return None;
                }
                let idx = *next;
                *next += 1;
                Some((idx, item))
            }
        }
    }
}

impl Interpreter {
    /// The next `(index, item)` a `for` loop binds, pulling it first from a
    /// streamed `.map`/`.grep` (one element, or one `arity`-element chunk for
    /// a loop that chunks its items).
    // Cost: O(1) for a materialized or live source; for a stream, one
    // callback call per source element the item needs.
    pub(super) fn next_for_loop_item(
        &mut self,
        items: &mut ForItemIter<'_>,
        arity: usize,
        spec: &ForLoopSpec,
    ) -> Result<Option<(usize, Value)>, RuntimeError> {
        let ForItemIter::Pulled {
            stream,
            next,
            pos,
            pending,
            ended,
        } = items
        else {
            return Ok(items.next_ready());
        };
        let item = if spec.chunks_items() {
            let mut chunk = Vec::with_capacity(arity);
            while chunk.len() < arity {
                match self.next_streamed_for_value(stream, pos, pending, ended, spec)? {
                    Some(v) => chunk.push(v),
                    None => break,
                }
            }
            if chunk.is_empty() {
                return Ok(None);
            }
            Value::array(chunk)
        } else {
            match self.next_streamed_for_value(stream, pos, pending, ended, spec)? {
                Some(v) => v,
                None => return Ok(None),
            }
        };
        let idx = *next;
        *next += 1;
        Ok(Some((idx, item)))
    }

    /// The next value a streamed `.map`/`.grep` hands a `for` loop: the rest
    /// of an autothreaded Junction first, then one element pulled from the
    /// stream. `None` once the stream ran out or reached `IterationEnd`.
    // Cost: O(1), plus one callback call per source element the value needs.
    fn next_streamed_for_value(
        &mut self,
        stream: &mut MapGrepStream,
        pos: &mut usize,
        pending: &mut std::collections::VecDeque<Value>,
        ended: &mut bool,
        spec: &ForLoopSpec,
    ) -> Result<Option<Value>, RuntimeError> {
        loop {
            if let Some(v) = pending.pop_front() {
                return Ok(Some(v));
            }
            if *ended {
                return Ok(None);
            }
            let Some(v) = self.for_map_grep_stream_item(stream, *pos)? else {
                *ended = true;
                return Ok(None);
            };
            *pos += 1;
            // The `IterationEnd` sentinel ends iteration wherever it sits
            // (#9809), after the partial chunk before it.
            if v.is_iteration_end() {
                *ended = true;
                return Ok(None);
            }
            // A parameter typed `Any` or narrower iterates a Junction's
            // eigenstates one by one, as the materialized path expands them.
            if spec.autothread_junctions
                && let ValueView::Junction { values, .. } = v.view()
            {
                pending.extend(values.iter().cloned());
                continue;
            }
            return Ok(Some(v));
        }
    }
}
