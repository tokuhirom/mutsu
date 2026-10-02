//! Strict force of the two generator-backed LazyList shapes whose cache is
//! only ever a prefix: an endpoint-less closure sequence (`1, {$_+1} ... *`)
//! and a triangle reduce (`[\+] 1..*`).
//!
//! Both used to answer whatever prefix happened to be generated (32-33
//! closure-sequence elements, 200_000 scan elements) as the complete list
//! (#10861). Rakudo never returns from such a force; mutsu answers
//! `X::Cannot::Lazy`, the verdict a strict force of an infinite map/grep pipe
//! and of an infinite sequence spec already reach.

use super::*;

/// How many elements a strict force drives an endpoint-less closure sequence
/// before concluding it is infinite. The generator can still end it on its own
/// (`last`, or returning `Nil`/an empty Slip), so a strict force tries; the
/// same bounded attempt the map/grep pipe force makes (`EAGER_FORCE_CAP`).
const CLOSURE_SEQ_EAGER_CAP: usize = 1_000_000;

impl Interpreter {
    /// Strict force of an endpoint-less closure sequence: the complete list
    /// when the generator ends within [`CLOSURE_SEQ_EAGER_CAP`] elements,
    /// otherwise `X::Cannot::Lazy`. `None` when `list` is not such a sequence.
    ///
    /// Cost: O(n) generator calls, n = min(sequence length, the cap).
    pub(crate) fn strict_force_unbounded_closure_seq(
        &mut self,
        list: &LazyList,
    ) -> Option<Result<Vec<Value>, RuntimeError>> {
        let state = list.closure_seq.as_ref()?;
        let finished = {
            let state = state.lock().unwrap();
            if state.endpoint.is_some() {
                return None;
            }
            state.finished
        };
        if !finished && let Err(e) = self.extend_closure_sequence(list, CLOSURE_SEQ_EAGER_CAP) {
            return Some(Err(e));
        }
        if !list
            .closure_seq
            .as_ref()
            .is_some_and(|state| state.lock().unwrap().finished)
        {
            return Some(Err(Self::infinite_strict_force_error()));
        }
        Some(Ok(list.cache.lock().unwrap().clone().unwrap_or_default()))
    }

    /// Strict force of a triangle reduce: the scan over the strictly forced
    /// source. An infinite range source answers `X::Cannot::Lazy` at once; a
    /// lazy source applies its own strict-force verdict (a finite gather scans
    /// to its end, an infinite pipe throws). `None` when `list` is not a scan.
    ///
    /// Cost: O(n) reduction steps, n = the source's length.
    pub(crate) fn strict_force_scan(
        &mut self,
        list: &LazyList,
    ) -> Option<Result<Vec<Value>, RuntimeError>> {
        let source = list.scan_spec.as_ref()?.lock().unwrap().source.clone();
        if crate::builtins::is_infinite_range(&source) {
            return Some(Err(Self::infinite_strict_force_error()));
        }
        let source_len = match source.view() {
            ValueView::LazyList(inner) => match self.force_lazy_list_vm(&inner) {
                Ok(items) => items.len(),
                Err(e) => return Some(Err(e)),
            },
            // A finite range or list: the scan walk stops at its end.
            _ => usize::MAX,
        };
        let (computed, cached) = {
            let spec = list.scan_spec.as_ref()?.lock().unwrap();
            let cached = list.cache.lock().unwrap().as_ref().map_or(0, Vec::len);
            (spec.computed_count, cached)
        };
        let needed = cached.saturating_add(source_len.saturating_sub(computed));
        Some(self.force_scan_lazy_list(list, needed))
    }

    /// The error a strict force of an infinite list answers.
    ///
    /// Cost: O(1).
    fn infinite_strict_force_error() -> RuntimeError {
        RuntimeError::typed_msg(
            "X::Cannot::Lazy",
            "Cannot coerce an infinite lazy list to a strict list",
        )
    }
}
