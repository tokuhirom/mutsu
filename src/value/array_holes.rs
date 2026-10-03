//! `ArrayData`'s hole model (#10360): which slots are holes (`hole_at`), what
//! a new hole stores (`gap_fill`), and how element writes and list-assignment
//! copies keep the `initialized` record exact.

use super::{ArrayData, Value};

impl ArrayData {
    /// Whether index `i` is a hole (a deleted slot or an autovivification
    /// gap), as opposed to an explicitly-assigned element. The canonical
    /// predicate mirrored by `:exists`/`:k`/`:p`: a type-object slot (`Any`,
    /// or the element type of a typed array) is a gap unless the embedded
    /// `initialized` set records an explicit assignment (`None` means
    /// bulk-constructed — no gaps). ADR-0049 retired `Nil` as a second, less
    /// precise hole sentinel: a real `Array` element is a `Scalar` container
    /// and can never actually hold `Nil` (every element store decays a
    /// stored `Nil` to the container's own default), so `initialized` is now
    /// the SOLE hole discriminator. A completeness probe (a temporary
    /// `debug_assert!` in the now-deleted `Some(ValueView::Nil) => ...` arm)
    /// ran clean under the full local `t/` suite and a broad roast sweep
    /// before this arm was removed -- see ADR-0049 §5 open question 1 and
    /// §8's slice 5 entry.
    ///
    /// An array with an `is default(...)` value stores that value in its holes
    /// ([`Self::gap_fill`], #10360), so a slot holding the very default object
    /// is a gap candidate as well; `initialized` then decides, exactly as for
    /// the type-object marker.
    pub fn hole_at(&self, i: usize) -> bool {
        let Some(slot) = self.items[self.head..].get(i) else {
            return true;
        };
        // A slot `for @a` / `.values` aliased into an element cell is still a
        // hole until something is written through the cell (#10360): look at
        // what the cell holds.
        let deref;
        let slot = if slot.is_container_ref() {
            deref = slot.deref_container();
            &deref
        } else {
            slot
        };
        let is_gap_marker = match slot.view() {
            // `Mu` is the marker a Match's `.list` view leaves in an
            // unbound positional capture slot (`match_list_view`).
            crate::value::ValueView::Package(name) => {
                name == "Any"
                    || name == "Mu"
                    || self.value_type.as_deref().is_some_and(|t| name == t)
            }
            _ => self
                .stored_default()
                .is_some_and(|d| crate::value::identity::values_same_object(slot, d)),
        };
        is_gap_marker && self.initialized.as_ref().is_some_and(|s| !s.contains(&i))
    }

    /// The `is default(...)` value a hole of this array stores, if any. A
    /// `Nil` default is not stored (a `Scalar` element never holds `Nil`), so
    /// such an array keeps the type-object marker.
    // Cost: O(1).
    pub(crate) fn stored_default(&self) -> Option<&Value> {
        self.default.as_deref().filter(|d| !d.is_nil())
    }

    /// The value a newly made hole of this array holds -- a slot `:delete`
    /// emptied, or a gap an out-of-range store grew: the `is default(...)`
    /// value when the array has one (#10360), so every whole-array view and
    /// iteration that reads the slots directly sees the default, as `@a[$i]`
    /// does; otherwise `marker`, the type-object hole marker the caller uses.
    /// Hole-ness itself is recorded by `initialized` ([`Self::hole_at`]).
    // Cost: O(1).
    pub(crate) fn gap_fill(&self, marker: Value) -> Value {
        match self.stored_default() {
            Some(d) => d.clone(),
            None => marker,
        }
    }

    /// The live elements as an iteration or a whole-array view reads them: a
    /// hole -- a `:delete`d slot or a gap an out-of-range store grew -- reads
    /// as the container's `is default(...)` value, exactly as `@a[$i]` does
    /// (`resolve_array_entry` in the VM's element read).
    ///
    /// A hole made by `:delete` or by an out-of-range store already holds the
    /// default ([`Self::gap_fill`], #10360), so this only still substitutes
    /// for a hole some other writer left holding the type-object marker. A
    /// `Nil`/absent default, or an array without such a hole, borrows the
    /// elements untouched.
    // Cost: O(1) for an array with no `is default` value or no `initialized`
    // set (borrowed); O(e), e = elements, to look for holes (and to copy when
    // there are any).
    pub fn items_with_default(&self) -> std::borrow::Cow<'_, [Value]> {
        let items = self.items();
        let Some(default) = self.default.as_deref().filter(|d| !d.is_nil()) else {
            return std::borrow::Cow::Borrowed(items);
        };
        // A bulk-constructed array (`initialized == None`) has no gaps.
        if self.initialized.is_none()
            || !items.iter().enumerate().any(|(i, v)| {
                matches!(v.view(), crate::value::ValueView::Package(_)) && self.hole_at(i)
            })
        {
            return std::borrow::Cow::Borrowed(items);
        }
        std::borrow::Cow::Owned(
            items
                .iter()
                .enumerate()
                .map(|(i, v)| {
                    if self.hole_at(i) {
                        default.clone()
                    } else {
                        v.clone()
                    }
                })
                .collect(),
        )
    }

    /// Turn a copy of an array's state into the contents a list assignment
    /// (`@b = @a`, `my @b = @a`) stores (#10360): iterating the source yields
    /// each hole as its `is default(...)` value (or the type-object marker),
    /// and the target holds a real container for every one of them, so none
    /// is a hole any more. The source's `is default` is not the target's: it
    /// is dropped here, and a target with its own default sets it again.
    // Cost: O(1) for an array without tracked holes; O(e), e = elements,
    // otherwise.
    pub(crate) fn settle_for_list_assignment(&mut self) {
        if self.initialized.is_some() {
            let resolved = self.items_with_default().into_owned();
            *self.items_mut() = resolved;
            self.initialized = None;
        }
        self.default = None;
    }

    /// Record an explicit assignment to index `i` while preserving the
    /// all-present meaning of `initialized == None` for bulk-constructed
    /// arrays. Once a bulk array receives an element-wise write, materialize
    /// its existing range before recording the write; starting with an empty
    /// set would incorrectly turn untouched explicit `Any` elements into
    /// holes.
    pub(crate) fn mark_initialized(&mut self, i: usize) {
        let len = self.len();
        self.initialized
            .get_or_insert_with(|| (0..len).collect())
            .insert(i);
    }
}

impl ArrayData {
    /// `@a[$i] = $v`'s store into the array node, the one body the `[]=`
    /// opcode's fast lane and the `ASSIGN-POS` method share (ADR-0118):
    /// write `value` at `i`, growing the array with `Any` hole markers when
    /// `i` is past the end, and record `i` as explicitly assigned so the
    /// grown gaps -- and only they -- read as holes (`:exists` is False).
    /// `ASSIGN-POS` used to rebuild the array instead, losing that record,
    /// so every gap it grew claimed to exist.
    // Cost: O(1) amortized; O(i - e) when growing, i = index, e = elements.
    pub(crate) fn store_element(&mut self, i: usize, value: Value) {
        let len = self.len();
        // Record the write BEFORE growing, or the new gaps would be
        // materialized as present.
        self.mark_initialized(i);
        if i >= len {
            let fill = self.gap_fill(Value::package(crate::symbol::wk::any()));
            self.resize(i + 1, fill);
        }
        self.live_mut()[i] = value;
    }
}
