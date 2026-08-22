//! The step vocabulary of a deferred vivification path.
//!
//! A subscript chain that reaches a not-yet-existent element hands out a
//! deferred [`ValueRepr::HashEntryRef`](crate::value::ValueRepr::HashEntryRef)
//! token instead of creating anything (so a *getter* built on the same routine
//! does not vivify the path it is looking up). The token is a container root
//! plus the path walked from it; the first write walk-creates that path.
//!
//! The path used to be a `Vec<String>`, which could only describe associative
//! descent. A positional step was stringified, so `my %h; my $x := %h<g>;
//! $x[0] = 'x'` vivified `{:g(${"0" => "x"})}` where raku produces
//! `{:g($["x"])}`, and every `Positional` candidate of a path-addressing
//! library (`Crane::In`'s `return-rw container[@steps[0]]`) was unusable.
//!
//! [`EntryStep`] records *how* each step descended, so the walk-create makes
//! the container the **next** step asks for — a `Hash` for a key, an `Array`
//! for an index — and [`EntryTerminal`] names the resulting slot so a write
//! lands in a map or a vec accordingly. Both halves reuse the ordinary
//! element chokepoints ([`Value::hash_insert_through`] /
//! [`Value::assign_element_slot`]), so a `:=`-bound `ContainerRef` cell that
//! already sits at the slot is written *through* rather than replaced.

use super::{ArrayData, HashData, Value, ValueView};
use crate::gc::Gc;
use std::sync::Mutex;

/// What a deferred vivification path is anchored to.
///
/// Almost always a `Hash`: only `hash_slot_ref` / `hash_autovivify` mint a
/// token, because the array side vivifies eagerly (`array_slot_ref` grows past
/// the end). The exception is a chain link that is an *already-materialized but
/// still empty* `ContainerRef` cell — the shape `array_slot_ref` leaves behind
/// when it promotes a fresh hole. Descending it associatively
/// (`my @a; my $x := @a[0]; my $y := $x<k>; $y = 5`) must stay deferred too, so
/// the cell itself can anchor a path.
#[derive(Debug, Clone)]
pub(crate) enum EntryRoot {
    /// A hash the first step keys into.
    Hash(Gc<HashData>),
    /// A shared scalar cell; the first step's container is created *inside* it.
    Cell(Gc<Mutex<Value>>),
}

/// One step of a deferred vivification path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EntryStep {
    /// An associative subscript (`{...}` / `<...>`): descends into a `Hash`.
    Key(String),
    /// A positional subscript (`[...]`): descends into an `Array`.
    Index(usize),
}

impl EntryStep {
    /// The step rendered as a hash key. A positional step stringifies to its
    /// decimal index — the shape the path had before it could distinguish the
    /// two, kept for the flat-string consumers (diagnostics, `.gist`).
    pub fn as_key(&self) -> std::borrow::Cow<'_, str> {
        match self {
            EntryStep::Key(k) => std::borrow::Cow::Borrowed(k.as_str()),
            EntryStep::Index(i) => std::borrow::Cow::Owned(i.to_string()),
        }
    }
}

/// One level of a deferred path walk: the container a step descends *into*.
#[derive(Debug, Clone)]
enum Level {
    Hash(Gc<HashData>),
    Array(Gc<ArrayData>),
}

impl Level {
    /// The level a `Value` denotes, if it is a container this walk can descend.
    fn of(value: &Value) -> Option<Level> {
        match value.view() {
            ValueView::Hash(arc) => Some(Level::Hash(arc.clone())),
            ValueView::Array(arc, _) => Some(Level::Array(arc.clone())),
            _ => None,
        }
    }

    /// Whether this level is the container kind `step` addresses.
    fn accepts(&self, step: &EntryStep) -> bool {
        matches!(
            (self, step),
            (Level::Hash(_), EntryStep::Key(_)) | (Level::Array(_), EntryStep::Index(_))
        )
    }

    /// The terminal slot `step` names in this level, without creating anything.
    fn terminal(&self, step: &EntryStep) -> Option<EntryTerminal> {
        match (self, step) {
            (Level::Hash(arc), EntryStep::Key(k)) => {
                Some(EntryTerminal::Hash(arc.clone(), k.clone()))
            }
            (Level::Array(arc), EntryStep::Index(i)) => Some(EntryTerminal::Array(arc.clone(), *i)),
            _ => None,
        }
    }
}

impl EntryRoot {
    /// The level this root presents to `want`, READ-ONLY. A cell root resolves
    /// to whatever it currently holds; anything that is not the container kind
    /// `want` addresses has no level yet.
    fn level(&self, want: &EntryStep) -> Option<Level> {
        match self {
            EntryRoot::Hash(arc) => Level::Hash(arc.clone())
                .accepts(want)
                .then(|| Level::Hash(arc.clone())),
            EntryRoot::Cell(cell) => {
                let inner = cell.lock().unwrap_or_else(|e| e.into_inner()).clone();
                Level::of(&inner).filter(|l| l.accepts(want))
            }
        }
    }

    /// The level this root presents to `want`, CREATING it when absent. A hash
    /// root is fixed (a positional first step simply has no level, which is
    /// unreachable: `path[0]` is always a key for a hash root); an EMPTY cell
    /// root is filled with a fresh container of the kind `want` needs.
    ///
    /// A cell holding a real value is left alone and reports no level, so the
    /// write is dropped rather than clobbering it. `my %h = a => 1; my $x :=
    /// %h<a><b>` binds through the cell `hash_slot_ref` promoted the scalar
    /// leaf `1` into; rakudo raises "Cannot assign to an immutable value" on
    /// the eventual write, and overwriting the `1` with a fresh Hash would be
    /// strictly worse than the no-op this yields.
    fn level_mut(&self, want: &EntryStep) -> Option<Level> {
        if let Some(level) = self.level(want) {
            return Some(level);
        }
        match self {
            EntryRoot::Hash(_) => None,
            EntryRoot::Cell(cell) => {
                let mut guard = cell.lock().unwrap_or_else(|e| e.into_inner());
                if !is_container_hole(&guard) {
                    return None;
                }
                let fresh = fresh_level_for(want);
                let level = Level::of(&fresh).expect("fresh_level_for builds a container");
                *guard = fresh;
                Some(level)
            }
        }
    }
}

/// Whether a slot is an *empty* one a deferred path may fill: the `Nil` or type
/// object an unwritten scalar/array hole holds. Mirrors `ensure_array_child`'s
/// `is_hole`. A slot holding anything else is real data the walk must not
/// overwrite.
pub(crate) fn is_container_hole(value: &Value) -> bool {
    matches!(value.view(), ValueView::Nil | ValueView::Package(..))
}

/// A fresh, empty container of the kind `step` descends into.
fn fresh_level_for(step: &EntryStep) -> Value {
    match step {
        EntryStep::Key(_) => Value::hash(std::collections::HashMap::new()),
        EntryStep::Index(_) => Value::real_array(Vec::new()),
    }
}

/// The container slot a deferred path terminates at, once located.
///
/// A hash terminal is `(hash, key)`; an array terminal is `(array, index)`.
/// Both are the *physical* container the token's root reaches, so a write here
/// is observed by every holder of the enclosing container.
#[derive(Debug, Clone)]
pub(crate) enum EntryTerminal {
    Hash(Gc<HashData>, String),
    Array(Gc<ArrayData>, usize),
}

impl EntryTerminal {
    /// Store `val` at this slot, growing an array to reach the index. Goes
    /// through the element write chokepoints, so an existing `ContainerRef`
    /// cell at the slot (a live `:=` binding) is written through, not replaced.
    pub(crate) fn insert(&self, val: Value) {
        match self {
            EntryTerminal::Hash(arc, key) => {
                // SAFETY: aliased in-place mutation of a shared hash; see
                // `gc_contents_mut`. No borrow into the map is live across the write.
                let data = unsafe { crate::value::gc_contents_mut(arc) };
                Value::hash_insert_through(&mut data.map, key.clone(), val);
            }
            EntryTerminal::Array(arc, idx) => {
                // SAFETY: aliased in-place mutation of a shared array; see
                // `gc_contents_mut`. No borrow into the items is live across the write.
                let data = unsafe { crate::value::gc_contents_mut(arc) };
                let hole = array_hole(data);
                while data.len() <= *idx {
                    data.push(hole.clone());
                }
                Value::assign_element_slot(&mut data[*idx], val);
            }
        }
    }

    /// The raw value currently stored at this slot, without decontainerizing
    /// and without creating anything.
    pub(crate) fn peek(&self) -> Option<Value> {
        match self {
            EntryTerminal::Hash(arc, key) => {
                // SAFETY: a shared read of the aliased container, mirroring the
                // other deferred-path walks. The clone ends the borrow before
                // any caller can mutate through `gc_contents_mut`.
                let data: &HashData = unsafe { &*Gc::as_ptr(arc) };
                data.get(key.as_str()).cloned()
            }
            EntryTerminal::Array(arc, idx) => {
                // SAFETY: as above.
                let data: &ArrayData = unsafe { &*Gc::as_ptr(arc) };
                data.get(*idx).cloned()
            }
        }
    }

    /// Whether both terminals name the same physical slot of the same
    /// container — the container-identity (`=:=`) test for a deferred token.
    pub(crate) fn same_slot(&self, other: &EntryTerminal) -> bool {
        match (self, other) {
            (EntryTerminal::Hash(a, ak), EntryTerminal::Hash(b, bk)) => {
                Gc::ptr_eq(a, b) && ak == bk
            }
            (EntryTerminal::Array(a, ai), EntryTerminal::Array(b, bi)) => {
                Gc::ptr_eq(a, b) && ai == bi
            }
            _ => false,
        }
    }
}

/// The value a missing array slot is filled with — the declared element type
/// object, or the `is default(...)` value. Mirrors `array_slot_ref`.
fn array_hole(data: &ArrayData) -> Value {
    data.default
        .as_ref()
        .map(|d| (**d).clone())
        .unwrap_or_else(|| {
            Value::Package(crate::symbol::Symbol::intern(
                data.value_type.as_deref().unwrap_or("Any"),
            ))
        })
}

impl Value {
    /// Walk-CREATE the intermediate levels of a deferred token's `path` and
    /// return the terminal slot so the caller can insert.
    ///
    /// Each intermediate level is created as the container the **next** step
    /// asks for — a `Hash` for [`EntryStep::Key`], an `Array` for
    /// [`EntryStep::Index`] — so `%h<g>[0] = 'x'` produces `{:g($["x"])}` and
    /// not the `{"0" => "x"}` a uniformly-hash walk produced. A level that
    /// exists but is the wrong kind is replaced, matching what the hash-only
    /// walk always did for a non-hash intermediate.
    pub(crate) fn hash_entry_terminal(&self) -> Option<EntryTerminal> {
        let ValueView::HashEntryRef { root, path, .. } = self.view() else {
            return None;
        };
        let (last, intermediates) = path.split_last()?;
        let mut cur = root.level_mut(path.first()?)?;
        for (i, step) in intermediates.iter().enumerate() {
            // The kind the NEXT step needs; the level we are creating here
            // exists only to be descended by it.
            let wanted = &path[i + 1];
            let slot = cur.terminal(step)?;
            let child = match slot.peek() {
                Some(v) => {
                    // See through a `:=`-bound cell so an already-materialized
                    // intermediate is descended rather than clobbered.
                    let inner = match v.view() {
                        ValueView::ContainerRef(cell) => {
                            cell.lock().unwrap_or_else(|e| e.into_inner()).clone()
                        }
                        _ => v,
                    };
                    match Level::of(&inner) {
                        Some(level) if level.accepts(wanted) => Some(level),
                        _ => None,
                    }
                }
                None => None,
            };
            cur = match child {
                Some(level) => level,
                None => {
                    let fresh = fresh_level_for(wanted);
                    let level = Level::of(&fresh).expect("fresh_level_for builds a container");
                    slot.insert(fresh);
                    level
                }
            };
        }
        cur.terminal(last)
    }

    /// Locate the terminal slot a deferred token points at, walking its `path`
    /// READ-ONLY. Returns `None` if any intermediate level is missing or is not
    /// the container kind its step addresses — the deferred path is then not
    /// materialized, so it has no stable container identity.
    ///
    /// Deliberately stricter than the write walk above: it does NOT see through
    /// a `ContainerRef` at an intermediate level. A deferred (non-eager) token
    /// connects on read ONLY through the cell a write *through the bound var*
    /// installs at its terminal (see `hash_entry_read`); loosening the
    /// intermediate levels would retro-bind paths rakudo leaves unbound
    /// (`t/phantom-entry-bind.t`).
    pub(crate) fn hash_entry_locate(&self) -> Option<EntryTerminal> {
        let ValueView::HashEntryRef { root, path, .. } = self.view() else {
            return None;
        };
        let (last, intermediates) = path.split_last()?;
        let mut cur = root.level(path.first()?)?;
        for step in intermediates {
            let child = cur.terminal(step)?.peek()?;
            let level = Level::of(&child)?;
            cur = level;
        }
        cur.terminal(last)
    }
}
