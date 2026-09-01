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
//!
//! Both container kinds anchor a path: [`EntryRoot::Hash`] for a key that does
//! not exist yet, [`EntryRoot::Array`] for an index past the end. The array
//! side used to vivify eagerly ([`Value::array_slot_ref`] grew the vec at
//! *bind* time), so `my @a = 1, 2; my $r := @a[5]` reported six elements where
//! raku reports two; it now mints a token like the hash side and
//! [`EntryTerminal`]'s array arm fills the gap on the first write.

use super::{ArrayData, HashData, Value, ValueView};
use crate::gc::Gc;

/// What a deferred vivification path is anchored to.
///
/// Usually a `Hash` (`hash_slot_ref` / `hash_autovivify` mint one for a key
/// that is not there yet), but an `Array` anchors a token just the same:
/// `array_slot_ref` hands one out for an index past the end, so
/// `my @a = 1, 2; my $r := @a[5]` leaves `@a` two elements long until something
/// is actually written through `$r` — raku's behaviour, and the shape the hash
/// side always had. The third root is a chain link that is an
/// *already-materialized but still empty* `ContainerRef` cell — what
/// `array_slot_ref` leaves behind when it promotes a fresh hole. Descending it
/// associatively (`my @a; my $x := @a[0]; my $y := $x<k>; $y = 5`) must stay
/// deferred too, so the cell itself can anchor a path.
#[derive(Debug, Clone)]
pub(crate) enum EntryRoot {
    /// A hash the first step keys into.
    Hash(Gc<HashData>),
    /// An array the first step indexes into.
    Array(Gc<ArrayData>),
    /// A shared scalar cell; the first step's container is created *inside* it.
    Cell(Gc<crate::value::ContainerCell>),
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
            EntryRoot::Array(arc) => Level::Array(arc.clone())
                .accepts(want)
                .then(|| Level::Array(arc.clone())),
            EntryRoot::Cell(cell) => {
                let inner = cell.lock().unwrap_or_else(|e| e.into_inner()).clone();
                Level::of(&inner).filter(|l| l.accepts(want))
            }
        }
    }

    /// The level this root presents to `want`, CREATING it when absent. A hash
    /// or array root is fixed (a step of the other shape simply has no level,
    /// which is unreachable: `path[0]` always matches the root's kind); an
    /// EMPTY cell root is filled with a fresh container of the kind `want`
    /// needs.
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
            EntryRoot::Hash(_) | EntryRoot::Array(_) => None,
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
///
/// ADR-0040: the walk-create stores this container into an element slot of the
/// level above it, and an element of a real `Array`/`Hash` is a `Scalar`
/// container -- so a level vivified on the way down a deferred path itemizes
/// exactly like one a direct `%h<a><b> = ...` vivifies (raku renders
/// `my %h; my $r := %h<a>[1]; $r = "x"; %h<a>.raku` as `$[Any, "x"]`).
/// Itemizing an `Array` only flips its `ArrayKind` tag (a `Hash`, a bool on the
/// repr), so the shared backing `Gc` the walk keeps descending through is
/// untouched.
fn fresh_level_for(step: &EntryStep) -> Value {
    let fresh = match step {
        EntryStep::Key(_) => Value::hash(std::collections::HashMap::new()),
        EntryStep::Index(_) => Value::real_array(Vec::new()),
    };
    fresh.itemize_for_element_store()
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

    /// The element type constraint of the container this terminal writes into,
    /// as `(of-type, owner-sigil)` — the same pair `array_slot_ref` /
    /// `hash_slot_ref` seed onto a cell they promote (ADR-0036 slice 4).
    ///
    /// A DEFERRED vivification token (`my Str @a; my $r := @a[5]`) never
    /// reaches those primitives: the slot does not exist yet, so the token
    /// materializes into a *fresh* cell at the first write. Without picking the
    /// constraint up here that write bypasses the element type check the
    /// equivalent in-range bind and the direct `@a[5] = v` store both perform.
    pub(crate) fn element_constraint(&self) -> Option<(String, &'static str)> {
        match self {
            // SAFETY: a shared read of the aliased container, mirroring `peek`.
            // The clone ends the borrow before any caller can mutate through
            // `gc_contents_mut`.
            EntryTerminal::Hash(arc, _) => {
                let data: &HashData = unsafe { &*Gc::as_ptr(arc) };
                data.value_type.clone().map(|ty| (ty, "%"))
            }
            // SAFETY: as above.
            EntryTerminal::Array(arc, _) => {
                let data: &ArrayData = unsafe { &*Gc::as_ptr(arc) };
                data.value_type.clone().map(|ty| (ty, "@"))
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

    /// What a read of this slot yields while the deferred bind is still
    /// UNCONNECTED — nothing was ever written through the bound variable.
    ///
    /// A hash entry that does not exist reads as `Any`. An array slot past the
    /// end reads as the array's hole value, which is `Any` only by default:
    /// `my Int @i; my $r := @i[5]` reads `Int`, and an `is default(42)` array
    /// reads `42` (verified against rakudo). That is the same value
    /// [`EntryTerminal::insert`] fills the gap with, so the read agrees with
    /// what the eventual write leaves behind.
    pub(crate) fn unwritten_read(&self) -> Value {
        match self {
            EntryTerminal::Hash(..) => Value::Package(crate::symbol::Symbol::intern("Any")),
            // SAFETY: a shared read of the aliased container, mirroring `peek`.
            // The clone ends the borrow before any caller can mutate through
            // `gc_contents_mut`.
            EntryTerminal::Array(arc, _) => array_hole(unsafe { &*Gc::as_ptr(arc) }),
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
