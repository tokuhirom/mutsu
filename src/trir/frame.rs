//! The TRIR frame stack — ADR-0110 §3.3's half of "static call linkage".
//!
//! Stage 1 gave each invocation its own pooled buffers, which was enough
//! because a Stage 1 body could not call anything. Once it can, three things
//! change at once and all three are this module's subject:
//!
//! - **Frames must nest.** A callee's slots have to sit above the caller's in
//!   one contiguous region, exactly as [`crate::runtime::locals::Locals`] does
//!   for the untyped frames (ADR-0077).
//! - **A native `is rw` parameter becomes a reference**, not a copy in and
//!   out. It holds the ABSOLUTE index of the slot it aliases, so a callee's
//!   write lands in the caller's own slot — MoarVM's `getlexref_i`, and the
//!   only shape that stays correct when the callee itself passes the
//!   parameter on. An index rather than a pointer because pushing a frame may
//!   reallocate.
//! - **The collector must see the boxed halves.** A generic call inside a
//!   TRIR body can reach a safepoint, so anything reachable only from a TRIR
//!   frame has to be a root.

use crate::value::{Value, ValueView};

/// All live TRIR frames' slots, plus the two operand stacks.
///
/// The native halves (`nl`, `ns`) hold raw `i64`/`f64`-as-bits and no
/// references at all, so the collector never has to visit them and a frame
/// teardown is a `truncate`. The boxed halves (`ol`, `os`, `outers`) hold
/// `Value`s and ARE visited (`gc_roots.rs`).
#[derive(Debug, Default)]
pub(crate) struct TrStacks {
    /// Native frame slots, oldest frame first.
    pub(crate) nl: Vec<i64>,
    /// Native operand stack.
    pub(crate) ns: Vec<i64>,
    /// Boxed frame slots, oldest frame first.
    pub(crate) ol: Vec<Value>,
    /// Boxed operand stack.
    pub(crate) os: Vec<Value>,
    /// Each live frame's resolved free variables, oldest frame first.
    pub(crate) outers: Vec<Value>,
    /// Codepoint memo for the operand-direct string reads, keyed by the
    /// string's own allocation identity and shared by every frame.
    ///
    /// It deliberately outlives a frame: a scanner calls its `nom-ws` tens of
    /// thousands of times over one document, and collecting that document's
    /// characters afresh per call was the largest single per-call cost
    /// measured in Stage 1 (345 instructions plus a malloc/free pair).
    pub(crate) chars: TrCharCache,
}

/// One frame's bases into [`TrStacks`].
#[derive(Debug, Clone, Copy)]
pub(crate) struct TrFrame {
    pub(crate) nbase: u32,
    pub(crate) obase: u32,
    pub(crate) outer_base: u32,
    /// Heights of the two operand stacks on entry, so an abnormal exit (a
    /// `Bail`, an error out of a generic call) can restore them.
    pub(crate) ns_mark: u32,
    pub(crate) os_mark: u32,
}

impl TrStacks {
    /// Open a frame of `n_native` zeroed native slots and `n_obj` `Nil` boxed
    /// slots on top of the stacks.
    pub(crate) fn push_frame(&mut self, n_native: u16, n_obj: u16) -> TrFrame {
        let frame = TrFrame {
            nbase: self.nl.len() as u32,
            obase: self.ol.len() as u32,
            outer_base: self.outers.len() as u32,
            ns_mark: self.ns.len() as u32,
            os_mark: self.os.len() as u32,
        };
        self.nl.resize(self.nl.len() + n_native as usize, 0);
        if n_obj > 0 {
            self.ol.resize(self.ol.len() + n_obj as usize, Value::NIL);
        }
        frame
    }

    /// Drop everything `frame` opened, including anything its body left on
    /// either operand stack.
    pub(crate) fn pop_frame(&mut self, frame: TrFrame) {
        self.nl.truncate(frame.nbase as usize);
        self.ol.truncate(frame.obase as usize);
        self.outers.truncate(frame.outer_base as usize);
        self.ns.truncate(frame.ns_mark as usize);
        self.os.truncate(frame.os_mark as usize);
    }

    /// Every `Value` any live TRIR frame can reach. GC roots visit this.
    pub(crate) fn boxed_slots(&self) -> impl Iterator<Item = &Value> {
        self.ol
            .iter()
            .chain(self.os.iter())
            .chain(self.outers.iter())
            .chain(self.chars.sources())
    }
}

/// A small, identity-keyed memo of one string's codepoints.
///
/// Probed by the string's own `Arc` address, so a different string can never
/// read another's entry and a store into the slot needs no invalidation. Four
/// entries: a JSON parse walks one document, its keys and its values, and
/// past that the probe is a miss either way.
#[derive(Debug, Default)]
pub(crate) struct TrCharCache {
    entries: Vec<CharMemo>,
    /// Round-robin replacement cursor.
    next: usize,
}

#[derive(Debug)]
struct CharMemo {
    src: Value,
    chars: Vec<char>,
}

/// How many strings the memo remembers at once.
const CHAR_CACHE_SLOTS: usize = 4;

impl TrCharCache {
    /// The index at which `v`'s codepoints are memoized, filling the memo if
    /// needed. `None` when `v` is not a string at all.
    pub(crate) fn index_of(&mut self, v: &Value) -> Option<usize> {
        for (i, e) in self.entries.iter().enumerate() {
            if same_string(&e.src, v) {
                return Some(i);
            }
        }
        let chars: Vec<char> = v.as_str()?.chars().collect();
        let memo = CharMemo {
            src: v.clone(),
            chars,
        };
        if self.entries.len() < CHAR_CACHE_SLOTS {
            self.entries.push(memo);
            return Some(self.entries.len() - 1);
        }
        let i = self.next;
        self.next = (self.next + 1) % CHAR_CACHE_SLOTS;
        self.entries[i] = memo;
        Some(i)
    }

    /// The codepoints at a previously answered index.
    #[inline]
    pub(crate) fn chars_at(&self, i: usize) -> &[char] {
        &self.entries[i].chars
    }

    /// The strings the memo is keeping alive, for GC roots.
    fn sources(&self) -> impl Iterator<Item = &Value> {
        self.entries.iter().map(|e| &e.src)
    }
}

/// Whether two values are the SAME string allocation.
fn same_string(a: &Value, b: &Value) -> bool {
    match (a.view(), b.view()) {
        (ValueView::Str(x), ValueView::Str(y)) => std::sync::Arc::ptr_eq(&x, &y),
        _ => false,
    }
}
