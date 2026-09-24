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

use crate::value::Value;

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
    /// A spare, empty argument vector for the generic `nqp::` op
    /// (`TrOp::NqpOpGen`). Never holds a value outside that one call.
    pub(crate) nqp_args: Vec<Value>,
    /// `CallGen` sites linked to the TRIR routine they reach (ADR-0112 Step
    /// 1, `gen_link.rs`).
    pub(crate) gen_links: super::gen_link::GenLinks,
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
    }
}
