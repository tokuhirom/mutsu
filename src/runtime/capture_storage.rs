//! The `@!list` / `%!hash` storage of an instance of a user `Capture`
//! subclass, as nqp code reaches it (#16888).
//!
//! Rakudo's `Capture` keeps its positionals in a raw `@!list` and its nameds in
//! a raw `%!hash`, and nqp code reads and installs them directly. EEL's
//! `Event is Capture` shares one hash between the event and a `Hash` its
//! actions write captures into:
//!
//! ```raku
//! nqp::bindattr(self, Capture, '%!hash',
//!     nqp::getattr(nqp::decont(%captures), Map, '$!storage'));   # EEL-captures
//! my $hash := nqp::create(Hash);                                   # EEL-hash
//! nqp::bindattr(nqp::decont($hash), Map, '$!storage',
//!     nqp::getattr(self, Capture, '%!hash'));
//! ```
//!
//! An instance keeps its positionals in the `Capture` payload
//! (`native_base::CAPTURE_PAYLOAD`) and its nameds in a hash node of their own
//! (`native_base::CAPTURE_HASH`), so the node is what both binds share:
//!
//! * reading `%!hash` answers the raw view of that node, and binding it
//!   installs the node it is given;
//! * a `Map`'s `$!storage` bind copies the storage into the map's own node and
//!   re-points the raw handle *variable* at it (ADR-12666 §9). The handle here
//!   is held by the capture instead, so the capture whose `%!hash` was read
//!   last is re-pointed at the map's node too
//!   ([`Interpreter::capture_hash_follow_bind`]). One slot suffices: nqp code
//!   reads the storage and installs it in consecutive statements.
//!
//! The capture's `.hash`, `<key>` and `.elems` read the live node
//! (`native_base::subclass_payload`).

use super::Interpreter;
use crate::builtins::native_base::{CAPTURE_HASH, CAPTURE_PAYLOAD};
use crate::value::{Value, ValueView};
use std::cell::RefCell;

thread_local! {
    /// The capture whose `%!hash` node nqp code read last, with that node.
    static HANDED_OUT: RefCell<Option<(Value, Value)>> = const { RefCell::new(None) };
}

impl Interpreter {
    /// `nqp::getattr($capture, Capture, '@!list' | '%!hash')` on an instance of
    /// a user `Capture` subclass. `None` when `obj` is not one, or `bare` is
    /// neither attribute; `Some(None)` when it has no named storage yet (rakudo's
    /// null `%!hash`, which `nqp::ishash` rejects).
    // Cost: O(1) for `%!hash`; O(p) for `@!list`, p = positionals.
    pub(crate) fn capture_storage_read(obj: &Value, bare: &str) -> Option<Option<Value>> {
        let ValueView::Instance { attributes, .. } = obj.view() else {
            return None;
        };
        let map = attributes.as_map();
        let payload = map.get(CAPTURE_PAYLOAD)?;
        match bare {
            "list" => match payload.view() {
                ValueView::Capture { positional, .. } => {
                    Some(Some(Value::vm_array(positional.to_vec())))
                }
                _ => Some(None),
            },
            "hash" => {
                let Some(node) = map.get(CAPTURE_HASH).and_then(Value::raw_storage_view) else {
                    return Some(None);
                };
                HANDED_OUT.with(|slot| *slot.borrow_mut() = Some((obj.clone(), node.clone())));
                Some(Some(node))
            }
            _ => None,
        }
    }

    /// `nqp::bindattr($capture, Capture, '@!list' | '%!hash', $raw)` on an
    /// instance of a user `Capture` subclass: `%!hash` installs the hash node
    /// `$raw` is (shared, not copied), `@!list` replaces the positionals.
    /// `false` when `obj` is not one or the attribute is neither.
    // Cost: O(1) for `%!hash`; O(p) for `@!list`, p = positionals.
    pub(crate) fn capture_storage_bind(obj: &Value, attr_key: &str, raw: &Value) -> bool {
        let ValueView::Instance { attributes, .. } = obj.view() else {
            return false;
        };
        let Some(payload) = attributes.as_map().get(CAPTURE_PAYLOAD).cloned() else {
            return false;
        };
        match attr_key {
            "hash" => {
                let ValueView::Hash(_) = raw.view() else {
                    return false;
                };
                attributes.bind_attr_replace(CAPTURE_HASH, raw.clone());
                true
            }
            "list" => {
                let Some(elems) = Self::nqp_elems_of(raw) else {
                    return false;
                };
                let named = match payload.view() {
                    ValueView::Capture { named, .. } => (*named).clone(),
                    _ => Default::default(),
                };
                attributes.bind_attr_replace(CAPTURE_PAYLOAD, Value::capture(elems, named));
                true
            }
            _ => false,
        }
    }

    /// After a `Map`'s `$!storage` was bound from `storage` into `container`'s
    /// node: if `storage` is the `%!hash` node a capture handed out last, that
    /// capture now shares `container`'s node (see the module docs).
    // Cost: O(1).
    pub(crate) fn capture_hash_follow_bind(storage: &Value, container: &Value) {
        let ValueView::Hash(src) = storage.view() else {
            return;
        };
        let owner = HANDED_OUT.with(|slot| {
            let mut slot = slot.borrow_mut();
            let matches = slot.as_ref().is_some_and(|(_, node)| match node.view() {
                ValueView::Hash(handed) => crate::gc::Gc::ptr_eq(&handed, &src),
                _ => false,
            });
            if matches { slot.take().map(|(owner, _)| owner) } else { None }
        });
        if let Some(owner) = owner
            && let ValueView::Instance { attributes, .. } = owner.view()
        {
            attributes.bind_attr_replace(CAPTURE_HASH, container.clone());
        }
    }
}
