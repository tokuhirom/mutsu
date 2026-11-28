//! Building a named-argument `Pair` cheaply: a key shared with its constant,
//! and the value replaced in place.
//!
//! A named argument whose value is a caller variable travels as a `Pair` that
//! still holds the `VarRef` naming the variable (#19384): the call op decides
//! whether the callee needs the variable's shared container or only its value,
//! and then rewrites the Pair's value. The Pair was minted by `MakeNamedArg` a
//! few ops earlier and nothing else holds it, so the rewrite reuses its node.

use super::{PairData, PairKey, Value, ValueRepr};

impl Clone for PairData {
    /// A copy is a new Pair object, so it gets a fresh identity (`.WHICH`).
    fn clone(&self) -> Self {
        PairData {
            key: match &self.key {
                PairKey::String(s) => PairKey::String(s.clone()),
                PairKey::Shared(body) => PairKey::Shared(body.clone()),
                PairKey::Value(v) => PairKey::Value(v.clone()),
            },
            value: self.value.clone(),
            source: self.source.clone(),
            which_id: Default::default(),
        }
    }
}

impl Value {
    /// A string-keyed (named) Pair `key => val` whose key is the `Str` value
    /// `key`. A flat body is shared with `key` instead of copied: a named
    /// argument's key is a constant, and copying it was a `String` allocation
    /// (and a free) per argument per call (#19384). Any other key is
    /// stringified into a copy, as [`Value::pair`] stores it.
    // Cost: O(1) for a flat `Str`; O(m) otherwise, m = bytes of the key.
    pub(crate) fn pair_str_key(key: &Value, val: Value) -> Value {
        if let ValueRepr::Str(body) = key.clone().into_repr()
            && matches!(*body, super::StrBody::Flat(_))
        {
            return Value::from_repr(ValueRepr::Pair(crate::gc::Gc::new(PairData {
                key: PairKey::Shared(body),
                value: val,
                source: None,
                which_id: Default::default(),
            })));
        }
        Value::pair(key.to_string_value(), val)
    }

    /// Put `new` as the value of this string-keyed `Pair` and return the value
    /// it held. The Pair's node is reused when this handle is its only owner,
    /// and copied otherwise, so another holder never sees the change. Any other
    /// value is left alone and `None` is returned.
    // Cost: O(1); one node copy when the Pair is shared.
    pub(crate) fn swap_pair_value(&mut self, new: Value) -> Option<Value> {
        match std::mem::replace(self, Value::NIL).into_repr() {
            ValueRepr::Pair(mut data) => {
                let old = std::mem::replace(&mut data.make_mut().value, new);
                *self = Value::from_repr(ValueRepr::Pair(data));
                Some(old)
            }
            other => {
                *self = Value::from_repr(other);
                None
            }
        }
    }
}
