//! `.Stringy` of an instance of a user subclass of `Str` (#11026).
//!
//! `class MyStr is Str {}; MyStr.new(value => "q")` is an ordinary instance
//! whose string lives in the reserved `__mutsu_str_value` attribute
//! (`runtime::seed_native_subclass_payloads`). Rakudo's `Str.Stringy` returns
//! `self` — the instance already *is* a `Str` — so a subclass that declares
//! its own `Str` but no `Stringy` still renders its *payload* wherever a
//! string context asks for `.Stringy`: interpolation and an explicit
//! `.Stringy`. The native `Str` operators (infix `~`, `eq`, `join`) read the
//! payload even past a user `Stringy`, and only paths that call `.Str` itself
//! (prefix `~`, `.Str`) reach a user `Str`. This is the mirror image of an `Int`/`Num`
//! subclass, whose `Numeric.Stringy` is `self.Str` (#10992).

use crate::runtime::Interpreter;
use crate::value::{Value, ValueView};

/// The string payload of a `Str`-subclass instance, `None` for any other
/// value. The native `Str` operators (infix `~`, `eq`, `join`) read it
/// directly, whatever `Str`/`Stringy` the subclass declares.
// Cost: O(1) — one attribute lookup.
pub(crate) fn str_subclass_payload(value: &Value) -> Option<Value> {
    let ValueView::Instance { attributes, .. } = value.view() else {
        return None;
    };
    attributes.as_map().get("__mutsu_str_value").cloned()
}

impl Interpreter {
    /// The string payload a `Str`-subclass instance answers `.Stringy` with,
    /// when its class does not declare its own `Stringy`; `None` for any
    /// other value.
    // Cost: O(1) — one attribute lookup and one user-method probe.
    pub(crate) fn str_subclass_stringy_payload(&mut self, value: &Value) -> Option<Value> {
        let payload = str_subclass_payload(value)?;
        let ValueView::Instance { class_name, .. } = value.view() else {
            return None;
        };
        if self.has_user_method(&class_name.resolve(), "Stringy") {
            return None;
        }
        Some(payload)
    }
}
