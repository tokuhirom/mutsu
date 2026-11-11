//! `Supply.new($tappable)`: a supply over a user `Tappable`.
//!
//! Rakudo's `Supply` wraps a `Tappable` (src/core.c/Supply.rakumod:
//! `method new(Tappable $tappable)`), and tapping the supply calls the
//! tappable's `tap(&emit, &done, &quit, &tap)`. Ecosystem code builds custom
//! supplies this way (EEL's `MatcherSupply does Tappable`, #16888).
//!
//! mutsu's `Supply` is native, and its on-demand form already runs a producer
//! for every tap with an emitter. A `Tappable` is therefore carried as an
//! on-demand supply whose producer forwards the emitter's `emit`, `done` and
//! `quit` to the tappable's `tap`, so every Supply method and `whenever` that
//! drives an on-demand supply drives a `Tappable` too. The adapter is the same
//! shape as `container_element_proxy`: a small Raku closure, parsed once per
//! process and built once per interpreter.

use super::Interpreter;
use crate::ast::Stmt;
use crate::value::{RuntimeError, Value, ValueView};

// `tap` receives the Tap the tappable answers for this subscription and
// passes it to the caller's `:tap` callback; the Tap `tap` returns is closed
// with the emitter-driven one mutsu returns to the caller (`supply_tap_hook`).
const SUPPLY_TAPPABLE_ADAPTER_SRC: &str = r#"sub (\tappable) {
    -> $emitter {
        __mutsu_supply_tappable_tap(tappable.tap(
            -> \value { $emitter.emit(value) },
            -> { $emitter.done },
            -> \ex { $emitter.quit(ex) },
            -> \t { __mutsu_supply_tapped(t) }));
    }
}"#;

impl Interpreter {
    /// The adapter described in the module docs. `None` only if the snippet
    /// failed to build, which a test would catch.
    // Cost: O(1) after the first call.
    fn supply_tappable_adapter(&mut self) -> Option<Value> {
        if let Some(cached) = self.caches.supply_tappable_adapter.clone() {
            return Some(cached);
        }
        use std::sync::OnceLock;
        static PARSED: OnceLock<Vec<Stmt>> = OnceLock::new();
        let stmts = PARSED.get_or_init(|| {
            crate::runtime::prelude_source::parse_prelude_source(SUPPLY_TAPPABLE_ADAPTER_SRC)
                .map(|(s, _)| s)
                .unwrap_or_default()
        });
        if stmts.is_empty() {
            return None;
        }
        let value = self.eval_block_value(stmts).ok()?;
        value.as_sub()?;
        self.caches.supply_tappable_adapter = Some(value.clone());
        Some(value)
    }

    /// `Supply.new($tappable)`: an on-demand supply whose producer taps
    /// `tappable`. Any other argument list is rakudo's `X::Supply::New`.
    // Cost: O(1), plus the adapter's one-time build.
    pub(crate) fn supply_from_tappable(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let [tappable] = args else {
            return Err(RuntimeError::supply_new());
        };
        let does_tappable = match tappable.view() {
            ValueView::Instance { class_name, .. } => {
                self.class_does_role(&class_name.resolve(), "Tappable")
            }
            _ => false,
        };
        if !does_tappable {
            return Err(RuntimeError::supply_new());
        }
        let adapter = self
            .supply_tappable_adapter()
            .ok_or_else(|| RuntimeError::new("Supply.new: the Tappable adapter failed to build"))?;
        let producer = self.eval_call_on_value(adapter, vec![tappable.clone()])?;
        let supply = self.dispatch_supply_on_demand(&[producer])?;
        if let ValueView::Instance { attributes, .. } = supply.view() {
            attributes.insert(super::supply_tap_hook::TAPPABLE_SUPPLY, Value::TRUE);
        }
        Ok(supply)
    }
}
