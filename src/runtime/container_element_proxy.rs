//! The native `AT-KEY` base candidate of a container subclass, as a callable.
//!
//! Rakudo's `Baggy::AT-KEY` (and its `Setty`/`Mixy` siblings) returns a
//! `Proxy`, which is what makes the documented override idiom work:
//!
//! ```raku
//! multi method AT-KEY(::?CLASS:D: $key is raw) is raw {
//!     my &nextone := nextcallee;
//!     Proxy.new(
//!       FETCH => { nextone(self,$key) },
//!       STORE => -> $, $value { nextone(self,$key) = $value }
//!     )
//! }
//! ```
//!
//! `nextcallee` there has to hand back something that both *reads* the element
//! and can be *assigned to*. mutsu's native QuantHash element access is an
//! opcode, not a `MethodDef`, so there is no routine object to return — this
//! module supplies one, spelled in Raku exactly the way Rakudo spells it: a
//! two-argument closure that answers a `Proxy` over the element.
//!
//! `__mutsu_container_at_key` / `__mutsu_container_assign_key` are the internal
//! element protocol the Proxy is written against. They reach the instance's
//! backing store directly (`vm_baggy_subclass_delegate.rs`), never back through
//! the user's own `AT-KEY`, so the idiom above cannot recurse.
//!
//! The same shape as NativeCall's `cglobal` prelude (`runtime::run`'s
//! `NATIVECALL_SUB_PRELUDES`): a small Raku source snippet whose `Proxy`
//! bottoms out in internal builtins.

use super::Interpreter;
use crate::ast::Stmt;
use crate::value::{RuntimeError, Value};

// `is rw` marks the routine rw-capable, which is what lets
// `nextone(self,$key) = $value` write through the `Proxy` it returns
// (`assign_callable_lvalue_with_values`).
const CONTAINER_ELEMENT_PROXY_SRC: &str = r#"sub ($obj, $key) is rw {
    Proxy.new(
        FETCH => { $obj.__mutsu_container_at_key($key) },
        STORE => -> $, $value { $obj.__mutsu_container_assign_key($key, $value) }
    )
}"#;

impl Interpreter {
    /// The cached closure described in the module docs. Parsed once per
    /// process, built once per interpreter.
    pub(crate) fn container_element_proxy_base(&mut self) -> Option<Value> {
        if let Some(cached) = self.container_element_proxy.clone() {
            return Some(cached);
        }
        use std::sync::OnceLock;
        static PARSED: OnceLock<Vec<Stmt>> = OnceLock::new();
        let stmts = PARSED.get_or_init(|| {
            crate::parse_dispatch::parse_source(CONTAINER_ELEMENT_PROXY_SRC)
                .map(|(s, _)| s)
                .unwrap_or_default()
        });
        if stmts.is_empty() {
            return None;
        }
        let built: Result<Value, RuntimeError> = self.eval_block_value(stmts);
        let value = built.ok()?;
        value.as_sub()?;
        self.container_element_proxy = Some(value.clone());
        Some(value)
    }
}

impl Interpreter {
    /// [`Self::container_element_proxy_base`], gated on the current deferral
    /// actually being a subscript-protocol call on a container subclass — the
    /// only situation where the element `Proxy` is the right answer.
    pub(crate) fn container_element_base_callee(&mut self) -> Option<Value> {
        let ctx = self.samewith_context_stack.last().cloned()?;
        if ctx.name != "AT-KEY" {
            return None;
        }
        let invocant = self
            .method_dispatch_stack
            .last()
            .map(|f| f.invocant.clone())
            .or_else(|| ctx.invocant.clone())
            .or_else(|| self.env.get("self").cloned())?;
        let crate::value::ValueView::Instance { attributes, .. } = invocant.view() else {
            return None;
        };
        if !attributes.contains_key("__baggy_data__") {
            return None;
        }
        self.container_element_proxy_base()
    }
}
