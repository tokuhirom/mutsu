//! `&?ROUTINE`: the routine lexically enclosing the code that reads it.
//!
//! A block or pointy block is not a routine boundary, so `&?ROUTINE` inside one
//! means the routine the block was WRITTEN in. A closure that reads it carries
//! that routine from its creation (`build_closure` binds it into the closure's
//! own captured env), so it still means the same routine when it runs after the
//! routine returned (a lazy `map`) or from another routine. Only code with no
//! such binding (a block run on a path that never went through `build_closure`)
//! falls back to the routine currently on the stack.

use super::*;

impl Interpreter {
    /// The env key a closure carries its creator's enclosing routine under.
    pub(crate) const CAPTURED_ROUTINE_KEY: &'static str = "&?ROUTINE";

    /// The value of `&?ROUTINE` at this point of execution; `Nil` outside a routine.
    // Cost: O(d), d = pointy-block frames above the enclosing routine on the routine stack.
    pub(crate) fn enclosing_routine_value(&self) -> Value {
        // A running closure answers from the binding it was created with.
        if self
            .routine_stack
            .last()
            .is_some_and(|frame| frame.name == "<pointy-block>")
            && let Some(top) = self.block_stack_top()
            && let ValueView::Sub(data) = top.view()
            && let Some(bound) = data.env.get(Self::CAPTURED_ROUTINE_KEY)
        {
            return bound.clone();
        }
        // Skip pointy-block entries to find the enclosing routine
        let entry = self
            .routine_stack
            .iter()
            .rev()
            .find(|frame| frame.name != "<pointy-block>");
        if let Some(frame) = entry {
            // Anonymous subs are pushed with "<anon>" as the sentinel name.
            // Return the block_stack Sub directly so callers can invoke it.
            if frame.name.is_empty() || frame.name == "<anon>" {
                if let Some(val) = self.block_stack_top()
                    && matches!(val.view(), ValueView::Sub(_))
                {
                    return val;
                }
                return Value::NIL;
            }
            if frame.is_submethod {
                let mut env = crate::env::Env::new();
                env.insert(
                    "__mutsu_callable_type".to_string(),
                    Value::str_from("Submethod"),
                );
                env.insert(
                    "__mutsu_routine_name".to_string(),
                    Value::str(
                        crate::qualified::qualified(frame.package, frame.name)
                            .as_str()
                            .to_string(),
                    ),
                );
                return Value::make_sub(
                    frame.package,
                    frame.name,
                    Vec::new(),
                    Vec::new(),
                    Vec::new(),
                    false,
                    env,
                );
            }
            // A named sub's `&?ROUTINE` is the routine `&name` denotes, the
            // same object (`&?ROUTINE === &f`), not a fresh by-name stub.
            if !frame.is_method && !crate::qualified::is_qualified(frame.name) {
                let by_name = self.resolve_code_var_scoped(frame.name.as_str(), true);
                if matches!(by_name.view(), ValueView::Sub(_)) {
                    return by_name;
                }
            }
            return Value::routine_parts(frame.package, frame.name, false);
        }
        Value::NIL
    }
}
