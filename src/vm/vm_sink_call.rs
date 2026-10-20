//! Whether a sunk statement call's value runs its user `sink` method
//! (`OpCode::SinkPopCall`, #11537).
//!
//! Rakudo sinks the object a call hands back. A routine that is not `is rw`
//! or `is raw` decontainerizes its implicit result, so `sub g { $x }; g;`
//! runs `$x`'s `sink`; a routine that hands back a container (`is rw`,
//! `return-rw`, `return $x`) is not sunk through. mutsu's call result carries
//! no container, so the answer comes from the callee's declaration
//! (`RoutineBodyFacts::result_is_decontainerized` and the traits), asked only
//! once the value has a user `sink` to run.

use crate::opcode::CompiledCode;
use crate::runtime::Interpreter;
use crate::value::ValueView;

impl Interpreter {
    /// Whether the value on the stack top, the result of a statement call to
    /// the routine named by constant `name_idx`, runs its user `sink` method.
    /// `false` for a value without one, for a name that resolves to no single
    /// routine (a multi, a code variable) and for a routine that hands back a
    /// container.
    // Cost: O(1) when the value has no user `sink` (a tag probe, then one
    // method-table lookup for an instance); otherwise O(d) for the method
    // lookup, d = MRO depth, plus one routine lookup by name.
    pub(crate) fn sunk_call_result_runs_sink(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> bool {
        let Some(value) = self.stack.last() else {
            return false;
        };
        let has_user_sink = match value.view() {
            ValueView::Instance { class_name, .. } => {
                self.has_user_method(class_name.as_str(), "sink")
            }
            _ => value.is_mixin_value() && self.mixin_composes_method(value, "sink"),
        };
        if !has_user_sink {
            return false;
        }
        let name = code.constants[name_idx as usize].to_string_value();
        self.resolve_function(&name).is_some_and(|def| {
            !def.is_rw && !def.is_raw && {
                let facts = Self::routine_body_facts(&def);
                !facts.uses_return_rw && facts.result_is_decontainerized
            }
        })
    }
}
