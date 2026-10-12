//! Which chunks touch the routine-owned implicits (`$/`, `$0`, `$<name>`,
//! `$!`): the compile-time facts behind `CompiledCode::uses_routine_state` /
//! `sets_routine_state` (ADR-12529 phase 3, the `$/` and `$!` families).

use crate::opcode::{CompiledCode, OpCode};
use crate::value::{Value, ValueView};

/// The methods that set their caller's `$/`, as rakudo's `Str.match`,
/// `Str.subst`, `Str.subst-mutate` and `Grammar.parse` / `.subparse` /
/// `.parsefile` do.
const MATCH_SETTING_METHODS: &[&str] = &[
    "match",
    "subst",
    "subst-mutate",
    "parse",
    "subparse",
    "parsefile",
];

/// Whether a string constant names a routine-owned variable (`/`, `0`,
/// `<name>`, `!`) or `make`, which reads and writes `$/`'s `.made`.
fn names_routine_state(name: &str) -> bool {
    name == "make" || crate::symbol::is_routine_scoped_implicit_var(name)
}

impl CompiledCode {
    /// `(uses_routine_state, sets_routine_state)` for this chunk, see the fields.
    /// Reads are answered from the constant pool: every by-name read of `$/`
    /// or a capture view puts the name there, so the scan never misses one,
    /// and a stray literal spelled the same only costs a frame root.
    // Cost: O(ops + constants), once per chunk at finalization.
    pub(crate) fn scan_routine_state(&self) -> (bool, bool) {
        let sets_match = |idx: u32| -> bool {
            matches!(self.constants.get(idx as usize).map(Value::view),
                Some(ValueView::Str(s)) if MATCH_SETTING_METHODS.contains(&s.as_str()))
        };
        let sets = self.ops.iter().any(|op| match op {
            OpCode::SmartMatchExpr {
                rhs_pure_regex,
                rhs_is_match_regex,
                ..
            } => *rhs_pure_regex || *rhs_is_match_regex,
            OpCode::Subst { .. } | OpCode::NonDestructiveSubst { .. } => true,
            // A `try` or `CATCH` sets `$!`.
            OpCode::TryCatch { .. } => true,
            OpCode::CallMethod { name_idx, .. } | OpCode::CallMethodMut { name_idx, .. } => {
                sets_match(*name_idx)
            }
            _ => false,
        });
        let reads = self.constants.iter().any(|c| match c.view() {
            ValueView::Str(s) => names_routine_state(s.as_str()),
            _ => false,
        });
        // A block nested in this one shares this one's `$/` and `$!`, so its
        // use is this chunk's too (a nested routine owns its own).
        let nested = self
            .closure_compiled_codes
            .iter()
            .any(|c| c.uses_routine_state && !c.is_routine);
        (sets || reads || nested, sets)
    }
}
