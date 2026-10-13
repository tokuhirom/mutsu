//! `__mutsu_routine_set_returns` — the native primitive behind the
//! `trait_mod:<of>` / `trait_mod:<returns>` CORE.setting prelude
//! (`runtime::run::TRAIT_MOD_OF_PRELUDE`, injected via
//! `runtime::run_prelude::inject_trait_mod_of_prelude`).
//!
//! A declaration's `of T` / `returns T` is applied natively by the parser and
//! registration. A distribution's own routine-trait handler reaches the
//! routine only as a value, after it was declared, and calls CORE's candidate
//! directly: `Path::Finder`'s `trait_mod:<is>(Method $m, :$constraint!)` does
//! `trait_mod:<of>($method, Path::Finder:D)`.

use super::*;

impl Interpreter {
    /// `__mutsu_routine_set_returns($routine, $type)`. `None` for any other
    /// function name, matching `try_routine_export`. Records `$type` as the
    /// routine's return type for every value of the routine; a routine that
    /// already declares one throws, as Rakudo's `X::Redeclaration` does.
    // Cost: O(r), r = bytes in an already-declared return-type spelling.
    pub(super) fn try_routine_set_returns(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if name != "__mutsu_routine_set_returns" {
            return None;
        }
        if args.len() != 2 {
            return Some(Err(RuntimeError::new(format!(
                "__mutsu_routine_set_returns expects 2 arguments, got {}",
                args.len()
            ))));
        }
        let ValueView::Sub(data) = args[0].view() else {
            return Some(Ok(Value::NIL));
        };
        let previous = self
            .callable_return_type(&args[0])
            .filter(|ty| ty != "Mu")
            .or_else(|| {
                data.routine_cell
                    .return_type()
                    .map(|ty| ty.to_string_value())
            });
        if let Some(previous) = previous {
            return Some(Err(RuntimeError::new(format!(
                "Redeclaration of return type for '{}' (previous return type was {})",
                data.name.resolve(),
                previous
            ))));
        }
        data.routine_cell.note_return_type(args[1].clone());
        Some(Ok(args[0].clone()))
    }
}
