pub(crate) use crate::value::str_numeric;
mod supply_callback_args;
mod supply_classify;
mod supply_done_drive;
mod supply_emit_drive;
mod supply_emit_frame;
mod supply_preserving_derive;
mod supply_quit_drive;
pub(crate) mod supply_tap_stream;
pub(crate) use supply_emit_frame::EmitFrame;
mod supply_promise;
mod supply_transform;
mod system;
mod system_eval_names;
mod system_eval_redecl;
mod system_eval_string;
mod system_eval_vars;
mod system_introspect;
mod tap_state;
mod test_module_predicates;
mod type_check_repr;
pub(crate) mod types;
// `pub(crate)`: the analysis frontend (`crate::analysis`, ADR-0065) calls the
// interpreter-free entry point directly.
