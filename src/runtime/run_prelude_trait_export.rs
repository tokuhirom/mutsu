//! CORE's `trait_mod:<is>(Routine:D, :$export!)` candidate, as real Raku
//! source over the `__mutsu_routine_export` primitive
//! (`vm::vm_trait_mod_export_ops`).

use super::source_code_text::CodeText;
use super::*;

/// `trait_mod:<is>(Routine:D \r, :$export!)` — CORE.setting's candidate
/// behind `is export`, made callable directly so a distribution's own
/// routine-trait handler can re-dispatch to it. `Exportable`'s
/// `is exportable` records the routine and then calls
/// `trait_mod:<is>(r, :export($exportable))`; without this candidate the
/// call has nothing to dispatch to and, since it runs nested inside the
/// outer `is exportable` dispatch, is swallowed as that trait being unknown.
/// `__mutsu_routine_export` performs the registration a declaration's own
/// `is export` gets (see `vm::vm_trait_mod_export_ops`).
const TRAIT_MOD_IS_EXPORT_PRELUDE: &str = r#"
multi sub trait_mod:<is>(Routine:D \r, :$export!) is export {
    __mutsu_routine_export(r, $export);
}
"#;

impl Interpreter {
    /// Prepend [`TRAIT_MOD_IS_EXPORT_PRELUDE`] to a compunit that calls
    /// `trait_mod:<is>` with an `:export(...)` argument itself.
    ///
    /// Gated like [`Interpreter::inject_trait_mod_is_default_prelude`], and for
    /// the same reason: adding a candidate at all turns `trait_mod:<is>` into
    /// a multi-candidate routine, which changes how a file that merely
    /// declares or re-exports its own handler resolves `&trait_mod:<is>`. The
    /// `:export(` call-site spelling is what a re-dispatch to CORE's
    /// candidate writes; a declaration's own `is export(...)` never needs it,
    /// because mutsu applies that natively.
    pub(super) fn inject_trait_mod_is_export_prelude(source: &CodeText<'_>, stmts: &mut Vec<Stmt>) {
        if !source.contains("trait_mod:<is>") || !source.contains(":export(") {
            return;
        }
        use std::sync::OnceLock;
        static TRAIT_MOD_IS_EXPORT_STMTS: OnceLock<Vec<Stmt>> = OnceLock::new();
        let prelude = TRAIT_MOD_IS_EXPORT_STMTS.get_or_init(|| {
            let mut stmts = crate::parse_dispatch::parse_source(TRAIT_MOD_IS_EXPORT_PRELUDE)
                .map(|(s, _)| s)
                .unwrap_or_default();
            Self::mark_prelude_subs(&mut stmts);
            stmts
        });
        if prelude.is_empty() {
            return;
        }
        let mut combined = prelude.clone();
        combined.append(stmts);
        *stmts = combined;
    }
}
