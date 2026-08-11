//! The parse-time side of slang activation (ADR-0026 §2.1).
//!
//! `use_stmt` calls [`maybe_activate_slang_use`] after the ordinary module
//! scan. When the used module's source directly `use`s Slangify, the module
//! is executed at parse time in a fresh interpreter on a fresh thread
//! (`runtime::slang_activation`), its `$*LANG.define_slang` registrations are
//! mapped onto parser mode flags, and the rest of the current compilation
//! unit parses in the changed mode. Slang state is lexically scoped to the
//! unit: `reset_user_subs` clears it at parse start, and nested module scans
//! snapshot/restore it.

use super::*;

/// Activate slang parser modes for the rest of the current unit if `module`
/// is slang-activating. Returns an error message when activation itself
/// fails (module load error, or an override of a grammar rule mutsu does not
/// support) — the `use` statement must then fail to parse, never silently
/// continue in the wrong grammar.
pub(in crate::parser) fn maybe_activate_slang_use(module: &str) -> Result<(), String> {
    if !super::module_exports::module_activates_slang(module) {
        return Ok(());
    }
    // No recursive activation: the activation sub-interpreter's own parses
    // (the slang module chain) must not spawn further activation threads.
    if std::thread::current().name()
        == Some(crate::runtime::slang_activation::ACTIVATION_THREAD_NAME)
    {
        return Ok(());
    }
    let rules = crate::runtime::slang_activation::run_slang_activation(
        module.to_string(),
        parser_lib_paths(),
    )
    .map_err(|e| format!("slang activation for '{module}' failed: {e}"))?;
    let mut modes = slang_modes();
    for rule in &rules {
        if apply_slang_rule_override(&mut modes, rule).is_none() {
            // `define_slang` already validated; this only fires if the two
            // maps ever drift apart.
            return Err(format!(
                "slang activation for '{module}': unsupported grammar rule override '{rule}'"
            ));
        }
    }
    if modes != slang_modes() {
        set_slang_modes(modes);
        // A memoized parse from before the mode flip must not be replayed
        // under the new grammar.
        crate::parser::invalidate_all_memos();
    }
    Ok(())
}
