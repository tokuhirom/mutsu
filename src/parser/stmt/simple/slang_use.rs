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

/// Apply one slang registration's overrides to the current unit's parser
/// state: production overrides become [`SlangModes`] flags, and `L10N::XX`
/// vocabulary tokens / `<category>2ast` mappings become the unit's
/// [`L10nVocabulary`].
///
/// An override that is neither is a hard error naming the rule (ADR-0026
/// §2.2): silently ignoring it would leave the unit parsing under a grammar
/// the slang meant to change. Production overrides are tried first — Tuxic's
/// `routine-declarator:sym<sub>` would otherwise look like an L10N `routine-`
/// token.
pub(crate) fn apply_slang_overrides(
    overrides: &[crate::runtime::slang_activation::SlangRuleOverride],
) -> Result<(), String> {
    let mut modes = slang_modes();
    let mut vocabulary = l10n_vocabulary_snapshot()
        .map(|v| (*v).clone())
        .unwrap_or_default();
    for over in overrides {
        if apply_slang_rule_override(&mut modes, &over.name).is_some() {
            continue;
        }
        if !over.aliases.is_empty() {
            vocabulary.insert_aliases(
                over.aliases
                    .iter()
                    .map(|(localized, canonical)| (localized.as_str(), canonical.as_str())),
            );
            continue;
        }
        if vocabulary.try_insert_token(&over.name, over.body.as_deref()) {
            continue;
        }
        return Err(format!(
            "Slang activation NYI: grammar rule override '{}' is not supported by this \
             implementation (recognized: term:sym<identifier>, methodop, \
             routine-declarator:sym<sub>, identifier, name, and the L10N vocabulary \
             token categories)",
            over.name
        ));
    }
    let vocabulary = (!vocabulary.is_empty()).then(|| std::rc::Rc::new(vocabulary));
    let changed = modes != slang_modes() || vocabulary != l10n_vocabulary_snapshot();
    if changed {
        set_slang_modes(modes);
        set_l10n_vocabulary(vocabulary);
        // A memoized parse from before the grammar changed must not be
        // replayed under the new one.
        crate::parser::invalidate_all_memos();
    }
    Ok(())
}

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
    // `define_slang` already validated these on the activation thread; this
    // only re-reports if the two maps ever drift apart.
    apply_slang_overrides(&rules).map_err(|e| format!("slang activation for '{module}': {e}"))
}
