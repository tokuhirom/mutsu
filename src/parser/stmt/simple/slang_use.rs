//! The parse-time side of slang activation (ADR-0026 §2.1).
//!
//! `use_stmt` calls [`maybe_activate_slang_use`] after the ordinary module
//! scan. When the used module activates a slang — its source directly `use`s
//! Slangify, or it calls `$*LANG.define_slang` from its own no-argument
//! `sub EXPORT` (ADR-0091) — the module is executed at parse time in a fresh
//! interpreter on a fresh thread (`runtime::slang_activation`), its
//! `$*LANG.define_slang` registrations are mapped onto parser mode flags and
//! package declarator keywords, and the rest of the current compilation unit
//! parses in the changed mode. Slang state is lexically scoped to the
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
            let aliases = over
                .aliases
                .iter()
                .map(|(localized, canonical)| (localized.as_str(), canonical.as_str()));
            if over.name.ends_with("2str") {
                vocabulary.insert_position_aliases(&over.name, aliases);
            } else {
                vocabulary.insert_aliases(aliases);
            }
            continue;
        }
        if vocabulary.try_insert_token(&over.name, over.body.as_deref()) {
            continue;
        }
        // The names a bare `my NAME` may declare (Slang::Nogil / Slang::Emoji).
        if over.name == "sigilless-variable"
            && let Some(body) = over.body.as_deref()
            && (body.contains("<.ident>") || body.contains(":So>"))
        {
            modes.sigilless_ident |= body.contains("<.ident>");
            modes.sigilless_symbol |= body.contains(":So>");
            continue;
        }
        // The lexemes that open a pointy block (Slang::Lambda's `λ`).
        if over.name == "pointy-block-starter"
            && let Some(starters) = over.body.as_deref().and_then(parse_pointy_starters)
        {
            register_pointy_starters(starters);
            continue;
        }
        // A new production of a hookable category (ADR-15762): the slang host
        // runs its token and action where the parser reaches it.
        // A core action the slang overrides (ADR-15762 slice 3): the
        // expressions the parse builds go through it before the unit is finalized.
        if slang_action_rule(&over.name) {
            register_action(&over.name);
            continue;
        }
        // Its token must be written by the slang: an actions-only method names
        // a production nothing declares, which stays an error.
        if slang_hook_rule(&over.name).is_some()
            && overrides
                .iter()
                .any(|o| o.name == over.name && o.body.is_some())
        {
            if over.body.is_some() {
                register_hook(&over.name, over.body.as_deref());
            }
            continue;
        }
        // A helper token of the slang's own productions (`eel-marker`, called as
        // `<.eel-marker>` from another token it defines): it runs on the slang host
        // when that token does, so the parser has nothing to register. A new rule
        // nothing of the slang's refers to is still refused, as is a `:sym<...>`
        // production of a category the parser does not hook.
        if over.body.is_some()
            && !over.name.contains(':')
            && is_helper_of_other_rule(&over.name, overrides)
        {
            continue;
        }
        return Err(format!(
            "Slang activation NYI: grammar rule override '{}' is not supported by this \
             implementation (recognized: {}, and the L10N vocabulary token categories)",
            over.name,
            RECOGNIZED_PRODUCTION_OVERRIDES.join(", ")
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
pub(in crate::parser) fn maybe_activate_slang_use(
    module: &str,
    arg: Option<&Expr>,
) -> Result<(), String> {
    if !super::module_exports::module_activates_slang(module) {
        return Ok(());
    }
    // Activation executes the module; a non-executing parse (`--dump-ast`,
    // the analysis API) stays in the ordinary grammar instead (#11212).
    if crate::parser::no_execute::no_execute() {
        return Ok(());
    }
    // No recursive activation: the activation sub-interpreter's own parses
    // (the slang module chain) must not spawn further activation threads.
    if std::thread::current().name()
        == Some(crate::runtime::slang_activation::ACTIVATION_THREAD_NAME)
    {
        return Ok(());
    }
    let activation = crate::runtime::slang_activation::run_slang_activation(
        module.to_string(),
        parser_lib_paths(),
        arg.and_then(literal_use_args),
    )
    .map_err(|e| format!("slang activation for '{module}' failed: {e}"))?;
    // Package declarators the slang added (ADR-0091): each keyword parses
    // like `class`/`role` for the rest of this unit, with the declarator's
    // metaclass attached at registration time.
    for decl in &activation.declarators {
        register_declare_keyword(module, &decl.keyword, decl.is_role());
    }
    for keyword in &activation.supersede_keywords {
        register_supersede_keyword(module, keyword);
    }
    if !activation.declarators.is_empty() || !activation.supersede_keywords.is_empty() {
        // A memoized parse from before the keyword existed must not be
        // replayed now that it is a declarator.
        crate::parser::invalidate_all_memos();
    }
    if let Some(host) = activation.host {
        note_slang_activation_scope();
        set_slang_host(host);
        set_activator(module);
    }
    // `define_slang` already validated these on the activation thread; this
    // only re-reports if the two maps ever drift apart.
    apply_slang_overrides(&activation.rules)
        .map_err(|e| format!("slang activation for '{module}': {e}"))
}

/// The literal arguments of a `use` statement (`use Foo 'a', 42`), in the form
/// the activation thread carries them. `None` when any argument is not a plain
/// string/number/boolean literal or a `&sub` of the unit: such a `use` activates with no arguments, as
/// it did before arguments were passed at all.
use crate::runtime::slang_activation::{SlangRuleOverride, SlangUseArg};

fn literal_use_args(arg: &Expr) -> Option<Vec<SlangUseArg>> {
    let items: &[Expr] = match arg.peel_parens() {
        Expr::ArrayLiteral(items) => items,
        single => std::slice::from_ref(single),
    };
    items
        .iter()
        .map(|item| match item.peel_parens() {
            Expr::Literal(v) => SlangUseArg::from_literal(v),
            // A routine of the unit being parsed (`use overload::constant &integer`):
            // the slang host rebuilds it from the declaration's text (#17055).
            Expr::CodeVar(name) => unit_sub_source(name).map(|source| SlangUseArg::Routine {
                name: name.clone(),
                source,
            }),
            _ => None,
        })
        .collect()
}

/// Whether another rule of `overrides` calls the rule `name` as a subrule
/// (`<name>`, `<.name>`, `<name=...>` and `$<x>=<name>` all contain `name>` or
/// `name=` right after an opening `<`, optionally with a leading `.` or `?`/`!`).
// Cost: O(r * n), r = overrides, n = size of a rule body.
fn is_helper_of_other_rule(name: &str, overrides: &[SlangRuleOverride]) -> bool {
    let calls = [
        format!("<{name}>"),
        format!("<.{name}>"),
        format!("<?{name}>"),
        format!("<!{name}>"),
        format!("<{name} "),
        format!("<.{name} "),
        format!("<{name}("),
        format!("<.{name}("),
        format!("=<{name}>"),
    ];
    overrides.iter().any(|other| {
        other.name != name
            && other
                .body
                .as_deref()
                .is_some_and(|body| calls.iter().any(|call| body.contains(call.as_str())))
    })
}
