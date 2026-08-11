//! Parse-time slang activation (ADR-0026 §2.1/§2.2).
//!
//! When the parser meets `use X` where module X activates a slang (its source
//! directly `use`s Slangify), it runs X's whole load — mainline plus the
//! Slangify-generated inner `&EXPORT` — in a fresh [`Interpreter`] on a fresh
//! thread, with a compile-time `$*LANG` object bound. Slangify's inner EXPORT
//! calls `$*LANG.define_slang('MAIN', $*LANG.slang_grammar('MAIN').^mixin($role),
//! ...)`; `define_slang` maps the roles' overridden grammar-rule names onto
//! parser modes (`apply_slang_rule_override`), erroring hard on an unknown
//! rule. The rule names travel back to the parser via the thread's return
//! value; the fresh thread means the in-progress outer parse's thread-local
//! state is untouched — no save/restore of parser state is needed at all.
//!
//! The `$*LANG` object graph is deliberately minimal (ADR-0026 §4 rejects
//! executing the Rakudo-internal token bodies): `Mutsu::Slang::CompLang` is
//! the language handle, and `slang_grammar`/`slang_actions` return opaque
//! `Mutsu::Slang::Grammar`/`Mutsu::Slang::Actions` handles whose `.^mixin`
//! only *records* the role composition.

use super::*;
use crate::value::ValueView;

/// Classes of the compile-time `$*LANG` object graph. `.^name` on these must
/// not start with `Raku::` — Slangify keys its legacy-grammar selection on
/// that prefix, and mutsu deliberately selects the legacy (NQP-named) roles;
/// either role set maps to the same rule names (ADR-0026 §2.2).
pub(crate) const COMP_LANG_CLASS: &str = "Mutsu::Slang::CompLang";
const GRAMMAR_HANDLE_CLASS: &str = "Mutsu::Slang::Grammar";
const ACTIONS_HANDLE_CLASS: &str = "Mutsu::Slang::Actions";

/// The thread name marks the activation sub-interpreter, so the parser hook
/// can refuse to recurse from inside one (a slang module chain that somehow
/// names another slang-activating module).
pub(crate) const ACTIVATION_THREAD_NAME: &str = "mutsu-slang-activation";

pub(crate) fn comp_lang_instance() -> Value {
    Value::make_instance(
        crate::symbol::Symbol::intern(COMP_LANG_CLASS),
        HashMap::new(),
    )
}

fn handle_instance(class: &str, kind: &str, roles: Vec<Value>) -> Value {
    let mut attrs = HashMap::new();
    attrs.insert("kind".to_string(), Value::str(kind.to_string()));
    attrs.insert("roles".to_string(), Value::array(roles));
    Value::make_instance(crate::symbol::Symbol::intern(class), attrs)
}

/// Run `use <module>` in a fresh interpreter on a fresh thread with `$*LANG`
/// bound, and return the grammar-rule names its slang registration overrode.
/// `lib_paths` is the parser's current module search path list.
pub(crate) fn run_slang_activation(
    module: String,
    lib_paths: Vec<String>,
) -> Result<Vec<String>, String> {
    let handle = std::thread::Builder::new()
        .name(ACTIVATION_THREAD_NAME.to_string())
        .spawn(move || -> Result<Vec<String>, String> {
            let mut interp = Interpreter::new();
            for path in lib_paths {
                interp.add_lib_path(path);
            }
            interp.env.insert("*LANG".to_string(), comp_lang_instance());
            interp.use_module(&module).map_err(|e| e.message.clone())?;
            Ok(std::mem::take(&mut interp.defined_slang_rules))
        })
        .map_err(|e| format!("could not spawn slang activation thread: {e}"))?;
    handle
        .join()
        .map_err(|_| "slang activation thread panicked".to_string())?
}

impl Interpreter {
    /// Native methods of the `$*LANG` object graph. Returns `None` for
    /// methods this dispatcher does not know, letting the normal instance
    /// dispatch produce its usual error.
    pub(crate) fn dispatch_slang_comp_lang_method(
        &mut self,
        class_name: &str,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        match (class_name, method) {
            (COMP_LANG_CLASS, "slang_grammar") => {
                let kind = args
                    .first()
                    .map(Value::to_string_value)
                    .unwrap_or_else(|| "MAIN".to_string());
                Some(Ok(handle_instance(GRAMMAR_HANDLE_CLASS, &kind, Vec::new())))
            }
            (COMP_LANG_CLASS, "slang_actions") => {
                let kind = args
                    .first()
                    .map(Value::to_string_value)
                    .unwrap_or_else(|| "MAIN".to_string());
                Some(Ok(handle_instance(ACTIONS_HANDLE_CLASS, &kind, Vec::new())))
            }
            (COMP_LANG_CLASS, "define_slang") => Some(self.slang_define_slang(args)),
            _ => None,
        }
    }

    /// `$*LANG.define_slang($name, $grammar, $actions?)`: read the roles the
    /// grammar handle accumulated via `.^mixin` and map each role's overridden
    /// rule names onto parser modes. An unknown rule is a hard error naming
    /// the rule (ADR-0026 §2.2) — never a silent ignore. Actions mixins are
    /// recorded-but-inert (Slang::Tuxic passes Mu for actions).
    fn slang_define_slang(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let Some(grammar) = args.get(1) else {
            return Err(RuntimeError::new("define_slang requires a grammar handle"));
        };
        let mut rules: Vec<String> = Vec::new();
        for role in Self::slang_handle_roles(grammar) {
            rules.extend(self.slang_role_rule_names(&role)?);
        }
        let mut modes = crate::parser::slang_modes();
        for rule in &rules {
            if crate::parser::apply_slang_rule_override(&mut modes, rule).is_none() {
                return Err(RuntimeError::new(format!(
                    "Slang activation NYI: grammar rule override '{rule}' is not supported \
                     by this implementation (recognized: term:sym<identifier>, methodop, \
                     routine-declarator:sym<sub>)"
                )));
            }
        }
        self.defined_slang_rules.extend(rules);
        Ok(Value::NIL)
    }

    /// The roles recorded on a `Mutsu::Slang::Grammar`/`Actions` handle.
    /// A handle that never went through `.^mixin` (Slangify passes the plain
    /// `slang_grammar('MAIN')` result when the module gave no grammar) has an
    /// empty list; any non-handle value contributes nothing.
    fn slang_handle_roles(handle: &Value) -> Vec<Value> {
        if let ValueView::Instance { attributes, .. } = handle.view()
            && let Some(roles) = attributes.as_map().get("roles")
            && let ValueView::Array(items, ..) = roles.view()
        {
            return items.iter().cloned().collect();
        }
        Vec::new()
    }

    /// The grammar-rule names a slang role overrides: its declared
    /// `token`/`rule` members. Role tokens live in the role's deferred body
    /// (`DeferredBodyOpKind::TokenRule`), not its `methods` map.
    fn slang_role_rule_names(&self, role: &Value) -> Result<Vec<String>, RuntimeError> {
        let role_name = match role.view() {
            ValueView::Package(name) => name.resolve(),
            _ => role.to_string_value(),
        };
        let registry = self.registry();
        let Some(def) = registry.roles.get(&role_name) else {
            return Err(RuntimeError::new(format!(
                "Slang activation: '{role_name}' is not a known role"
            )));
        };
        let mut names = Vec::new();
        for op in &def.deferred_body {
            if op.kind != crate::opcode::DeferredBodyOpKind::TokenRule {
                continue;
            }
            match &op.raw {
                crate::ast::Stmt::TokenDecl { name, .. }
                | crate::ast::Stmt::RuleDecl { name, .. } => names.push(name.resolve()),
                _ => {}
            }
        }
        Ok(names)
    }

    /// `.^mixin(Role)` on a `Mutsu::Slang::*` handle: record the composition,
    /// returning a new handle carrying the accumulated role set (ADR-0026
    /// §2.2). Purely a recording — the role is never actually composed.
    pub(crate) fn slang_handle_mixin(
        class_name: &str,
        attributes: &AttrMap,
        extra_roles: &[Value],
    ) -> Value {
        let kind = attributes
            .get("kind")
            .map(Value::to_string_value)
            .unwrap_or_else(|| "MAIN".to_string());
        let mut roles: Vec<Value> = attributes
            .get("roles")
            .map(|r| match r.view() {
                ValueView::Array(items, ..) => items.iter().cloned().collect(),
                _ => Vec::new(),
            })
            .unwrap_or_default();
        roles.extend(extra_roles.iter().cloned());
        handle_instance(class_name, &kind, roles)
    }
}
