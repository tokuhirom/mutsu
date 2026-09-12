//! Parse-time slang activation (ADR-0026 §2.1/§2.2, extended by ADR-0091).
//!
//! When the parser meets `use X` where module X activates a slang — its source
//! either directly `use`s Slangify or calls `$*LANG.define_slang` itself — it
//! runs X's whole load, mainline plus `sub EXPORT` (or the Slangify-generated
//! inner `&EXPORT`), in a fresh [`Interpreter`] on a fresh thread with a
//! compile-time `$*LANG` object bound. Slangify's inner EXPORT calls
//! `$*LANG.define_slang('MAIN', $*LANG.slang_grammar('MAIN').^mixin($role),
//! ...)`; `define_slang` then does two things with the roles' declared rules:
//!
//! - a rule that **overrides** an existing grammar rule maps onto a parser
//!   mode or an L10N vocabulary entry (`apply_slang_overrides`), erroring hard
//!   on an unknown rule;
//! - a `package_declarator:sym<name>` candidate **adds** a package declarator,
//!   read out of the candidate by [`super::slang_declarator`] (ADR-0091).
//!
//! Both travel back to the parser via the thread's return value
//! ([`SlangActivation`]); the fresh thread means the in-progress outer parse's
//! thread-local state is untouched — no save/restore of parser state is
//! needed at all.
//!
//! The `$*LANG` object graph is deliberately minimal (ADR-0026 §4 rejects
//! executing the Rakudo-internal token bodies, and ADR-0091 keeps the
//! refusal): `Mutsu::Slang::CompLang` is the language handle, and
//! `slang_grammar`/`slang_actions`/`actions`/`WHAT` return opaque
//! `Mutsu::Slang::Grammar`/`Mutsu::Slang::Actions` handles whose `.^mixin`
//! only *records* the role composition.

use super::*;
use crate::runtime::slang_declarator::{SlangDeclarator, declarator_keyword};
use crate::value::ValueView;

/// What one slang activation run learned: the grammar-rule overrides the
/// module's roles declare, and the package declarators they add (ADR-0091).
#[derive(Default)]
pub(crate) struct SlangActivation {
    pub(crate) rules: Vec<SlangRuleOverride>,
    pub(crate) declarators: Vec<SlangDeclarator>,
}

/// Classes of the compile-time `$*LANG` object graph. `.^name` on these must
/// not start with `Raku::` — Slangify keys its legacy-grammar selection on
/// that prefix, and mutsu deliberately selects the legacy (NQP-named) roles;
/// either role set maps to the same rule names (ADR-0026 §2.2).
pub(crate) const COMP_LANG_CLASS: &str = "Mutsu::Slang::CompLang";
pub(crate) const GRAMMAR_HANDLE_CLASS: &str = "Mutsu::Slang::Grammar";
const ACTIONS_HANDLE_CLASS: &str = "Mutsu::Slang::Actions";

/// The thread name marks the activation sub-interpreter, so the parser hook
/// can refuse to recurse from inside one (a slang module chain that somehow
/// names another slang-activating module).
pub(crate) const ACTIVATION_THREAD_NAME: &str = "mutsu-slang-activation";

/// One grammar-rule override a slang role declares: the overridden rule/token
/// name, plus the token's raw regex source when it has one.
///
/// Slang::Tuxic-style slangs override *productions* and the body is Rakudo
/// internals mutsu never executes (ADR-0026 §4), so `body` goes unread. An
/// `L10N::XX` vocabulary role overrides *spellings*, and the body is the
/// localized spelling itself — data, which `parser::stmt::simple::l10n`
/// interprets.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct SlangRuleOverride {
    pub(crate) name: String,
    pub(crate) body: Option<String>,
    /// `localized => canonical` pairs read out of an L10N role's
    /// `<category>2ast` method (`method core2ast { my constant %mapping = ... }`),
    /// which is where the generated roles keep the identifier-position half of
    /// a vocabulary. Empty for every other kind of override.
    pub(crate) aliases: Vec<(String, String)>,
}

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
///
/// Spawned via `spawn_user_thread` (not a raw `std::thread::Builder::spawn`):
/// this closure builds an `Interpreter` and runs a module's mainline, i.e. it
/// creates, clones, and drops `Gc` values like any other user-code thread, so
/// it must be a REGISTERED GC mutator (see `gc::stw`'s quiescence rule and
/// `builtins_system::spawn_user_thread`'s doc comment) and must get the same
/// large stack every other user-code thread gets, since a slang module's
/// grammar can recurse arbitrarily deep. The join is wrapped in
/// `gc::block_quiescent` so this (registered) parent thread does not starve a
/// stop-the-world requested while it waits.
pub(crate) fn run_slang_activation(
    module: String,
    lib_paths: Vec<String>,
) -> Result<SlangActivation, String> {
    let handle = crate::runtime::builtins_system::spawn_user_thread(
        ACTIVATION_THREAD_NAME,
        move || -> Result<SlangActivation, String> {
            let mut interp = Interpreter::new();
            for path in lib_paths {
                interp.add_lib_path(path);
            }
            interp.env.insert("*LANG".to_string(), comp_lang_instance());
            interp
                .use_module(&module)
                .map_err(|e| e.message.to_string())?;
            Ok(SlangActivation {
                rules: std::mem::take(&mut interp.defined_slang_rules),
                declarators: std::mem::take(&mut interp.defined_slang_declarators),
            })
        },
    );
    crate::gc::block_quiescent(|| handle.join())
        .map_err(|_| "slang activation thread panicked".to_string())?
}

impl Interpreter {
    /// `Str.AST($slang)`: parse `source` under the localized surface syntax of
    /// the `L10N::<$slang>` distribution.
    ///
    /// Lives on the interpreter, like `EVAL`, because the sub-parse resolves a
    /// module: the parser's search-path list is only populated around a parse,
    /// so this installs this interpreter's paths for the duration exactly as
    /// `run_program` / `require` do. An undefined `$slang` (rakudo's `Mu
    /// $slang?` default) is the plain, unlocalized parse.
    pub(crate) fn str_ast_with_slang(
        &mut self,
        source: &str,
        slang: &Value,
    ) -> Result<Value, RuntimeError> {
        let slang = crate::runtime::types::value_is_defined(slang).then(|| slang.to_string_value());
        crate::parser::set_parser_lib_paths(self.parser_scan_lib_paths());
        crate::parser::set_parser_program_path(self.program_path.clone());
        let result = crate::rakuast::str_dot_ast_with_slang(source, slang.as_deref());
        crate::parser::clear_parser_lib_paths();
        result
    }

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
            // `$*LANG.actions` is the host actions object; mutsu never runs a
            // slang's actions methods (they build QAST), so the handle only
            // has to accept `.^mixin`.
            (COMP_LANG_CLASS, "actions") => Some(Ok(handle_instance(
                ACTIONS_HANDLE_CLASS,
                "MAIN",
                Vec::new(),
            ))),
            // `$*LANG.HOW.mixin($*LANG.WHAT, $role)` is how a module mixes a
            // grammar role into the language itself rather than into a named
            // slang. `.WHAT` therefore has to be the same kind of grammar
            // handle `slang_grammar('MAIN')` returns, so the mixin records the
            // role the same way and `define_slang` finds it.
            (COMP_LANG_CLASS, "WHAT") => Some(Ok(handle_instance(
                GRAMMAR_HANDLE_CLASS,
                "MAIN",
                Vec::new(),
            ))),
            // `$*LANG.set_how($pkgdecl, $HOW)` swaps the metaclass a package
            // declaration of that kind is built with. Record it: a declarator
            // candidate that names no HOW of its own inherits this one.
            (COMP_LANG_CLASS, "set_how") if args.len() >= 2 => {
                self.slang_declarator_hows
                    .insert(args[0].to_string_value(), args[1].clone());
                Some(Ok(Value::NIL))
            }
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
        let mut rules: Vec<SlangRuleOverride> = Vec::new();
        let mut declarators: Vec<SlangDeclarator> = Vec::new();
        for role in Self::slang_handle_roles(grammar) {
            for over in self.slang_role_rule_names(&role)? {
                // A `package_declarator:sym<name>` candidate *adds* a keyword
                // rather than overriding an existing rule (ADR-0091), so it is
                // read as a declarator registration instead of being mapped
                // onto a parser mode.
                if let Some(keyword) = declarator_keyword(&over.name) {
                    declarators
                        .push(self.slang_declarator_from_candidate(keyword, over.body.as_deref()));
                    continue;
                }
                rules.push(over);
            }
        }
        crate::parser::apply_slang_overrides(&rules).map_err(RuntimeError::new)?;
        self.defined_slang_rules.extend(rules);
        // `sub EXPORT` runs once per import, so a module `use`d from several
        // compunits registers its declarators again each time; keep one entry
        // per keyword rather than growing the list without bound.
        for decl in declarators {
            self.defined_slang_declarators
                .retain(|d| d.keyword != decl.keyword);
            self.defined_slang_declarators.push(decl);
        }
        Ok(Value::NIL)
    }

    /// The metaclass a slang declarator keyword builds its package with, if
    /// this interpreter has seen the slang register it.
    ///
    /// An `EXPORTHOW::DECLARE` declarator finds its HOW through an ordinary
    /// env lookup, because the `constant` that names it is declared in the
    /// module's mainline and outlives the load. A slang declarator is
    /// registered from `sub EXPORT`, whose env is restored the moment the call
    /// returns, so the record lives on the interpreter instead.
    pub(crate) fn slang_declarator_how(&self, keyword: &str) -> Option<Value> {
        self.defined_slang_declarators
            .iter()
            .find(|d| d.keyword == keyword && !d.how_type.is_empty())
            .map(|d| Value::package(crate::symbol::Symbol::intern(&d.how_type)))
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

    /// The grammar-rule overrides a slang role declares: its `token`/`rule`
    /// members, each with the raw regex source of its body. Role tokens live in
    /// the role's deferred body (`DeferredBodyOpKind::TokenRule`), not its
    /// `methods` map.
    fn slang_role_rule_names(&self, role: &Value) -> Result<Vec<SlangRuleOverride>, RuntimeError> {
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
                crate::ast::Stmt::TokenDecl { name, body, .. }
                | crate::ast::Stmt::RuleDecl { name, body, .. } => names.push(SlangRuleOverride {
                    name: name.resolve(),
                    body: regex_literal_source(body),
                    aliases: Vec::new(),
                }),
                _ => {}
            }
        }
        // An L10N role translates identifier-position names (core routine
        // names, `is` trait arguments) through a `<category>2ast` method rather
        // than through a token, so read those maps too.
        for (method_name, defs) in &def.methods {
            let Some(category) = method_name
                .strip_suffix("2ast")
                .or_else(|| method_name.strip_suffix("2str"))
            else {
                continue;
            };
            if category.is_empty() {
                continue;
            }
            let Some(aliases) = defs.first().and_then(|d| constant_mapping_pairs(&d.body)) else {
                continue;
            };
            names.push(SlangRuleOverride {
                name: method_name.clone(),
                body: None,
                aliases,
            });
        }
        Ok(names)
    }
}

/// The `localized => canonical` pairs of a `my constant %mapping = "a", "b",
/// ...;` declaration in `body`.
///
/// The L10N roles are machine-generated and always spell the map this way: a
/// flat list of string literals, alternating localized spelling and canonical
/// Raku name. A declaration that is not exactly that yields `None` rather than
/// a half-read map.
///
/// `constant` is not required: the generators have shipped both `my constant
/// %mapping = ...` (`L10N::JA` 0.0.3) and a plain `my %mapping = ...`
/// (`L10N::TLH` 0.0.3). The name inside a `<category>2ast` method is what
/// identifies the map.
fn constant_mapping_pairs(body: &[crate::ast::Stmt]) -> Option<Vec<(String, String)>> {
    for stmt in body {
        let crate::ast::Stmt::VarDecl {
            name,
            expr: crate::ast::Expr::ArrayLiteral(items),
            ..
        } = stmt
        else {
            continue;
        };
        if name != "%mapping" {
            continue;
        }
        if items.len() % 2 != 0 {
            return None;
        }
        let mut pairs = Vec::with_capacity(items.len() / 2);
        for pair in items.chunks_exact(2) {
            let (Some(localized), Some(canonical)) =
                (string_literal(&pair[0]), string_literal(&pair[1]))
            else {
                return None;
            };
            pairs.push((localized, canonical));
        }
        return Some(pairs);
    }
    None
}

fn string_literal(expr: &crate::ast::Expr) -> Option<String> {
    let crate::ast::Expr::Literal(value) = expr else {
        return None;
    };
    match value.view() {
        ValueView::Str(s) => Some(s.to_string()),
        _ => None,
    }
}

/// The raw regex source of a `token`/`rule` body, when the body is the single
/// regex literal the declarator compiles to. Anything else (a token with
/// embedded code, an empty body) yields `None`.
fn regex_literal_source(body: &[crate::ast::Stmt]) -> Option<String> {
    match body {
        [crate::ast::Stmt::Expr(crate::ast::Expr::Literal(value))] => match value.view() {
            ValueView::Regex(source, ..) => Some(source.to_string()),
            _ => None,
        },
        _ => None,
    }
}

impl Interpreter {
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

#[cfg(test)]
mod tests {
    use super::*;

    /// `parser::stmt::simple::slang_use::maybe_activate_slang_use` refuses to
    /// recurse by checking `std::thread::current().name() ==
    /// Some(ACTIVATION_THREAD_NAME)` — that recursion guard silently vanishes
    /// if the activation thread ever stops reporting this name (e.g. a future
    /// refactor of `spawn_user_thread` that stops passing `name` through to
    /// `Builder::name`). Pin the name directly, on the actual spawned thread,
    /// rather than trusting `spawn_user_thread`'s doc comment.
    #[test]
    fn activation_thread_reports_the_name_the_recursion_guard_checks() {
        // The GC worker-registration counters are process-global; serialize
        // against other tests that touch them (same convention as
        // `gc::stw`'s own tests).
        let _s = crate::gc::test_support::serial_lock();
        let handle =
            crate::runtime::builtins_system::spawn_user_thread(ACTIVATION_THREAD_NAME, || {
                std::thread::current().name().map(str::to_string)
            });
        let name =
            crate::gc::block_quiescent(|| handle.join()).expect("activation thread panicked");
        assert_eq!(name.as_deref(), Some(ACTIVATION_THREAD_NAME));
    }

    fn mapping_of(source: &str) -> Option<Vec<(String, String)>> {
        let (stmts, _) = crate::parser::parse_program(source).expect("fixture must parse");
        constant_mapping_pairs(&stmts)
    }

    /// The L10N generators have shipped the `<category>2ast` translation map in
    /// two spellings: `my constant %mapping = ...` (`L10N::JA` 0.0.3) and a
    /// plain `my %mapping = ...` (`L10N::TLH` 0.0.3). Reading only the first
    /// left `jatlh` (Klingon for `say`) untranslated and `L10N::TLH` red while
    /// its eleven sibling languages were green.
    #[test]
    fn both_spellings_of_the_generated_mapping_are_read() {
        let expected = Some(vec![
            ("jatlh".to_string(), "say".to_string()),
            ("Hoch".to_string(), "elems".to_string()),
        ]);
        assert_eq!(
            mapping_of(r#"my constant %mapping = "jatlh", "say", "Hoch", "elems";"#),
            expected
        );
        assert_eq!(
            mapping_of(r#"my %mapping = "jatlh", "say", "Hoch", "elems";"#),
            expected
        );
    }

    /// A map mutsu cannot read exactly yields nothing, rather than a
    /// half-translated vocabulary that would mis-resolve names.
    #[test]
    fn a_mapping_that_is_not_a_flat_string_list_is_refused() {
        assert_eq!(mapping_of(r#"my %mapping = "jatlh", "say", "odd";"#), None);
        assert_eq!(mapping_of(r#"my %mapping = "jatlh", 42;"#), None);
        assert_eq!(mapping_of(r#"my %other = "jatlh", "say";"#), None);
    }
}
