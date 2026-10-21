//! Reading a slang role's members for `$*LANG.define_slang` (ADR-0026 §2.2,
//! ADR-0098, ADR-13220): the roles a `Mutsu::Slang::*` handle recorded, and the
//! token/rule overrides, vocabulary maps and candidate production handlers
//! each role declares. Nothing here runs a member's body.

use super::*;
use crate::runtime::slang_activation::{SlangRoleKind, SlangRuleOverride};
use crate::value::ValueView;

impl Interpreter {
    /// The roles recorded on a `Mutsu::Slang::Grammar`/`Actions` handle.
    /// A handle that never went through `.^mixin` (Slangify passes the plain
    /// `slang_grammar('MAIN')` result when the module gave no grammar) has an
    /// empty list; any non-handle value contributes nothing.
    pub(super) fn slang_handle_roles(handle: &Value) -> Vec<Value> {
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
    ///
    /// An `Actions` role (ADR-0098) additionally contributes every one of its
    /// plain methods, each named after the production it would handle; the
    /// caller drops the ones naming no production, which are helpers
    /// (ADR-13220). A grammar role's plain methods are not read that way —
    /// only its `<category>2ast` / `<category>2str` vocabulary maps are,
    /// exactly as before.
    pub(super) fn slang_role_rule_names(
        &self,
        role: &Value,
        kind: SlangRoleKind,
    ) -> Result<Vec<SlangRuleOverride>, RuntimeError> {
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
        // than through a token, so read those maps too. `def.methods` is a hash,
        // so walk it in name order: the set of overrides decides which mode
        // flags flip, but the *first unsupported* one decides the error message.
        let mut method_names: Vec<&String> = def.methods.keys().collect();
        method_names.sort_unstable();
        for method_name in method_names {
            let defs = &def.methods[method_name];
            let vocabulary = method_name
                .strip_suffix("2ast")
                .or_else(|| method_name.strip_suffix("2str"))
                .filter(|category| !category.is_empty())
                .and_then(|_| defs.first())
                .and_then(|d| constant_mapping_pairs(&d.body));
            match vocabulary {
                Some(aliases) => names.push(SlangRuleOverride {
                    name: method_name.clone(),
                    body: None,
                    aliases,
                }),
                // An actions role's every other method is a candidate
                // production handler (the caller classifies it); a grammar
                // role's is not.
                None if kind == SlangRoleKind::Actions => names.push(SlangRuleOverride {
                    name: method_name.clone(),
                    body: None,
                    aliases: Vec::new(),
                }),
                None => {}
            }
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
        for [first, second] in items.as_chunks::<2>().0 {
            let (Some(localized), Some(canonical)) =
                (string_literal(first), string_literal(second))
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

#[cfg(test)]
mod tests {
    use super::*;

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
