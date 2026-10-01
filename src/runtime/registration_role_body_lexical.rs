//! The eager lexical-type pass of role declaration: `my class`/`my
//! grammar`/`my enum`/exported `my constant`… declared directly in a role
//! body are registered when the role is declared, not only at composition.

use super::*;
use serde_json::Value as Json;

impl Interpreter {
    /// Register every lexical TYPE declaration (`my class`, `my grammar`, `my
    /// role`, `my token`/`rule`/`regex`, `my enum`) declared directly in a
    /// role body when the role is declared, rather than waiting for a later
    /// composition — the type-declaration counterpart of
    /// [`Self::register_role_body_exported_subs`], for the same reason.
    ///
    /// A `my class`/`my grammar` in a role body is, like a plain `sub`, a
    /// lexical declaration of the role's own compilation unit: Rakudo makes
    /// it resolvable the moment the role loads, with no class ever composing
    /// it. `Date::Calendar::Strftime`'s private `_strftime` helper parses
    /// with a `my grammar prt-format {...}` and builds results with a `my
    /// class re-format {...}`, both declared alongside it in the role body —
    /// without this, `prt-format`/`re-format` stayed undeclared (resolving
    /// as a bare `Str`, `No such method 'parse'...`) for exactly the same
    /// reason `_strftime` itself used to be unresolvable (#8950).
    ///
    /// In a **parameterized** role a declaration is run here only when it
    /// does not mention any of the role's parameters
    /// ([`stmt_mentions_role_params`]). A declaration that does (`role
    /// R[::T] { my class C { has T $x } }`) cannot be evaluated before
    /// composition binds `T` to a concrete type — re-evaluating the body with
    /// real bindings is exactly what `run_composed_role_deferred_body` does
    /// at every composition site, and this eager pass must not pre-empt that
    /// with an unbound `T`. One that does not (`unit role V6[::K]; my class
    /// Foo is export { }`, `my constant X is export = 5`) means the same thing
    /// under every parameterization, so, as in a non-parameterized role, it
    /// is declared once here — importable the moment the role loads (#10244)
    /// — and a later composition re-running it is idempotent. Rakudo agrees:
    /// `role R[::T] { my class C {} }` has one `R::C` shared by `R[Int]` and
    /// `R[Str]`; because it already exists when a composition re-runs the
    /// body, the composition-time `R::C[Int]` renaming (reserved for classes
    /// the composition itself creates) leaves it alone.
    ///
    /// Rakudo goes further and declares even a parameter-mentioning
    /// declaration eagerly, *generically* over the unbound `T`; mutsu has no
    /// generic-type representation for that, so such a declaration stays
    /// composition-only.
    ///
    /// Only declarative statement kinds are eagerly run — never a plain
    /// statement or expression, whose side effects must not replay both here
    /// and at composition.
    pub(crate) fn register_role_body_lexical_types(
        &mut self,
        role_name: &str,
        deferred_body_ops: &[crate::opcode::DeferredBodyOp],
        type_params: &[String],
    ) -> Result<(), RuntimeError> {
        let parameterized = !type_params.is_empty();
        let saved_package = self.current_package().to_string();
        self.set_current_package(role_name.to_string());
        for op in deferred_body_ops {
            let eager = match &op.raw {
                // An exported `my constant`/`my $x` is a compile-time
                // declaration too (#9981).
                Stmt::VarDecl {
                    is_export: true, ..
                }
                | Stmt::EnumDecl { .. }
                | Stmt::ClassDecl { .. }
                | Stmt::RoleDecl { .. }
                | Stmt::TokenDecl { .. }
                | Stmt::SubsetDecl { .. } => {
                    !parameterized || !stmt_mentions_role_params(&op.raw, type_params)
                }
                _ => false,
            };
            if !eager {
                continue;
            }
            if let Err(error) = self.run_block_raw(std::slice::from_ref(&op.raw)) {
                self.set_current_package(saved_package);
                return Err(error);
            }
            // A lexical `my class C is export` carries its tags on an internal
            // marker (see `class_decl`); publish it the way an exported subset is.
            if let Stmt::ClassDecl {
                name,
                custom_traits,
                ..
            } = &op.raw
                && let Some((_, tags)) = custom_traits
                    .iter()
                    .find(|(t, _)| t == "__mutsu_export_type")
                && !self.suppress_exports
            {
                let tags = match tags {
                    Some(Expr::ArrayLiteral(items)) => items
                        .iter()
                        .filter_map(|e| match e {
                            Expr::Literal(v) => Some(v.to_string_value()),
                            _ => None,
                        })
                        .collect(),
                    _ => vec!["DEFAULT".to_string()],
                };
                let (pkg, short) = match crate::qualified::package_parent(*name) {
                    Some(parent) => (
                        parent.resolve().to_string(),
                        crate::qualified::unqualified_part(*name)
                            .resolve()
                            .to_string(),
                    ),
                    None => (role_name.to_string(), name.resolve().to_string()),
                };
                self.register_exported_var(pkg, short, tags);
            }
        }
        self.set_current_package(saved_package);
        Ok(())
    }
}

/// Whether `stmt` mentions any of a parameterized role's parameters
/// (`params`: the bare names of `::T` captures and `$n` value parameters).
///
/// Conservative by construction: the statement is walked through its derived
/// `Serialize` impl, so every string leaf of the AST — variable and type
/// names, type constraints (`T:D`, `Array[T]`), literal text — is inspected,
/// and one containing a parameter's name as a whole word counts as a mention.
/// A false positive only defers a declaration to composition (the behavior
/// before #10244); a statement that cannot be serialized counts as a mention
/// for the same reason.
///
/// Cost: O(n), n = size of `stmt`'s AST (times the parameter count).
fn stmt_mentions_role_params(stmt: &Stmt, params: &[String]) -> bool {
    let Ok(json) = serde_json::to_value(stmt) else {
        return true;
    };
    let params: Vec<&str> = params
        .iter()
        .map(|p| p.as_str())
        .filter(|p| !p.is_empty())
        .collect();
    json_mentions_any(&json, &params)
}

fn json_mentions_any(json: &Json, params: &[&str]) -> bool {
    match json {
        Json::String(s) => params.iter().any(|p| contains_word(s, p)),
        Json::Array(items) => items.iter().any(|v| json_mentions_any(v, params)),
        Json::Object(map) => map.iter().any(|(k, v)| {
            params.iter().any(|p| contains_word(k, p)) || json_mentions_any(v, params)
        }),
        _ => false,
    }
}

/// `word` occurs in `s` not flanked by an identifier character.
fn contains_word(s: &str, word: &str) -> bool {
    let is_ident = |c: char| c.is_alphanumeric() || c == '_';
    s.match_indices(word).any(|(i, _)| {
        !s[..i].chars().next_back().is_some_and(is_ident)
            && !s[i + word.len()..].chars().next().is_some_and(is_ident)
    })
}

#[cfg(test)]
mod tests {
    use super::contains_word;

    #[test]
    fn contains_word_matches_whole_words_only() {
        assert!(contains_word("T", "T"));
        assert!(contains_word("Array[T]", "T"));
        assert!(contains_word("T:D", "T"));
        assert!(contains_word("Key-T", "T"));
        assert!(!contains_word("Test", "T"));
        assert!(!contains_word("KT", "T"));
        assert!(!contains_word("T_x", "T"));
    }
}
