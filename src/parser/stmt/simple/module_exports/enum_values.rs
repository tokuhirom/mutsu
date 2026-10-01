//! Which of a `use`d module's enum *values* the importer's parse learns.
//!
//! The parser records an imported enum value as a complete nullary term
//! (`is_user_declared_enum_value`). That is usually a harmless superset of
//! what the `use` really imports (ADR-0087): the name only steers diagnostics
//! and listop-vs-term guesses. It is **not** harmless for a value spelled like
//! a quote language (`s`, `m`, `q`, `tr`, ...), because a declared symbol of
//! that name shadows the quote construct (`parser::quote_shadow`): CSS::Units
//! declares `my enum Time is export(:Time) « :s(1.0) :ms(0.001) »`, and a plain
//! `use CSS::Units;` then made `$x ~~ s/a/b/;` parse as a division by `s`.
//!
//! So a value whose enum carries an explicit `is export(:Tag)` with no
//! `DEFAULT`/`MANDATORY` tag travels only to an importer whose tag list admits
//! it — the one case where the scan knows exactly which `use` imports the name.
//! Every other enum value (untagged exports, unexported enums, enums a module
//! merely re-exports) keeps the superset behaviour.

use crate::ast::Stmt;

/// Whether an enum declared with these export tags is imported by a `use`
/// that names no tag, i.e. whether its values belong in the unconditional set.
fn exported_by_default(export_tags: &[String]) -> bool {
    export_tags.is_empty()
        || export_tags
            .iter()
            .any(|t| t == "DEFAULT" || t == "MANDATORY")
}

/// Whether a `use` carrying `import_tags` imports an enum exported under
/// `export_tags` (which [`exported_by_default`] already rejected).
pub(super) fn import_admits(export_tags: &[String], import_tags: &[String]) -> bool {
    import_tags
        .iter()
        .any(|t| t == "ALL" || export_tags.contains(t))
}

/// The value names of every `enum` a module declares, at any nesting depth,
/// split by how they travel to an importer: `out` receives the values every
/// `use` makes visible to the parse, `tagged` those of an enum exported only
/// under explicit non-default tags, paired with those tags.
///
/// Unlike a type name an enum value is never package-composed here: the
/// importer spells it bare, which is the only spelling the `?? then !!` guard
/// ever sees.
pub(super) fn collect_module_enum_values(
    stmts: &[Stmt],
    out: &mut Vec<String>,
    tagged: &mut Vec<(String, Vec<String>)>,
) {
    for stmt in stmts {
        match stmt {
            Stmt::EnumDecl {
                variants,
                is_export,
                export_tags,
                ..
            } => {
                let mut names: Vec<String> = variants
                    .iter()
                    .map(|(name, _)| name.clone())
                    .filter(|name| name != "__DYNAMIC__" && !name.is_empty())
                    .collect();
                if variants.len() == 1
                    && variants[0].0 == "__DYNAMIC__"
                    && let Some(body) = variants[0].1.as_ref()
                {
                    super::super::super::decl::collect_dynamic_enum_value_names(body, &mut names);
                }
                if *is_export && !exported_by_default(export_tags) {
                    tagged.extend(names.into_iter().map(|n| (n, export_tags.clone())));
                } else {
                    out.extend(names);
                }
            }
            Stmt::ClassDecl { body, .. }
            | Stmt::RoleDecl { body, .. }
            | Stmt::Package { body, .. }
            // A traited declarator (`enum E is export < a b >`) is wrapped in a
            // bare block by the parser; walk into it like any other body.
            | Stmt::Block(body)
            | Stmt::SyntheticBlock(body) => collect_module_enum_values(body, out, tagged),
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tags(t: &[&str]) -> Vec<String> {
        t.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn default_and_mandatory_exports_are_unconditional() {
        assert!(exported_by_default(&tags(&[])));
        assert!(exported_by_default(&tags(&["DEFAULT"])));
        assert!(exported_by_default(&tags(&["Time", "MANDATORY"])));
        assert!(!exported_by_default(&tags(&["Time"])));
    }

    #[test]
    fn tagged_export_needs_a_matching_import_tag() {
        let time = tags(&["Time"]);
        assert!(!import_admits(&time, &tags(&[])));
        assert!(!import_admits(&time, &tags(&["pt", "Lengths"])));
        assert!(import_admits(&time, &tags(&["Time"])));
        assert!(import_admits(&time, &tags(&["ALL"])));
    }
}
