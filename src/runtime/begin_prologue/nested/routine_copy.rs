//! Which routines of an inner scope a lifted BEGIN can declare again, and the
//! declaration it repeats (ADR-0134, slice 2; [`super::routines`]).

use crate::ast::Stmt;

/// A routine a lifted BEGIN can declare again: the name a call spells, whether
/// it is one candidate of a multi (or its proto), and the declaration to
/// repeat. `None` for a redeclaring one, a method-level proto, one whose name
/// is computed, and one whose name has a colon without being an operator or
/// other category routine (`infix:<plus>`). Code uses such a routine through
/// the syntax the parser registered for it (`1 plus 2`), which a scan of the
/// called names need not see, so every one of the enclosing scopes is copied
/// ([`super::Walker::add_category_routines`]).
///
/// The repeat is lexical only. An `our sub` would otherwise install into its
/// package ahead of the scope, and an exported one would be exported a second
/// time; the body reaches the copy by its bare name either way, and the
/// in-place declaration still does the installing.
// Cost: O(t), t = traits on the declaration.
pub(super) fn copyable_routine(decl: &Stmt) -> Option<(String, bool, Stmt)> {
    match decl {
        Stmt::SubDecl {
            name,
            name_expr: None,
            supersede: false,
            multi,
            custom_traits,
            ..
        } => {
            let name = name.resolve();
            let internal = custom_traits
                .iter()
                .any(|(t, _)| t.starts_with("__") && t != OUR_SCOPED && t != OP_PREC);
            if internal || (name.contains(':') && !is_category_routine(&name)) {
                return None;
            }
            let mut copy = decl.clone();
            if let Stmt::SubDecl {
                is_export,
                export_tags,
                custom_traits,
                ..
            } = &mut copy
            {
                *is_export = false;
                export_tags.clear();
                custom_traits.retain(|(t, _)| t != OUR_SCOPED);
            }
            Some((name, *multi, copy))
        }
        // A proto's own traits dispatch to a user `trait_mod:<is>`, which would
        // run again.
        Stmt::ProtoDecl {
            name,
            is_method: false,
            custom_traits,
            ..
        } if custom_traits.is_empty() => {
            let name = name.resolve();
            if name.contains(':') {
                return None;
            }
            let mut copy = decl.clone();
            if let Stmt::ProtoDecl {
                is_export,
                export_tags,
                is_our,
                ..
            } = &mut copy
            {
                *is_export = false;
                export_tags.clear();
                *is_our = false;
            }
            Some((name, true, copy))
        }
        _ => None,
    }
}

const OUR_SCOPED: &str = "__our_scoped";
/// An operator's precedence and associativity, which the copy registers again.
const OP_PREC: &str = "__prec";

/// Whether `name` names an operator or other category routine
/// (`infix:<plus>`, `term:<now>`, `trait_mod:<is>`).
// Cost: O(n), n = the length of `name`.
fn is_category_routine(name: &str) -> bool {
    name.split_once(':').is_some_and(|(category, rest)| {
        !category.is_empty()
            && category.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_')
            && (rest.starts_with('<') || rest.starts_with('«') || rest.starts_with('['))
    })
}
