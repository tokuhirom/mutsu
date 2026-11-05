//! The `:ver<>` / `:auth<>` / `:api<>` adverbs of a `unit` declaration's name.

use crate::ast::{Expr, Stmt};

/// The adverbs a package header keeps, in the written order.
// Cost: O(n), n = adverbs.
pub(super) fn header_adverbs(adverbs: Vec<(String, Expr)>) -> Vec<(String, Expr)> {
    adverbs
        .into_iter()
        .filter(|(key, _)| matches!(key.as_str(), "ver" | "auth" | "api"))
        .collect()
}

/// A `unit` declaration with its adverbs: the declaration itself when it has
/// none, else the typed `PackageHeader` node (the same one a block-form
/// declaration gets). `stmt_list` absorbs the rest of the unit into the
/// declaration, whichever form it takes.
// Cost: O(n), n = size of the declaration.
pub(super) fn with_header(adverbs: Vec<(String, Expr)>, name: &str, decl: Stmt) -> Stmt {
    crate::parser::stmt::control::parsed_block::finish_package_header(
        decl,
        name,
        crate::ast::package_header::Header {
            adverbs,
            export_tags: None,
        },
    )
}

/// The positions of a `unit` declaration the parser knows: its name
/// (`name_start` is the input at the name, `after_name` the input after it).
/// The body is the rest of the unit, whose range the RakuAST conversion fills
/// in (#15780).
// Cost: O(1).
pub(super) fn name_only_spans(
    name_start: &str,
    after_name: &str,
) -> Option<Box<crate::ast::parsed::PackageSpans>> {
    let name = crate::parser::primary::source_span(name_start, after_name);
    name.is_some().then(|| {
        Box::new(crate::ast::parsed::PackageSpans {
            name,
            block: crate::ast::parsed::BlockSpans {
                span: crate::ast::SrcSpan::NONE,
                blockoid: crate::ast::SrcSpan::NONE,
                list: crate::ast::SrcSpan::NONE,
            },
        })
    })
}
