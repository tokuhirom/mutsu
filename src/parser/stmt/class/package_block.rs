use super::package_decl::check_pseudo_package_in_decl;
use super::*;
use crate::ast::Stmt;
use crate::parser::helpers::{ws, ws1};
use crate::parser::parse_result::{PError, PResult};
use crate::parser::stmt::{keyword, package_body_block, qualified_ident};
use crate::symbol::Symbol;

/// Parse `package` declaration.
pub(crate) fn package_decl(input: &str) -> PResult<'_, Stmt> {
    package_decl_with_scope(input, false)
}

/// Parse `my package` declaration (lexically scoped).
pub(crate) fn package_decl_my(input: &str) -> PResult<'_, Stmt> {
    package_decl_with_scope(input, true)
}

pub(crate) fn package_decl_with_scope(input: &str, is_my: bool) -> PResult<'_, Stmt> {
    let rest = keyword("package", input).ok_or_else(|| PError::expected("package declaration"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, name) = qualified_ident(rest)?;
    check_pseudo_package_in_decl(&name)?;
    let (rest, adverbs) = parse_declarator_traits(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, traits) = super::package_traits::parse_package_is_traits(rest)?;
    super::package_syntax::require_block(rest, "package")?;
    let (rest, mut body) = {
        let _pkg = super::super::simple::push_package_path(&name);
        package_body_block(rest)?
    };
    crate::ast::routine_trait::attach(&mut body, traits.source);
    let declaration = Stmt::Package {
        name: Symbol::intern(&name),
        body,
        kind: crate::ast::PackageKind::Package,
        custom_traits: traits.custom,
        is_unit: false,
        is_my,
    };
    Ok((
        rest,
        crate::parser::stmt::control::parsed_block::finish_package_header(
            declaration,
            &name,
            crate::ast::package_header::Header {
                adverbs,
                export_tags: traits.export_tags,
            },
        ),
    ))
}
