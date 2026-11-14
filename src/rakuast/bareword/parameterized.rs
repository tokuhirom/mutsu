//! A parameterized type word whose base is not spelled as a type name.
//!
//! The parser plants a typed attribute's implicit default (and other type
//! positions) as the bareword it wrote, `gpointer[T]`. A word whose base starts
//! with an uppercase letter is carried as a type name (`carried::expr_carried`);
//! a base that a declaration resolves but that is spelled in lowercase -- a
//! sigilless constant aliasing a parametric type, `constant \gpointer =
//! Pointer`, as Gnome::N's `GlibToRakuTypes` exports it -- needs the
//! classifier to say it names something.

use super::view_node;
use crate::ast::Expr;
use crate::rakuast::RakuAstClass;
use crate::scope::NameScope;

/// The word itself when it is `base[args]` and the classifier resolves `base`
/// to a type or a term (a constant or sigilless alias of a type): the lowering
/// hands the parameterized word back, as it does for `Pointer[int32]`. `None`
/// for any other word (the conversion decides).
// Cost: O(k + d), k = length of `name`, d = parser scope depth.
pub(in crate::rakuast) fn parameterized_type_word_expr(
    names: &NameScope,
    name: &str,
) -> Option<Expr> {
    let open = name.find('[')?;
    if !name.ends_with(']') {
        return None;
    }
    let base = &name[..open];
    let plain = base.starts_with(|c: char| c.is_alphabetic() || c == '_')
        && base
            .chars()
            .all(|c| c.is_alphanumeric() || matches!(c, '_' | '-' | '\'' | ':'));
    if !plain {
        return None;
    }
    let node = view_node(names, base)?;
    matches!(
        node.class,
        RakuAstClass::TypeSimple | RakuAstClass::TermName
    )
    .then(|| Expr::BareWord(name.to_string()))
}
