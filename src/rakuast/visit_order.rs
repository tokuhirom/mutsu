//! Fields Rakudo's `visit-children` visits in another order than `.raku`
//! lists them (the constructor's order, which a node keeps its fields in).

use super::{RakuAstClass, RakuAstField, RakuAstNode};

/// Move the fields of `node` that Rakudo visits out of their listed order:
/// a declaration visits its initializer before its `shape` (`my @a[3] =
/// 1,2,3`), a substitution its pattern before its `infix` (`s[a] = b`), and
/// an enum its term before its `of` type (`our Str enum E <a>`).
// Cost: O(f), f = fields of `node`.
pub(crate) fn reorder(node: &RakuAstNode, fields: &mut Vec<&RakuAstField>) {
    let (moved, after) = match node.class {
        RakuAstClass::VarDeclarationSimple => ("shape", "initializer"),
        RakuAstClass::Substitution => ("infix", "pattern"),
        RakuAstClass::TypeEnum => ("of", "term"),
        _ => return,
    };
    if let Some(at) = fields.iter().position(|f| f.name == Some(moved))
        && let Some(to) = fields.iter().position(|f| f.name == Some(after))
        && at < to
    {
        let field = fields.remove(at);
        fields.insert(to, field);
    }
}

/// `my @a of Int`: a declaration with no type of its own takes its `of`
/// trait's, which Rakudo answers as `.type` and visits first.
// Cost: O(t), t = traits of `node`.
pub(crate) fn of_trait_type(node: &RakuAstNode) -> Option<crate::value::Value> {
    use super::RakuAstFieldValue;
    use crate::value::ValueView;
    if node.class != RakuAstClass::VarDeclarationSimple
        || node.fields.iter().any(|f| f.name == Some("type"))
    {
        return None;
    }
    let field = node.fields.iter().find(|f| f.name == Some("traits"))?;
    let RakuAstFieldValue::List(traits) = &field.value else {
        return None;
    };
    traits.iter().find_map(|t| match t.view() {
        ValueView::RakuAst(t) if t.class == RakuAstClass::TraitOf => {
            match &t.fields.first()?.value {
                RakuAstFieldValue::Node(ty) => Some(ty.clone()),
                _ => None,
            }
        }
        _ => None,
    })
}
