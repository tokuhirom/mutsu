//! Fields Rakudo's `visit-children` visits in another order than `.raku`
//! lists them (the constructor's order, which a node keeps its fields in).

use super::{RakuAstClass, RakuAstField, RakuAstNode};

/// Move the fields of `node` that Rakudo visits out of their listed order:
/// a declaration visits its initializer before its `shape` (`my @a[3] =
/// 1,2,3`), a substitution its pattern before its `infix` (`s[a] = b`), an
/// enum its term before its `of` type and traits (`our Str enum E <a>`), a constant
/// its initializer before its traits (`constant C is export = 1`), a typed
/// `is` trait its argument before its type, and a statement its labels after
/// everything else (`L: for @a { }`).
// Cost: O(f), f = fields of `node`.
pub(crate) fn reorder(node: &RakuAstNode, fields: &mut Vec<&RakuAstField>) {
    // `has R $.r .= new` / `has $.x = 1 + 2`: Rakudo visits the default
    // through the thunk in the attribute's `Trait::WillBuild`, not its
    // initializer.
    if super::method_initializer::is_thunked(node) {
        fields.retain(|f| f.name != Some("initializer"));
    }
    // `L: for @a { }`: a statement visits its labels last.
    if let Some(at) = fields.iter().position(|f| f.name == Some("labels")) {
        let field = fields.remove(at);
        fields.push(field);
    }
    // `our Str enum E is export <a>`: an enum visits its term right after its
    // name, then its `of` type and its traits.
    if node.class == RakuAstClass::TypeEnum
        && let Some(term) = fields.iter().position(|f| f.name == Some("term"))
        && let Some(name) = fields.iter().position(|f| f.name == Some("name"))
        && name < term
    {
        let field = fields.remove(term);
        fields.insert(name + 1, field);
        return;
    }
    let (moved, after) = match node.class {
        RakuAstClass::VarDeclarationSimple => ("shape", "initializer"),
        RakuAstClass::Substitution => ("infix", "pattern"),
        RakuAstClass::VarDeclarationConstant => ("traits", "initializer"),
        RakuAstClass::TraitIs => ("type", "argument"),
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

/// `my Int $x where * > 1 = 3`: Rakudo visits a declaration with a `where`
/// clause as the clause, the type, the initializer, the type again, then the
/// rest (its traits). `None` for any other node.
// Cost: O(f), f = fields of `node`.
pub(crate) fn where_declaration(node: &RakuAstNode) -> Option<Vec<crate::value::Value>> {
    use super::RakuAstFieldValue;
    if node.class != RakuAstClass::VarDeclarationSimple {
        return None;
    }
    let field = |name: &str| {
        node.fields.iter().find_map(|f| match &f.value {
            RakuAstFieldValue::Node(v) if f.name == Some(name) && super::is_node_value(v) => {
                Some(v.clone())
            }
            _ => None,
        })
    };
    let clause = field("where")?;
    let ty = field("type");
    let mut out = vec![clause];
    out.extend(ty.clone());
    out.extend(field("initializer"));
    out.extend(ty);
    for f in &node.fields {
        if matches!(f.name, Some("where" | "type" | "initializer"))
            || super::is_unvisited_field(node, f)
            || super::is_unwalked_field(node, f)
        {
            continue;
        }
        match &f.value {
            RakuAstFieldValue::Node(v) if super::is_node_value(v) => out.push(v.clone()),
            RakuAstFieldValue::List(items) => {
                out.extend(items.iter().filter(|v| super::is_node_value(v)).cloned())
            }
            _ => {}
        }
    }
    Some(out)
}
