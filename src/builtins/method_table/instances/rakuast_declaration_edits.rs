//! What a slang action reads from and edits on a declaration and a lookup it
//! builds (#16888, EEL's `:my` locals): `VarDeclaration::Simple.lexical-name`
//! and `.set-initializer`, and `Var::Lexical.set-resolution`.

use super::super::{Handler, MethodRow, RowFlags};
use crate::value::{RuntimeError, Value, ValueView};

/// Whether `target` is a node of `class`: the rows share the `RakuAST::Node`
/// shape, so each declines another class's node.
// Cost: O(1).
fn is_class(target: &Value, class: crate::rakuast::RakuAstClass) -> bool {
    matches!(target.view(), ValueView::RakuAst(node) if node.class == class)
}

/// `$decl.lexical-name`: the name the declaration binds in its scope (`$x`).
// Cost: O(f), f = fields of the node.
fn lexical_name(target: &Value, _args: &[Value]) -> Result<Value, RuntimeError> {
    Ok(match target.view() {
        ValueView::RakuAst(node) => crate::rakuast::node_accessor(node, "name").unwrap_or(Value::NIL),
        _ => Value::NIL,
    })
}

/// `$decl.set-initializer($init)`: the declaration's initializer is `$init`; the
/// `RakuAST::Initializer` type object (rakudo's "none") removes it.
// Cost: O(f), f = fields of the node.
fn set_initializer(
    _interp: &mut crate::runtime::Interpreter,
    target: &Value,
    args: &[Value],
    _named: super::super::Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let initializer = args.first()?.clone();
    let target = target.deref_container();
    if !is_class(&target, crate::rakuast::RakuAstClass::VarDeclarationSimple) {
        return None;
    }
    target.rakuast_edit(move |node| {
        node.fields.retain(|f| f.name != Some("initializer"));
        if matches!(initializer.view(), ValueView::RakuAst(_)) {
            node.fields.push(crate::rakuast::RakuAstField {
                name: Some("initializer"),
                value: crate::rakuast::RakuAstFieldValue::Node(initializer),
            });
        }
        Ok(Value::NIL)
    })
}

/// `$lookup.set-resolution($declaration)`: the lookup resolves to
/// `$declaration` (`is-resolved` / `resolution` answer it). Lowering still binds
/// the name in the scope the node is lowered in, where that declaration is.
// Cost: O(1).
fn set_resolution(target: &Value, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let declaration = args[0].clone();
    let target = target.deref_container();
    if !is_class(&target, crate::rakuast::RakuAstClass::VarLexical) {
        return None;
    }
    target.rakuast_edit(move |node| {
        node.origin.2 = Some(declaration);
        Ok(Value::NIL)
    })
}

pub(super) static ROWS: &[MethodRow] = &[
    MethodRow {
        owner: "RakuAST::VarDeclaration::Simple",
        name: "lexical-name",
        arity: 0,
        handler: Handler::Pure(lexical_name),
        flags: RowFlags::OWNER_ONLY,
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::VarDeclaration::Simple",
        name: "set-initializer",
        arity: 1,
        // `Interp`: the guard of the pure entries admits no type object, and
        // `RakuAST::Initializer` is one.
        handler: Handler::Interp(set_initializer),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
    MethodRow {
        owner: "RakuAST::Var::Lexical",
        name: "set-resolution",
        arity: 1,
        handler: Handler::Narrow(set_resolution),
        flags: RowFlags::OWNER_ONLY.or(RowFlags::ANY_ARGS),
        named: &[],
    },
];
