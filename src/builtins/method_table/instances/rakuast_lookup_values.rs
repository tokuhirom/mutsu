//! `compile-time-value` / `maybe-compile-time-value` of the lookups whose value
//! is their resolution's (#14916): a type name (`Type::Simple`, `Type::Setting`;
//! rakudo's `RakuAST::Meta` / `RakuAST::CompileTimeValue`) and a lexical
//! variable (`Var::Lexical.maybe-compile-time-value`).
//!
//! Measured on rakudo 2026.09 (`RAKUDO_RAKUAST=1`), in `$*CU` at CHECK time:
//! `Str` answers the type object `Str`, a unit's `class Foo` answers `Foo`;
//! `$x` of `my Int $x` answers `Int` (its container's type object), an untyped
//! variable, a parameter or `$_` answers `Any`, `&say` the routine. A node
//! built by hand is not resolved: a type name dies with `'Int' has not been
//! resolved.`, a variable with `This element has not been resolved.`.

use super::super::{Handler, MethodRow, Named, RowFlags};
use super::rakuast_accessors::compiled_value;
use crate::rakuast::RakuAstClass;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

/// The compile-time value of a lookup's `resolution`, by the kind of
/// declaration it resolved to; `None` for a kind that has none.
// Cost: O(1) plus the interpreter's lookup of a core or imported name.
fn resolution_value(
    interp: &mut Interpreter,
    resolution: &Value,
) -> Option<Result<Value, RuntimeError>> {
    match resolution.view() {
        ValueView::Instance { class_name, .. } => match class_name.resolve().as_str() {
            "RakuAST::Declaration::External::Setting"
            | "RakuAST::Declaration::External::Constant"
            | "RakuAST::Declaration::Import"
            | "RakuAST::Declaration::LexicalPackage" => {
                super::rakuast_node::external_value(interp, resolution, &[], Named::NONE)
            }
            // `$_`, `$/`, `$!`: an untyped container.
            "RakuAST::VarDeclaration::Implicit::Special" => {
                Some(Ok(Value::package(Symbol::intern("Any"))))
            }
            _ => None,
        },
        ValueView::RakuAst(node) => match node.class {
            // A variable's value at compile time is its container's type.
            RakuAstClass::VarDeclarationSimple | RakuAstClass::ParameterTargetVar => {
                crate::rakuast::node_accessor(node, "compile-time-value").map(Ok)
            }
            RakuAstClass::DeclarationImport => {
                super::rakuast_compile_state::built_import_field(resolution, "compile-time-value")
                    .map(Ok)
            }
            // A unit-declared routine, package, subset, ...: its meta-object.
            _ => super::rakuast_node::sub_value(interp, resolution, &[], Named::NONE),
        },
        _ => None,
    }
}

/// The resolution `target`'s lookup node carries, or the "not resolved" error
/// rakudo raises for it (`what` names the lookup in the message).
// Cost: O(f), f = fields of the node.
fn resolution_of(
    target: &Value,
    what: impl FnOnce(&Value) -> String,
) -> Result<Value, RuntimeError> {
    let resolution = match target.view() {
        ValueView::RakuAst(node) => crate::rakuast::node_accessor(node, "resolution"),
        _ => None,
    }
    .unwrap_or(Value::NIL);
    if resolution.is_nil() {
        let class = match target.view() {
            ValueView::RakuAst(node) => node.class.printed_name(),
            _ => "RakuAST::Node",
        };
        return Err(RuntimeError::new(format!("{} Type: {class}", what(target))));
    }
    Ok(resolution)
}

/// `Type::Simple.compile-time-value` / `Var::Lexical.maybe-compile-time-value`:
/// the value the lookup's resolution has at compile time (a type object, a
/// variable's container type, a routine). `None` for any other node.
// Cost: as `resolution_value`.
pub(super) fn lookup_value(
    interp: &mut Interpreter,
    target: &Value,
) -> Option<Result<Value, RuntimeError>> {
    let ValueView::RakuAst(node) = target.view() else {
        return None;
    };
    let resolution = match node.class {
        RakuAstClass::TypeSimple | RakuAstClass::TypeSetting => resolution_of(target, |t| {
            let name = match t.view() {
                ValueView::RakuAst(node) => crate::rakuast::node_accessor(node, "name")
                    .and_then(|name| match name.view() {
                        ValueView::RakuAst(name) => {
                            crate::rakuast::node_accessor(name, "canonicalize")
                        }
                        _ => None,
                    })
                    .map(|n| n.to_string_value())
                    .unwrap_or_default(),
                _ => String::new(),
            };
            format!("'{name}' has not been resolved.")
        }),
        RakuAstClass::VarLexical | RakuAstClass::TermName | RakuAstClass::TermEnum => {
            resolution_of(target, |_| "This element has not been resolved.".into())
        }
        _ => return None,
    };
    let resolution = match resolution {
        Ok(resolution) => resolution,
        Err(err) => return Some(Err(err)),
    };
    Some(resolution_value(interp, &resolution).unwrap_or(Ok(Value::NIL)))
}

/// The one `maybe-compile-time-value` row of a plain RakuAST node (every
/// class shares the node's dispatch shape): a lookup's resolution value, a
/// literal's value, a quoted string's text.
// Cost: as `lookup_value`, or O(s) for a quoted string, s = its length.
fn maybe_value(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let target = super::rakuast_visit::native_node(target)?;
    let ValueView::RakuAst(node) = target.view() else {
        return None;
    };
    if node.class == RakuAstClass::QuotedString {
        return Some(Ok(
            crate::rakuast::literal_queries::quoted_string_maybe_compile_time_value(node),
        ));
    }
    if let Some(answer) = lookup_value(interp, &target) {
        return Some(answer);
    }
    Some(Ok(
        crate::rakuast::literal_queries::literal_compile_time_value(node),
    ))
}

macro_rules! lookup_row {
    ($owner:literal, $name:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: $name,
            arity: 0,
            handler: Handler::Interp($handler),
            flags: RowFlags::OWNER_ONLY,
            named: &[],
        }
    };
}

// `compile-time-value` is served by the node's one row of that name
// (`compiled_value`, which reaches [`lookup_value`] through `sub_value`); these
// rows declare it on the classes that have it.
pub(super) static ROWS: &[MethodRow] = &[
    lookup_row!(
        "RakuAST::Type::Simple",
        "compile-time-value",
        compiled_value
    ),
    lookup_row!(
        "RakuAST::Type::Simple",
        "maybe-compile-time-value",
        maybe_value
    ),
    lookup_row!(
        "RakuAST::Type::Setting",
        "compile-time-value",
        compiled_value
    ),
    lookup_row!(
        "RakuAST::Type::Setting",
        "maybe-compile-time-value",
        maybe_value
    ),
    lookup_row!(
        "RakuAST::Var::Lexical",
        "maybe-compile-time-value",
        maybe_value
    ),
];
