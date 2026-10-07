//! Hand-built `Type::Definedness`, `Type::AnyDefinedness` and `Type::Coercion`.
//!
//! The read direction (`convert::build_type_node`) already renders `Int:D`,
//! `Int:_` and `Int(Str)` as these nodes; this is the matching `.new`:
//!
//! ```text
//! RakuAST::Type::Definedness.new(base-type => $int, definite => True)
//! RakuAST::Type::AnyDefinedness.new(base-type => $int)
//! RakuAST::Type::Coercion.new(base-type => $int, constraint => $str)
//! ```
//!
//! Field order is the one `build_type_node` produces and rakudo prints
//! (`base-type`, then `definite` / `constraint`); `constraint` is omitted when
//! absent, as for `Int()`.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode, named_arg};
use super::require_rakuast_type;
use crate::value::{RuntimeError, Value, ValueView};

fn field(name: &'static str, value: Value) -> RakuAstField {
    RakuAstField {
        name: Some(name),
        value: RakuAstFieldValue::Node(value),
    }
}

/// The `.new` of one of the three classes, or `None` for any other class.
// Cost: O(1) -- a fixed number of named-argument lookups.
pub(super) fn construct(
    class_name: &str,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    let class = match class_name {
        "RakuAST::Type::Definedness" => RakuAstClass::TypeDefinedness,
        "RakuAST::Type::AnyDefinedness" => RakuAstClass::TypeAnyDefinedness,
        "RakuAST::Type::Coercion" => RakuAstClass::TypeCoercion,
        _ => return None,
    };
    Some(build(class, class_name, args))
}

fn build(
    class: RakuAstClass,
    class_name: &str,
    args: &[Value],
) -> Result<Value, RuntimeError> {
    let ctor = format!("{class_name}.new");
    let base = named_arg(args, "base-type")
        .ok_or_else(|| RuntimeError::new(format!("{ctor} requires `base-type`")))?;
    require_rakuast_type(&base, &ctor)?;
    let mut fields = vec![field("base-type", base)];
    match class {
        RakuAstClass::TypeDefinedness => {
            let definite = named_arg(args, "definite")
                .ok_or_else(|| RuntimeError::new(format!("{ctor} requires `definite`")))?;
            if !matches!(definite.view(), ValueView::Bool(_)) {
                return Err(RuntimeError::new(format!(
                    "{ctor} expects `definite` to be Bool"
                )));
            }
            fields.push(field("definite", definite));
        }
        RakuAstClass::TypeCoercion => {
            if let Some(constraint) = named_arg(args, "constraint") {
                require_rakuast_type(&constraint, &ctor)?;
                fields.push(field("constraint", constraint));
            }
        }
        _ => {}
    }
    Ok(Value::rakuast(Box::new(RakuAstNode { class, fields })))
}
