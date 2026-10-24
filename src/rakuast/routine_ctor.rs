//! Shared constructor contract of Sub, Method and Submethod nodes.

use super::*;

// Cost: O(n), n = constructor arguments and traits.
pub(super) fn construct(class_name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let class = match class_name {
        "RakuAST::Sub" => RakuAstClass::Sub,
        "RakuAST::Method" => RakuAstClass::Method,
        "RakuAST::Submethod" => RakuAstClass::Submethod,
        _ => return None,
    };
    Some(build(class, args))
}

fn build(class: RakuAstClass, args: &[Value]) -> Result<Value, RuntimeError> {
    let constructor = format!("{}.new", class.printed_name());
    let mut fields = Vec::new();
    for key in ["scope", "multiness"] {
        if let Some(value) = named_arg(args, key) {
            if !matches!(value.view(), ValueView::Str(_)) {
                return Err(RuntimeError::new(format!(
                    "{constructor} expects `{key}` to be Str"
                )));
            }
            fields.push(RakuAstField {
                name: Some(key),
                value: RakuAstFieldValue::Node(value),
            });
        }
    }
    if class != RakuAstClass::Sub
        && let Some(value) = named_arg(args, "private")
    {
        if !matches!(value.view(), ValueView::Bool(_)) {
            return Err(RuntimeError::new(format!(
                "{constructor} expects `private` to be Bool"
            )));
        }
        if value.truthy() {
            fields.push(convert::true_field("private"));
        }
    }
    for (key, expected) in [
        ("name", RakuAstClass::Name),
        ("signature", RakuAstClass::Signature),
    ] {
        if let Some(value) = named_arg(args, key) {
            require_rakuast_class(&value, expected, &constructor)?;
            fields.push(RakuAstField {
                name: Some(key),
                value: RakuAstFieldValue::Node(value),
            });
        }
    }
    if let Some(value) = named_arg(args, "traits") {
        let traits = value
            .as_list_items()
            .map(<[Value]>::to_vec)
            .ok_or_else(|| {
                RuntimeError::new(format!("{constructor} expects `traits` to be a list"))
            })?;
        for value in &traits {
            if !matches!(value.view(), ValueView::RakuAst(node)
                if super::type_object_does(node.class.printed_name(), "RakuAST::Trait"))
            {
                return Err(RuntimeError::new(format!(
                    "{constructor} expects trait nodes"
                )));
            }
        }
        fields.push(RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(traits),
        });
    }
    let body = named_arg(args, "body").unwrap_or_else(empty_blockoid);
    // A proto's `{*}` body is a bare `OnlyStar`.
    if !matches!(body.view(), ValueView::RakuAst(node) if node.class == RakuAstClass::OnlyStar) {
        require_rakuast_class(&body, RakuAstClass::Blockoid, &constructor)?;
    }
    fields.push(RakuAstField {
        name: Some("body"),
        value: RakuAstFieldValue::Node(body),
    });
    Ok(Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class,
        fields,
    })))
}
