//! Shared constructor contract of regex, token and rule declarations.
use super::*;

// Cost: O(a + t), a = arguments, t = traits.
pub(super) fn construct(class_name: &str, args: &[Value]) -> Option<Result<Value, RuntimeError>> {
    let class = match class_name {
        "RakuAST::RegexDeclaration" => RakuAstClass::RegexDeclaration,
        "RakuAST::TokenDeclaration" => RakuAstClass::TokenDeclaration,
        "RakuAST::RuleDeclaration" => RakuAstClass::RuleDeclaration,
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
            fields.push(field(key, value));
        }
    }
    for (key, expected) in [
        ("name", RakuAstClass::Name),
        ("signature", RakuAstClass::Signature),
    ] {
        if let Some(value) = named_arg(args, key) {
            require_rakuast_class(&value, expected, &constructor)?;
            fields.push(field(key, value));
        }
    }
    if let Some(value) = named_arg(args, "traits") {
        let traits = rakuast_node_list(&value, &constructor, "traits")?;
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
    let body = named_arg(args, "body")
        .ok_or_else(|| RuntimeError::new(format!("{constructor} requires `body`")))?;
    // A proto's `{*}` body is a bare `OnlyStar`.
    if !matches!(body.view(), ValueView::RakuAst(node) if node.class == RakuAstClass::OnlyStar) {
        require_regex_node(&body, &constructor)?;
    }
    fields.push(field("body", body));
    Ok(Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class,
        fields,
    })))
}

fn field(name: &'static str, value: Value) -> RakuAstField {
    RakuAstField {
        name: Some(name),
        value: RakuAstFieldValue::Node(value),
    }
}
