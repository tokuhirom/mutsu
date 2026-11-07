//! Reading the declared model fields and derived RakuAST properties.

use super::*;

// Cost: O(f + c + b), f = fields inspected, c = child values copied, b = text bytes produced. Rakudo: O(c + b) -- see #16130.
pub fn node_accessor(node: &RakuAstNode, method: &str) -> Option<Value> {
    if let Some(child) = regex_extension::nested_accessor(node, method) {
        return Some(child);
    }
    // A statement records where it began, and so does a node of a family the
    // parser builds in its RakuAST shape (ADR-10723 Stage 2); every other node
    // (and a hand-built one) has no origin, which reads as the undefined
    // `RakuAST::Origin` (rakudo's answer for a hand-built node) so `with`
    // skips it.
    if method == "origin" {
        return Some(
            origin::origin_of(node).unwrap_or_else(|| {
                Value::package(crate::symbol::Symbol::intern("RakuAST::Origin"))
            }),
        );
    }
    // A lookup's resolution, added by [`resolve`] to the unit tree `$*CU`
    // answers (and absent from a tree built any other way, which resolves
    // nothing).
    // The declaration a parameter's target makes is built when the target is
    // read through its parameter (which knows the declared type), so
    // `.target.declaration` answers without a resolution pass having run.
    if method == "target"
        && node.class == RakuAstClass::Parameter
        && let Some(field) = node.fields.iter().find(|f| f.name == Some("target"))
        && let RakuAstFieldValue::Node(target) = &field.value
        && let ValueView::RakuAst(held) = target.view()
        && held.class == RakuAstClass::ParameterTargetVar
        && held.origin.2.is_none()
    {
        let type_node = node.fields.iter().find_map(|f| match (&f.name, &f.value) {
            (Some("type"), RakuAstFieldValue::Node(ty)) => Some(ty.clone()),
            _ => None,
        });
        let mut target = target.clone();
        resolve::embed_declaration(&mut target, type_node.as_ref());
        return Some(target);
    }
    if method == "compile-time-value"
        && matches!(
            node.class,
            RakuAstClass::VarDeclarationSimple | RakuAstClass::ParameterTargetVar
        )
    {
        return Some(resolve::declared_type(node));
    }
    if node.class == RakuAstClass::Nqp && matches!(method, "op" | "args") {
        let mut positional = node.fields.iter().filter(|f| f.name.is_none());
        let op = positional.next()?;
        if method == "op" {
            return match &op.value {
                RakuAstFieldValue::Node(v) => Some(v.clone()),
                _ => None,
            };
        }
        return Some(Value::real_array(
            positional
                .filter_map(|f| match &f.value {
                    RakuAstFieldValue::Node(v) => Some(v.clone()),
                    _ => None,
                })
                .collect(),
        ));
    }
    if method == "is-block-statement" {
        match node.class {
            // A statement ends where its block does, or at a semicolon.
            RakuAstClass::StatementExpression => return Some(Value::TRUE),
            // A block is a block statement when it is a statement itself.
            RakuAstClass::Block | RakuAstClass::PointyBlock => {
                return Some(Value::truth(node.origin.2.as_ref().is_some_and(|mark| {
                    mark.truthy() && mark.to_string_value() != "immediate"
                })));
            }
            _ => {}
        }
    }
    // 1 for the body of a conditional or loop (it runs in place), else 0.
    if method == "is-immediate-block-user-body" && node.class == RakuAstClass::Block {
        let immediate = node
            .origin
            .2
            .as_ref()
            .is_some_and(|mark| mark.to_string_value() == "immediate");
        return Some(Value::int(i64::from(immediate)));
    }
    if method == "name" && node.class == RakuAstClass::WhateverCodeArgument {
        return Some(
            node.origin
                .2
                .clone()
                .unwrap_or_else(|| Value::str("_whatever_arg_1".to_string())),
        );
    }
    if method == "return-type" && node.class == RakuAstClass::VarAttribute {
        return Some(
            node.origin
                .2
                .clone()
                .unwrap_or_else(|| Value::package(crate::symbol::Symbol::intern("Mu"))),
        );
    }
    // A variable of an anonymous subset keeps its `where` on that subset.
    if method == "where"
        && node.class == RakuAstClass::VarDeclarationSimple
        && !node.fields.iter().any(|f| f.name == Some("where"))
        && let Some(field) = node.fields.iter().find(|f| f.name == Some("type"))
        && let RakuAstFieldValue::Node(ty) = &field.value
        && let ValueView::RakuAst(ty) = ty.view()
        && ty.class == RakuAstClass::TypeSubset
    {
        return node_accessor(ty, "where");
    }
    if method == "container-type" && node.class == RakuAstClass::VarDeclarationSimple {
        return Some(resolve::container_type(node));
    }
    if is_resolvable_lookup(node.class) {
        match method {
            // A package declaration keeps its qualified name there, not a resolution.
            "is-resolved" => {
                return Some(Value::truth(
                    (node.origin.2.is_some() && resolve::package_qualified_name(node).is_none())
                        || resolve::late_resolution(node).is_some(),
                ));
            }
            "resolution" if resolve::package_qualified_name(node).is_some() => {
                return Some(Value::NIL);
            }
            "resolution" => {
                return Some(
                    node.origin
                        .2
                        .clone()
                        .or_else(|| resolve::late_resolution(node))
                        .unwrap_or(Value::NIL),
                );
            }
            _ => {}
        }
    }
    if let Some(value) = super::doc_block::accessor(node, method) {
        return Some(value);
    }
    for f in &node.fields {
        if f.name == Some(method) && !is_unvisited_field(node, f) {
            return Some(field_to_value(&f.value));
        }
    }
    // Methods rakudo computes from the attributes (`default-scope`, `scope`
    // without a written scope, `Name.is-anonymous`, `Call::Method.dispatch`).
    if let Some(value) = derived_methods::call(node, method) {
        return Some(value);
    }
    // This inherited Block capability defaults to false and is not one of
    // the class's local model fields reported by reflection.
    if node.class == RakuAstClass::Block && method == "may-have-signature" {
        return Some(Value::truth(false));
    }
    // `code-statements` (undocumented in rakudo; `static::CLI`'s analysis
    // reads it, #15608) answers the same statements as `statements` for every
    // statement kind measured under `raku` (expressions, declarations, `use`,
    // phasers, loops, `CATCH`, empty statements).
    if (method == "statements"
        && matches!(
            node.class,
            RakuAstClass::StatementList | RakuAstClass::StatementSequence | RakuAstClass::SemiList
        ))
        || (method == "code-statements"
            && matches!(
                node.class,
                RakuAstClass::StatementList | RakuAstClass::SemiList
            ))
        || (method == "args" && node.class == RakuAstClass::ArgList)
        || (method == "atoms" && node.class == RakuAstClass::DocParagraph)
    {
        let items = node
            .fields
            .iter()
            .filter(|f| !origin::is_origin(f) && !is_unvisited_field(node, f))
            .map(|f| field_to_value(&f.value))
            .collect();
        return Some(Value::array(items));
    }
    // Positional-leaf accessors: a node whose positional field is its payload
    // exposes it under a class-specific name (`IntLiteral.value`,
    // `Var::Lexical.name`). Regex sequences and alternations are the one
    // variadic case: their accessor returns all positional children as a List.
    if matches!(
        node.class,
        RakuAstClass::RegexSequence
            | RakuAstClass::RegexAlternation
            | RakuAstClass::RegexSequentialAlternation
            | RakuAstClass::RegexConjunction
            | RakuAstClass::RegexSequentialConjunction
            | RakuAstClass::RegexAssertionCharClass
    ) && fields::positional_accessor(node.class) == Some(method)
    {
        let items = node
            .fields
            .iter()
            .filter(|f| f.name.is_none())
            .map(|f| field_to_value(&f.value))
            .collect();
        return Some(Value::array(items));
    }
    // The named-field loop above runs first, so a class with a named field of
    // the same name (e.g. `Call::Name.name`) is unaffected.
    if fields::positional_accessor(node.class) == Some(method)
        && let Some(f) = node.fields.first()
        && f.name.is_none()
    {
        return Some(field_to_value(&f.value));
    }
    // `Var::Lexical.sigil` / `Var::Dynamic.sigil`: derived from the spelling.
    if method == "sigil"
        && node.class.is_simple_variable()
        && let Some(f) = node.fields.first()
        && let RakuAstFieldValue::Node(v) = &f.value
        && let ValueView::Str(s) = v.view()
        && let Some(sigil) = s.chars().next()
    {
        return Some(Value::str(sigil.to_string()));
    }
    // `VarDeclaration::Simple.name`: the full spelling, sigil, twigil and
    // `desigilname` together (`$x`, `$*x`, `@!a`), as rakudo derives it.
    if method == "name"
        && node.class == RakuAstClass::VarDeclarationSimple
        && let Ok(sigil) = lower::leaf_str(node, "sigil")
        && let Some(RakuAstFieldValue::Node(desigil)) = node
            .fields
            .iter()
            .find(|f| f.name == Some("desigilname"))
            .map(|f| &f.value)
        && let Some(desigil) = lower::rakuast_node_of(desigil)
        && let Some(name_parts::NameShape::Identifier(desigil)) = name_parts::name_shape(desigil)
    {
        let twigil = lower::leaf_str(node, "twigil").unwrap_or_default();
        return Some(Value::str(format!("{sigil}{twigil}{desigil}")));
    }
    // `Name.from-identifier("a")` stores one identifier leaf; `.parts` answers
    // the one `Name::Part::Simple` it denotes (rakudo does not split on `::`).
    if method == "parts"
        && node.class == RakuAstClass::Name
        && let Some(RakuAstField {
            name: None,
            value: RakuAstFieldValue::Node(v),
        }) = node.fields.first()
        && let ValueView::Str(s) = v.view()
    {
        return Some(Value::array(vec![name_parts::simple_part(&s)]));
    }
    // A name without any part (the `without-first-part` of a one-part name)
    // spells as the empty string.
    if method == "canonicalize"
        && node.class == RakuAstClass::Name
        && matches!(
            node.fields.iter().find(|f| f.name == Some("parts")).map(|f| &f.value),
            Some(RakuAstFieldValue::List(parts)) if parts.is_empty()
        )
    {
        return Some(Value::str(String::new()));
    }
    // `Name.canonicalize`: the `::`-joined spelling of a static identifier name.
    if method == "canonicalize"
        && let Some(name_parts::NameShape::Identifier(s)) = name_parts::name_shape(node)
    {
        return Some(Value::str(s));
    }
    // A field the class DECLARES but this node does not carry still answers:
    // rakudo models every field as an attribute, so an absent optional clause
    // reads as an undefined type object (or `()` / `False` / `0`) rather than
    // dying. That is what makes `.defined` the way to test for one — see
    // `fields::Absent`.
    let (name, absent) = fields::model_fields(node.class)
        .iter()
        .chain(inherited_fields::for_class(node.class))
        .find(|(name, _)| *name == method)?;
    match absent {
        fields::Absent::EmptyNode(class) => Some(empty_fields::empty_node(node, name, *class)),
        _ => absent.value(),
    }
}
