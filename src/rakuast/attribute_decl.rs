//! Attribute declarations retain value types and keyed hash shapes independently.

use crate::scope::NameScope;
use super::convert::{
    Initializer, convert_expr, node_field, statement_expression, unsupported as unsupported_source,
    var_declaration,
};
use super::lower::{
    child_node, leaf_str, lower_expr, named_child, named_child_or_positional, positional_leaf,
    simple_type_name, unsupported,
};
use super::{RakuAstClass, RakuAstNode, attribute};
use crate::ast::Stmt;
use crate::value::{RuntimeError, ValueView};

pub(super) fn convert(names: &NameScope, stmt: &Stmt) -> Result<Option<RakuAstNode>, RuntimeError> {
    let Stmt::HasDecl {
        name,
        is_public,
        default,
        handles,
        is_rw,
        is_readonly,
        type_constraint,
        hash_key_type,
        hash_key_type_expr,
        type_smiley,
        is_required,
        sigil,
        where_constraint,
        is_alias,
        is_our,
        is_my,
        is_default,
        is_type,
        deprecated_message,
        unknown_traits,
        is_built,
        default_is_seed,
        default_is_trait,
        default_is_bind,
        handles_terms,
        trait_order,
        is_embedded,
        ..
    } = stmt
    else {
        unreachable!("attribute converter requires HasDecl");
    };
    // A `has [Type] $.x` attribute -> a `VarDeclaration::Simple` with
    // `scope => "has"` and a `twigil` (`.` public accessor / `!`
    // private; none for the alias `has $x`). `our $.x` has the `our`
    // scope, `my $.x` none (the default). An explicit `= EXPR` default
    // is both the implicit `Trait::WillBuild` and the `initializer`; a
    // typed attribute (`has Int $.z`) carries an *implicit*
    // `BareWord(<TypeName>)` default that is no default at all. The
    // written traits come in written order (`rakuast::attribute`); a
    // `:D` / `:U` smiley is the type's `Type::Definedness`; a `where`
    // is the last field.
    let explicit_default = default
        .as_ref()
        .filter(|_| !*default_is_seed && !*default_is_trait);
    // Rakudo's RakuAST represents `Int:_` on a declaration as the plain
    // `Int` type: the smiley adds no refinement beyond the default type.
    let smiley_type = match (type_constraint, type_smiley.as_deref()) {
        (_, None) => None,
        (Some(base), Some(smiley @ ("D" | "U"))) => Some(format!("{base}:{smiley}")),
        (Some(_), Some("_")) => None,
        _ => return Err(unsupported_source("attribute with a `:_` smiley")),
    };
    if !handles.is_empty() && handles_terms.is_empty() {
        return Err(unsupported_source(
            "attribute with a `handles` spelling not kept as a term",
        ));
    }
    let type_name = smiley_type.as_deref().or(type_constraint.as_deref());
    let twigil = match (*is_alias, *is_public) {
        (true, _) => None,
        (false, true) => Some("."),
        (false, false) => Some("!"),
    };
    // NativeCall's `HAS` (an attribute stored by value) is its own scope.
    let scope = match (*is_embedded, *is_our, *is_my) {
        (true, _, _) => Some("HAS"),
        (false, true, _) => Some("our"),
        (false, false, true) => None,
        (false, false, false) => Some("has"),
    };
    let full_name = if *sigil == '$' {
        name.resolve()
    } else {
        format!("{}{}", sigil, name.resolve())
    };
    let mut decl = var_declaration(names, 
        &full_name,
        explicit_default.map(|value| {
            if *default_is_bind {
                Initializer::Bind(value)
            } else {
                Initializer::Assign(value)
            }
        }),
        scope,
        type_name,
        twigil,
        if *default_is_bind {
            None
        } else {
            explicit_default
        },
        false,
    )?;
    if let Some(key) = hash_key_type {
        if let Some(expression) = hash_key_type_expr {
            super::keyed_hash::insert_subset_shape(names, &mut decl, expression, key)?;
        } else {
            super::keyed_hash::insert_shape(names, &mut decl, key)?;
        }
    }
    attribute::add_traits(names, 
        &mut decl,
        &attribute::AttributeTraits {
            order: trait_order.clone(),
            is_rw: *is_rw,
            is_readonly: *is_readonly,
            is_required: is_required.clone(),
            is_default: is_default.clone(),
            is_built: *is_built,
            deprecated_message: deprecated_message.clone(),
            is_type: is_type.clone(),
            unknown_traits: unknown_traits.clone(),
            handles_terms: handles_terms.clone(),
            handles: handles.clone(),
        },
    )?;
    if let Some(constraint) = where_constraint {
        decl.fields
            .push(node_field(Some("where"), convert_expr(names, constraint)?));
    }
    Ok(Some(statement_expression(decl)))
}

/// `has [Type] $.x [is rw] [= EXPR]` -> `Stmt::HasDecl`. The converter renders
/// an attribute as a `VarDeclaration::Simple` with `scope => "has"` and a
/// `twigil` (`.` public / `!` private, none for the alias `has $x`); `our $.x`
/// has the `our` scope and `my $.x` none. Its traits are read by
/// `rakuast::attribute`, and an `Initializer::Assign` is the default (the
/// implicit `WillBuild` beside it carries the same expression).
pub(super) fn lower(names: &NameScope, node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let traits = super::attribute::lower_traits(names, node)?;
    let mut default_is_bind = false;
    let initializer = match node.fields.iter().find(|f| f.name == Some("initializer")) {
        None => None,
        Some(f) => {
            let init = child_node(&f.value)?;
            if !matches!(
                init.class,
                RakuAstClass::InitializerAssign | RakuAstClass::InitializerBind
            ) {
                return Err(unsupported(node));
            }
            default_is_bind = init.class == RakuAstClass::InitializerBind;
            Some(lower_expr(named_child_or_positional(init)?)?)
        }
    };
    let where_constraint = match node.fields.iter().find(|f| f.name == Some("where")) {
        None => None,
        Some(f) => Some(Box::new(lower_expr(child_node(&f.value)?)?)),
    };
    let sigil = leaf_str(node, "sigil")?;
    let mut chars = sigil.chars();
    let (Some(sigil_char), None) = (chars.next(), chars.next()) else {
        return Err(unsupported(node));
    };
    // The scope: `has` is the per-instance attribute; `our` and the default
    // `my` are class-level ones (the parser makes those `is rw`).
    // `HAS` is NativeCall's by-value `has`.
    let mut is_embedded = false;
    let (is_our, is_my, is_has) = match node.fields.iter().find(|f| f.name == Some("scope")) {
        None => (false, true, false),
        Some(_) => match leaf_str(node, "scope")?.as_str() {
            "has" => (false, false, true),
            "HAS" => {
                is_embedded = true;
                (false, false, true)
            }
            "our" => (true, false, false),
            _ => return Err(unsupported(node)),
        },
    };
    if is_has && default_is_bind {
        return Err(RuntimeError::new(
            "Cannot use := to initialize an attribute",
        ));
    }
    let (is_public, is_alias) = match node.fields.iter().find(|f| f.name == Some("twigil")) {
        // `has $x`, the alias of a private attribute.
        None if is_has => (false, true),
        None => return Err(unsupported(node)),
        Some(_) => match leaf_str(node, "twigil")?.as_str() {
            "." => (true, false),
            "!" => (false, false),
            _ => return Err(unsupported(node)),
        },
    };
    let desigil = match positional_leaf(named_child(node, "desigilname")?)?.view() {
        ValueView::Str(s) => s.to_string(),
        _ => return Err(unsupported(node)),
    };
    let type_name = match node.fields.iter().find(|f| f.name == Some("type")) {
        Some(f) => Some(simple_type_name(names, node, child_node(&f.value)?)?),
        None => None,
    };
    // `Int:D` comes back as the parser's base type plus its smiley.
    let (type_constraint, type_smiley) = match type_name.as_deref() {
        Some(name) => match name.rsplit_once(':') {
            Some((base, smiley @ ("D" | "U"))) => {
                (Some(base.to_string()), Some(smiley.to_string()))
            }
            _ => (type_name.clone(), None),
        },
        None => (None, None),
    };
    // A scalar `is default(EXPR)` attribute without an initializer starts
    // with the trait's value, as the parser records it.
    let default_is_trait =
        initializer.is_none() && traits.is_default.is_some() && matches!(sigil_char, '$' | '&');
    let initializer = if default_is_trait {
        traits.is_default.clone()
    } else {
        initializer
    };
    // The implicit default of a typed scalar attribute (see `attribute_type_seed`).
    let type_seed = match (&initializer, type_constraint.as_deref()) {
        (None, Some(tc)) => crate::parser::attribute_type_seed(
            sigil_char,
            tc,
            traits.is_required.is_some(),
            traits
                .unknown_traits
                .iter()
                .any(|(kind, name, _)| kind == "is" && name == "box_target"),
        ),
        _ => None,
    };
    Ok(Stmt::HasDecl {
        name: crate::symbol::Symbol::intern(&desigil),
        is_public,
        // A typed attribute carries an implicit default in the internal AST
        // (its type object, or a native type's zero); the parser plants it,
        // and the converter skips it on the way out, so re-plant the same one
        // here to keep the two sides symmetric.
        default_is_seed: type_seed.is_some(),
        default_is_trait,
        default: initializer.or(type_seed),
        handles: traits.handles,
        handles_terms: traits.handles_terms,
        is_rw: traits.is_rw || !is_has,
        is_readonly: traits.is_readonly,
        type_constraint,
        hash_key_type: super::keyed_hash::lower_key_type(names, node, &sigil)?,
        hash_key_type_expr: None,
        type_smiley,
        is_required: traits.is_required,
        sigil: sigil_char,
        where_constraint,
        is_alias,
        is_embedded,
        is_our,
        is_my,
        is_default: traits.is_default,
        is_type: traits.is_type,
        deprecated_message: traits.deprecated_message,
        is_built: traits.is_built,
        unknown_traits: traits.unknown_traits,
        default_is_bind,
        trait_order: traits.order,
    })
}
