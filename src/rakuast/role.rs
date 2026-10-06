//! `role NAME[PARAMS] TRAITS { body }` <-> `RakuAST::Role`, in both directions.
//!
//! Measured on rakudo 2026.09: the node holds `name`, then `traits` (the
//! header's `does` / `is` clauses and `is export` / `is rw`, in source order),
//! then `body` (a `RoleBody` around a `Blockoid`), then `parameterization`, a
//! `Signature` whose parameters carry no implicit `Type::Setting`.
//!
//! The parser folds the header's `does R` / `is P` clauses into `DoesDecl`
//! statements at the front of the body; an `also does R` statement in the body
//! is a `DoesDecl` too, told apart by its `also` flag. Lowering rebuilds the
//! same leading statements, and an `also` one is a `Statement::Also`.

use super::convert::{
    MY_SCOPED, blockoid, build_type_node, is_colons_package, node_field, package_header_fields,
    signature, unsupported,
};
use super::lower::{
    lower_package_body, lower_signature_parameters, lower_stmts, named_child,
    named_child_or_positional, package_head,
};
use super::routine_traits::IsTraits;
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{ParamDef, Stmt};
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

/// The fields of a `Stmt::RoleDecl` the RakuAST node is built from.
pub(super) struct RoleDecl<'a> {
    pub name: Symbol,
    pub type_params: &'a [String],
    pub type_param_defs: &'a [ParamDef],
    pub is_export: bool,
    pub export_tags: &'a [String],
    pub body: &'a [Stmt],
    pub is_rw: bool,
    pub custom_traits: &'a [(String, Option<crate::ast::Expr>)],
}

/// The `RakuAST::Role` node for a role declaration.
// Cost: O(n), n = size of the role's body.
pub(super) fn convert(role: RoleDecl<'_>) -> Result<RakuAstNode, RuntimeError> {
    let is_lexical = role.custom_traits.iter().any(|(t, _)| t == MY_SCOPED);
    if role
        .custom_traits
        .iter()
        .any(|(t, _)| t != MY_SCOPED && t != crate::parser::ANON_COLONS_TRAIT)
        || role.is_export != !role.export_tags.is_empty()
    {
        return Err(unsupported("role with custom traits"));
    }
    // The fallback parameter parser (defaults such as `::T = my role { }`)
    // names its parameters differently from the signature it records.
    if role.type_params != crate::parser::role_type_param_names(role.type_param_defs) {
        return Err(unsupported("role parameters the signature does not name"));
    }
    let header_len = role
        .body
        .iter()
        .take_while(|stmt| matches!(stmt, Stmt::DoesDecl { also: false, .. }))
        .count();
    let mut traits = Vec::new();
    for stmt in &role.body[..header_len] {
        let Stmt::DoesDecl {
            name,
            args,
            from_is,
            ..
        } = stmt
        else {
            unreachable!("counted above");
        };
        let name = name.resolve();
        // `hides` and `is hidden` are internal markers with their own shape.
        if name.starts_with("__mutsu_role_") {
            return Err(unsupported("role with `hides` / `is hidden`"));
        }
        let type_node = if let Some(args) = args {
            super::type_args::parameterized_type_node(&name, Some(args))?
        } else {
            build_type_node(&name)?
        };
        traits.push(Value::rakuast(Box::new(if *from_is {
            RakuAstNode {
                class: RakuAstClass::TraitIs,
                fields: vec![node_field(Some("type"), type_node)],
            }
        } else {
            RakuAstNode {
                class: RakuAstClass::TraitDoes,
                fields: vec![node_field(None, type_node)],
            }
        })));
    }
    // Rakudo lists the traits in source order, which the parser does not keep
    // between the parent clauses, `is export` and `is rw`.
    let kinds =
        usize::from(!traits.is_empty()) + usize::from(role.is_export) + usize::from(role.is_rw);
    if kinds > 1 {
        return Err(unsupported(
            "role traits whose source order is not recorded",
        ));
    }
    let flags = IsTraits {
        is_rw: role.is_rw,
        is_raw: false,
        export_tags: role.export_tags.to_vec(),
    };
    traits.extend(
        flags
            .nodes()
            .into_iter()
            .map(|t| Value::rakuast(Box::new(t))),
    );
    let mut fields =
        package_header_fields(role.name, is_lexical, is_colons_package(role.custom_traits));
    if !traits.is_empty() {
        fields.push(RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(traits),
        });
    }
    let body = crate::parser::unhoist_nested_methods(&role.body[header_len..]);
    fields.push(node_field(
        Some("body"),
        RakuAstNode {
            class: RakuAstClass::RoleBody,
            fields: vec![node_field(Some("body"), blockoid(&body)?)],
        },
    ));
    if !role.type_param_defs.is_empty() {
        fields.push(node_field(
            Some("parameterization"),
            signature(role.type_param_defs, false, None)?,
        ));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::Role,
        fields,
    })
}

/// A header parent clause (`does R[Int]`, `is P`) as the `DoesDecl` the
/// parser builds for it.
// Cost: O(k), k = length of the type's spelling.
fn parent_clause(
    owner: &RakuAstNode,
    type_node: &RakuAstNode,
    from_is: bool,
) -> Result<Stmt, RuntimeError> {
    let name = super::type_lower::type_constraint(owner, type_node)?;
    let args = super::type_lower::type_application_args(owner, type_node)?;
    Ok(Stmt::DoesDecl {
        name: Symbol::intern(&name),
        args,
        from_is,
        also: false,
    })
}

/// `RakuAST::Role` -> `Stmt::RoleDecl`, with the header clauses back at the
/// front of the body.
// Cost: O(n), n = size of the node.
pub(super) fn lower(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let head = package_head(node, crate::parser::next_anon_role_name)?;
    let mut custom_traits = Vec::new();
    if head.is_lexical {
        custom_traits.push((MY_SCOPED.to_string(), None));
    }
    custom_traits.extend(head.custom_traits());
    let mut header = Vec::new();
    let mut flags = IsTraits::default();
    if let Some(field) = node.fields.iter().find(|f| f.name == Some("traits")) {
        let RakuAstFieldValue::List(items) = &field.value else {
            return Err(unsupported_node(node));
        };
        for item in items {
            let ValueView::RakuAst(t) = item.view() else {
                return Err(unsupported_node(node));
            };
            match t.class {
                RakuAstClass::TraitDoes => {
                    header.push(parent_clause(node, named_child_or_positional(t)?, false)?);
                }
                RakuAstClass::TraitIs => {
                    if let Ok(type_node) = named_child(t, "type") {
                        header.push(parent_clause(node, type_node, true)?);
                        continue;
                    }
                    if !flags.read(t)? || flags.is_raw {
                        return Err(unsupported_node(node));
                    }
                }
                _ => return Err(unsupported_node(node)),
            }
        }
    }
    let role_body = named_child(node, "body")?;
    if role_body.class != RakuAstClass::RoleBody {
        return Err(unsupported_node(node));
    }
    let body = lower_package_body(lower_stmts(named_child_or_positional(named_child(
        role_body, "body",
    )?)?)?);
    header.extend(body);
    let type_param_defs = match named_child(node, "parameterization") {
        Ok(sig) => lower_signature_parameters(sig, node)?,
        Err(_) => Vec::new(),
    };
    Ok(Stmt::RoleDecl {
        name: Symbol::intern(&head.name),
        type_params: crate::parser::role_type_param_names(&type_param_defs),
        type_param_defs,
        is_export: !flags.export_tags.is_empty(),
        export_tags: flags.export_tags,
        body: header,
        is_rw: flags.is_rw,
        language_version: crate::parser::current_language_version(),
        custom_traits,
        decl_id: crate::ast::next_class_decl_id(),
    })
}

/// `also does R;` -> `Statement::Also(traits => (Trait::Does(R),))`.
// Cost: O(k), k = length of the role's spelling.
pub(super) fn also_statement(role: &str) -> Result<RakuAstNode, RuntimeError> {
    let does = RakuAstNode {
        class: RakuAstClass::TraitDoes,
        fields: vec![node_field(None, build_type_node(role)?)],
    };
    Ok(RakuAstNode {
        class: RakuAstClass::StatementAlso,
        fields: vec![RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(does))]),
        }],
    })
}

/// `Statement::Also` with one `Trait::Does` -> the `also`-flagged `DoesDecl`.
// Cost: O(k), k = length of the role's spelling.
pub(super) fn lower_also(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let Some(RakuAstField {
        value: RakuAstFieldValue::List(items),
        ..
    }) = node.fields.iter().find(|f| f.name == Some("traits"))
    else {
        return Err(unsupported_node(node));
    };
    let [item] = items.as_slice() else {
        return Err(unsupported_node(node));
    };
    match item.view() {
        ValueView::RakuAst(t) if t.class == RakuAstClass::TraitDoes => {
            let Stmt::DoesDecl { name, args, .. } =
                parent_clause(node, named_child_or_positional(t)?, false)?
            else {
                unreachable!("parent_clause builds a DoesDecl");
            };
            Ok(Stmt::DoesDecl {
                name,
                args,
                from_is: false,
                also: true,
            })
        }
        _ => Err(unsupported_node(node)),
    }
}

/// `RakuAST::Statement::Also.new(traits => (...))`, or `None` for any other
/// class.
// Cost: O(a), a = number of arguments.
pub(super) fn construct(
    class_name: &str,
    method: &str,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    if (class_name, method) != ("RakuAST::Statement::Also", "new") {
        return None;
    }
    let mut traits = Vec::new();
    for arg in args {
        match arg.view() {
            ValueView::Pair(key, value) if key.as_str() == "traits" => {
                traits = match value.view() {
                    ValueView::RakuAst(_) => vec![value.clone()],
                    _ => match value.as_list_items() {
                        Some(items) => items.to_vec(),
                        None => {
                            return Some(Err(RuntimeError::new(
                                "RakuAST::Statement::Also.new expects `traits` to be a list",
                            )));
                        }
                    },
                };
            }
            _ => {
                return Some(Err(RuntimeError::new(
                    "RakuAST::Statement::Also.new takes only `traits`",
                )));
            }
        }
    }
    Some(Ok(Value::rakuast(Box::new(RakuAstNode {
        class: RakuAstClass::StatementAlso,
        fields: vec![RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(traits),
        }],
    }))))
}

fn unsupported_node(node: &RakuAstNode) -> RuntimeError {
    super::lower::unsupported(node)
}
