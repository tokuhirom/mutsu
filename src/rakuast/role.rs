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

use crate::scope::NameScope;
use super::convert::{
    blockoid, build_type_node, node_field, package_header_fields, signature, unsupported,
};
use super::lower::{
    lower_package_body, lower_signature_parameters, lower_stmts, named_child,
    named_child_or_positional, package_head, positional_leaf,
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
    pub is_lexical: bool,
    pub custom_traits: &'a [(String, Option<crate::ast::Expr>)],
    pub marks: crate::ast::PackageMarks,
}

/// The `RakuAST::Role` node for a role declaration.
// Cost: O(n), n = size of the role's body.
pub(super) fn convert(scope: &NameScope, role: RoleDecl<'_>) -> Result<RakuAstNode, RuntimeError> {
    let is_lexical = role.is_lexical;
    // Any plain trait name is a trait of the program's own (`is array_type(T)`),
    // rendered by name.
    let user_traits = role.custom_traits;
    // Rakudo drops `is repr<...>` from a role's traits, so it cannot come back.
    if user_traits
        .iter()
        .any(|(t, _)| !super::decl_traits::is_class_trait(t) || t == "repr")
        || role.is_export != !role.export_tags.is_empty()
    {
        return Err(unsupported(
            "role with a trait the converter does not render",
        ));
    }
    // The fallback parameter parser (defaults such as `::T = my role { }`)
    // names its parameters differently from the signature it records.
    if role.type_params != crate::parser::role_type_param_names(role.type_param_defs) {
        return Err(unsupported("role parameters the signature does not name"));
    }
    // The header's `does R` / `is P` / `hides H` clauses lead the body as
    // `DoesDecl`s (a hidden role adds a marker, `hides` a second statement);
    // the traits themselves are the ones the source wrote, in written order.
    let header_len = role
        .body
        .iter()
        .take_while(|stmt| {
            stmt.is_routine_traits() || matches!(stmt, Stmt::DoesDecl { also: false, .. })
        })
        .count();
    let written = crate::ast::routine_trait::get(role.body).unwrap_or_default();
    let expected: usize = written
        .iter()
        .map(|t| match t {
            crate::ast::routine_trait::RoutineTrait::Parent {
                kind: crate::ast::routine_trait::ParentKind::Hides,
                ..
            } => 2,
            crate::ast::routine_trait::RoutineTrait::Parent { .. } => 1,
            crate::ast::routine_trait::RoutineTrait::Is { name, .. } if name == "hidden" => 1,
            _ => 0,
        })
        .sum();
    let composed: Vec<&Stmt> = role.body[..header_len]
        .iter()
        .filter(|stmt| matches!(stmt, Stmt::DoesDecl { .. }))
        .collect();
    if composed.len() < expected || role.is_export != super::convert::has_is(written, "export") {
        return Err(unsupported(
            "role header the written-trait record does not account for",
        ));
    }
    let mut traits = super::routine_source::header_traits(scope, role.body)?;
    // What follows the written clauses is `also is Parent;` / `also is rw;`
    // lifted out of the body: a parent without a header spelling renders as
    // a trailing `is` trait, which lowers back to the same clause.
    for stmt in &composed[expected..] {
        let Stmt::DoesDecl {
            name,
            args,
            from_is: true,
            ..
        } = stmt
        else {
            return Err(unsupported(
                "role header the written-trait record does not account for",
            ));
        };
        let parent = crate::ast::routine_trait::RoutineTrait::Parent {
            kind: crate::ast::routine_trait::ParentKind::Is,
            name: name.resolve(),
            args: args.clone(),
        };
        traits.push(Value::rakuast(Box::new(super::routine_source::trait_node(scope, 
            &parent,
        )?)));
    }
    if role.is_rw && !super::convert::has_is(written, "rw") {
        traits.push(Value::rakuast(Box::new(super::routine_traits::trait_is(
            "rw", None,
        ))));
    }
    let scope = if role.marks.anon {
        Some("anon")
    } else {
        is_lexical.then_some("my")
    };
    let mut fields = package_header_fields(role.name, scope, role.marks.colons);
    super::role_header::retain_name(&mut fields, role.name);
    super::declare_how::add_field(&mut fields, role.marks.declarator.as_deref(), "role");
    if !traits.is_empty() {
        fields.push(RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(traits),
        });
    }
    let body = super::nested_methods::unhoist(&role.body[header_len..]);
    fields.push(node_field(
        Some("body"),
        super::role_implicit::add(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::RoleBody,
            fields: vec![node_field(Some("body"), blockoid(scope, &body)?)],
        }),
    ));
    if !role.type_param_defs.is_empty() {
        fields.push(node_field(
            Some("parameterization"),
            signature(role.type_param_defs, false, None)?,
        ));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
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
    let name = super::type_lower::parent_type_constraint(owner, type_node)?;
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
    let retained = super::role_header::retained_name(node)?;
    let head = package_head(node, || {
        retained.unwrap_or_else(crate::parser::next_anon_role_name)
    })?;
    if head.is_unit {
        return Err(super::lower::unsupported(node));
    }
    let mut custom_traits = Vec::new();
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
                    if let Ok(name) = named_child(t, "name")
                        && matches!(positional_leaf(name)?.view(), ValueView::Str(s) if s.as_str() == "hidden")
                    {
                        header.push(Stmt::DoesDecl {
                            name: Symbol::intern("__mutsu_role_hidden__"),
                            args: None,
                            from_is: false,
                            also: false,
                        });
                        continue;
                    }
                    if let Some(entry) = super::routine_traits::lower_custom(node, t)? {
                        custom_traits.push(entry);
                        continue;
                    }
                    if !flags.read(t)? || flags.is_raw || flags.is_hidden_from_backtrace {
                        return Err(unsupported_node(node));
                    }
                }
                RakuAstClass::TraitHides => {
                    let hidden_type = named_child_or_positional(t)?;
                    let hidden_name = super::type_lower::type_constraint(node, hidden_type)?;
                    header.push(Stmt::DoesDecl {
                        name: Symbol::intern(&hidden_name),
                        args: None,
                        from_is: false,
                        also: false,
                    });
                    header.push(Stmt::DoesDecl {
                        name: Symbol::intern(
                            &crate::meta_ns::MetaNs::RoleHides.owned_key_for_str(&hidden_name),
                        ),
                        args: None,
                        from_is: false,
                        also: false,
                    });
                }
                _ => return Err(unsupported_node(node)),
            }
        }
    }
    let role_body = named_child(node, "body")?;
    if role_body.class != RakuAstClass::RoleBody {
        return Err(unsupported_node(node));
    }
    let stripped = super::role_implicit::shown(role_body);
    let role_body = stripped.as_ref().unwrap_or(role_body);
    let body = lower_package_body(lower_stmts(named_child_or_positional(named_child(
        role_body, "body",
    )?)?)?);
    header.extend(body);
    let type_param_defs = match named_child(node, "parameterization") {
        Ok(sig) => lower_signature_parameters(sig, node)?,
        Err(_) => Vec::new(),
    };
    Ok(head.finish(Stmt::RoleDecl {
        spans: Default::default(),
        name: Symbol::intern(&head.name),
        type_params: crate::parser::role_type_param_names(&type_param_defs),
        type_param_defs,
        is_export: !flags.export_tags.is_empty(),
        export_tags: flags.export_tags,
        body: header,
        is_rw: flags.is_rw,
        is_lexical: head.is_lexical,
        language_version: crate::parser::current_language_version(),
        custom_traits,
        decl_id: crate::ast::next_class_decl_id(),
        marks: Default::default(),
    }))
}

/// `also does R[ARGS];` -> `Statement::Also(traits => (Trait::Does(R[ARGS]),))`.
// Cost: O(k + a), k = role spelling length; a = number of arguments.
pub(super) fn also_statement(
    role: &str,
    args: Option<&[crate::ast::Expr]>,
) -> Result<RakuAstNode, RuntimeError> {
    let does = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::TraitDoes,
        fields: vec![node_field(
            None,
            if let Some(args) = args {
                super::type_args::parameterized_type_node(role, Some(args))?
            } else {
                build_type_node(role)?
            },
        )],
    };
    Ok(RakuAstNode {
        origin: Default::default(),
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
        // `also is Parent;`: only a class body folds it into its parents
        // (`lower_class` takes these markers out of the body again).
        ValueView::RakuAst(t) if t.class == RakuAstClass::TraitIs => {
            // `is Parent` is a `type` when the parent is a known type, a plain
            // `name` when it is not (declared outside this unit).
            let (name, args) = match named_child(t, "type") {
                Ok(type_node) => {
                    let Stmt::DoesDecl { name, args, .. } = parent_clause(node, type_node, true)?
                    else {
                        unreachable!("parent_clause builds a DoesDecl");
                    };
                    (name, args)
                }
                Err(_) => match positional_leaf(named_child(t, "name")?)?.view() {
                    ValueView::Str(s) if !matches!(s.as_str(), "rw" | "hidden") => {
                        (Symbol::intern(s.as_str()), None)
                    }
                    _ => return Err(unsupported_node(node)),
                },
            };
            Ok(Stmt::DoesDecl {
                name,
                args,
                from_is: true,
                also: true,
            })
        }
        _ => Err(unsupported_node(node)),
    }
}

/// The class parents a lowered body spells as `also is Parent;`, taken out of
/// `body` in source order.
// Cost: O(n), n = number of statements in the body.
pub(super) fn take_also_is_parents(body: &mut Vec<Stmt>) -> Vec<String> {
    let mut parents = Vec::new();
    body.retain(|stmt| match stmt {
        Stmt::DoesDecl {
            name,
            from_is: true,
            also: true,
            ..
        } => {
            parents.push(name.resolve());
            false
        }
        _ => true,
    });
    parents
}

/// `also is P;` statements (one per parent) put in front of a class body's
/// `Block`: the parser lifts them out of the body into the class's parents and
/// does not keep where they stood, so they lead it.
// Cost: O(n), n = number of statements in the body.
pub(super) fn with_leading_statements(
    mut block: RakuAstNode,
    statements: Vec<RakuAstNode>,
) -> RakuAstNode {
    fn child(node: &mut RakuAstNode, name: Option<&str>) -> Option<RakuAstNode> {
        let field = node.fields.iter_mut().find(|f| f.name == name)?;
        match &field.value {
            RakuAstFieldValue::Node(v) => match v.view() {
                ValueView::RakuAst(n) => Some(n.clone()),
                _ => None,
            },
            _ => None,
        }
    }
    fn put(node: &mut RakuAstNode, name: Option<&str>, value: RakuAstNode) {
        if let Some(field) = node.fields.iter_mut().find(|f| f.name == name) {
            field.value = RakuAstFieldValue::Node(Value::rakuast(Box::new(value)));
        }
    }
    let Some(mut blockoid) = child(&mut block, Some("body")) else {
        return block;
    };
    let Some(mut list) = child(&mut blockoid, None) else {
        return block;
    };
    let mut fields: Vec<RakuAstField> = statements
        .into_iter()
        .map(|s| node_field(None, s))
        .collect();
    fields.append(&mut list.fields);
    list.fields = fields;
    put(&mut blockoid, None, list);
    put(&mut block, Some("body"), blockoid);
    block
}

/// `also is P;` -> `Statement::Also(traits => (Trait::Is(type => P),))`.
// Cost: O(k), k = length of the parent's spelling.
pub(super) fn also_is_statement(trait_node: Value) -> RakuAstNode {
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::StatementAlso,
        fields: vec![RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(vec![trait_node]),
        }],
    }
}

fn unsupported_node(node: &RakuAstNode) -> RuntimeError {
    super::lower::unsupported(node)
}
