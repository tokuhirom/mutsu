//! Attribute traits (`has $.x is rw = 5`) across the RakuAST boundary.
//!
//! Measured against rakudo 2026.09, an attribute is a
//! `VarDeclaration::Simple(scope => "has", …)` whose `traits` list holds one
//! node per written trait, in written order, followed by the implicit
//! `Trait::WillBuild(EXPR)` an `= EXPR` default adds:
//!
//! - `is NAME` / `is NAME(ARGS)` is `Trait::Is(name => Name.from-identifier(NAME)
//!   [, argument => Circumfix::Parentheses(SemiList(Statement::Expression(ARGS)))])`
//!   for `rw`, `readonly`, `required`, `default`, `built`, `DEPRECATED` and any
//!   trait of the program's own; `is TYPE` (a type known at parse time) is
//!   `Trait::Is(type => Type::Simple)`;
//! - `does ROLE` is `Trait::Does(Type::Simple)`;
//! - `handles TERM` is `Trait::Handles(TERM)`.
//!
//! The default is also the `initializer`, and the gist omits the implicit
//! `WillBuild` (only `.traits` shows it). A scalar attribute with
//! `is default(EXPR)` and no initializer starts with that value, which the
//! parser also stores as its `default` (flagged `default_is_trait`), and rakudo
//! shows no initializer.
//!
//! The parser records each trait in a field of `Stmt::HasDecl` and their order
//! in `trait_order` (`ast::attr_trait`); a custom trait, a `does` and each
//! `handles` clause take the next entry of their own list. A `handles TERM`
//! clause keeps its written term beside the delegation specs it reads from it (a
//! name, a word list, `*`), and a clause spelled any other way (a rename pair, a
//! regex, a variable) stays refused. The parser keeps `is built`'s argument as a
//! `Bool`, so `is built(True)` comes back as the bare `is built` it means, and
//! `is DEPRECATED("m")`'s as its message string (a bare `is DEPRECATED` is the
//! message `something else`).

mod argument;
use crate::scope::NameScope;
use super::convert::{convert_expr, name_from_identifier, node_field, unsupported};
use super::lower::{lower_expr, named_child, named_child_or_positional, positional_leaf};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::ast::attr_trait::AttrTrait;
use crate::ast_visit::VisitMut;
use crate::value::{RuntimeError, Value, ValueView};
use argument::trait_argument;
pub(super) use argument::{lower_paren_argument, paren_argument};

/// The message the parser records for a bare `is DEPRECATED`.
const DEPRECATED_DEFAULT: &str = "something else";

/// The traits of an attribute, as `HasDecl` records them.
#[derive(Debug, Default, Clone)]
pub(super) struct AttributeTraits {
    /// The kinds in written order.
    pub(super) order: Vec<AttrTrait>,
    pub(super) is_rw: bool,
    pub(super) is_readonly: bool,
    /// `None` = not required, `Some(None)` = `is required`,
    /// `Some(Some(reason))` = `is required("reason")`.
    pub(super) is_required: Option<Option<String>>,
    /// `is default(EXPR)`.
    pub(super) is_default: Option<Expr>,
    /// `is built` (`true`) or `is built(False)`.
    pub(super) is_built: Option<crate::ast::BuiltTrait>,
    /// `is DEPRECATED` / `is DEPRECATED("message")`.
    pub(super) deprecated_message: Option<String>,
    /// `is TYPE` on an `@` / `%` attribute.
    pub(super) is_type: Option<String>,
    /// `(kind, name, argument)` of each other `is NAME`, `will` and `does`.
    pub(super) unknown_traits: Vec<(String, String, Option<Expr>)>,
    /// The written term of each `handles` clause.
    pub(super) handles_terms: Vec<Expr>,
    /// The specs those terms name (`parser::handle_specs_from_term`).
    pub(super) handles: Vec<crate::ast::HandleSpec>,
}

fn trait_is(name: &str, argument: Option<RakuAstNode>) -> Value {
    let mut fields = vec![node_field(Some("name"), name_from_identifier(name))];
    if let Some(argument) = argument {
        fields.push(node_field(Some("argument"), argument));
    }
    Value::rakuast(Box::new(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::TraitIs,
        fields,
    }))
}

/// `is NAME`, which is a container type when `NAME` names one at parse time.
fn trait_is_name_or_type(scope: &NameScope, name: &str) -> Value {
    if super::decl_traits::is_container_type(scope, name) {
        return Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::TraitIs,
            fields: vec![node_field(
                Some("type"),
                super::bareword::simple_type_node(name),
            )],
        }));
    }
    trait_is(name, None)
}

impl AttributeTraits {
    /// The `Trait::*` nodes of the written traits, in written order.
    // Cost: O(t + a), t = traits, a = size of their arguments.
    fn items(&self, scope: &NameScope) -> Result<Vec<Value>, RuntimeError> {
        let mut custom = self.unknown_traits.iter();
        let mut handles = self.handles_terms.iter();
        let mut items = Vec::with_capacity(self.order.len());
        for kind in &self.order {
            let item = match kind {
                AttrTrait::Rw => trait_is("rw", None),
                AttrTrait::Readonly => trait_is("readonly", None),
                AttrTrait::Required => match &self.is_required {
                    Some(Some(reason)) => trait_is(
                        "required",
                        Some(paren_argument(&Expr::Literal(Value::str(reason.clone())))?),
                    ),
                    _ => trait_is("required", None),
                },
                AttrTrait::Default => {
                    let value = self
                        .is_default
                        .as_ref()
                        .ok_or_else(|| unsupported("attribute `is default` without a value"))?;
                    trait_is("default", Some(trait_argument(value)?))
                }
                AttrTrait::Built => match self.is_built {
                    Some(crate::ast::BuiltTrait::No) => trait_is(
                        "built",
                        Some(paren_argument(&Expr::Literal(Value::truth(false)))?),
                    ),
                    Some(crate::ast::BuiltTrait::Bind) => {
                        trait_is("built", Some(paren_argument(&bind_adverb_expr())?))
                    }
                    _ => trait_is("built", None),
                },
                AttrTrait::Deprecated => match self.deprecated_message.as_deref() {
                    Some(DEPRECATED_DEFAULT) | None => trait_is("DEPRECATED", None),
                    Some(message) => trait_is(
                        "DEPRECATED",
                        Some(paren_argument(&Expr::Literal(Value::str(
                            message.to_string(),
                        )))?),
                    ),
                },
                AttrTrait::Type => {
                    let name = self
                        .is_type
                        .as_deref()
                        .ok_or_else(|| unsupported("attribute `is TYPE` without a type"))?;
                    Value::rakuast(Box::new(RakuAstNode {
                        origin: Default::default(),
                        class: RakuAstClass::TraitIs,
                        fields: vec![node_field(
                            Some("type"),
                            super::decl_traits::type_node(name)?,
                        )],
                    }))
                }
                AttrTrait::Custom => {
                    let (kind, name, argument) = custom
                        .next()
                        .ok_or_else(|| unsupported("attribute trait list out of step"))?;
                    match kind.as_str() {
                        "is" => match argument {
                            None => trait_is_name_or_type(scope, name),
                            Some(argument) => trait_is(name, Some(trait_argument(argument)?)),
                        },
                        "does" => Value::rakuast(Box::new(RakuAstNode {
                            origin: Default::default(),
                            class: RakuAstClass::TraitDoes,
                            fields: vec![node_field(None, super::bareword::simple_type_node(name))],
                        })),
                        "will" => match argument {
                            Some(Expr::AnonSub {
                                body,
                                is_block: true,
                                is_rw: false,
                                is_raw: false,
                                ..
                            }) => Value::rakuast(Box::new(RakuAstNode {
                                origin: Default::default(),
                                class: RakuAstClass::TraitWill,
                                fields: vec![
                                    super::convert::leaf_field(
                                        Some("phase"),
                                        Value::str_from(name.as_str()),
                                    ),
                                    node_field(
                                        Some("block"),
                                        super::convert::signature_block_node(body)?,
                                    ),
                                ],
                            })),
                            _ => return Err(unsupported("attribute `will` trait without a block")),
                        },
                        _ => return Err(unsupported("attribute with an unknown trait kind")),
                    }
                }
                AttrTrait::Handles => {
                    let term = handles
                        .next()
                        .ok_or_else(|| unsupported("attribute `handles` spelling not kept"))?;
                    Value::rakuast(Box::new(RakuAstNode {
                        origin: Default::default(),
                        class: RakuAstClass::TraitHandles,
                        fields: vec![node_field(None, convert_handles_term(term)?)],
                    }))
                }
            };
            items.push(item);
        }
        Ok(items)
    }
}

/// Bare identifiers in a `handles(...)` list name delegated methods, so their
/// term value is the method name string rather than a lexical bareword.
// Cost: O(e), e = size of the handles term.
fn convert_handles_term(scope: &NameScope, term: &Expr) -> Result<RakuAstNode, RuntimeError> {
    struct MethodNames;
    impl VisitMut for MethodNames {
        fn visit_expr_mut(&mut self, expr: &mut Expr) {
            if matches!(
                expr,
                Expr::Unary {
                    op: crate::token_kind::TokenKind::Pipe,
                    ..
                }
            ) {
                return;
            }
            if let Expr::BareWord(name) = expr {
                *expr = Expr::Literal(Value::str(name.clone()));
                return;
            }
            crate::ast_visit::walk_expr_mut(self, expr);
        }
    }
    let mut term = term.clone();
    MethodNames.visit_expr_mut(&mut term);
    convert_expr(scope, &term)
}

/// Put the attribute's written traits at the front of `decl`'s `traits` list,
/// ahead of an implicit `WillBuild`, creating the field (after `desigilname`)
/// when the declaration has no default.
// Cost: O(f + t), f = fields of `decl`, t = traits.
pub(super) fn add_traits(
    decl: &mut RakuAstNode,
    traits: &AttributeTraits,
) -> Result<(), RuntimeError> {
    let mut items = traits.items()?;
    if items.is_empty() {
        return Ok(());
    }
    if let Some(field) = decl.fields.iter_mut().find(|f| f.name == Some("traits")) {
        let RakuAstFieldValue::List(existing) = &mut field.value else {
            return Err(unsupported("attribute traits field"));
        };
        items.append(existing);
        *existing = items;
        return Ok(());
    }
    let at = decl
        .fields
        .iter()
        .position(|f| f.name == Some("desigilname"))
        .map_or(decl.fields.len(), |i| i + 1);
    decl.fields.insert(
        at,
        RakuAstField {
            name: Some("traits"),
            value: RakuAstFieldValue::List(items),
        },
    );
    Ok(())
}

/// The reason of an `is required("reason")` / the message of an
/// `is DEPRECATED("message")`: a plain string.
fn string_argument(argument: &Expr) -> Option<String> {
    match argument {
        Expr::Literal(v) => v.as_str().map(str::to_string),
        _ => None,
    }
}

/// The traits of an attribute `VarDeclaration::Simple`, back as `HasDecl`
/// fields and their written order. An implicit `WillBuild` is accepted only
/// alongside an `initializer`, which carries the same default.
// Cost: O(t + a), t = traits of the declaration, a = size of their arguments.
pub(super) fn lower_traits(node: &RakuAstNode) -> Result<AttributeTraits, RuntimeError> {
    let refuse = || super::lower::unsupported(node);
    let mut traits = AttributeTraits::default();
    let Some(field) = node.fields.iter().find(|f| f.name == Some("traits")) else {
        return Ok(traits);
    };
    let RakuAstFieldValue::List(items) = &field.value else {
        return Err(refuse());
    };
    let has_initializer = node.fields.iter().any(|f| f.name == Some("initializer"));
    // `is TYPE` is a container type of an `@` / `%` attribute; on a `$`
    // attribute the parser keeps it as a custom trait.
    let is_aggregate = matches!(super::lower::leaf_str(node, "sigil")?.as_str(), "@" | "%");
    for item in items {
        let ValueView::RakuAst(t) = item.view() else {
            return Err(refuse());
        };
        match t.class {
            RakuAstClass::TraitWillBuild if has_initializer => {}
            // `handles TERM`: the specs come from the term, as the parser
            // builds them.
            RakuAstClass::TraitHandles => {
                let term = lower_expr(named_child_or_positional(t)?)?;
                let specs = crate::parser::handle_specs_from_term(&term).ok_or_else(refuse)?;
                traits.handles.extend(specs);
                traits.handles_terms.push(term);
                traits.order.push(AttrTrait::Handles);
            }
            // `does ROLE`.
            RakuAstClass::TraitDoes => {
                let role = super::lower::simple_type_name(node, named_child_or_positional(t)?)?;
                traits.unknown_traits.push(("does".to_string(), role, None));
                traits.order.push(AttrTrait::Custom);
            }
            // `will PHASE BLOCK`: the block is trait_mod:<will>'s positional
            // Callable, as the parser keeps it.
            RakuAstClass::TraitWill => {
                let phase = super::lower::leaf_str(t, "phase")?;
                let body = super::lower::lower_block(named_child(t, "block")?)?;
                traits.unknown_traits.push((
                    "will".to_string(),
                    phase,
                    Some(Expr::AnonSub {
                        body,
                        is_rw: false,
                        is_raw: false,
                        is_block: true,
                        doc: Default::default(),
                    }),
                ));
                traits.order.push(AttrTrait::Custom);
            }
            RakuAstClass::TraitIs => lower_trait_is(node, t, is_aggregate, &mut traits)?,
            _ => return Err(refuse()),
        }
    }
    Ok(traits)
}

/// One `Trait::Is` of an attribute.
fn lower_trait_is(
    node: &RakuAstNode,
    t: &RakuAstNode,
    is_aggregate: bool,
    traits: &mut AttributeTraits,
) -> Result<(), RuntimeError> {
    let refuse = || super::lower::unsupported(node);
    // `is TYPE`.
    if let [field] = t.fields.as_slice()
        && field.name == Some("type")
    {
        let type_node = named_child(t, "type")?;
        if !matches!(
            type_node.class,
            RakuAstClass::TypeSimple | RakuAstClass::TypeParameterized
        ) {
            return Err(refuse());
        }
        let name = super::type_lower::type_constraint(t, type_node)?;
        if is_aggregate {
            traits.is_type = Some(name);
            traits.order.push(AttrTrait::Type);
        } else {
            traits.unknown_traits.push(("is".to_string(), name, None));
            traits.order.push(AttrTrait::Custom);
        }
        return Ok(());
    }
    let name_leaf = positional_leaf(named_child(t, "name")?)?;
    let ValueView::Str(name) = name_leaf.view() else {
        return Err(refuse());
    };
    let name = name.to_string();
    let argument = match named_child(t, "argument") {
        Ok(argument) => Some(if argument.class == RakuAstClass::CircumfixArrayComposer {
            lower_expr(argument)?
        } else {
            lower_paren_argument(node, argument)?
        }),
        Err(_) if t.fields.len() == 1 => None,
        Err(_) => return Err(refuse()),
    };
    match (name.as_str(), argument) {
        ("rw", None) => {
            traits.is_rw = true;
            traits.order.push(AttrTrait::Rw);
        }
        ("readonly", None) => {
            traits.is_readonly = true;
            traits.order.push(AttrTrait::Readonly);
        }
        ("required", None) => {
            traits.is_required = Some(None);
            traits.order.push(AttrTrait::Required);
        }
        ("required", Some(reason)) => {
            let reason = string_argument(&reason).ok_or_else(refuse)?;
            traits.is_required = Some(Some(reason));
            traits.order.push(AttrTrait::Required);
        }
        ("default", Some(value)) => {
            traits.is_default = Some(value);
            traits.order.push(AttrTrait::Default);
        }
        ("built", None) => {
            traits.is_built = Some(crate::ast::BuiltTrait::Yes);
            traits.order.push(AttrTrait::Built);
        }
        ("built", Some(argument)) if bool_keyword(&argument).is_some() => {
            traits.is_built = Some(if bool_keyword(&argument) == Some(true) {
                crate::ast::BuiltTrait::Yes
            } else {
                crate::ast::BuiltTrait::No
            });
            traits.order.push(AttrTrait::Built);
        }
        ("built", Some(argument)) if is_bind_adverb(&argument) => {
            traits.is_built = Some(crate::ast::BuiltTrait::Bind);
            traits.order.push(AttrTrait::Built);
        }
        ("DEPRECATED", None) => {
            traits.deprecated_message = Some(DEPRECATED_DEFAULT.to_string());
            traits.order.push(AttrTrait::Deprecated);
        }
        ("DEPRECATED", Some(message)) => {
            let message = string_argument(&message).ok_or_else(refuse)?;
            traits.deprecated_message = Some(message);
            traits.order.push(AttrTrait::Deprecated);
        }
        // An uppercase name on an `@` / `%` attribute is a container type to
        // the parser, whether or not it names one.
        (_, None) if is_aggregate && name.starts_with(|c: char| c.is_ascii_uppercase()) => {
            traits.is_type = Some(name);
            traits.order.push(AttrTrait::Type);
        }
        (_, argument) => {
            traits
                .unknown_traits
                .push(("is".to_string(), name, argument));
            traits.order.push(AttrTrait::Custom);
        }
    }
    Ok(())
}

/// The `:bind` of `is built(:bind)`, as the parser spells a colon pair.
// Cost: O(1).
fn bind_adverb_expr() -> Expr {
    Expr::Binary {
        left: Box::new(Expr::Literal(Value::str("bind".to_string()))),
        op: crate::token_kind::TokenKind::FatArrow,
        right: Box::new(Expr::Literal(Value::truth(true))),
        form: crate::ast::BinaryForm::ColonPairTrue,
    }
}

/// The Bool a bare `True`/`False` argument stands for. In a unit that imported
/// through a run-time `sub EXPORT` hook the keyword lowers to
/// `ShadowableTermKeyword` instead of a plain literal (#9047), and `is built(False)`
/// is the same trait either way, as the parser's `has` path reads it by spelling.
// Cost: O(1).
fn bool_keyword(argument: &Expr) -> Option<bool> {
    match argument {
        Expr::Literal(value) => match value.view() {
            ValueView::Bool(b) => Some(b),
            _ => None,
        },
        Expr::ShadowableTermKeyword { value, .. } => match value.view() {
            ValueView::Bool(b) => Some(b),
            _ => None,
        },
        _ => None,
    }
}

fn is_bind_adverb(argument: &Expr) -> bool {
    let Expr::Binary {
        left,
        op: crate::token_kind::TokenKind::FatArrow,
        right,
        form: crate::ast::BinaryForm::ColonPairTrue,
    } = argument
    else {
        return false;
    };
    matches!(left.as_ref(), Expr::Literal(v) if matches!(v.view(), ValueView::Str(s) if s.as_str() == "bind"))
        && matches!(right.as_ref(), Expr::Literal(v) if matches!(v.view(), ValueView::Bool(true)))
}

/// The gist of an attribute omits the implicit `WillBuild` its default adds
/// (`.traits` still answers it): `decl` without it, or `None` when there is
/// none to hide.
// Cost: O(f + t), f = fields of `decl`, t = its traits.
pub(super) fn without_implicit_traits(decl: &RakuAstNode) -> Option<RakuAstNode> {
    let is_will_build = |v: &Value| matches!(v.view(), ValueView::RakuAst(t) if t.class == RakuAstClass::TraitWillBuild);
    let traits = decl.fields.iter().find(|f| f.name == Some("traits"))?;
    let RakuAstFieldValue::List(items) = &traits.value else {
        return None;
    };
    if !items.iter().any(is_will_build) {
        return None;
    }
    let mut shown = decl.clone();
    shown.fields.retain_mut(|f| {
        if f.name != Some("traits") {
            return true;
        }
        if let RakuAstFieldValue::List(items) = &mut f.value {
            items.retain(|v| !is_will_build(v));
            !items.is_empty()
        } else {
            true
        }
    });
    Some(shown)
}
