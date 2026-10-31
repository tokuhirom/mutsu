//! `.AST` of a construct the parser built in its RakuAST shape (ADR-10723
//! Stage 2): the RakuAST node rendered from the parser's [`ParsedStmt`], with
//! the origins the parser recorded on its blocks. Nothing here reverse-engineers a desugaring;
//! the statement's own origin is added by the statement list, as for every
//! statement.

use super::convert::{
    block_node, convert_expr, label_fields, loop_setup_node, node_field, pointy_block,
    signature_block_node, statement_expression, topic_block_node,
};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Stmt;
use crate::ast::parsed::{
    BlockParams, BlockSpans, ClauseKind, CompoundMeta, Conditional, ConditionalKind, LoopKind,
    LoopStmt, Modified, ModifierKind, ParsedBlock, ParsedExpr, ParsedStmt,
};
use crate::value::{RuntimeError, Value, ValueView};

/// The RakuAST node of a construct the parser built in its RakuAST shape.
// Cost: O(n), n = size of the construct.
pub(super) fn parsed_node(node: &ParsedStmt) -> Result<RakuAstNode, RuntimeError> {
    Ok(match node {
        ParsedStmt::Conditional(c) => conditional_node(c)?,
        ParsedStmt::Given { topic, block } => RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::StatementGiven,
            fields: vec![
                node_field(Some("source"), convert_expr(topic)?),
                node_field(Some("body"), view_block(block, BlockKind::Topic)?),
            ],
        },
        ParsedStmt::When { condition, block } => RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::StatementWhen,
            fields: vec![
                node_field(Some("condition"), convert_expr(condition)?),
                node_field(Some("body"), view_block(block, BlockKind::Signature)?),
            ],
        },
        ParsedStmt::Default(block) => RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::StatementDefault,
            fields: vec![node_field(
                Some("body"),
                view_block(block, BlockKind::Plain)?,
            )],
        },
        ParsedStmt::WhileLoop(w) => {
            let mut fields = label_fields(&w.label);
            fields.push(node_field(Some("condition"), convert_expr(&w.condition)?));
            fields.push(node_field(
                Some("body"),
                view_block(&w.block, BlockKind::Signature)?,
            ));
            RakuAstNode {
                origin: Default::default(),
                class: if w.until {
                    RakuAstClass::StatementLoopUntil
                } else {
                    RakuAstClass::StatementLoopWhile
                },
                fields,
            }
        }
        ParsedStmt::BareBlock(block) => {
            statement_expression(view_block(block, BlockKind::Signature)?)
        }
        ParsedStmt::Loop(l) => loop_node(l)?,
        ParsedStmt::BindDecl(decl) => {
            let parts = super::convert::var_decl_parts(decl)?;
            super::convert::var_decl_statement(parts, true)?
                .ok_or_else(|| super::convert::unsupported("binding declaration"))?
        }
        ParsedStmt::DeclTail { head, tail } => {
            statement_expression(super::decl_tail::convert(head, tail)?)
        }
        ParsedStmt::DeclCompound { decl, op, source } => statement_expression(
            super::convert::declaration_compound_assign(decl, op, source)?,
        ),
        ParsedStmt::PackageHeader { decl, header, .. } => {
            let statement = super::convert::convert_stmt(decl)?
                .ok_or_else(|| super::convert::unsupported("a declaration with a header"))?;
            let expression = super::convert::expression_of(&statement)
                .ok_or_else(|| super::convert::unsupported("a declaration with a header"))?;
            statement_expression(super::package_header::apply(&expression, header)?)
        }
        ParsedStmt::TempDecl(decl) => {
            statement_expression(super::temporize::convert_declaration(decl)?)
        }
        ParsedStmt::RoutineTraits(_) => {
            return Err(super::convert::unsupported("routine traits outside a body"));
        }
        ParsedStmt::TakeLogicalTail { value, op, right } => {
            statement_expression(super::convert::take_logical_tail_node(value, op, right)?)
        }
        ParsedStmt::InlineProtoUse { proto, use_stmt } => {
            super::use_stmt::convert_inline_proto(proto, use_stmt)?
        }
        // Rakudo keeps `done VALUE` as the one call with its payload.
        ParsedStmt::DoneValue(value) => statement_expression(super::convert::control_call(
            "done",
            std::slice::from_ref(value),
        )?),
        ParsedStmt::CallAssignList { head, items } => {
            statement_expression(super::decl_tail::convert_call_assign_list(head, items)?)
        }
        ParsedStmt::CommaDecls(declarations) => {
            statement_expression(super::convert::comma_declarations_node(declarations)?)
        }
        ParsedStmt::ImmediateCall { decl, args } => {
            statement_expression(super::immediate_call::convert(decl, args)?)
        }
        ParsedStmt::AttributeGroup(attributes) => {
            statement_expression(super::attribute_group::to_signature(attributes)?)
        }
        ParsedStmt::AttributeColonArglist(attributes) => {
            statement_expression(super::attribute_group::convert_colon(attributes)?)
        }
        ParsedStmt::Temporize(t) => statement_expression(super::temporize::convert_temporized(t)?),
        ParsedStmt::SignatureDecl(decl) => {
            statement_expression(super::signature_decl::convert(decl)?)
        }
        ParsedStmt::SignatureBind { signature, rhs } => {
            statement_expression(super::convert::signature_bind_node(signature, rhs)?)
        }
        ParsedStmt::MethodAssignDecl(form) => {
            statement_expression(super::method_assign_decl::convert(form)?)
        }
        ParsedStmt::SigillessDecl(d) => {
            let call_assign = match (&d.form, &d.expr) {
                (
                    crate::ast::parsed::SigillessForm::CallAssign,
                    crate::ast::Expr::MethodCall { name, args, .. },
                ) => Some((name, args.as_slice())),
                _ => None,
            };
            statement_expression(super::convert::term_declaration(
                &crate::ast::sigilless_decl::SigillessDecl {
                    name: &d.name,
                    expr: &d.expr,
                    type_constraint: d.type_constraint.as_deref(),
                    is_state: d.is_state,
                    is_our: d.is_our,
                    assigned: d.form != crate::ast::parsed::SigillessForm::Bind,
                    call_assign,
                },
            )?)
        }
        ParsedStmt::WillDecl { decl, phasers, .. } => super::var_will::convert(decl, phasers)?,
        ParsedStmt::DoesDecl { decl, role, init } => {
            super::convert::does_declaration(decl, role, init.as_ref())?
                .ok_or_else(|| super::convert::unsupported("does declaration"))?
        }
        ParsedStmt::Modified(m) => modified_node(m)?,
        ParsedStmt::For { stmt, block } => {
            let mut node = super::convert::convert_stmt(stmt)?
                .ok_or_else(|| super::convert::unsupported("empty for loop"))?;
            if let Some(field) = node.fields.iter_mut().find(|f| f.name == Some("body"))
                && let RakuAstFieldValue::Node(value) = &field.value
                && let ValueView::RakuAst(body) = value.view()
            {
                let body = with_block_origins(RakuAstNode::clone(body), *block);
                field.value = RakuAstFieldValue::Node(Value::rakuast(Box::new(body)));
            }
            node
        }
    })
}

/// The RakuAST node of an expression the parser built in its RakuAST shape.
/// An assignment is an `ApplyInfix` over `Assignment`; whether it is an item
/// assignment is decided by the parser (`Assign::item`). Rakudo
/// folds an assignment to a positional or a literal hash subscript into the
/// subscript's `assignee` (`@a[0] = 1`, `%h<k> = 1`; measured on 2026.09).
// Cost: O(n), n = size of the expression.
pub(super) fn parsed_expr_node(node: &ParsedExpr) -> Result<RakuAstNode, RuntimeError> {
    match node {
        ParsedExpr::Assign(a) => match &a.target {
            crate::ast::Expr::Index {
                target,
                index,
                is_positional: true,
                ..
            } => super::convert::subscript_node(target, index, true, Some(&a.source), Vec::new()),
            crate::ast::Expr::Index {
                target,
                index,
                is_positional: false,
                spelling: crate::ast::IndexSpelling::Angle,
                ..
            } if super::convert::angle_key_text(index).is_some() => {
                super::convert::angle_subscript_node(target, index, Some(&a.source), Vec::new())
            }
            // `@a[|| @i] = 7`: one (spread) dimension folds the same way.
            crate::ast::Expr::MultiDimIndex {
                target,
                dimensions,
                is_positional,
            } if dimensions.len() == 1 => super::convert::subscript_dims_node(
                target,
                &super::convert::splat_dimensions(dimensions),
                *is_positional,
                Some(&a.source),
                Vec::new(),
            ),
            _ => assignment_node(a),
        },
        ParsedExpr::CompoundAssign(a) => compound_assignment_node(a),
        ParsedExpr::Supply(body) => super::react::convert_supply(body),
        ParsedExpr::StartBody(body) => block_node(body),
        ParsedExpr::HyperAssign(a) => {
            super::convert::hyper_infix(&a.target, "=", &a.value, a.dwim_left, a.dwim_right)
        }
        ParsedExpr::InlineSubset(declaration) => inline_subset_node(declaration),
        ParsedExpr::HyperPrefix(p) => super::hyper_prefix::node(&p.symbol, &p.operand),
        ParsedExpr::AtomicVar(a) => super::atomic_op::var_node(a),
        ParsedExpr::ObjectHash(entries) => super::hash_literal::object_hash_node(entries),
        ParsedExpr::WordList(list) => super::word_list::list_node(list),
        ParsedExpr::WordsAtom(atom) => super::word_list::atom_node(atom),
        ParsedExpr::SubscriptAdverbs(s) => {
            super::subscript_adverb::postcircumfix_node(&s.subscript, &s.pairs)
        }
    }
}

/// An inline `subset` term: the `Type::Subset` expression of the declaration
/// as written.
// Cost: O(n), n = size of the declaration.
fn inline_subset_node(declaration: &Stmt) -> Result<RakuAstNode, RuntimeError> {
    let statement = super::convert::convert_stmt(declaration)?
        .ok_or_else(|| super::convert::unsupported("inline subset declaration"))?;
    statement
        .fields
        .iter()
        .find(|field| field.name == Some("expression"))
        .and_then(|field| match &field.value {
            RakuAstFieldValue::Node(value) => match value.view() {
                ValueView::RakuAst(node) => Some(node.clone()),
                _ => None,
            },
            _ => None,
        })
        .ok_or_else(|| super::convert::unsupported("inline subset declaration"))
}

/// `TARGET = SOURCE` as an `ApplyInfix` over a plain `Assignment`.
// Cost: O(n), n = size of the target and source.
fn assignment_node(a: &crate::ast::parsed::Assign) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyInfix,
        fields: vec![
            node_field(Some("left"), target_node(&a.target)?),
            node_field(
                Some("infix"),
                RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::Assignment,
                    fields: if a.item {
                        vec![RakuAstField {
                            name: None,
                            value: RakuAstFieldValue::Adverb("item"),
                        }]
                    } else {
                        Vec::new()
                    },
                },
            ),
            node_field(Some("right"), convert_expr(&a.source)?),
        ],
    })
}

/// `TARGET OP= SOURCE` as an `ApplyInfix` over `MetaInfix::Assign`, whose
/// infix is wrapped the way it was written: `[+]=` in a `BracketedInfix` per
/// bracket, a scalar `Z+=` in `MetaInfix::Zip` (measured on 2026.09).
// Cost: O(n + d), n = size of the target and source, d = bracket depth.
fn compound_assignment_node(
    a: &crate::ast::parsed::CompoundAssign,
) -> Result<RakuAstNode, RuntimeError> {
    if a.meta == CompoundMeta::Dotty {
        return super::convert::dotty_assignment(&a.target, &a.source);
    }
    if a.meta == CompoundMeta::TopicDotty {
        return super::convert::topic_dotty_assignment(&a.source);
    }
    if let CompoundMeta::BracketedMeta(kind, depth) = a.meta
        && a.op == "="
    {
        return meta_assignment_eq_node(
            kind,
            depth,
            target_node(&a.target)?,
            convert_expr(&a.source)?,
        );
    }
    if let CompoundMeta::BracketedMeta(kind, depth) = a.meta {
        let written = crate::ast::Expr::MetaOp {
            meta: kind.spelling().to_string(),
            op: a.op.clone(),
            left: Box::new(a.target.clone()),
            right: Box::new(a.source.clone()),
        };
        return super::meta_infix::convert_bracketed_assignment(&written, usize::from(depth));
    }
    let node =
        super::convert::compound_assignment_infix(&a.target, &compound_spelling(a), &a.source)?;
    with_compound_meta(node, a.meta)
}

/// `TARGET X= SOURCE`, `TARGET Z= SOURCE`, `TARGET [Z=] SOURCE`: the
/// metaoperator over rakudo's `Assignment` (`:item`), inside `depth`
/// brackets, applied as a list infix (measured on 2026.09).
// Cost: O(d), d = bracket depth.
pub(super) fn meta_assignment_eq_node(
    kind: crate::ast::parsed::BracketedMeta,
    depth: u8,
    target: RakuAstNode,
    source: RakuAstNode,
) -> Result<RakuAstNode, RuntimeError> {
    let class = match kind {
        crate::ast::parsed::BracketedMeta::Cross => RakuAstClass::MetaInfixCross,
        crate::ast::parsed::BracketedMeta::Zip => RakuAstClass::MetaInfixZip,
        _ => return Err(super::convert::unsupported("this metaoperator assignment")),
    };
    let mut infix = RakuAstNode {
        origin: Default::default(),
        class,
        fields: vec![node_field(
            None,
            RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::Assignment,
                fields: vec![RakuAstField {
                    name: None,
                    value: RakuAstFieldValue::Adverb("item"),
                }],
            },
        )],
    };
    for _ in 0..depth {
        infix = RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::BracketedInfix,
            fields: vec![node_field(None, infix)],
        };
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyListInfix,
        fields: vec![
            node_field(Some("infix"), infix),
            RakuAstField {
                name: Some("operands"),
                value: RakuAstFieldValue::List(vec![
                    Value::rakuast(Box::new(target)),
                    Value::rakuast(Box::new(source)),
                ]),
            },
        ],
    })
}

/// The assignment infix a compound marker spells: `+=`, or `R-=` for the
/// reversed infix (`MetaInfix::Reverse` around the assignment).
// Cost: O(k), k = length of the operator.
pub(super) fn compound_spelling(a: &crate::ast::parsed::CompoundAssign) -> String {
    match a.meta {
        CompoundMeta::Reverse => format!("R{}=", a.op),
        CompoundMeta::Dotty | CompoundMeta::TopicDotty => ".=".to_string(),
        _ => format!("{}=", a.op),
    }
}

/// `node`, a compound `ApplyInfix` over `MetaInfix::Assign(Infix(OP))`, with
/// its infix wrapped as `meta` spells it.
// Cost: O(d), d = bracket depth.
pub(super) fn with_compound_meta(
    mut node: RakuAstNode,
    meta: CompoundMeta,
) -> Result<RakuAstNode, RuntimeError> {
    let wrap = |inner: RakuAstNode, class: RakuAstClass| RakuAstNode {
        origin: Default::default(),
        class,
        fields: vec![node_field(None, inner)],
    };
    if matches!(
        meta,
        CompoundMeta::Plain
            | CompoundMeta::Reverse
            | CompoundMeta::Set
            | CompoundMeta::Dotty
            | CompoundMeta::TopicDotty
            | CompoundMeta::BracketedMeta(..)
    ) {
        return Ok(node);
    }
    let infix = |meta: RakuAstNode| -> Option<RakuAstNode> {
        let field = meta.fields.into_iter().next()?;
        let RakuAstFieldValue::Node(value) = field.value else {
            return None;
        };
        let ValueView::RakuAst(inner) = value.view() else {
            return None;
        };
        Some(RakuAstNode::clone(inner))
    };
    for field in node.fields.iter_mut() {
        if field.name != Some("infix") {
            continue;
        }
        let RakuAstFieldValue::Node(value) = &field.value else {
            continue;
        };
        let ValueView::RakuAst(assign) = value.view() else {
            continue;
        };
        let mut inner = infix(RakuAstNode::clone(assign))
            .ok_or_else(|| super::convert::unsupported("compound assignment infix"))?;
        match meta {
            CompoundMeta::Plain
            | CompoundMeta::Reverse
            | CompoundMeta::Set
            | CompoundMeta::Dotty
            | CompoundMeta::TopicDotty
            | CompoundMeta::BracketedMeta(..) => {}
            CompoundMeta::Bracketed(depth) => {
                for _ in 0..depth {
                    inner = wrap(inner, RakuAstClass::BracketedInfix);
                }
            }
            CompoundMeta::Zip => inner = wrap(inner, RakuAstClass::MetaInfixZip),
        }
        *field = node_field(Some("infix"), wrap(inner, RakuAstClass::MetaInfixAssign));
    }
    Ok(node)
}

/// An assignment's target. A variable is the variable as written, including a
/// double-underscore name (a user's `$__c`, or an anonymous `$`'s minted name)
/// that an expression read refuses as an internal marker.
// Cost: O(n), n = size of the target.
fn target_node(target: &crate::ast::Expr) -> Result<RakuAstNode, RuntimeError> {
    let (sigil, name) = match target {
        crate::ast::Expr::Var(name) => ("$", name),
        crate::ast::Expr::ArrayVar(name) => ("@", name),
        crate::ast::Expr::HashVar(name) => ("%", name),
        _ => return convert_expr(target),
    };
    if name.starts_with("__") {
        return Ok(super::convert::var_lexical(sigil, name));
    }
    convert_expr(target)
}

/// The statement as written, with its modifiers as its `condition-modifier`
/// and `loop-modifier`, and the positions the parser recorded for the
/// modified expression and each modifier.
// Cost: O(n), n = size of the statement and its modifiers' operands.
fn modified_node(m: &Modified) -> Result<RakuAstNode, RuntimeError> {
    let mut node = super::convert::convert_stmt(&m.stmt)?
        .ok_or_else(|| super::convert::unsupported("empty modified statement"))?;
    if m.body.is_some()
        && m.body.from < m.body.to
        && let Some(field) = node
            .fields
            .iter_mut()
            .find(|f| f.name == Some("expression"))
        && let RakuAstFieldValue::Node(value) = &field.value
        && let ValueView::RakuAst(expression) = value.view()
    {
        let mut expression = RakuAstNode::clone(expression);
        super::origin::set_span(&mut expression, m.body);
        super::infix_origins::derive(&mut expression);
        field.value = RakuAstFieldValue::Node(Value::rakuast(Box::new(expression)));
    }
    for modifier in m.modifiers() {
        let mut modifier_node = RakuAstNode {
            origin: Default::default(),
            class: modifier_class(modifier.kind),
            fields: vec![node_field(None, convert_expr(&modifier.expr)?)],
        };
        if modifier.span.is_some() {
            super::origin::set_span(&mut modifier_node, modifier.span);
        }
        let name = if modifier.kind.is_loop() {
            "loop-modifier"
        } else {
            "condition-modifier"
        };
        node.fields.push(node_field(Some(name), modifier_node));
    }
    Ok(node)
}

/// The `StatementModifier::*` class of a modifier.
// Cost: O(1).
fn modifier_class(kind: ModifierKind) -> RakuAstClass {
    match kind {
        ModifierKind::If => RakuAstClass::StatementModifierIf,
        ModifierKind::Unless => RakuAstClass::StatementModifierUnless,
        ModifierKind::When => RakuAstClass::StatementModifierWhen,
        ModifierKind::With => RakuAstClass::StatementModifierWith,
        ModifierKind::Without => RakuAstClass::StatementModifierWithout,
        ModifierKind::For => RakuAstClass::StatementModifierFor,
        ModifierKind::While => RakuAstClass::StatementModifierWhile,
        ModifierKind::Until => RakuAstClass::StatementModifierUntil,
        ModifierKind::Given => RakuAstClass::StatementModifierGiven,
    }
}

/// `Statement::Loop` (C-style or bare) or `Statement::Loop::RepeatWhile` /
/// `::RepeatUntil`.
// Cost: O(n), n = size of the loop.
fn loop_node(l: &LoopStmt) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = label_fields(&l.label);
    if l.kind == LoopKind::Loop {
        if let Some(init) = l.init.as_deref() {
            fields.push(node_field(Some("setup"), loop_setup_node(init)?));
        }
        if let Some(cond) = &l.condition {
            fields.push(node_field(Some("condition"), convert_expr(cond)?));
        }
        if let Some(step) = &l.step {
            fields.push(node_field(Some("increment"), convert_expr(step)?));
        }
        fields.push(node_field(
            Some("body"),
            view_block(&l.block, BlockKind::Plain)?,
        ));
        return Ok(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::StatementLoop,
            fields,
        });
    }
    fields.push(node_field(
        Some("body"),
        view_block(&l.block, BlockKind::Signature)?,
    ));
    if let Some(cond) = &l.condition {
        fields.push(node_field(Some("condition"), convert_expr(cond)?));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class: if l.kind == LoopKind::RepeatUntil {
            RakuAstClass::StatementLoopRepeatUntil
        } else {
            RakuAstClass::StatementLoopRepeatWhile
        },
        fields,
    })
}

/// Which `Block` an unparameterized block renders as.
#[derive(Clone, Copy)]
enum BlockKind {
    /// `may-have-signature`: placeholders may give it one.
    Signature,
    /// `implicit-topic`: it topicalizes what its statement tested.
    Topic,
    /// No flags.
    Plain,
}

/// `Statement::If` / `::Unless` / `::With` / `::Without` for `node`.
// Cost: O(n), n = size of the conditional.
pub(super) fn conditional_node(node: &Conditional) -> Result<RakuAstNode, RuntimeError> {
    let topical = matches!(node.kind, ConditionalKind::With | ConditionalKind::Without);
    let (class, block_name) = match node.kind {
        ConditionalKind::If => (RakuAstClass::StatementIf, "then"),
        ConditionalKind::Unless => (RakuAstClass::StatementUnless, "body"),
        ConditionalKind::With => (RakuAstClass::StatementWith, "then"),
        ConditionalKind::Without => (RakuAstClass::StatementWithout, "body"),
    };
    let mut fields = vec![
        node_field(Some("condition"), convert_expr(&node.condition)?),
        node_field(Some(block_name), clause_block(&node.then, topical)?),
    ];
    let mut elsifs = Vec::with_capacity(node.clauses.len());
    // An `else` topicalizes the value the clause before it tested, when that
    // clause topicalizes (`with`, `orwith`).
    let mut else_topical = topical;
    for clause in &node.clauses {
        let orwith = clause.kind == ClauseKind::Orwith;
        let clause_node = RakuAstNode {
            origin: Default::default(),
            class: if orwith {
                RakuAstClass::StatementOrwith
            } else {
                RakuAstClass::StatementElsif
            },
            fields: vec![
                node_field(Some("condition"), convert_expr(&clause.condition)?),
                node_field(Some("then"), clause_block(&clause.then, orwith)?),
            ],
        };
        elsifs.push(Value::rakuast(Box::new(clause_node)));
        else_topical = orwith;
    }
    if !elsifs.is_empty() {
        fields.push(RakuAstField {
            name: Some("elsifs"),
            value: RakuAstFieldValue::List(elsifs),
        });
    }
    if let Some(block) = &node.else_block {
        fields.push(node_field(Some("else"), clause_block(block, else_topical)?));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class,
        fields,
    })
}

/// A clause's `Block` (`implicit-topic` when the clause topicalizes) or
/// `PointyBlock`.
// Cost: O(n), n = size of the block.
fn clause_block(block: &ParsedBlock, topical: bool) -> Result<RakuAstNode, RuntimeError> {
    view_block(
        block,
        if topical {
            BlockKind::Topic
        } else {
            BlockKind::Signature
        },
    )
}

/// `block` as a `PointyBlock` when it has parameters, else as `kind`, with the
/// origins of the block, its `Blockoid` and its `StatementList`.
// Cost: O(n), n = size of the block.
fn view_block(block: &ParsedBlock, kind: BlockKind) -> Result<RakuAstNode, RuntimeError> {
    let node = match &block.params {
        BlockParams::None => match kind {
            BlockKind::Signature => signature_block_node(&block.body)?,
            BlockKind::Topic => topic_block_node(&block.body)?,
            BlockKind::Plain => block_node(&block.body)?,
        },
        BlockParams::List(defs) => pointy_block(defs, &block.body, None)?,
        BlockParams::Topic { def, .. } => pointy_block(
            def.as_deref().map_or(&[], std::slice::from_ref),
            &block.body,
            None,
        )?,
    };
    Ok(with_block_origins(node, block.spans()))
}

/// `node` (a `Block` or `PointyBlock` over a `Blockoid` over a
/// `StatementList`) with the three origins the parser recorded for `block`.
// Cost: O(f), f = fields of the three nodes.
fn with_block_origins(mut node: RakuAstNode, block: BlockSpans) -> RakuAstNode {
    super::origin::set_span(&mut node, block.span);
    // `-> { ... }`: the empty `Signature` sits right after the arrow.
    if node.class == super::RakuAstClass::PointyBlock
        && node.fields.iter().all(|f| f.name != Some("signature"))
        && let Some(arrow) = super::unit_source::with_text(block.span, |text| {
            ["->", "<->"]
                .into_iter()
                .find(|a| text.starts_with(a))
                .map(str::len)
        })
    {
        let at = block.span.from as usize + arrow;
        super::empty_fields::set_span(&mut node, "signature", crate::ast::SrcSpan::new(at, at));
    }
    let Some(body) = node.fields.iter_mut().find(|f| f.name == Some("body")) else {
        return node;
    };
    let RakuAstFieldValue::Node(value) = &body.value else {
        return node;
    };
    let ValueView::RakuAst(blockoid) = value.view() else {
        return node;
    };
    let mut blockoid = RakuAstNode::clone(blockoid);
    super::origin::set_span(&mut blockoid, block.blockoid);
    if let Some(list_field) = blockoid.fields.first_mut()
        && let RakuAstFieldValue::Node(list) = &list_field.value
        && let ValueView::RakuAst(list) = list.view()
    {
        let mut list = RakuAstNode::clone(list);
        super::origin::set_span(&mut list, block.list);
        list_field.value = RakuAstFieldValue::Node(Value::rakuast(Box::new(list)));
    }
    body.value = RakuAstFieldValue::Node(Value::rakuast(Box::new(blockoid)));
    node
}
