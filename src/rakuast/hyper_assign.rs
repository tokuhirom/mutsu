//! The lowering of a hyper assignment (`TARGET »=» VALUE` and the `«=«` / `»=«`
//! / `«=»` dwim spellings): an ordinary hyper op for a plain target, the
//! element-wise destructuring of a literal list of lvalues otherwise. One
//! implementation for the parser and for `rakuast::lower`.

use crate::ast::Expr;
use std::sync::atomic::Ordering;

use super::compound::TMP_INDEX_COUNTER;
use super::lower::{
    bool_field, lower_expr, named_child, named_child_or_positional, positional_leaf, unsupported,
};
use super::view::{leaf_field, node_field, view_expr};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::scope::NameScope;
use crate::value::{RuntimeError, Value, ValueView};

/// If `op` is an assignment meta-operator (`+=`, `~=`, `min=`, ...), return the
/// base operator. Comparison/identity operators that merely *end* in `=` are
/// excluded.
pub(crate) fn assignment_metaop_base(op: &str) -> Option<&str> {
    let base = op.strip_suffix('=')?;
    if base.is_empty() {
        return None;
    }
    // Exclude operators where the trailing `=` is part of the operator itself.
    if matches!(
        op,
        "==" | "!=" | "<=" | ">=" | "===" | "!==" | "=:=" | "!=:=" | "<=>"
    ) {
        return None;
    }
    // A base that itself ends in `=`/`<`/`>`/`!` is a comparison-like operator
    // (`==`, `<=`, `>=`, `!=`), not an assignment meta-op.
    if base.ends_with(['=', '<', '>', '!']) {
        return None;
    }
    Some(base)
}

/// A hyper infix `LEFT >>OP<< RIGHT` -> `ApplyInfix(left, MetaInfix::Hyper(
/// [dwim-left,] infix, [dwim-right]), right)`; raku omits a dwim field whose
/// value is False, and the hyper assignment's infix is the `Assignment` node.
/// A hyper compound assignment (`»~=»`) wraps the hyper operator over the base
/// operator in a `MetaInfix::Assign`, as rakudo does:
/// `MetaInfix::Assign(MetaInfix::Hyper(Infix("~")))`.
// Cost: O(n), n = nodes of the operands.
pub(super) fn hyper_infix(
    names: &NameScope,
    left: &Expr,
    op: &str,
    right: &Expr,
    dwim_left: bool,
    dwim_right: bool,
) -> Result<RakuAstNode, RuntimeError> {
    let compound_base = assignment_metaop_base(op);
    let mut hyper_fields = Vec::with_capacity(3);
    if dwim_left {
        hyper_fields.push(RakuAstField {
            name: Some("dwim-left"),
            value: RakuAstFieldValue::Node(Value::truth(true)),
        });
    }
    hyper_fields.push(node_field(
        Some("infix"),
        if op == "=" {
            RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::Assignment,
                fields: vec![RakuAstField {
                    name: None,
                    value: RakuAstFieldValue::Adverb("item"),
                }],
            }
        } else {
            RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::Infix,
                fields: vec![leaf_field(
                    None,
                    Value::str(compound_base.unwrap_or(op).to_string()),
                )],
            }
        },
    ));
    if dwim_right {
        hyper_fields.push(RakuAstField {
            name: Some("dwim-right"),
            value: RakuAstFieldValue::Node(Value::truth(true)),
        });
    }
    let hyper = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::MetaInfixHyper,
        fields: hyper_fields,
    };
    let infix = if compound_base.is_some() {
        RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::MetaInfixAssign,
            fields: vec![node_field(None, hyper)],
        }
    } else {
        hyper
    };
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ApplyInfix,
        fields: vec![
            node_field(Some("left"), view_expr(names, left)?),
            node_field(Some("infix"), infix),
            node_field(Some("right"), view_expr(names, right)?),
        ],
    })
}

/// `LEFT »OP=» RIGHT` built the way [`hyper_infix`] builds it
/// (`ApplyInfix(left, MetaInfix::Assign(MetaInfix::Hyper(Infix(OP))), right)`),
/// as the hyper operator `»OP=»`; `None` for any other application.
// Cost: O(n), n = nodes of the operands.
pub(super) fn lower_assign_hyper(
    names: &NameScope,
    node: &RakuAstNode,
) -> Option<Result<Expr, RuntimeError>> {
    let assign = named_child(node, "infix")
        .ok()
        .filter(|i| i.class == RakuAstClass::MetaInfixAssign)?;
    let hyper = named_child_or_positional(assign)
        .ok()
        .filter(|h| h.class == RakuAstClass::MetaInfixHyper)?;
    let infix = named_child(hyper, "infix")
        .ok()
        .filter(|i| i.class == RakuAstClass::Infix)?;
    Some((|| {
        let base = positional_leaf(infix)?;
        let ValueView::Str(base) = base.view() else {
            return Err(unsupported(node));
        };
        let base = base.to_string();
        let op = format!("{base}=");
        if assignment_metaop_base(&op) != Some(base.as_str()) {
            return Err(unsupported(node));
        }
        Ok(Expr::HyperOp {
            op,
            left: Box::new(lower_expr(names, named_child(node, "left")?)?),
            right: Box::new(lower_expr(names, named_child(node, "right")?)?),
            dwim_left: bool_field(hyper, "dwim-left")?,
            dwim_right: bool_field(hyper, "dwim-right")?,
        })
    })())
}

/// Build the *shape* operand for a hyper assignment into a literal list of
/// lvalues.
///
/// `»=»`'s leaf op yields its right operand, so the left operand's values never
/// reach the result -- only its shape does, and that shape is what the dwim
/// rules measure. Reading the target back is the shape: a listy leaf carries
/// its own length, which is why raku fills `@a` in `(@a, $x) »=» 5` with one
/// `5` per existing element rather than a single one.
///
/// One leaf cannot be read: a target element that *declares* its variable
/// (`(my $a, my $b) »=» (1, 2)`) has nothing to read yet, and evaluating the
/// declaration here would run it twice. Such a leaf stands in as a scalar
/// placeholder, which is exactly what a freshly declared scalar contributes.
fn hyper_assign_shape(target: &Expr) -> Expr {
    match target {
        Expr::Grouped(inner) => hyper_assign_shape(inner),
        Expr::ArrayLiteral(items) => {
            Expr::ArrayLiteral(items.iter().map(hyper_assign_shape).collect())
        }
        Expr::DoStmt(_) => Expr::Literal(crate::value::Value::NIL),
        other => other.clone(),
    }
}

fn lower_hyper_assign_target(target: Expr, source: Expr) -> Expr {
    match target {
        Expr::Grouped(inner) => lower_hyper_assign_target(*inner, source),
        Expr::ArrayLiteral(items) => Expr::desugar_block(
            items
                .into_iter()
                .enumerate()
                .map(|(index, item)| {
                    crate::ast::Stmt::Expr(lower_hyper_assign_target(
                        item,
                        Expr::Index {
                            target: Box::new(source.clone()),
                            index: Box::new(Expr::Literal(crate::value::Value::int(index as i64))),
                            is_positional: true,
                            spelling: Default::default(),
                        },
                    ))
                })
                .collect(),
        ),
        target => super::assign_target::assign_to_target_expr(target, source),
    }
}

/// `TARGET »=» VALUE` (and the `«=«` / `»=«` / `«=»` dwim spellings), as the
/// compiler runs it. Built here for the parser and for RakuAST lowering.
pub(crate) fn lower_hyper_assignment(
    target: Expr,
    value: Expr,
    dwim_left: bool,
    dwim_right: bool,
) -> Expr {
    // A literal list of lvalues destructures positionally, nested sublists
    // included (`(($a, ($b, $c)), $d) »=« ((4, (5, 6)), 7)`), so it keeps the
    // element-wise lowering below.
    //
    // Every other target is an ordinary hyper op: `=` distributes its RIGHT
    // operand across the LEFT's shape, and the compiler's assignment-hyper-op
    // write-back stores the resulting list — the same route `»+=»` takes.
    // Lowering to a plain `target = value` instead made `@a »=» 7` store a
    // ONE-element array, and left a slice target to be filled by the store
    // path broadcasting its short RHS, which is not the same rule (a plain
    // `@a[0,1,2] = 7` must pad, not broadcast).
    if !matches!(target.peel_parens(), Expr::ArrayLiteral(_)) {
        return Expr::HyperOp {
            op: "=".to_string(),
            left: Box::new(target),
            right: Box::new(value),
            dwim_left,
            dwim_right,
        };
    }
    let index = TMP_INDEX_COUNTER.fetch_add(1, Ordering::Relaxed);
    let shape_name = format!("__mutsu_hyper_shape_{}", index);
    let temp_name = format!("__mutsu_hyper_assign_{}", index);
    // Distribute the RHS across the target's shape BEFORE destructuring it, so
    // the destructuring below is a plain positional walk over an already
    // correctly-sized list. Indexing the raw RHS instead re-implemented the
    // distribution rule with a bare `source[i]`, which made a scalar RHS
    // (`($x, $y) »=» 5`, a broadcast in raku) an out-of-range index, a short
    // list pad with Any instead of cycling, and a non-dwim length mismatch
    // truncate silently instead of raising X::HyperOp::NonDWIM.
    //
    // `»=»`'s leaf yields its right operand, so this hyper op is exactly the
    // distribution and nothing else. Its left operand is the shape temp rather
    // than the target itself because the compiler routes an assignment hyper op
    // over a literal list of lvalues through `__mutsu_assign_callable_lvalue`,
    // which cannot reach nested sublists; the write-back it emits for a plain
    // scalar left just re-stores the shape temp, which is inert.
    //
    // The RHS still evaluates exactly once: it is this hyper op's right
    // operand, and the result is bound to a temp before any target is touched.
    let distributed = Expr::HyperOp {
        op: "=".to_string(),
        left: Box::new(Expr::Var(shape_name.clone())),
        right: Box::new(value),
        dwim_left,
        dwim_right,
    };
    let temp_decl = |name: String, expr: Expr| crate::ast::Stmt::VarDecl {
        marks: Default::default(),
        anonymous: false,
        name,
        expr,
        type_constraint: None,
        is_state: false,
        is_our: false,
        is_dynamic: false,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits: Vec::new(),
        has_initializer: false,
        constant: None,
        bind: None,
        where_constraint: None,
    };
    Expr::desugar_block(vec![
        temp_decl(shape_name, hyper_assign_shape(&target)),
        temp_decl(temp_name.clone(), distributed),
        crate::ast::Stmt::Expr(lower_hyper_assign_target(
            target,
            Expr::Var(temp_name.clone()),
        )),
        // The assignment's own value is the distributed list, not the last
        // element stored: `my $r = (($x, $y) »=» (5, 6))` is `$(5, 6)` in
        // raku. The temp is itemized by its `my $` declaration, which is
        // the itemization raku shows.
        crate::ast::Stmt::Expr(Expr::Var(temp_name)),
    ])
}
