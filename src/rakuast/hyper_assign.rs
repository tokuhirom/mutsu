//! The lowering of a hyper assignment (`TARGET »=» VALUE` and the `«=«` / `»=«`
//! / `«=»` dwim spellings): an ordinary hyper op for a plain target, the
//! element-wise destructuring of a literal list of lvalues otherwise. One
//! implementation for the parser and for `rakuast::lower`.

use crate::ast::Expr;
use std::sync::atomic::Ordering;

use super::compound::TMP_INDEX_COUNTER;

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
