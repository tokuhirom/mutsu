use super::*;

/// Parse a comma expression (may produce a list).
pub(in crate::parser) fn parse_comma_or_expr(input: &str) -> PResult<'_, Expr> {
    let (rest, first) = expression(input)?;
    let (r, _) = ws(rest)?;
    if r.starts_with(',') && !r.starts_with(",,") {
        let (r, _) = parse_char(r, ',')?;
        let (r, _) = ws(r)?;
        if r.starts_with(';') || r.is_empty() || r.starts_with('}') || r.starts_with(')') {
            return Ok((r, Expr::ArrayLiteral(vec![first])));
        }
        let mut items = vec![first];
        let (mut r, second) = expression(r)?;
        items.push(second);
        loop {
            let (r2, _) = ws(r)?;
            if !r2.starts_with(',') {
                let items = merge_sequence_seeds(lift_meta_ops_in_list(items));
                return Ok((r2, finalize_list(items)));
            }
            let (r2, _) = parse_char(r2, ',')?;
            let (r2, _) = ws(r2)?;
            if r2.starts_with(';') || r2.is_empty() || r2.starts_with('}') || r2.starts_with(')') {
                let items = merge_sequence_seeds(lift_meta_ops_in_list(items));
                return Ok((r2, finalize_list(items)));
            }
            let (r2, next) = expression(r2)?;
            items.push(next);
            r = r2;
        }
    }
    Ok((rest, first))
}

/// If the list has exactly one item, return it directly instead of wrapping
/// in an ArrayLiteral.
fn finalize_list(items: Vec<Expr>) -> Expr {
    if items.len() == 1 {
        items.into_iter().next().unwrap()
    } else {
        Expr::ArrayLiteral(items)
    }
}

/// In a comma-separated list, if the last item is a sequence expression
/// (Binary { ..., DotDotDot/DotDotDotCaret, ... }), merge all preceding
/// items into the sequence LHS.
/// E.g. `[0, 1, (*+* ... *)]` → `[ArrayLiteral([0, 1, *+*]) ... *]`
fn merge_sequence_seeds(items: Vec<Expr>) -> Vec<Expr> {
    if items.len() < 2 {
        return items;
    }
    let last = items.last().unwrap();
    if let Expr::Binary { left, op, right } = last
        && matches!(op, TokenKind::DotDotDot | TokenKind::DotDotDotCaret)
    {
        // Merge preceding items + sequence LHS into a new ArrayLiteral
        let mut seeds: Vec<Expr> = items[..items.len() - 1].to_vec();
        seeds.push(*left.clone());
        let merged = Expr::Binary {
            left: Box::new(Expr::ArrayLiteral(seeds)),
            op: op.clone(),
            right: right.clone(),
        };
        vec![merged]
    } else {
        items
    }
}

/// In a comma-separated list, if an item is a MetaOp (X+, Z-, etc.), merge
/// all preceding items into its left operand. This gives meta-ops effective
/// list-infix precedence: `1, 2 X+ 10` → `MetaOp(X, +, [1,2], 10)`.
fn lift_meta_ops_in_list(items: Vec<Expr>) -> Vec<Expr> {
    // Find the first MetaOp in the list
    let meta_idx = items.iter().position(|e| matches!(e, Expr::MetaOp { .. }));
    if let Some(idx) = meta_idx
        && idx > 0
        && let Expr::MetaOp {
            meta,
            op,
            left,
            right,
        } = &items[idx]
    {
        // Merge preceding items + meta's original left into a single list
        let mut seeds: Vec<Expr> = items[..idx].to_vec();
        seeds.push(*left.clone());
        let new_left = Expr::ArrayLiteral(seeds);
        let new_meta = Expr::MetaOp {
            meta: meta.clone(),
            op: op.clone(),
            left: Box::new(new_left),
            right: right.clone(),
        };
        let mut result = vec![new_meta];
        result.extend(items[idx + 1..].to_vec());
        return result;
    }
    items
}
