use super::*;

/// Parse a comma expression (may produce a list).
pub(in crate::parser) fn parse_comma_or_expr(input: &str) -> PResult<'_, Expr> {
    parse_comma_or_expr_impl(input, false)
}

fn parse_comma_or_expr_impl(input: &str, item_context: bool) -> PResult<'_, Expr> {
    let (rest, first) = expression(input)?;
    let (r, _) = ws(rest)?;
    if r.starts_with(',') && !r.starts_with(",,") {
        let (r, _) = parse_char(r, ',')?;
        let (r, _) = ws(r)?;
        if r.starts_with(';') || r.is_empty() || r.starts_with('}') || r.starts_with(')') {
            // Single element with a trailing comma. In list context this is a
            // 1-element list (`@a = 1..5,` keeps the Range unflattened); in item
            // context it collapses to the element (`return 5,` -> `5`).
            if item_context {
                return Ok((r, first));
            }
            return Ok((r, Expr::ArrayLiteral(vec![first])));
        }
        let mut items = vec![first];
        let (mut r, second) = expression(r)?;
        items.push(second);
        loop {
            let (r2, _) = ws(r)?;
            if !r2.starts_with(',') {
                let items = normalize_comma_list_items(items);
                return Ok((r2, finalize_list(items)));
            }
            let (r2, _) = parse_char(r2, ',')?;
            let (r2, _) = ws(r2)?;
            if r2.starts_with(';') || r2.is_empty() || r2.starts_with('}') || r2.starts_with(')') {
                let items = normalize_comma_list_items(items);
                return Ok((r2, finalize_list(items)));
            }
            let (r2, next) = expression(r2)?;
            items.push(next);
            r = r2;
        }
    }
    Ok((rest, first))
}

/// Like [`parse_comma_or_expr`], but each element is parsed with
/// [`expression_no_word_logical`] so the loose word-logicals bind looser than
/// the comma. Parsing therefore stops at a top-level `... and ...`, leaving it
/// for the statement-level word-logical tail. Used by the assignment /
/// declaration / `return` RHS parsers.
pub(in crate::parser) fn parse_comma_or_expr_no_word_logical(input: &str) -> PResult<'_, Expr> {
    parse_comma_or_expr_no_wl_impl(input, false)
}

/// Item-context variant of [`parse_comma_or_expr_no_word_logical`]: a bare
/// single element with a trailing comma (`return 5,`) yields the scalar element,
/// not a 1-element list. Rakudo distinguishes bare `5,` (scalar) from the
/// parenthesized `(5,)` (a 1-element List). Used by `return` (and `fail`).
pub(in crate::parser) fn parse_comma_or_expr_item_no_word_logical(
    input: &str,
) -> PResult<'_, Expr> {
    parse_comma_or_expr_no_wl_impl(input, true)
}

fn parse_comma_or_expr_no_wl_impl(input: &str, item_context: bool) -> PResult<'_, Expr> {
    let (rest, first) = expression_no_word_logical(input)?;
    let (r, _) = ws(rest)?;
    if r.starts_with(',') && !r.starts_with(",,") {
        let (r, _) = parse_char(r, ',')?;
        let (r, _) = ws(r)?;
        if r.starts_with(';') || r.is_empty() || r.starts_with('}') || r.starts_with(')') {
            if item_context {
                return Ok((r, first));
            }
            return Ok((r, Expr::ArrayLiteral(vec![first])));
        }
        let mut items = vec![first];
        let (mut r, second) = expression_no_word_logical(r)?;
        items.push(second);
        loop {
            let (r2, _) = ws(r)?;
            if !r2.starts_with(',') {
                let items = normalize_comma_list_items(items);
                return Ok((r2, finalize_list(items)));
            }
            let (r2, _) = parse_char(r2, ',')?;
            let (r2, _) = ws(r2)?;
            if r2.starts_with(';') || r2.is_empty() || r2.starts_with('}') || r2.starts_with(')') {
                let items = normalize_comma_list_items(items);
                return Ok((r2, finalize_list(items)));
            }
            let (r2, next) = expression_no_word_logical(r2)?;
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

/// Give the operators that are *looser than the comma* their real precedence.
///
/// Every parser that splits a comma list itself has to run this, or those operators end
/// up binding tighter than the comma: the sequence `...` would take only its adjacent
/// element as a seed (`0, 1, *+* ... *`), and a list infix meta-op only its adjacent
/// element as an operand (`[0, |@p Z+ |@p, 0]`).
pub(in crate::parser) fn normalize_comma_list_items(items: Vec<Expr>) -> Vec<Expr> {
    merge_sequence_seeds(lift_meta_ops_in_list(items))
}

/// In a comma-separated list, if the last item is a sequence expression
/// (Binary { ..., DotDotDot/DotDotDotCaret, ... }), merge all preceding
/// items into the sequence LHS.
/// E.g. `[0, 1, (*+* ... *)]` → `[ArrayLiteral([0, 1, *+*]) ... *]`
fn merge_sequence_seeds(items: Vec<Expr>) -> Vec<Expr> {
    if items.len() < 2 {
        return items;
    }
    let seeds: Vec<Expr> = items[..items.len() - 1].to_vec();
    match merge_seeds_into_sequence(items.last().unwrap(), &seeds) {
        Some(merged) => vec![merged],
        None => items,
    }
}

/// Fold `seeds` into the LHS of the sequence at (or inside) `last`.
///
/// A feed is the loosest operator in Raku, so it sits *outside* the sequence:
/// `$[1], -> @p {...} ... * ==> f()` is `(($[1], -> @p {...}) ... *) ==> f()`. The
/// comma-list splitter sees the whole `-> @p {...} ... * ==> f()` as one element, so
/// the sequence to merge into is one level down, under the feed's source.
fn merge_seeds_into_sequence(last: &Expr, seeds: &[Expr]) -> Option<Expr> {
    match last {
        Expr::Binary { left, op, right }
            if matches!(op, TokenKind::DotDotDot | TokenKind::DotDotDotCaret) =>
        {
            let mut merged_seeds: Vec<Expr> = seeds.to_vec();
            merged_seeds.push(*left.clone());
            Some(Expr::Binary {
                left: Box::new(Expr::ArrayLiteral(merged_seeds)),
                op: op.clone(),
                right: right.clone(),
            })
        }
        Expr::Feed {
            source,
            sink,
            append,
            left_is_source,
        } if *left_is_source => {
            let merged_source = merge_seeds_into_sequence(source, seeds)?;
            Some(Expr::Feed {
                source: Box::new(merged_source),
                sink: sink.clone(),
                append: *append,
                left_is_source: *left_is_source,
            })
        }
        _ => None,
    }
}

/// In a comma-separated list, if an item is a MetaOp (X+, Z-, etc.), merge
/// all preceding items into its left operand. This gives meta-ops effective
/// list-infix precedence: `1, 2 X+ 10` → `MetaOp(X, +, [1,2], 10)`.
///
/// The array composer needs this for `[0, |@p Z+ |@p, 0]`, which is
/// `(0, |@p) Z+ (|@p, 0)` in Raku.
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
