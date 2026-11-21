use super::*;

/// Build a deferred `Expr::Feed` node, resolving the operator's direction so the
/// node always means "`source` flows into `sink`". Deferring the fold (rather than
/// folding into the sink call immediately) lets an assignment/declaration on the
/// textually-left side split it at Sequencer precedence (looser than `=`); see
/// `Expr::Feed` and `feed_leftmost_operand_mut`.
pub(in crate::parser::expr) fn make_feed_node(op: FeedOp, left: Expr, right: Expr) -> Expr {
    let (source, sink, append, left_is_source) = match op {
        FeedOp::ToRight => (left, right, false, true),
        FeedOp::ToLeft => (right, left, false, false),
        FeedOp::AppendRight => (left, right, true, true),
        FeedOp::AppendLeft => (right, left, true, false),
    };
    Expr::Feed {
        source: Box::new(source),
        sink: Box::new(sink),
        append,
        left_is_source,
    }
}

/// Descend a (possibly chained) `Expr::Feed` to the mutable slot holding its
/// textually-leftmost operand — the one adjacent to a preceding `=`. For `==>`
/// chains that is the deepest `source`; for `<==` chains the deepest `sink`.
/// Used to split `my @a = … ==> …` so the declaration wraps that operand.
pub(crate) fn feed_leftmost_operand_mut(feed: &mut Expr) -> &mut Expr {
    match feed {
        Expr::Feed {
            source,
            sink,
            left_is_source,
            ..
        } => {
            if *left_is_source {
                feed_leftmost_operand_mut(source)
            } else {
                feed_leftmost_operand_mut(sink)
            }
        }
        other => other,
    }
}

/// Give a feed operator its real precedence: looser than the comma. The
/// per-element parse leaves `1, 2 ==> f()` as `[1, Feed(2 ==> f())]`; this folds
/// the comma-list neighbours into the feed's source, giving `Feed((1, 2) ==> f())`.
/// For `==>` the elements *before* the feed join its leftmost operand; for `<==`
/// (`f() <== 1, 2`) the elements *after* it join its source. Only a feed whose
/// source is not itself a feed is lifted; every other list is returned untouched.
// Cost: O(n), n = number of list items.
pub(crate) fn lift_feed_in_list(items: Vec<Expr>) -> Vec<Expr> {
    let liftable = |idx: usize, e: &Expr| match e {
        Expr::Feed {
            source,
            left_is_source,
            ..
        } if !matches!(source.as_ref(), Expr::Feed { .. }) => {
            if *left_is_source {
                idx > 0
            } else {
                idx + 1 < items.len()
            }
        }
        _ => false,
    };
    let Some(idx) = items.iter().enumerate().position(|(i, e)| liftable(i, e)) else {
        return items;
    };
    let mut items = items;
    let mut tail = items.split_off(idx + 1);
    let mut feed = items.pop().expect("feed item at idx");
    let mut head = items;
    if let Expr::Feed {
        source,
        left_is_source,
        ..
    } = &mut feed
    {
        let own = std::mem::replace(source.as_mut(), Expr::ArrayLiteral(Vec::new()));
        let parts = if *left_is_source {
            head.drain(..).chain(std::iter::once(own)).collect()
        } else {
            std::iter::once(own).chain(tail.drain(..)).collect()
        };
        **source = Expr::ArrayLiteral(parts);
    }
    head.push(feed);
    head.append(&mut tail);
    head
}

/// The source of a leftward feed (`<==`, `<<==`) whose first item, `first`,
/// was parsed up to `input`: a feed is looser than the comma, so the items
/// after it (`@p <== 3, 4`) are part of the source, an [`Expr::ArrayLiteral`]
/// as a declaration feed's (`my @p <== 3, 4`). A `,,` or a list end stops it.
// Cost: O(n), n = length of the list's text.
pub(in crate::parser::expr) fn leftward_source_list(
    input: &str,
    first: Expr,
) -> PResult<'_, Expr> {
    let mut items = vec![first];
    let mut rest = input;
    loop {
        let (r, _) = ws(rest)?;
        let Some(r) = r.strip_prefix(',').filter(|r| !r.starts_with(',')) else {
            break;
        };
        let (r, _) = ws(r)?;
        if r.is_empty() || r.starts_with([';', '}', ')', ']']) {
            rest = r;
            break;
        }
        let (r, item) = sequence_expr(r)?;
        items.push(item);
        rest = r;
    }
    match items.pop() {
        Some(only) if items.is_empty() => Ok((input, only)),
        Some(last) => {
            items.push(last);
            Ok((rest, Expr::ArrayLiteral(items)))
        }
        None => Ok((input, Expr::ArrayLiteral(items))),
    }
}
