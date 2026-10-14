//! `ACCEPTS` of the quant hashes, `Pair` and `Range` (ADR-11276 §9.105).
//!
//! Each candidate Rakudo declares binds `Mu \topic`, so a user object is an
//! argument like any other, and what the method does with it is user code
//! (its `.Set`, the method a `Pair`'s key names, its `.Stringy`). The rows are
//! [`Handler::Interp`] and [`RowFlags::OBJECT_ARGS`]: the guard step hands
//! them the plain arguments, and `invoke_unadmitted` every other one.

use super::subscript::{accepts_quant, accepts_range};
use super::{Handler, MethodRow, RowFlags};
use crate::builtins::method_table::Named;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

macro_rules! row {
    ($owner:literal, $handler:ident) => {
        MethodRow {
            owner: $owner,
            name: "ACCEPTS",
            arity: 1,
            handler: Handler::Interp($handler),
            flags: RowFlags::ANY_ARGS.or(RowFlags::OBJECT_ARGS),
            named: &[],
        }
    };
}

pub(super) static ROWS: &[MethodRow] = &[
    row!("Set", quant),
    row!("SetHash", quant),
    row!("Bag", quant),
    row!("BagHash", quant),
    row!("Mix", quant),
    row!("MixHash", quant),
    row!("Pair", pair),
    row!("Range", range),
];

/// `Setty.ACCEPTS`, `Baggy.ACCEPTS`, `Mixy.ACCEPTS`: a quant hash of the
/// receiver's kind is compared as it is; any other topic is coerced to that
/// kind first (`set(1, 2).ACCEPTS(1..2)` is `True`, as `(1..2).Set` is).
// Cost: O(e) to compare, e = elements of the receiver, plus the coercion of
// the topic (O(t), t = its elements, or a user `.Set`).
fn quant(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let topic = args[0].descalarize();
    let same_kind = matches!(
        (target.view(), topic.view()),
        (ValueView::Set(..), ValueView::Set(..))
            | (ValueView::Bag(..), ValueView::Bag(..))
            | (ValueView::Mix(..), ValueView::Mix(..))
    );
    if same_kind {
        return accepts_quant(target, std::slice::from_ref(topic));
    }
    let kind = match target.view() {
        ValueView::Set(..) => "Set",
        ValueView::Bag(..) => "Bag",
        ValueView::Mix(..) => "Mix",
        _ => return None,
    };
    Some(
        interp
            .call_method_with_values(topic.clone(), kind, Vec::new())
            .and_then(|coerced| {
                accepts_quant(target, std::slice::from_ref(&coerced)).unwrap_or(Ok(Value::FALSE))
            }),
    )
}

/// `Pair.ACCEPTS`, after `Type/Pair.rakudoc`: an `Associative` topic is
/// looked up by the pair's key and the value smartmatched against the
/// pair's value; any other topic is called the method the key names, and the
/// result's truth compared with the value's (`3 ~~ :is-prime`).
// Cost: O(1) plus the lookup or the method call, plus the smartmatch.
fn pair(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let (key, value) = match target.view() {
        ValueView::Pair(k, v) => (Value::str(k.to_string()), v.clone()),
        ValueView::ValuePair(k, v) => (k.clone(), v.clone()),
        _ => return None,
    };
    let topic = args[0].descalarize();
    let answer = match topic.view() {
        ValueView::Hash(_) | ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..) => interp
            .call_method_with_values(topic.clone(), "AT-KEY", vec![key])
            .map(|found| interp.smart_match(&found, &value)),
        // A positional `Pair` topic (`:42a ~~ :42a`): the keys and the values
        // smartmatch.
        ValueView::ValuePair(topic_key, topic_value) => {
            let (topic_key, topic_value) = (topic_key.clone(), topic_value.clone());
            Ok(interp.smart_match(&topic_key, &key) && interp.smart_match(&topic_value, &value))
        }
        _ => interp
            .call_method_with_values(topic.clone(), &key.to_string_value(), Vec::new())
            .map(|result| result.truthy() == value.truthy()),
    };
    Some(answer.map(Value::truth))
}

/// `Range.ACCEPTS`: a built-in topic is contained (or, for a range, a subset)
/// as `accepts_range` decides; any other topic is compared with the ends by
/// `cmp`, which for a user object reads its `.Stringy` (Rakudo's
/// `(topic cmp $!min) > -(!$!excludes-min)` and the same for `$!max`).
// Cost: O(1) for a built-in topic with numeric ends; otherwise the topic's
// `.Stringy` plus O(n) to compare, n = chars of the strings.
fn range(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let topic = args[0].descalarize();
    if !matches!(
        topic.view(),
        ValueView::Instance { .. } | ValueView::Package(_)
    ) {
        return accepts_range(target, std::slice::from_ref(topic));
    }
    let (min, max, excludes_min, excludes_max) =
        crate::builtins::arith::range::range_bounds(target)?;
    Some(
        interp
            .call_method_with_values(topic.clone(), "Stringy", Vec::new())
            .map(|text| {
                let text = text.to_string_value();
                let above = match text.as_str().cmp(min.to_string_value().as_str()) {
                    std::cmp::Ordering::Greater => true,
                    std::cmp::Ordering::Equal => !excludes_min,
                    std::cmp::Ordering::Less => false,
                };
                let below = match text.as_str().cmp(max.to_string_value().as_str()) {
                    std::cmp::Ordering::Less => true,
                    std::cmp::Ordering::Equal => !excludes_max,
                    std::cmp::Ordering::Greater => false,
                };
                Value::truth(above && below)
            }),
    )
}
