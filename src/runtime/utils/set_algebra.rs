//! The one body of the six binary set operators -- `(|)` `(&)` `(+)` `(.)`
//! `(-)` `(^)` -- shared by every form that exposes them: the `Set*`
//! opcodes and `apply_reduction_op` (`[op]`, `&infix:<op>`) (#9451).
//!
//! Each operator promotes both operands to the higher of their levels (Set <
//! Bag < Mix; `(+)` and `(.)` start at Bag), reads them there with the shared
//! operand coercions of `set_operand.rs`, combines them key by key, and then
//! gives the result the left operand's shape: its mutability, and its role
//! mixin when the result stayed at the left operand's own type.
use super::*;
use crate::value::ValueMap;

/// A binary set operator.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum SetOp {
    /// `(|)` / `∪`: the larger weight of each key.
    Union,
    /// `(&)` / `∩`: the smaller weight of each key both sides hold.
    Intersect,
    /// `(+)` / `⊎`: the sum of the weights.
    Addition,
    /// `(.)` / `⊍`: the product of the weights of each key both sides hold.
    Multiply,
    /// `(-)` / `∖`: the left weight less the right one.
    Diff,
    /// `(^)` / `⊖`: the distance between the weights.
    SymDiff,
}

impl SetOp {
    /// The operator an infix name spells, in either its ASCII or its Unicode
    /// form.
    pub(crate) fn from_op(op: &str) -> Option<Self> {
        Some(match op {
            "(|)" | "∪" => Self::Union,
            "(&)" | "∩" => Self::Intersect,
            "(+)" | "⊎" => Self::Addition,
            "(.)" | "⊍" => Self::Multiply,
            "(-)" | "∖" => Self::Diff,
            "(^)" | "⊖" => Self::SymDiff,
            _ => return None,
        })
    }

    /// The two routine names a user may override the operator under.
    pub(crate) fn routine_names(self) -> [&'static str; 2] {
        match self {
            Self::Union => ["infix:<(|)>", "infix:<∪>"],
            Self::Intersect => ["infix:<(&)>", "infix:<∩>"],
            Self::Addition => ["infix:<(+)>", "infix:<⊎>"],
            Self::Multiply => ["infix:<(.)>", "infix:<⊍>"],
            Self::Diff => ["infix:<(-)>", "infix:<∖>"],
            Self::SymDiff => ["infix:<(^)>", "infix:<⊖>"],
        }
    }

    /// The level the operator promotes to at the least.
    fn floor(self) -> SetLevel {
        match self {
            Self::Addition | Self::Multiply => SetLevel::Bag,
            _ => SetLevel::Set,
        }
    }

    /// The level `val` promotes this operator to. `(+)` alone also promotes
    /// on a QuantHash type object, which it then counts as an element
    /// (`Mix (+) Mix` is `mix(Mix, Mix)`); the other operators take the type
    /// object as a plain element (`(Bag) ∪ (Bag)` is `Set(Bag)`).
    fn operand_level(self, val: &Value) -> SetLevel {
        let level = set_level(val);
        match type_object_set_level(val) {
            Some(named) if self == Self::Addition => level.max(named),
            _ => level,
        }
    }

    /// The error for an operand that would have to reify an infinite list.
    fn lazy_error(self, lazy_right: bool) -> RuntimeError {
        match self {
            Self::Addition | Self::Multiply => {
                RuntimeError::cannot_lazy_with_action("coerce", "Bag")
            }
            Self::Diff if lazy_right => {
                RuntimeError::cannot_lazy_with_action("set difference", "Set")
            }
            _ => RuntimeError::cannot_lazy_with_action("coerce", "Set"),
        }
    }
}

/// Combine two key sets.
fn combine_sets(op: SetOp, a: HashSet<String>, b: HashSet<String>) -> HashSet<String> {
    match op {
        SetOp::Union | SetOp::Addition => {
            let mut a = a;
            a.extend(b);
            a
        }
        SetOp::Intersect | SetOp::Multiply => a.intersection(&b).cloned().collect(),
        SetOp::Diff => a.difference(&b).cloned().collect(),
        SetOp::SymDiff => a.symmetric_difference(&b).cloned().collect(),
    }
}

/// Combine two count maps; only positive counts survive.
fn combine_bags(
    op: SetOp,
    mut a: HashMap<String, BigInt>,
    b: HashMap<String, BigInt>,
) -> HashMap<String, BigInt> {
    match op {
        SetOp::Union => {
            for (k, v) in b {
                let e = a.entry(k).or_default();
                if v > *e {
                    *e = v;
                }
            }
        }
        SetOp::Addition => {
            for (k, v) in b {
                *a.entry(k).or_default() += v;
            }
        }
        SetOp::Intersect => a = keep_common(a, &b, |x, y| x.min(y).clone()),
        SetOp::Multiply => a = keep_common(a, &b, |x, y| x * y),
        SetOp::Diff => {
            for (k, v) in b {
                if let Some(e) = a.get_mut(&k) {
                    *e -= v;
                }
            }
        }
        SetOp::SymDiff => {
            for (k, v) in b {
                let e = a.entry(k).or_default();
                *e = (&*e - v).abs();
            }
        }
    }
    a.retain(|_, v| v.is_positive());
    a
}

/// Combine two weight maps; only non-zero weights survive.
fn combine_mixes(
    op: SetOp,
    mut a: HashMap<String, f64>,
    b: HashMap<String, f64>,
) -> HashMap<String, f64> {
    use crate::builtins::mix_weight;
    match op {
        SetOp::Union => {
            for (k, v) in b {
                a.entry(k).and_modify(|e| *e = e.max(v)).or_insert(v);
            }
        }
        SetOp::Addition => {
            for (k, v) in b {
                let e = a.entry(k).or_insert(0.0);
                *e = mix_weight::add(*e, v);
            }
        }
        SetOp::Intersect => a = keep_common(a, &b, |x, y| x.min(*y)),
        SetOp::Multiply => a = keep_common(a, &b, |x, y| mix_weight::mul(*x, *y)),
        SetOp::Diff => {
            for (k, v) in b {
                let e = a.entry(k).or_insert(0.0);
                *e = mix_weight::sub(*e, v);
            }
        }
        SetOp::SymDiff => {
            for (k, v) in b {
                let e = a.entry(k).or_insert(0.0);
                *e = mix_weight::sub(*e, v);
            }
            for v in a.values_mut() {
                *v = v.abs();
            }
        }
    }
    a.retain(|_, v| *v != 0.0);
    a
}

/// The keys both maps hold, weighed by `f` of the two weights.
fn keep_common<W>(
    a: HashMap<String, W>,
    b: &HashMap<String, W>,
    f: impl Fn(&W, &W) -> W,
) -> HashMap<String, W> {
    a.into_iter()
        .filter_map(|(k, x)| b.get(&k).map(|y| f(&x, y)).map(|w| (k, w)))
        .collect()
}

/// `left OP right` for a binary set operator, in every form.
// Cost: O(l + r), l/r = elements of the operands.
pub(crate) fn set_op_values(op: SetOp, left: &Value, right: &Value) -> Result<Value, RuntimeError> {
    let l = set_operand(left);
    let r = set_operand(right);
    let is_failure = |v: &Value| matches!(v.view(), ValueView::Instance { class_name, .. } if class_name == "Failure");
    if is_failure(&l) || is_failure(&r) {
        return Err(RuntimeError::new("Exception"));
    }
    if is_lazy_set_operand(&l) {
        return Err(op.lazy_error(false));
    }
    if is_lazy_set_operand(&r) {
        return Err(op.lazy_error(true));
    }
    let level = op
        .operand_level(&l)
        .max(op.operand_level(&r))
        .max(op.floor());
    let mut originals = ValueMap::default();
    let result = match level {
        SetLevel::Set => {
            let a = coerce_to_set(&l, &mut originals);
            let b = coerce_to_set(&r, &mut originals);
            Value::set_typed(combine_sets(op, a, b), originals)
        }
        SetLevel::Bag => {
            let a = operand_bag_counts(&l, &mut originals);
            let b = operand_bag_counts(&r, &mut originals);
            Value::bag_typed_big(combine_bags(op, a, b), originals)
        }
        SetLevel::Mix => {
            let a = operand_mix_weights(&l, &mut originals);
            let b = operand_mix_weights(&r, &mut originals);
            Value::mix_with_original_keys(combine_mixes(op, a, b), originals)
        }
    };
    Ok(shape_result(op, left, right, &l, &r, result))
}

/// Give a freshly built result the left operand's shape.
///
/// - **Mutability** follows the left operand (a SetHash on the left makes a
///   SetHash); `(^)` additionally demotes to an immutable Set when the right
///   operand is not a QuantHash (see [`set_sym_diff_mutability`]).
/// - **A role mixin** on the left operand carries over when the result is of
///   the left operand's own type: Rakudo builds that result by cloning the
///   left operand, so `SetHash+{R} ∪ set(3)` is a `SetHash+{R}`, while a
///   promotion (`SetHash+{R} ∪ bag(1)`) builds a fresh `BagHash`. The one
///   exception is `(+)` on a mutable Bag, which Rakudo always builds fresh.
/// - **A Baggy subclass** (`class Foo is Bag`) survives `(+)` of two instances
///   of the same class (roast S02-types/bag.t, rakudo#5190).
fn shape_result(
    op: SetOp,
    left: &Value,
    right: &Value,
    l: &Value,
    r: &Value,
    result: Value,
) -> Value {
    let mutable = if op == SetOp::SymDiff {
        set_sym_diff_mutability(l, r)
    } else {
        set_result_mutability(l)
    };
    let result = with_set_mutability(result, mutable);
    let level = set_level(&result);
    if let ValueView::Mixin(_, mixins) = left.descalarize().view()
        && mixins.keys().any(|k| k.starts_with("__mutsu_role__"))
        && is_quanthash_instance(l)
        && set_level(l) == level
        && set_result_mutability(l) == mutable
        && !(op == SetOp::Addition && mutable && level == SetLevel::Bag)
    {
        return Value::mixin_with_state(result, mixins.as_ref().clone());
    }
    if op == SetOp::Addition
        && level < SetLevel::Mix
        && let (
            ValueView::Instance {
                class_name: lc,
                attributes: la,
                ..
            },
            ValueView::Instance {
                class_name: rc,
                attributes: ra,
                ..
            },
        ) = (left.descalarize().view(), right.descalarize().view())
        && lc == rc
        && la.contains_key("__baggy_data__")
        && ra.contains_key("__baggy_data__")
    {
        let mut attrs = HashMap::new();
        attrs.insert("__baggy_data__".to_string(), result);
        return Value::make_instance(lc, attrs);
    }
    result
}
