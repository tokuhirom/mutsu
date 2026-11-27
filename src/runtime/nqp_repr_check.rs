//! The REPR capability a positional `nqp::` op asks of its operand
//! (ADR-12666 §3.9, #12671).
//!
//! In Rakudo the positional ops (`atpos`, `bindpos`, `push`, `pop`, `shift`,
//! `unshift`, `splice`, `setelems`, `existspos`, `elems`, `iterator`) are REPR
//! operations. A `VMArray` has them: a raw `nqp::list`, a `$!reified` view, a
//! native `array[T]`, a `Buf`/`Blob`, a `Uni`, a `CArray`, an
//! `IterationBuffer`. A high-level `List`/`Array`/`Hash` is `P6opaque`, and so
//! are `Str`, `Seq`, `Range` and `Capture`: the op dies, and nqp code is
//! expected to reach the storage through `$!reified` / `$!storage`. A
//! `Hash`/`Map` delegates the associative ops (and `elems`) to its
//! `$!storage`, so those keep working on it.
//!
//! This is the one check every positional op shares: the dispatch entry
//! points ask [`check_operand`] before the op's own arm runs.

use crate::runtime::RuntimeError;
use crate::value::{Value, ValueView};
use std::sync::OnceLock;

/// What a positional op needs of its first operand.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Need {
    /// Not a positional op: nothing to check.
    None,
    /// `atpos`, `bindpos`, `push`, ...: the positional REPR ops.
    Positional,
    /// `existspos`: positional, but MoarVM reports it as `elems`.
    ExistsPos,
    /// `elems`: positional or associative.
    Elems,
    /// `iterator`: anything but a high-level `List`/`Array`/`Hash`.
    Iterate,
}

/// The need of op `name` (without the `nqp::` prefix).
// Cost: O(1).
fn need_of(name: &str) -> Need {
    match name {
        "elems" => Need::Elems,
        "existspos" => Need::ExistsPos,
        "iterator" => Need::Iterate,
        "splice" | "setelems" => Need::Positional,
        _ => {
            let stem = name
                .strip_suffix("_i")
                .or_else(|| name.strip_suffix("_n"))
                .or_else(|| name.strip_suffix("_s"))
                .or_else(|| name.strip_suffix("_u"))
                .unwrap_or(name);
            match stem {
                "atpos" | "bindpos" | "push" | "pop" | "shift" | "unshift" => Need::Positional,
                _ => Need::None,
            }
        }
    }
}

/// [`need_of`] for every registered op, indexed by op id.
// Cost: O(n) once, n = registered ops; O(1) per lookup after.
fn need_of_id(id: u16) -> Need {
    static TABLE: OnceLock<Box<[Need]>> = OnceLock::new();
    let table = TABLE.get_or_init(|| {
        (0..crate::nqp_op_ids::nqp_op_count())
            .map(|i| need_of(crate::nqp_op_ids::nqp_op_name(i as u16)))
            .collect()
    });
    table.get(id as usize).copied().unwrap_or(Need::None)
}

/// Whether `v` is one of the `P6opaque` shapes the positional ops refuse,
/// and its type name when it is. A high-level hash is refused only by
/// `positional`.
// Cost: O(1).
fn refused_type(v: &Value, positional: bool) -> Option<&'static str> {
    // The name is only needed for a refusal; computing it up front cost every
    // accepted operand a type-name lookup.
    let name = || crate::value::type_name::value_type_name(v);
    match v.view() {
        ValueView::Array(..) => {
            (!crate::runtime::boot_collection::repr_is_vm_array(v)).then(name)
        }
        // A lexical pad (`BOOTContext`) is iterable and keyed by name.
        ValueView::Hash(h) if h.declared_type.as_deref() == Some("BOOTContext") => None,
        ValueView::Hash(_) => (positional && !v.is_vm_hash()).then(name),
        ValueView::Str(_)
        | ValueView::Int(_)
        | ValueView::BigInt(_)
        | ValueView::Num(_)
        | ValueView::Rat(..)
        | ValueView::Seq(_)
        | ValueView::Slip(_)
        | ValueView::LazyList(_)
        | ValueView::Range(..)
        | ValueView::RangeExcl(..)
        | ValueView::RangeExclStart(..)
        | ValueView::RangeExclBoth(..)
        | ValueView::GenericRange { .. }
        | ValueView::Capture { .. } => Some(name()),
        _ => None,
    }
}

/// Die as Rakudo does when op `id`'s first operand lacks the REPR the op
/// needs. Every other operand, and every non-positional op, passes.
// Cost: O(1).
pub(crate) fn check_operand(
    interp: &crate::runtime::Interpreter,
    id: u16,
    args: &[Value],
) -> Result<(), RuntimeError> {
    let need = need_of_id(id);
    check(need, args)?;
    check_opaque_iteration_buffer(interp, need, args)
}

/// [`check_operand`] for a call that only has the op's name.
// Cost: O(m), m = chars of the name.
pub(crate) fn check_operand_named(
    interp: &crate::runtime::Interpreter,
    op: &str,
    args: &[Value],
) -> Result<(), RuntimeError> {
    let need = need_of(op);
    check(need, args)?;
    check_opaque_iteration_buffer(interp, need, args)
}

/// A class declared `is IterationBuffer` without `is repr('VMArray')` is
/// `P6opaque` in Rakudo: it inherits the methods, not the representation, so
/// the positional ops refuse its instances (#17627). Such an instance has no
/// element storage (`create_instance` seeds it only for a `VMArray` class),
/// which keeps the check off the hot path: an `IterationBuffer` or a
/// `VMArray` subclass answers on the first test.
// Cost: O(1) for an operand with storage; O(d) otherwise, d = MRO depth.
fn check_opaque_iteration_buffer(
    interp: &crate::runtime::Interpreter,
    need: Need,
    args: &[Value],
) -> Result<(), RuntimeError> {
    if matches!(need, Need::None | Need::Iterate) {
        return Ok(());
    }
    let Some(ValueView::Instance {
        class_name,
        attributes,
        ..
    }) = args.first().map(Value::view)
    else {
        return Ok(());
    };
    if class_name == crate::sym!("IterationBuffer")
        || attributes
            .as_map()
            .contains_key(crate::runtime::nqp_ops_list::iteration_buffer_items_key())
        || !interp.is_opaque_iteration_buffer_class(class_name.as_str())
    {
        return Ok(());
    }
    let what = if need == Need::Positional {
        "positional operations"
    } else {
        "elems"
    };
    Err(RuntimeError::new(format!(
        "This type ({}) does not support {what}",
        crate::value::user_facing_type_name(&class_name.resolve())
    )))
}

// Cost: O(1).
fn check(need: Need, args: &[Value]) -> Result<(), RuntimeError> {
    let Some(v) = args.first() else {
        return Ok(());
    };
    match need {
        Need::None => Ok(()),
        Need::Positional => match refused_type(v, true) {
            Some(name) => Err(RuntimeError::new(format!(
                "This type ({name}) does not support positional operations"
            ))),
            None => Ok(()),
        },
        Need::ExistsPos | Need::Elems => {
            if let ValueView::Package(name) = v.view()
                && need == Need::Elems
                && crate::runtime::boot_collection::boot_type_of(v).is_none()
            {
                return Err(RuntimeError::new(format!(
                    "elems requires a concrete object (got a {} type object instead)",
                    name.as_str()
                )));
            }
            match refused_type(v, need == Need::ExistsPos) {
                Some(name) => Err(RuntimeError::new(format!(
                    "This type ({name}) does not support elems"
                ))),
                None => Ok(()),
            }
        }
        Need::Iterate => match v.view() {
            ValueView::Array(..) | ValueView::Hash(_)
                if refused_type(v, true).is_some() =>
            {
                Err(RuntimeError::new(format!(
                    "Cannot iterate object with P6opaque representation ({})",
                    crate::value::type_name::value_type_name(v)
                )))
            }
            _ => Ok(()),
        },
    }
}
