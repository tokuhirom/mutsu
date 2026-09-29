//! The container-type half of `eqv` for `Array`s and `Hash`es.
//!
//! `eqv` is type-strict: both operands must have the same `.WHAT`, so a
//! parameterized container never equals a differently parameterized (or an
//! unparameterized) one with the same contents — `Array[Int].new(1) eqv [1]`,
//! `(my Int:D @ = 1) eqv Array[Int].new(1)` and `Hash[Int].new eqv {}` are all
//! `False` in Rakudo. The parameterization lives in the container's embedded
//! metadata (`ArrayData`/`HashData`'s `value_type`/`key_type`/
//! `declared_type`), which is exactly what `.WHAT` renders from.
//!
//! Both `eqv` layers — the interpreter-free recursive [`Value::eqv`] and the
//! VM's lock-step array walk — ask this one function, so they cannot drift.

use super::*;

/// The parameterization triple `(declared, value, key)` with an empty value
/// type read as absent, or `None` for a plain `Array`/`Hash`.
///
/// Some constructors record the parameterized name itself as the declared
/// type (`Array[Int].new` carries `declared_type: "Array[Int]"` next to
/// `value_type: "Int"`) while a typed declaration records only the element
/// type (`my Int @a`); both are the same `.WHAT`, so a declared name that
/// merely restates `<kind>[<value type>]` is dropped. A declared name that says
/// more (`array[int]`, `Map`) is kept.
fn parameterization<'a>(
    kind: &str,
    value_type: &'a Option<String>,
    key_type: &'a Option<String>,
    declared_type: &'a Option<String>,
) -> Option<(Option<&'a str>, Option<&'a str>, Option<&'a str>)> {
    let value_type = value_type.as_deref().filter(|vt| !vt.is_empty());
    let declared = declared_type.as_deref().filter(|declared| {
        let restated = declared
            .strip_prefix(kind)
            .and_then(|rest| rest.strip_prefix('['))
            .and_then(|rest| rest.strip_suffix(']'));
        restated.is_none() || restated != value_type
    });
    let triple = (declared, value_type, key_type.as_deref());
    (triple != (None, None, None)).then_some(triple)
}

/// Whether two containers of the same kind carry the same type
/// parameterization. Operands that are not both `Array`s or both `Hash`es
/// answer `true` (their kinds are compared elsewhere).
// Cost: O(t), t = the length of the type names compared.
pub(crate) fn same_container_parameterization(a: &Value, b: &Value) -> bool {
    match (a.view(), b.view()) {
        (ValueView::Array(x, _), ValueView::Array(y, _)) => {
            parameterization("Array", &x.value_type, &x.key_type, &x.declared_type)
                == parameterization("Array", &y.value_type, &y.key_type, &y.declared_type)
        }
        (ValueView::Hash(x), ValueView::Hash(y)) => {
            parameterization("Hash", &x.value_type, &x.key_type, &x.declared_type)
                == parameterization("Hash", &y.value_type, &y.key_type, &y.declared_type)
        }
        _ => true,
    }
}
