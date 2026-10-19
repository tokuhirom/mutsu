//! The zen slice (`$x[]`, `$x<>`, `$x{}`): the `DeitemizeZen` opcode's value
//! transform.

use super::*;

impl Interpreter {
    /// Strip one level of itemization: an itemized Array/List, Hash or Slip
    /// loses its item flag, a `Scalar` wrapper unwraps, and every other value
    /// is itself (`(1..3)<>` is the Range, as in Rakudo).
    // Cost: O(1).
    pub(crate) fn deitemize_zen(val: Value) -> Value {
        // A variable's container cell reads as the value it holds.
        let val = if val.is_container_ref() {
            val.deref_container()
        } else {
            val
        };
        match val.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name.as_str() == "IO::Path::Parts" => {
                // TODO: Rakudo's zen slice of a `Parts` is the `Parts`, which
                // iterates its pairs through its own `iterator`; mutsu's `Parts`
                // does not iterate yet, so the slice hands out the pairs instead
                // (#14034).
                let attrs = attributes.as_map();
                // ADR-0021 I2: a data-minted pair defaults positional.
                Value::array(
                    crate::runtime::utils::io_path_parts_keys()
                        .iter()
                        .map(|key| {
                            Value::value_pair(
                                Value::str((*key).to_string()),
                                attrs.get(*key).cloned().unwrap_or(Value::NIL),
                            )
                        })
                        .collect(),
                )
            }
            ValueView::Array(items, kind) if kind.is_itemized() => {
                Value::array_with_kind(items.clone(), kind.decontainerize())
            }
            // A Hash and a Slip carry their itemization as a flag on the
            // value (ADR-0040), so the zen slice clears that flag:
            // `$%h<>.raku` is `{}`, not `${}`.
            ValueView::Hash(_) if val.hash_is_itemized() => val.clone().with_hash_itemized(false),
            ValueView::Slip(_) if val.slip_is_itemized() => val.clone().with_slip_itemized(false),
            // A `$`-held Seq is itemized on its handle (`SeqView::ItemSeq`):
            // the bare handle reads the same reification core.
            ValueView::Seq(body) if body.view() == crate::value::SeqView::ItemSeq => {
                Value::seq_body(body.as_bare_seq_view())
            }
            ValueView::Scalar(inner) if inner.is_container_ref() => inner.deref_container(),
            ValueView::Scalar(inner) => (*inner).clone(),
            _ => val,
        }
    }
}
