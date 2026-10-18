//! `.raku` of a container with a declared element type (ADR-11276 §9.185):
//! `Hash[Int,Str].new(...)`, `Array[Int].new(...)`, `array[int].new(:shape(2,), ...)`.
//! The element and key types live in the interpreter's container registry, so
//! these are the `raku` rows' interpreter answers (`collections::render_names`),
//! reached through the `Array`/`Hash` shapes and, for a shaped or itemized
//! container, through `answer_shapeless`.

use super::*;

impl Interpreter {
    /// The `.raku` text of a typed container, or `None` for a container with
    /// no declared type (the pure rendering answers it).
    // Cost: O(n), n = chars of the rendering, plus a registry lookup.
    pub(crate) fn typed_container_raku(
        &mut self,
        target: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
        // .raku/.perl on constrained Hash
        if matches!(target.view(), ValueView::Hash(_))
            && let Some(info) = self.container_type_metadata(target)
            && let ValueView::Hash(map) = target.view()
        {
            return Some(self.dispatch_constrained_hash_raku(&map, &info, target.hash_is_itemized()));
        }
        // .raku/.perl on native typed shaped array (e.g. array[int])
        if matches!(
                target.view(),
                ValueView::Array(_, crate::value::ArrayKind::Shaped)
            )
            && let Some(info) = self.container_type_metadata(target)
            && info.value_type != "Any"
            && info.value_type != "Mu"
        {
            let type_prefix = if let Some(ref dt) = info.declared_type {
                dt.clone()
            } else if crate::runtime::native_types::is_native_array_element_type(&info.value_type) {
                // Only a native element type is the lowercase `array[int]`;
                // a boxed one is `Array[Int]` (`my Int @a[3]`).
                format!("array[{}]", info.value_type)
            } else {
                format!("Array[{}]", info.value_type)
            };
            // Rakudo renders a shaped array as
            //   array[int].new(:shape(4,), [1, 2, 3, 4])           # 1-D
            //   array[int].new(:shape(2, 2), [1, 2], [3, 4])       # 2-D
            // i.e. `:shape(<dims>)` (a single dim gets a trailing comma, being a
            // 1-element list), then the data: the whole bracket for 1-D, or each
            // first-dimension slice as a separate positional arg for N-D.
            fn render_data(v: &Value) -> String {
                if let ValueView::Array(items, _) = v.view() {
                    let inner = items.iter().map(render_data).collect::<Vec<_>>().join(", ");
                    format!("[{}]", inner)
                } else {
                    crate::builtins::methods_0arg::raku_repr::raku_value(v)
                }
            }
            let shape = Self::infer_array_shape(target).unwrap_or_default();
            let shape_str = if shape.len() == 1 {
                format!("{},", shape[0])
            } else {
                shape
                    .iter()
                    .map(|d| d.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            };
            let ValueView::Array(items, _) = target.view() else {
                unreachable!()
            };
            let data = if shape.len() <= 1 {
                render_data(target)
            } else {
                items.iter().map(render_data).collect::<Vec<_>>().join(", ")
            };
            return Some(Ok(Value::str(format!(
                "{}.new(:shape({}), {})",
                type_prefix, shape_str, data
            ))));
        }
        // .raku/.perl on constrained regular Array (e.g. Array[Int])
        if matches!(target.view(), ValueView::Array(_, kind) if kind != crate::value::ArrayKind::Shaped)
            && let Some(info) = self.container_type_metadata(target)
            && info.value_type != "Any"
            && info.value_type != "Mu"
            && let ValueView::Array(items, _) = target.view()
        {
            // A hole renders as the container's `is default(...)` value, not
            // the element type object the slot holds (`items_with_default`).
            let items = items.items_with_default().into_owned();
            let inner = items
                .iter()
                .map(|item| self.raku_element_repr(item))
                .collect::<Vec<_>>()
                .join(", ");
            let type_name = info
                .declared_type
                .unwrap_or_else(|| format!("Array[{}]", info.value_type));
            return Some(Ok(Value::str(format!("{type_name}.new({inner})"))));
        }
        None
    }
}
