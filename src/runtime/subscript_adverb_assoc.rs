//! The `:k`/`:v`/`:kv`/`:p` slice adverbs on a user `Associative` object.
//!
//! Such an object (`class C does Associative { method AT-KEY ... }`, or
//! `my %h is Foo`) has no native Hash storage, so the adverbs go through its
//! subscript protocol. The requested keys are asked of `EXISTS-KEY` and read
//! through `AT-KEY`, and the answers are gathered into a plain Hash that the
//! ordinary Hash path then reports from. Only a zen or Whatever slice
//! (`%h{}:v`, `%h{*}:k`) needs the object's own `keys`.

use super::*;

impl Interpreter {
    /// The plain-Hash view of `inst` that a slice adverb with `index` reads.
    /// Also returns the instance's own key order when `index` is a zen or
    /// Whatever slice, which expands to those keys.
    // Cost: O(k), k = keys requested (or all keys, for a zen/Whatever slice);
    // one EXISTS-KEY and one AT-KEY call per key.
    pub(super) fn assoc_instance_adverb_view(
        &mut self,
        inst: &Value,
        index: &Value,
    ) -> Result<(Value, Option<Vec<Value>>), RuntimeError> {
        let whole = matches!(index.view(), ValueView::Whatever)
            || matches!(index.view(), ValueView::Num(f) if f.is_infinite() && f > 0.0);
        let (keys, ordered) = if whole {
            let keys_val = self.try_compiled_method_or_interpret(inst.clone(), "keys", vec![])?;
            let keys = crate::runtime::utils::value_to_list(&keys_val);
            let ordered = keys
                .iter()
                .map(|k| Value::str(k.to_string_value()))
                .collect();
            (keys, Some(ordered))
        } else {
            let keys = match index.view() {
                ValueView::Array(..) | ValueView::Seq(_) | ValueView::Slip(_) => {
                    crate::runtime::utils::value_to_list(index)
                }
                _ => vec![index.clone()],
            };
            (keys, None)
        };
        let has_exists = match inst.view() {
            ValueView::Instance { class_name, .. } => {
                self.has_user_method_including_role(&class_name.resolve(), "EXISTS-KEY")
            }
            ValueView::Mixin(..) => self.mixin_composes_method(inst, "EXISTS-KEY"),
            _ => false,
        };
        let mut map = ValueMap::default();
        for key in keys {
            // Keys `keys` itself listed exist by definition; a requested key
            // is asked of `EXISTS-KEY` when the class has one.
            if !whole && has_exists {
                let exists = self.try_compiled_method_or_interpret(
                    inst.clone(),
                    "EXISTS-KEY",
                    vec![key.clone()],
                )?;
                if !exists.truthy() {
                    continue;
                }
            }
            let value =
                self.try_compiled_method_or_interpret(inst.clone(), "AT-KEY", vec![key.clone()])?;
            // The view is read as a row (`("k", value)`), so a gather the
            // AT-KEY returned runs now, as it would as a list element.
            let value = self.force_finite_lazy_element(&value).unwrap_or(value);
            map.insert(key.to_string_value(), value);
        }
        Ok((Value::hash(map), ordered))
    }
}
