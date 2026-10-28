//! `X<k>:delete` / `X[i]:delete` through an object's own `DELETE-KEY` /
//! `DELETE-POS`, for a receiver reached through an expression.
use super::*;
use crate::opcode::SubscriptKind;
use crate::value::ValueView;

impl Interpreter {
    /// `target<k>:delete` / `target[i]:delete` where `target` is an object that
    /// declares `DELETE-KEY` / `DELETE-POS` itself (a class, or a role mixed in)
    /// and was reached through an expression, so no variable names it. The
    /// bracket decides the method when the compiler recorded it, else the
    /// index type does, falling back to whichever the object declares. A
    /// multi-element subscript is a slice: one call per key, answering a list.
    /// `None` leaves every other target to the container paths.
    // Cost: O(k) protocol calls for a k-key slice, O(1) to decline.
    pub(crate) fn delete_through_subscript_protocol(
        &mut self,
        target: &Value,
        idx: &Value,
        kind: SubscriptKind,
    ) -> Result<Option<Value>, RuntimeError> {
        let object = target.deref_container();
        let object = object.descalarize();
        if !matches!(
            object.view(),
            ValueView::Instance { .. } | ValueView::Mixin(..)
        ) {
            return Ok(None);
        }
        let (primary, secondary) = match kind {
            SubscriptKind::Positional => ("DELETE-POS", None),
            SubscriptKind::Associative => ("DELETE-KEY", None),
            SubscriptKind::Unknown if matches!(idx.view(), ValueView::Int(_)) => {
                ("DELETE-POS", Some("DELETE-KEY"))
            }
            SubscriptKind::Unknown => ("DELETE-KEY", Some("DELETE-POS")),
        };
        let declares = |vm: &mut Self, name: &str| match object.view() {
            ValueView::Instance { class_name, .. } => vm.has_user_method(class_name.as_str(), name),
            _ => vm.mixin_role_has_method(object, name),
        };
        let method = if declares(self, primary) {
            primary
        } else if let Some(secondary) = secondary
            && declares(self, secondary)
        {
            secondary
        } else {
            return Ok(None);
        };
        let slice_keys: Option<Vec<Value>> = match idx.view() {
            ValueView::Array(items, _) if items.len() != 1 => Some(items.iter().cloned().collect()),
            ValueView::Seq(items) if items.len() != 1 => Some(items.iter().cloned().collect()),
            ValueView::Slip(items) if items.len() != 1 => Some(items.iter().cloned().collect()),
            _ => None,
        };
        if let Some(keys) = slice_keys {
            let mut results = Vec::with_capacity(keys.len());
            for k in keys {
                results.push(self.call_method_with_values(object.clone(), method, vec![k])?);
            }
            return Ok(Some(Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(results)),
                crate::value::ArrayKind::List,
            )));
        }
        let idx_arg = match idx.view() {
            ValueView::Array(items, _) if items.len() == 1 => items[0].clone(),
            ValueView::Seq(items) if items.len() == 1 => items[0].clone(),
            ValueView::Slip(items) if items.len() == 1 => items[0].clone(),
            _ => idx.clone(),
        };
        self.call_method_with_values(object.clone(), method, vec![idx_arg])
            .map(Some)
    }
}
