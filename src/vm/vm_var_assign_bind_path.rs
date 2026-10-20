//! Resolve the read side of a chained positional bind before creating slots.

use super::*;

impl Interpreter {
    /// Step one subscript level down by value for read-only probes. Missing
    /// locations stay absent, rather than invoking an accessor or vivifying.
    pub(super) fn subscript_peek_step(
        container: &Value,
        key: &str,
        is_positional: bool,
    ) -> Option<Value> {
        let container = container.deref_container();
        match container.view() {
            ValueView::Array(items, ..) if is_positional => key
                .parse::<usize>()
                .ok()
                .and_then(|i| items.get(i).cloned()),
            ValueView::Seq(items) if is_positional => key
                .parse::<usize>()
                .ok()
                .and_then(|i| items.get(i).cloned()),
            ValueView::Slip(items) if is_positional => key
                .parse::<usize>()
                .ok()
                .and_then(|i| items.get(i).cloned()),
            ValueView::Hash(map) => map.get(key).cloned(),
            _ => None,
        }
    }

    /// A chained bind reads its prefix before invoking the last BIND-POS.
    /// Unlike ASSIGN-POS, Any's BIND-POS cannot vivify an undefined receiver.
    /// Probe native prefixes without changing their size or calling accessors;
    /// object prefixes remain the responsibility of the existing object walk.
    // Cost: O(d), d = subscript depth, excluding a user-defined BIND-POS call.
    pub(super) fn bind_positional_path_receiver(
        &mut self,
        code: &CompiledCode,
        name: &str,
        indices: &[Value],
        positional: &[bool],
        source: &Value,
        source_name: Option<&str>,
    ) -> Option<Result<Value, RuntimeError>> {
        if !positional.last().copied().unwrap_or(false) {
            return None;
        }
        let mut current = self.env().get(name)?.clone();
        for (index, is_positional) in indices.iter().zip(positional).take(indices.len() - 1) {
            let receiver = current.deref_container();
            let receiver = receiver.descalarize();
            let key = index.to_string_value();
            current = match receiver.view() {
                ValueView::Array(items, ..) if *is_positional => {
                    let i = key.parse::<usize>().ok()?;
                    items.get(i).cloned().unwrap_or_else(|| items.hole_value())
                }
                ValueView::Hash(items) => items
                    .get(&key)
                    .cloned()
                    .unwrap_or_else(|| self.typed_container_default(receiver)),
                // Reading through a still-missing intermediate location does
                // not commit its deferred vivification to the owning container.
                ValueView::Package(ty)
                    if !self.has_user_method(
                        ty.as_str(),
                        if *is_positional { "AT-POS" } else { "AT-KEY" },
                    ) =>
                {
                    Value::package(crate::symbol::wk::any())
                }
                _ => return None,
            };
        }
        let receiver = current.deref_container();
        let receiver = receiver.descalarize();
        if !matches!(receiver.view(), ValueView::Package(_)) {
            return None;
        }
        let args = [indices.last()?.clone(), source.clone()];
        if self.has_user_method(&crate::value::types::what_type_name(receiver), "BIND-POS")
            && let Some(source_name) = source_name
        {
            self.set_env_with_main_alias(source_name, source.clone());
            self.update_local_if_exists(code, source_name, source);
        }
        if let Some(result) = self.try_dispatch_compiled_method_direct(receiver, "BIND-POS", &args)
        {
            return Some(result);
        }
        let profile = args
            .iter()
            .map(|arg| {
                let value = arg.deref_container();
                let smiley = if crate::runtime::types::value_is_defined(&value) {
                    ":D"
                } else {
                    ":U"
                };
                format!("{}{smiley}", crate::value::types::what_type_name(&value))
            })
            .collect::<Vec<_>>()
            .join(", ");
        Some(Err(
            crate::runtime::methods_signature_errors::make_multi_no_match_error_detailed(
                "BIND-POS",
                &crate::value::types::what_type_name(receiver),
                false,
                &profile,
                &["(Any:D $:: **@indices is raw, *%_)".to_string()],
            ),
        ))
    }
}
