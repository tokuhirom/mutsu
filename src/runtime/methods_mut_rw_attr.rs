use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(crate) fn rw_method_attribute_target(body: &[Stmt]) -> Option<String> {
        let first = body.iter().find(|s| !matches!(s, Stmt::SetLine(_)))?;
        let extract_attr = |expr: &Expr| -> Option<String> {
            match expr {
                Expr::Var(name) if name.starts_with('!') && name.len() > 1 => {
                    Some(name[1..].to_string())
                }
                Expr::Call { name, args } if name == "return-rw" && args.len() == 1 => {
                    if let Expr::Var(attr) = &args[0]
                        && attr.starts_with('!')
                        && attr.len() > 1
                    {
                        return Some(attr[1..].to_string());
                    }
                    None
                }
                _ => None,
            }
        };
        match first {
            Stmt::Expr(expr) | Stmt::Return(expr) => extract_attr(expr),
            _ => None,
        }
    }

    /// Detect an `is rw` method whose body returns an *indexed* attribute
    /// element — `@!attr[$param]` or `%!attr{$param}` — where the index/key is
    /// a single positional parameter. Returns `(attr_name, param_name,
    /// is_positional)`. Such a method exposes the element as a writable lvalue,
    /// so `$obj.at($i) = v` assigns into the attribute container's element.
    /// (The simpler `{ $!attr }` form is handled by
    /// `rw_method_attribute_target`.)
    pub(crate) fn rw_method_indexed_attr_target(body: &[Stmt]) -> Option<(String, String, bool)> {
        let first = body.iter().find(|s| !matches!(s, Stmt::SetLine(_)))?;
        let expr = match first {
            Stmt::Expr(e) | Stmt::Return(e) => e,
            _ => return None,
        };
        // Unwrap `return-rw EXPR`.
        let expr = match expr {
            Expr::Call { name, args } if name == "return-rw" && args.len() == 1 => &args[0],
            other => other,
        };
        if let Expr::Index {
            target,
            index,
            is_positional,
        } = expr
            && let Expr::Var(param) = index.as_ref()
        {
            // `@!attr[...]` parses as `ArrayVar("!attr")`, `%!attr{...}` as
            // `HashVar("!attr")`.
            let attr = match target.as_ref() {
                Expr::ArrayVar(a) | Expr::HashVar(a) if a.starts_with('!') && a.len() > 1 => {
                    &a[1..]
                }
                _ => return None,
            };
            return Some((attr.to_string(), param.clone(), *is_positional));
        }
        None
    }

    /// Assign `value` into element `index_value` of the array/hash attribute
    /// `attr_name` on the instance, then write the updated instance back through
    /// `target_var`. Backs `$obj.rw-method(idx) = value` where the method
    /// returns `@!attr[idx]` / `%!attr{key}`.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn assign_rw_indexed_attr(
        &mut self,
        attributes: &std::sync::Arc<crate::value::InstanceAttrs>,
        class_name: Symbol,
        target_id: u64,
        target_var: Option<&str>,
        attr_name: &str,
        index_value: Value,
        is_positional: bool,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        let mut updated = attributes.to_map();
        let container = updated.get(attr_name).cloned().unwrap_or(Value::Nil);
        let new_container = if is_positional {
            let idx = crate::runtime::utils::to_int(&index_value);
            let (mut data, kind) = match container {
                Value::Array(items, kind) => ((*items).clone(), kind),
                Value::Nil => (
                    crate::value::ArrayData::new(Vec::new()),
                    crate::value::ArrayKind::Array,
                ),
                other => {
                    return Err(RuntimeError::assignment_ro_typename(
                        &crate::value::what_type_name(&other),
                        &value.to_string_value(),
                    ));
                }
            };
            if idx < 0 {
                return Err(RuntimeError::new(format!(
                    "Index {} out of range for rw element assignment",
                    idx
                )));
            }
            let idx = idx as usize;
            if idx >= data.items.len() {
                data.items.resize(idx + 1, Value::Nil);
            }
            data.items[idx] = value.clone();
            Value::Array(std::sync::Arc::new(data), kind)
        } else {
            let key = index_value.to_string_value();
            let mut data = match container {
                Value::Hash(items) => (*items).clone(),
                Value::Nil => crate::value::HashData::new(std::collections::HashMap::new()),
                other => {
                    return Err(RuntimeError::assignment_ro_typename(
                        &crate::value::what_type_name(&other),
                        &value.to_string_value(),
                    ));
                }
            };
            data.map.insert(key, value.clone());
            Value::Hash(std::sync::Arc::new(data))
        };
        updated.insert(attr_name.to_string(), new_container);
        if let Some(var_name) = target_var {
            self.env.insert(
                var_name.to_string(),
                Value::write_back_sharing(attributes, class_name, updated, target_id),
            );
        }
        Ok(value)
    }
}
