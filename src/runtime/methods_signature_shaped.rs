//! Shaped-array construction/inference and multi-candidate `call_method_all`.
use super::methods_signature_errors::{
    make_method_not_found_error, make_multi_no_match_error, make_private_permission_error,
};
use super::*;

impl Interpreter {
    pub(super) fn shaped_dims_from_new_args(
        &self,
        args: &[Value],
    ) -> Result<Option<Vec<usize>>, RuntimeError> {
        let shape_val = match args.iter().find_map(|arg| match arg {
            Value::Pair(name, value) if name == "shape" => Some(value.as_ref().clone()),
            _ => None,
        }) {
            Some(v) => v,
            None => return Ok(None),
        };
        let dims_vals = if let Some(items) = shape_val.as_list_items() {
            items.to_vec()
        } else {
            match shape_val {
                Value::Int(i) => vec![Value::Int(i)],
                Value::Package(ref name) => {
                    // Enum type as shape: use the number of enum variants
                    if let Some(variants) = self.registry().enum_types.get(&name.resolve()) {
                        vec![Value::Int(variants.len() as i64)]
                    } else if name == "Bool" {
                        // Bool is a builtin enum with 2 values (False, True)
                        vec![Value::Int(2)]
                    } else {
                        return Ok(None);
                    }
                }
                _ => return Ok(None),
            }
        };
        let mut dims = Vec::with_capacity(dims_vals.len());
        for dim in &dims_vals {
            let n = match dim {
                Value::Int(i) if *i > 0 => *i as usize,
                Value::Int(i) => {
                    return Err(RuntimeError::illegal_dimension_in_shape(*i));
                }
                Value::Num(f) if *f > 0.0 => *f as usize,
                Value::Num(f) => {
                    return Err(RuntimeError::illegal_dimension_in_shape(*f as i64));
                }
                Value::Package(name) => {
                    if let Some(variants) = self.registry().enum_types.get(&name.resolve()) {
                        variants.len()
                    } else if name == "Bool" {
                        2
                    } else {
                        return Ok(None);
                    }
                }
                _ => return Ok(None),
            };
            dims.push(n);
        }
        if dims.is_empty() {
            Ok(None)
        } else {
            Ok(Some(dims))
        }
    }

    /// Build a (possibly multidimensional) shaped array of the given dims.
    /// Each dimension allocates `dims[i]` slots via a fallible reservation, so
    /// an absurd declared shape (`my @a[1e15]` = `Array.new(:shape(1e15))`)
    /// yields a catchable `X::` instead of an uncatchable `handle_alloc_error`
    /// abort. (raku aborts with a MoarVM panic on the same input.)
    pub(super) fn make_shaped_array(dims: &[usize]) -> Result<Value, RuntimeError> {
        if dims.is_empty() {
            return Ok(Value::Nil);
        }
        let len = dims[0];
        if dims.len() == 1 {
            let mut items = Vec::new();
            Self::autoviv_resize(&mut items, len, Value::Nil)?;
            let value = Value::shaped_array(items);
            crate::runtime::utils::mark_shaped_array(&value, Some(dims));
            return Ok(value);
        }
        let child = Self::make_shaped_array(&dims[1..])?;
        crate::runtime::utils::mark_shaped_array(&child, Some(&dims[1..]));
        let mut items = Vec::new();
        items.try_reserve(len).map_err(|_| {
            RuntimeError::new(format!(
                "Cannot allocate shaped array of {len} elements: memory allocation failed"
            ))
        })?;
        items.extend((0..len).map(|_| child.clone()));
        let value = Value::shaped_array(items);
        crate::runtime::utils::mark_shaped_array(&value, Some(dims));
        Ok(value)
    }

    pub(super) fn infer_array_shape(value: &Value) -> Option<Vec<usize>> {
        if let Some(shape) = crate::runtime::utils::shaped_array_shape(value) {
            return Some(shape);
        }
        let Value::Array(items, ..) = value else {
            return None;
        };
        let mut shape = vec![items.len()];
        let mut current = items.first().cloned();
        while let Some(Value::Array(inner, ..)) = current {
            shape.push(inner.len());
            current = inner.first().cloned();
        }
        Some(shape)
    }

    pub(super) fn parse_parametric_type_name(name: &str) -> Option<(String, Vec<String>)> {
        let base_end = name.find('[')?;
        if !name.ends_with(']') {
            return None;
        }
        let base = name[..base_end].trim().to_string();
        let inner = &name[base_end + 1..name.len() - 1];
        let args = inner
            .split(',')
            .map(|part| part.trim())
            .filter(|part| !part.is_empty())
            .map(ToOwned::to_owned)
            .collect::<Vec<_>>();
        Some((base, args))
    }

    pub(crate) fn call_method_all_with_values(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Vec<Value>, RuntimeError> {
        if let Value::Instance {
            class_name,
            attributes,
            id: target_id,
        } = &target
        {
            if let Some(private_rest) = method.strip_prefix('!') {
                // Resolve: owner-qualified (!Owner::method) or unqualified (!method)
                let resolved = if let Some((owner_class, pm_name)) = private_rest.split_once("::") {
                    self.resolve_private_method_with_owner(
                        &class_name.resolve(),
                        owner_class,
                        pm_name,
                        &args,
                    )
                    .map(|r| (r, pm_name))
                } else {
                    self.resolve_private_method_any_owner(
                        &class_name.resolve(),
                        private_rest,
                        &args,
                    )
                    .map(|r| (r, private_rest))
                };
                if let Some(((resolved_owner, method_def), pm_name)) = resolved {
                    let caller_class = self
                        .method_class_stack
                        .last()
                        .cloned()
                        .or_else(|| Some(self.current_package().to_string()));
                    let caller_allowed = caller_class.as_deref() == Some(resolved_owner.as_str())
                        || self
                            .registry()
                            .class_trusts
                            .get(&resolved_owner)
                            .is_some_and(|trusted| {
                                caller_class
                                    .as_ref()
                                    .is_some_and(|caller| trusted.contains(caller))
                            });
                    if !caller_allowed {
                        return Err(make_private_permission_error(
                            pm_name,
                            &class_name.resolve(),
                            caller_class.as_deref().unwrap_or("GLOBAL"),
                        ));
                    }
                    let (result, updated) = self.run_resolved_method_compiled_or_treewalk(
                        &class_name.resolve(),
                        &resolved_owner,
                        pm_name,
                        method_def,
                        attributes.to_map(),
                        args,
                        Some(target.clone()),
                    )?;
                    attributes.commit_attrs(updated);
                    return Ok(vec![result]);
                } else {
                    // Private method not found -- return structured X::Method::NotFound
                    return Err(make_method_not_found_error(
                        private_rest,
                        &class_name.resolve(),
                        true,
                    ));
                }
            }

            let candidates =
                self.resolve_methods_per_mro_level(&class_name.resolve(), method, &args);
            if !candidates.is_empty() {
                let mut attrs = attributes.to_map();
                let mut out = Vec::with_capacity(candidates.len());
                for (resolved_owner, method_def) in candidates {
                    // Build an invocant with the latest attributes so that
                    // `$.attr` accessor reads inside the method body see
                    // values updated by preceding calls in the MRO chain.
                    let invocant = Value::write_back_sharing(
                        attributes,
                        *class_name,
                        attrs.clone(),
                        *target_id,
                    );
                    let (result, updated) = self.run_resolved_method_compiled_or_treewalk(
                        &class_name.resolve(),
                        &resolved_owner,
                        method,
                        method_def,
                        attrs,
                        args.clone(),
                        Some(invocant),
                    )?;
                    attrs = updated;
                    out.push(result);
                }
                attributes.commit_attrs(attrs);
                return Ok(out);
            }
            // If the method is defined but no multi candidate matched,
            // produce a dispatch error.
            let own_class = class_name.resolve();
            let method_exists = self.class_mro(&own_class).iter().any(|cn| {
                self.registry()
                    .classes
                    .get(cn.as_str())
                    .and_then(|c| c.methods.get(method))
                    .is_some_and(|ovs| {
                        // A submethod on an ancestor class is not visible to
                        // the receiver, so don't count it as "method exists".
                        let is_ancestor = cn.as_str() != own_class.as_str();
                        ovs.iter()
                            .any(|d| !d.is_private && (!d.is_my || !is_ancestor))
                    })
            });
            if method_exists {
                return Err(make_multi_no_match_error(method));
            }
        }

        Ok(vec![self.call_method_with_values(target, method, args)?])
    }

    pub(crate) fn overwrite_array_items_by_identity_for_vm(
        &mut self,
        needle: &std::sync::Arc<crate::value::ArrayData>,
        updated_items: Vec<Value>,
        kind: ArrayKind,
    ) {
        self.overwrite_array_bindings_by_identity(
            needle,
            Value::Array(
                std::sync::Arc::new(crate::value::ArrayData::new(updated_items)),
                kind,
            ),
        );
    }
}
