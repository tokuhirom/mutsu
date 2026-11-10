//! A public `is rw` accessor called with one argument as an assignment
//! (`$o.attr = v` reaches `call_method_mut_with_values` as `attr(v)`).
//!
//! Split out of `methods_mut_dispatch.rs`: only the assignment path
//! (`assign_method_lvalue`) may use it. A plain call `$o.attr(v)` is not an
//! assignment -- rakudo's generated accessor takes no positional -- and
//! must fall through to the ordinary arity error (Template::Nest::XS).

use super::*;
use crate::value::ValueView;

impl Interpreter {
    /// `Some(result)` when `method` is a public attribute accessor of
    /// `target` that handles the one-argument assignment `args`; `None`
    /// when the receiver or the method is not such an accessor.
    // Cost: O(a), a = attributes of the class (accessor lookup), plus the
    // container write.
    pub(crate) fn try_accessor_setter_assign(
        &mut self,
        target_var: &str,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let (class_name, attributes, target_id) = match target.view() {
            ValueView::Instance {
                class_name,
                attributes,
                id,
            } => (class_name, attributes.clone(), id),
            _ => return None,
        };
        let target = target.clone();
        if args.len() == 1 && !self.is_native_method(&class_name.resolve(), method) {
            let class_attrs = self.class_attributes_shared(&class_name.resolve());
            let is_public_rw_accessor = if class_attrs.is_empty() {
                attributes.contains_key(method)
            } else {
                class_attrs.iter().any(|a| {
                    a.is_public && a.name == method && (a.is_rw || matches!(a.sigil, '@' | '%'))
                })
            };
            if is_public_rw_accessor {
                // User-defined rw method takes priority over simple accessor
                let has_rw_method = self
                    .resolve_method(&class_name.resolve(), method, &[], &target)
                    .is_some_and(|m| m.is_rw);
                if !has_rw_method {
                    crate::vm::vm_stats::record_dispatch_entry_outcome(
                        "callmethodmutwithvalues",
                        "accessor",
                    );
                    let sigil = class_attrs
                        .iter()
                        .find(|a| a.is_public && a.name == method)
                        .map(|a| a.sigil)
                        .or_else(|| {
                            attributes.as_map().get(method).and_then(|value| {
                                match value.view() {
                                    ValueView::Array(..) => Some('@'),
                                    ValueView::Hash(_) => Some('%'),
                                    _ => None,
                                }
                            })
                        });
                    // A public rw accessor for an aggregate attribute is
                    // still an assignment to that attribute's container.
                    // Coerce list-shaped RHS values before replacing the
                    // existing contents; otherwise assigning a Seq (for
                    // example a `gather` result) leaves a bare Seq in an
                    // `@` attribute and later mutators cannot operate on
                    // it.
                    let assigned = match sigil {
                        Some(sigil @ ('@' | '%')) => {
                            Self::coerce_attr_value_by_sigil(args[0].clone(), sigil)
                        }
                        _ => args[0].clone(),
                    };
                    // An `@`/`%` attribute IS a container: `$obj.attr = (…)`
                    // assigns into the container the attribute already holds
                    // rather than rebinding the slot to a fresh one. Storing
                    // a new container here severed every other share of the
                    // old one — including the `Array`/`Hash` attribute
                    // containers `Mu.clone` deliberately shares between the
                    // original and the clone (raku: "Hash and Array attribute
                    // modifications in clone appear in original as well").
                    if let Some(existing) = attributes.as_map().get(method)
                        && existing.replace_container_contents(&assigned)
                    {
                        return Some(Ok(existing.clone()));
                    }
                    let mut updated = attributes.to_map();
                    updated.insert(method.to_string(), assigned.clone());
                    self.env.insert(
                        target_var.to_string(),
                        Value::write_back_sharing(&attributes, class_name, updated, target_id),
                    );
                    return Some(Ok(assigned));
                }
                // Signal to assign_method_lvalue to handle via Proxy
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmutwithvalues",
                    "rw-proxy-signal",
                );
                return Some(Err(super::methods_signature_errors::make_multi_no_match_error(
                    method,
                )));
            } else {
                // Check if there's a user-defined method with is_rw
                let has_rw_method = self
                    .resolve_method(&class_name.resolve(), method, &[], &target)
                    .is_some_and(|m| m.is_rw);
                if has_rw_method {
                    // Signal to assign_method_lvalue to handle via Proxy
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmutwithvalues",
                        "rw-proxy-signal",
                    );
                    return Some(Err(super::methods_signature_errors::make_multi_no_match_error(
                        method,
                    )));
                }
                // Public accessor exists but is not rw — reject assignment
                let is_public_accessor = if class_attrs.is_empty() {
                    false
                } else {
                    class_attrs.iter().any(|a| a.is_public && a.name == method)
                };
                if is_public_accessor {
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmutwithvalues",
                        "rw-readonly-reject",
                    );
                    let current = attributes
                        .as_map()
                        .get(method)
                        .cloned()
                        .unwrap_or(Value::NIL);
                    return Some(Err(RuntimeError::assignment_ro_typename(
                        super::utils::value_type_name(&current),
                        &current.to_string_value(),
                    )));
                }
            }
        }
        None
    }
}
