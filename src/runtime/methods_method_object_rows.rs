//! A method object's (`.^lookup`, `.^find_method`, `.^methods`) `wrap`,
//! `unwrap`, `assuming`, `cando` and `set_name`: the method-object bodies of
//! the `Routine`/`Code` rows (ADR-11276 §9.167). The rows of a `&name` handle and
//! a `Sub` are in `methods_code_*rows.rs`.

use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// `method` of the method object `target`, or `None` when the object does
    /// not answer it (a `Regex` object's `cando`, a candidate with no wrap
    /// slot).
    // Cost: O(1) to find the body; `assuming` O(a + c), a = assumed arguments,
    // c = a dispatcher's candidates; `cando` O(c * p); `wrap`/`unwrap` O(w),
    // w = wrappers in the method's chain.
    pub(crate) fn method_object_code_row(
        &mut self,
        target: &Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        let target = target.clone();
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        else {
            return None;
        };
        if !self.is_method_object_class(&class_name.resolve()) {
            return None;
        }
        if method == "assuming"
            && matches!(
                class_name.resolve().as_str(),
                "Method" | "Submethod" | "Regex"
            )
            && let Some(callable) = attributes.as_map().get("__mutsu_method_callable")
            && let Some(result) = self.dispatch_callable_method(callable, method, &args)
        {
            // The primed callable keeps the method's signature (minus what
            // was assumed) for introspection, but runs through this Method
            // object: its `CALL-ME` binds the invocant to the owner's
            // candidate, while binding the callable's anonymous invocant
            // parameter would leave `self` unset.
            return Some(result.map(|primed| match primed.view() {
                ValueView::Sub(data) if !matches!(class_name.as_str(), "Regex") => {
                    let mut carrier = (**data).clone();
                    carrier.body = std::sync::Arc::new(Vec::new());
                    carrier.compiled_code = None;
                    carrier.compiled_routine = None;
                    carrier.env.insert_sym(
                        crate::symbol::wk::assuming_callee_marker(),
                        target.clone(),
                    );
                    Value::sub_value(crate::gc::Gc::new(carrier))
                }
                _ => primed,
            }));
        }
        if method == "cando"
            && args.len() == 1
            && matches!(class_name.resolve().as_str(), "Method" | "Submethod")
        {
            return Some(Ok(self.method_object_cando(&target, &args[0])));
        }
        // A multi Method dispatcher has no single callable payload. Its
        // candidates do, however, so preserve `.assuming`'s bound
        // invocant by reusing the existing captured multi-dispatch call
        // path rather than re-entering named method lookup on every call.
        if method == "assuming"
            && matches!(class_name.resolve().as_str(), "Method" | "Submethod")
            && attributes
                .as_map()
                .get("is_dispatcher")
                .is_some_and(|value| value.truthy())
        {
            let candidates = attributes
                .as_map()
                .get("candidates")
                .cloned()
                .and_then(Value::into_array)
                .map(|(items, _)| {
                    items
                        .iter()
                        .filter_map(|candidate| {
                            let ValueView::Instance { attributes, .. } = candidate.view()
                            else {
                                return None;
                            };
                            attributes.as_map().get("__mutsu_method_callable").cloned()
                        })
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default();
            if candidates.is_empty() {
                return Some(Err(RuntimeError::new(
                    "A multi Method has no callable candidates",
                )));
            }
            let mut env = crate::env::Env::new();
            env.insert(
                "__mutsu_multi_dispatch_candidates".to_string(),
                Value::array(candidates),
            );
            let carrier = Value::make_sub(
                Symbol::intern("Method"),
                Symbol::intern("assuming"),
                Vec::<String>::new(),
                Vec::<crate::ast::ParamDef>::new(),
                Vec::<crate::ast::Stmt>::new(),
                false,
                env,
            );
            let ValueView::Sub(data) = carrier.view() else {
                unreachable!("method assuming carrier must be a Sub");
            };
            let mut data = (**data).clone();
            // Called through the dispatcher object's own `CALL-ME`, which
            // re-dispatches on the (assumed) invocant.
            data.env
                .insert_sym(crate::symbol::wk::assuming_callee_marker(), target.clone());
            for arg in args {
                if let ValueView::Pair(key, value) = arg.view() {
                    data.assumed_named.insert(key.clone(), value.clone());
                } else {
                    data.assumed_positional.push(arg);
                }
            }
            return Some(Ok(Value::sub_value(crate::gc::Gc::new(data))));
        }
                // `Method.set_name`: the rename belongs to the class's method
                // table entry, so it shows on every later `.^find_method` /
                // `.^methods` read, as `RoutineCell::renamed` does for a sub.
                if method == "set_name"
                    && let [new_name] = args.as_slice()
                    && let ValueView::Instance {
                        class_name,
                        attributes,
                        ..
                    } = target.view()
                    && self.is_method_object_class(&class_name.resolve())
                {
                    // The read guard must be gone before `insert` below takes
                    // the write lock.
                    let slot = crate::runtime::code_do_attr::method_instance_slot(&attributes.as_map());
                    let Some((cls, meth, idx)) = slot else {
                        return Some(Err(RuntimeError::new(
                            "Method.set_name: not a method table entry",
                        )));
                    };
                    let new_name = new_name.to_string_value();
                    self.registry_mut()
                        .rename_method_candidate(&cls, &meth, idx, &new_name);
                    attributes.insert("name", Value::str(new_name.clone()));
                    return Some(Ok(Value::str(new_name)));
                }

                // `.wrap` on a Method/Submethod object obtained from
                // `.^methods(:local)` or `.^find_method` (an instance carrying
                // its owning class via the `__mutsu_lookup_*` attributes):
                // register a class-keyed wrap chain, mirroring `.wrap` on a
                // `^lookup`/`^find_method` Sub, so the wrapper takes effect
                // for later dispatch of that method. Submethods use the same
                // method-wrap registry; only their public object type differs.
                if method == "wrap"
                    && let ValueView::Instance {
                        class_name,
                        attributes,
                        ..
                    } = target.view()
                    && self.is_method_object_class(&class_name.resolve())
                    && let Some(wrapper) = args.first().cloned()
                {
                    let am = attributes.as_map();
                    if let (Some(ValueView::Str(cls)), Some(ValueView::Str(meth)), Some(idx)) = (
                        am.get("__mutsu_lookup_class").map(Value::view),
                        am.get("__mutsu_lookup_method").map(Value::view),
                        super::dispatcher_wrap::method_object_wrap_slot(&am),
                    ) {
                        self.dispatch.wrap_handle_counter += 1;
                        let handle_id = self.dispatch.wrap_handle_counter;
                        let (cls, meth) = (cls.to_string(), meth.to_string());
                        self.registry_mut()
                            .push_method_wrap(&cls, &meth, idx, handle_id, wrapper);
                        let wh = super::methods_sub::method_wrap_handle_attrs(
                            &cls, &meth, idx, handle_id, &target,
                        );
                        return Some(Ok(Value::make_instance(
                            Symbol::intern("Routine::WrapHandle"),
                            wh,
                        )));
                    }
                }
                // `.unwrap` on a Method candidate `Instance`, mirroring the
                // `.wrap` block above and `dispatch_sub_method`'s equivalent
                // for the older Sub-shaped candidate (ADR-0019 E10; the same
                // `(class, method, idx)`-keyed `Registry::method_wrap_chains`
                // entry `.wrap` just registered).
                if method == "unwrap"
                    && let ValueView::Instance {
                        class_name,
                        attributes,
                        ..
                    } = target.view()
                    && self.is_method_object_class(&class_name.resolve())
                    && let am = attributes.as_map()
                    && let (Some(ValueView::Str(cls)), Some(ValueView::Str(meth)), Some(idx)) = (
                        am.get("__mutsu_lookup_class").map(Value::view),
                        am.get("__mutsu_lookup_method").map(Value::view),
                        super::dispatcher_wrap::method_object_wrap_slot(&am),
                    )
                {
                    let (cls, meth) = (cls.to_string(), meth.to_string());
                    if args.is_empty() {
                        return Some(match self.registry_mut().pop_method_wrap(&cls, &meth, idx) {
                            Some(_) => Ok(Value::TRUE),
                            None => Err(super::methods_sub::routine_unwrap_error(
                                "Cannot unwrap routine: not wrapped",
                            )),
                        });
                    }
                    let Some(handle_id) = self.extract_wrap_handle_id(&args[0]) else {
                        return Some(Err(super::methods_sub::routine_unwrap_error(
                            "Cannot unwrap routine: invalid wrap handle",
                        )));
                    };
                    return Some(if self
                        .registry_mut()
                        .remove_method_wrap(&cls, &meth, idx, handle_id)
                    {
                        Ok(Value::TRUE)
                    } else {
                        Err(super::methods_sub::routine_unwrap_error(
                            "Cannot unwrap routine: invalid wrap handle",
                        ))
                    });
                }
        None
    }
}
