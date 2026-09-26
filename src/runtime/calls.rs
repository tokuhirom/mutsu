use super::*;
use crate::ast::FunctionDef;
use crate::value::ValueMap;

impl Interpreter {
    /// Check if a function has the `is DEPRECATED` trait and record a deprecation event,
    /// using an explicit callsite line if provided.
    pub(crate) fn check_deprecation_for_def_with_line(
        &self,
        def: &FunctionDef,
        callsite_line: Option<i64>,
    ) {
        if let Some(ref msg) = def.deprecated_message {
            let file = self
                .env
                .get("*PROGRAM-NAME")
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            let line = callsite_line.unwrap_or(self.cur_source_line);
            let kind = def.declarator.callable_type().unwrap_or("Sub");
            let pkg = def.package.resolve();
            super::deprecation::record_deprecation(
                kind,
                &def.name.resolve(),
                &pkg,
                msg,
                &file,
                line,
            );
        }
    }

    /// Check deprecation for a method call using name, package, and message.
    pub(crate) fn check_deprecation_for_method(&self, name: &str, package: &str, message: &str) {
        self.check_deprecation_for_method_with_line(name, package, message, None);
    }

    /// Check deprecation for a method call, using an explicit callsite line if provided.
    pub(crate) fn check_deprecation_for_method_with_line(
        &self,
        name: &str,
        package: &str,
        message: &str,
        callsite_line: Option<i64>,
    ) {
        let file = self
            .env
            .get("*PROGRAM-NAME")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let line = callsite_line.unwrap_or(self.cur_source_line);
        super::deprecation::record_deprecation("Method", name, package, message, &file, line);
    }

    pub(crate) fn routine_writeback_excluded_names(
        def: &FunctionDef,
    ) -> std::collections::HashSet<String> {
        let mut names: std::collections::HashSet<String> = def
            .param_defs
            .iter()
            .filter_map(|pd| {
                if pd.name.is_empty() {
                    None
                } else if pd.name.starts_with('@') || pd.name.starts_with('%') {
                    // An `@`/`%` parameter is normally the caller's own container,
                    // so the return merge propagates the callee's mutations back
                    // under that name. A *slurpy* one never is: the binder builds
                    // a fresh Array/Hash out of the leftover arguments
                    // (`bind_function_args_values`), so writing it back only
                    // clobbers a same-named lexical of the caller. That is how
                    // `Test.rakumod`'s `throws-like(..., *%matcher)` came back
                    // from a nested `fails-like(..., *%matcher)` holding the
                    // callee's matcher and then called `.instead` on the wrong
                    // exception (roast S24-testing/fails-like.t).
                    (pd.slurpy || pd.double_slurpy).then(|| pd.name.clone())
                } else if let Some(name) = pd.name.strip_prefix(':') {
                    Some(name.to_string())
                } else {
                    Some(pd.name.clone())
                }
            })
            .collect();
        // Also exclude sub_signature parameter names (array unpacking) so that
        // recursive calls don't write back inner-scope values to the caller.
        for pd in &def.param_defs {
            Self::collect_sub_signature_names(&pd.sub_signature, &mut names);
        }
        // Collect for-loop parameters and *nested* `my` declarations (not just
        // top-level VarDecls) so a callee's body-local binding never leaks into a
        // same-named caller lexical on the return env merge.
        crate::ast::collect_routine_body_local_names(&def.body, &mut names);
        names
    }

    /// Recursively collect variable names from sub_signature parameters.
    pub(crate) fn collect_sub_signature_names(
        sub_sig: &Option<Vec<crate::ast::ParamDef>>,
        names: &mut std::collections::HashSet<String>,
    ) {
        if let Some(params) = sub_sig {
            for sp in params {
                if !sp.name.is_empty() {
                    names.insert(sp.name.clone());
                }
                Self::collect_sub_signature_names(&sp.sub_signature, names);
            }
        }
    }

    /// Build the positional/named argument type-name list used for the
    /// `arguments` attribute of `X::TypeCheck::Argument` (and call profiles).
    /// Skips the internal callsite-line pair and all named (Pair) arguments,
    /// matching Rakudo's `.arguments` which lists only positional argument types.
    pub(crate) fn arg_type_names(args: &[Value]) -> Vec<String> {
        args.iter()
            .filter(|a| {
                !matches!(
                    a.view(),
                    ValueView::Pair(k, _) if k == "__mutsu_test_callsite_line"
                )
            })
            .filter(|a| !matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..)))
            .map(|a| super::value_type_name(a).to_string())
            .collect()
    }

    /// Build the Raku signature string `(Type $name, ...)` from parameter
    /// definitions, used for the `signature` attribute of
    /// `X::TypeCheck::Argument` and in enhanced error messages.
    pub(crate) fn build_signature_string(param_defs: &[crate::ast::ParamDef]) -> String {
        let sig_parts: Vec<String> = param_defs
            .iter()
            .filter(|pd| !pd.traits.iter().any(|t| t == "invocant"))
            .map(|pd| {
                let sigil = if pd.name.starts_with('@')
                    || pd.name.starts_with('%')
                    || pd.name.starts_with('&')
                {
                    ""
                } else if pd.sigilless {
                    "\\"
                } else {
                    "$"
                };
                if pd.name == "__type_only__" {
                    // Type-only param: show just the type constraint
                    return pd.type_constraint.as_deref().unwrap_or("Any").to_string();
                }
                let name_part = if pd.name == "__ANON_STATE__" {
                    "$".to_string()
                } else {
                    format!("{}{}", sigil, pd.name)
                };
                if let Some(tc) = &pd.type_constraint {
                    format!("{} {}", tc, name_part)
                } else {
                    name_part
                }
            })
            .collect();
        format!("({})", sig_parts.join(", "))
    }

    /// Extract the expected type name from a binding type-check error message.
    /// Handles both message shapes mutsu emits:
    ///   "... expected Sub, got Block"
    ///   "... expected Int but got Str"
    fn extract_expected_type(message: &str) -> Option<String> {
        let after = message.rsplit_once("expected ")?.1;
        let end = after
            .find([',', ';'])
            .or_else(|| after.find(" but "))
            .unwrap_or(after.len());
        let ty = after[..end].trim();
        if ty.is_empty() {
            None
        } else {
            Some(ty.to_string())
        }
    }

    /// True for the simple built-in types whose binding failure raku surfaces as
    /// a compile-time X::TypeCheck::Argument (rather than a runtime
    /// X::TypeCheck::Binding::Parameter). Notably excludes `Sub`/`Block` and other
    /// `Callable` refinements, which are only checked at run time.
    fn is_simple_argument_type(ty: &str) -> bool {
        matches!(
            ty,
            "Int"
                | "Str"
                | "Bool"
                | "Num"
                | "Rat"
                | "Complex"
                | "Real"
                | "Numeric"
                | "Cool"
                | "Any"
                | "Mu"
                | "IO"
                | "Regex"
                | "Callable"
                | "Positional"
                | "Associative"
                | "Range"
                | "Match"
                | "Pair"
                | "List"
                | "Array"
                | "Hash"
                | "Set"
                | "Bag"
                | "Mix"
                | "Junction"
                | "Seq"
                | "Supply"
                | "Promise"
                | "Channel"
        )
    }

    /// Enhance a binding error with function name, call profile, and signature info.
    pub(crate) fn enhance_binding_error(
        mut err: RuntimeError,
        func_name: &str,
        param_defs: &[crate::ast::ParamDef],
        args: &[Value],
    ) -> RuntimeError {
        // Don't enhance errors that are already enhanced or are control flow
        if err.is_return() || err.is_last() || err.is_next() || func_name.is_empty() {
            return err;
        }
        // A subset/where constraint failure is a genuine *runtime* check in
        // raku; it surfaces verbatim ("Constraint type check failed in binding
        // to parameter ..."), never as a compile-flavored "will never work".
        if err.message.starts_with("Constraint type check failed") {
            return err;
        }
        // Likewise `is rw`/`is raw` binding a non-writable argument
        // (`X::Parameter::RW`) is a genuine runtime check, not a compile-time
        // shape mismatch -- and unlike the exception-carrying errors handled
        // below, wrapping it in "Calling f(Int) will never work..." would
        // destroy the class the error records, losing it to the generic
        // X::AdHoc fallback (`roast/S06-traits/misc.t`). Both spellings are
        // accepted: the real `X::Parameter::RW` instance the signature binder
        // and the `for`-loop bind site now raise, and the older "X::Type:
        // text" message convention that other sites still use.
        if err.message.starts_with("X::Parameter::RW:")
            || err.exception.as_ref().is_some_and(|ex| {
                matches!(ex.as_ref().view(),
                    ValueView::Instance { class_name, .. }
                        if class_name.resolve() == "X::Parameter::RW")
            })
        {
            return err;
        }
        // Concreteness failures are already complete runtime exceptions. In
        // particular, wrapping them would both change `.Str` and hide the
        // `:U` invocant hint (`multi` versus `.new`). Ordinary sub calls do
        // not have the routine name on the binder's context stack, so repair
        // that one field here while the call site still knows `func_name`.
        let concreteness = err.exception.as_ref().and_then(|ex| {
            let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = ex.as_ref().view()
            else {
                return None;
            };
            if class_name.resolve() != "X::Parameter::InvalidConcreteness" {
                return None;
            }
            let attrs = attributes.as_map();
            Some((
                attrs
                    .get("expected")
                    .map(Value::to_string_value)
                    .unwrap_or_default(),
                attrs
                    .get("got")
                    .map(Value::to_string_value)
                    .unwrap_or_default(),
                attrs
                    .get("routine")
                    .map(Value::to_string_value)
                    .unwrap_or_default(),
                attrs
                    .get("param")
                    .map(Value::to_string_value)
                    .unwrap_or_default(),
                attrs
                    .get("should-be-concrete")
                    .is_some_and(|v| matches!(v.view(), ValueView::Bool(true))),
                attrs
                    .get("param-is-invocant")
                    .is_some_and(|v| matches!(v.view(), ValueView::Bool(true))),
            ))
        });
        if let Some((expected, got, routine, param, should_be_concrete, param_is_invocant)) =
            concreteness
        {
            if !param_is_invocant && routine == "<anon>" {
                let hint = err.take_hint();
                let mut fixed = RuntimeError::parameter_invalid_concreteness(
                    &expected,
                    &got,
                    func_name,
                    &param,
                    should_be_concrete,
                    false,
                );
                fixed.set_hint(hint);
                return fixed;
            }
            return err;
        }
        // A signature with a generic type capture (`sub c(::T $x, T $y, $z)`)
        // cannot be checked at compile time at all -- what `T` means is only
        // known once `$x` binds -- so rakudo reports a plain RUNTIME
        // `X::TypeCheck::Binding::Parameter` for it, never the compile-time
        // "Calling c(Str, Int, Str) will never work with declared signature"
        // shape this wrapper models. (The `has_type_captures` test below
        // already refuses to *reclassify* such an error as the compile-time
        // `X::TypeCheck::Argument`; the message has to stay honest too.)
        //
        // A capture is recorded on the DECLARING parameter's `type_capture`
        // field (`::T $x` is `ParamDef { name: "x", type_capture: Some("T") }`),
        // not as a parameter of its own.
        let signature_has_type_captures = param_defs.iter().any(|pd| {
            pd.name.starts_with("::")
                || pd.name == "__type_capture__"
                || pd.captured_type_name().is_some()
        });
        if signature_has_type_captures {
            return err;
        }
        // Capture the hint before `err.exception` is (possibly) moved out below,
        // so the later `set_hint` does not clash with that partial move.
        let hint = err.take_hint();
        // Build call profile: func_name(Type1, Type2, ...)
        let arg_types: Vec<String> = Self::arg_type_names(args);
        let call_profile = format!("{}({})", func_name, arg_types.join(", "));

        // Build signature string: (Type $name, ...)
        let signature = Self::build_signature_string(param_defs);

        // Enhance the error message, preserving the original for exception type matching
        let enhanced_msg = format!(
            "Calling {} will never work with declared signature {}\n  {}",
            call_profile, signature, err.message
        );
        let mut enhanced = RuntimeError::new(enhanced_msg.clone());
        // For binding type-check errors on regular calls, wrap as X::TypeCheck::Argument
        // Only do this when the error has no existing exception and the message is
        // about a type-only parameter (where the parameter name IS the type constraint),
        // or about arity mismatch.
        let is_arity_error = err.message.contains("Too few positionals passed")
            || err.message.contains("Too many positionals passed");
        // Also detect named parameter type mismatches with simple builtin types
        // (e.g. `sub foo(Int $x) {}; foo("hi")` should be X::TypeCheck::Argument),
        // but NOT when type captures are involved (e.g. `sub foo(::T, T $a, T $b)`)
        // and NOT when the constraint is a user-defined type (subset, class, etc.).
        let has_type_captures = param_defs
            .iter()
            .any(|pd| pd.name.starts_with("::") || pd.name == "__type_capture__");
        let has_subsignature = param_defs
            .iter()
            .any(|pd| pd.sub_signature.is_some() || pd.outer_sub_signature.is_some());
        let is_binding_param_exception = err.exception.as_ref().is_some_and(|ex| {
            if let ValueView::Instance { class_name, .. } = ex.as_ref().view() {
                class_name.resolve() == "X::TypeCheck::Binding::Parameter"
            } else {
                false
            }
        });
        let named_binding_failure = err
            .message
            .split_once("parameter '")
            .and_then(|(_, rest)| rest.split_once('\''))
            .is_some_and(|(param, _)| {
                let key = param.strip_prefix(['$', '@', '%', '&']).unwrap_or(param);
                args.iter().any(|arg| {
                    matches!(
                        arg.view(),
                        ValueView::Pair(pair_key, _) if pair_key == key
                    )
                })
            });
        let is_type_only_mismatch = (is_binding_param_exception
            || (err.exception.is_none()
                && err
                    .message
                    .contains("X::TypeCheck::Binding::Parameter: Type check failed")))
            && !has_type_captures
            // Sigiled parameters such as `@a` and `%h` use container-level
            // constraints (`Positional`/`Associative`), which are still simple
            // enough for a statically visible positional call to receive the
            // compile-time-style wrapper. Runtime where constraints, named
            // colonpairs, and callable-signature checks have already been
            // excluded above or use non-simple expected types.
            // Named colon-pair arguments are bound at run time. Even a simple
            // nominal type such as `Int :$i` must retain its binding exception
            // rather than receive the static-call wrapper.
            && !named_binding_failure
            // Gate on the *failing* parameter's expected type (parsed from the
            // error message), NOT on whether any param happens to be simple. A
            // call like `foo(Sub $c, Str $a); foo(-> {}, "a")` fails on the `Sub`
            // param; the presence of a simple `Str` param must not reclassify it
            // as a compile-time X::TypeCheck::Argument. raku throws a runtime
            // X::TypeCheck::Binding::Parameter when the expected type is one (like
            // Sub/Block) it cannot rule out statically.
            && Self::extract_expected_type(&err.message)
                .as_deref()
                .is_some_and(|expected| {
                    // The expected type must be a simple built-in AND must appear
                    // as a *declared* constraint on some parameter. A type-capture
                    // call (`sub f(::T, T $a); f.assuming(Int)`) reports the
                    // resolved `Int` in its message while the declared constraint
                    // is still `T`, so it must NOT be reclassified to a
                    // compile-time X::TypeCheck::Argument — it stays a runtime
                    // X::TypeCheck::Binding::Parameter.
                    Self::is_simple_argument_type(expected)
                        && (param_defs
                            .iter()
                            .any(|pd| pd.type_constraint.as_deref() == Some(expected))
                            // The binder supplies these implicit constraints
                            // for ordinary `@`/`%` parameters. A positional
                            // call with a scalar therefore gets the same
                            // compile-time-style diagnostic as an explicit
                            // simple type constraint.
                            || (!has_subsignature
                                && expected == "Positional"
                                && err.message.contains("parameter '@"))
                            || (!has_subsignature
                                && expected == "Associative"
                                && err.message.contains("parameter '%")))
                });
        if !((is_arity_error || is_type_only_mismatch)
            && (err.exception.is_none() || is_binding_param_exception))
        {
            err.set_hint(hint);
            return err;
        }
        if (is_arity_error || is_type_only_mismatch)
            && (err.exception.is_none() || is_binding_param_exception)
        {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(enhanced_msg));
            attrs.insert("objname".to_string(), Value::str(func_name.to_string()));
            attrs.insert("signature".to_string(), Value::str(signature));
            let arg_type_values: Vec<Value> =
                arg_types.iter().map(|t| Value::str(t.clone())).collect();
            attrs.insert("arguments".to_string(), Value::array(arg_type_values));
            enhanced.exception = Some(Box::new(Value::make_instance(
                crate::symbol::Symbol::intern("X::TypeCheck::Argument"),
                attrs,
            )));
        } else if let Some(ex) = err.exception {
            // Update the exception object's message attribute so $! shows the enhanced message
            if let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = ex.view()
            {
                let mut new_attrs: ValueMap = attributes
                    .as_map()
                    .iter()
                    .map(|(k, v)| (k.resolve(), v.clone()))
                    .collect();
                new_attrs.insert("message".to_string(), Value::str(enhanced_msg));
                enhanced.exception = Some(Box::new(Value::make_instance(class_name, new_attrs)));
            } else {
                enhanced.exception = Some(ex);
            }
        }
        enhanced.set_hint(hint);
        enhanced
    }
}
