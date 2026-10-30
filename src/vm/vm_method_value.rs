//! Bytecode construction of Method values from declaration prototypes.

use super::*;

impl Interpreter {
    // Cost: O(1) expected; prototype metadata is looked up by exact site and owner.
    fn method_value_prototype(
        &self,
        site: Option<Symbol>,
    ) -> Result<Option<std::sync::Arc<crate::value::RoutinePrototype>>, RuntimeError> {
        let Some(site) = site else { return Ok(None) };
        self.registry()
            .method_value_prototypes
            .get(&self.current_package_sym())
            .and_then(|prototypes| prototypes.get(&site))
            .cloned()
            .map(Some)
            .ok_or_else(|| {
                RuntimeError::new("Method initializer has no registered declaration prototype")
            })
    }

    /// `OpCode::MakeAnonSubParams`: a routine literal with a signature
    /// (`sub ($x) {...}`, `anon sub NAME`, a method literal, a WhateverCode),
    /// then its custom `is` traits. See [`Interpreter::build_closure`].
    // Cost: O(f * d + n + l + m + w), f = captures, d = env depth,
    // n = narrow env entries, l = system layers, m = prototype metadata,
    // w = wrappers copied from the declaration.
    pub(super) fn exec_make_anon_sub_params_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
        cc_idx: Option<u32>,
        is_whatever_code: bool,
        prototype: Option<Symbol>,
        declarator: crate::ast::RoutineDeclarator,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::SubDecl {
            name,
            custom_traits,
            param_defs,
            return_type,
            is_rw,
            is_raw,
            ..
        } = stmt
        {
            self.check_param_custom_traits(param_defs)?;
            let signature = code.closure_signature(idx as usize);
            // A `method`/`submethod` literal compiles down this same routine
            // path; its declarator is what makes the closure answer
            // `Method`/`Submethod` rather than the `Sub` every other routine
            // literal is. It wins over `WhateverCode`.
            let callable_type = declarator
                .callable_type()
                .or(is_whatever_code.then_some("WhateverCode"));
            let is_pointy =
                cc_idx.is_some_and(|i| code.closure_compiled_codes[i as usize].is_pointy_block);
            let declared = self.method_value_prototype(prototype)?;
            let spec = crate::vm::vm_closure_build::ClosureSpec {
                routine_cell: declared.as_ref().map(|prototype| prototype.cell.forked()),
                // Anonymous closures pool a SubDecl with an empty name; a
                // named `anon sub NAME` decl carries its name through here.
                name: *name,
                empty_sig: signature.params.is_empty() && signature.param_defs.is_empty(),
                signature,
                is_rw: *is_rw,
                is_raw: *is_raw,
                // A pointy block (`-> $x {...}`) is a `Block`, not a `Sub`.
                // Named anonymous subs (`sub {...}`) stay `Sub`. (`WhateverCode`
                // already overrides via callable_type.)
                is_bare_block: is_pointy,
                return_type: return_type.as_deref(),
                callable_type,
                capture_match_var: false,
                freeze_readonly_captures: true,
            };
            let mut val = self.build_closure(code, idx, cc_idx, spec);
            if let Some(prototype) = declared {
                let wrappers = prototype.wrappers().ok_or_else(|| {
                    RuntimeError::new("Method prototype accessed before registration completed")
                })?;
                if !wrappers.is_empty()
                    && let ValueView::Sub(data) = val.view()
                {
                    std::sync::Arc::make_mut(&mut self.dispatch.wrap_chains)
                        .insert(data.id, wrappers.to_vec());
                }
            }
            // The value of a `multi` term is that candidate (see
            // `MULTI_CANDIDATE_VALUE_MARKER`): mark it the way `.candidates`
            // marks the candidates it hands out.
            if custom_traits
                .iter()
                .any(|(t, _)| t == crate::ast::MULTI_CANDIDATE_VALUE_MARKER)
                && let ValueView::Sub(gc) = val.view()
            {
                // SAFETY: the closure was built just above and is not yet
                // shared; no borrow into its `SubData` is live.
                unsafe {
                    crate::value::gc_contents_mut(&gc)
                        .env
                        .insert("__mutsu_is_multi_candidate".to_string(), Value::TRUE);
                }
            }
            // Anonymous routine literals carry their custom `is` traits in the
            // pooled declaration just like named subs do. Apply them after the
            // closure value exists so traits such as `Sub::Memoized` can wrap
            // this exact value. Parser markers (and built-in declaration-only
            // traits) never reach user `trait_mod:<is>` dispatch.
            let has_trait_mod = self.has_trait_mod_handler("trait_mod:<is>");
            for (trait_name, trait_arg) in custom_traits.iter().filter(|(t, _)| {
                !t.starts_with("__")
                    && t != "default"
                    && !t.starts_with("DEPRECATED")
                    && *t != "hidden-from-USAGE"
                    // `is cached` is a built-in routine trait, handled by the
                    // compiled closure path rather than trait_mod:<is>.
                    && *t != "cached"
                    && !matches!(t.as_str(), "native" | "symbol" | "nativeconv" | "encoded")
            }) {
                if !has_trait_mod {
                    if trait_name == "test-assertion" {
                        continue;
                    }
                    if self
                        .env()
                        .get("__mutsu_in_eval")
                        .is_some_and(|v| v.truthy())
                    {
                        return Err(RuntimeError::new(format!(
                            "Can't use unknown trait 'is' -> '{}' in anonymous sub declaration.",
                            trait_name
                        )));
                    }
                    continue;
                }
                let trait_arg_val = match trait_arg {
                    Some(arg) => Some(self.vm_eval_block_value(&[Stmt::Expr(arg.clone())])?),
                    None => None,
                };
                let type_obj = self.resolve_type_object(trait_name);
                let mut args = vec![val.clone()];
                if let Some(type_val) = type_obj {
                    args.push(type_val);
                    if let Some(arg_val) = trait_arg_val {
                        args.push(arg_val);
                    }
                } else {
                    args.push(Value::pair(
                        trait_name.clone(),
                        trait_arg_val.unwrap_or(Value::TRUE),
                    ));
                }
                let saved_writeback_key = self.trait_mod_writeback_key.take();
                let call_result = match self.try_imported_trait_mod("trait_mod:<is>", &args) {
                    Some(result) => result,
                    None => self.vm_call_function("trait_mod:<is>", args),
                };
                self.trait_mod_writeback_key = saved_writeback_key;
                let mixin_writeback = self.trait_mod_writeback_value.take();
                match call_result {
                    Ok(result) if matches!(result.view(), ValueView::Mixin(..)) => val = result,
                    Ok(_) => {
                        if let Some(mixin_val) = mixin_writeback {
                            val = mixin_val;
                        }
                    }
                    Err(e) if Self::is_trait_mod_no_candidate(&e) => {
                        if trait_name != "test-assertion"
                            && self
                                .env()
                                .get("__mutsu_in_eval")
                                .is_some_and(|v| v.truthy())
                        {
                            return Err(RuntimeError::new(format!(
                                "Can't use unknown trait 'is' -> '{}' in anonymous sub declaration.",
                                trait_name
                            )));
                        }
                    }
                    Err(e) => return Err(e),
                }
            }
            self.stack.push(val);
            Ok(())
        } else {
            Err(RuntimeError::new("MakeAnonSubParams expects SubDecl"))
        }
    }
}
