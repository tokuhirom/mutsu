//! Lambda/block-closure creation and sub/proto/token registration ops.
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// A parameter may carry a trait the signature machinery does not know
    /// (`sub f($x is nonesuch)`). That is legal only when some user
    /// `trait_mod:<is>` accepts a `Parameter` — Cro::HTTP::Router declares
    /// `multi trait_mod:<is>(Parameter:D $param, :$query!)` and friends. The
    /// parser therefore records the name instead of rejecting it, and the
    /// declaration site checks it here, the same way a *sub*-level custom trait
    /// is checked in `exec_register_proto_sub_op`.
    pub(crate) fn check_param_custom_traits(
        &mut self,
        params: &[crate::ast::ParamDef],
    ) -> Result<(), RuntimeError> {
        for p in params.iter() {
            for trait_name in &p.traits {
                if crate::parser::is_builtin_param_trait(trait_name) {
                    continue;
                }
                let unknown = || {
                    RuntimeError::new(format!(
                        "Can't use unknown trait 'is' -> '{trait_name}' in a parameter declaration."
                    ))
                };
                if !self.has_proto("trait_mod:<is>") && !self.has_multi_candidates("trait_mod:<is>")
                {
                    return Err(unknown());
                }
                // Hand the candidate a real Parameter, the way raku does. A
                // dispatch failure means no candidate accepts this trait name,
                // which is raku's compile-time "unknown trait" error.
                let param_val = crate::value::signature::make_parameter_value_from_param_def(p);
                let named_arg = Value::pair(trait_name.clone(), Value::TRUE);
                loan_env!(
                    self,
                    call_function("trait_mod:<is>", vec![param_val, named_arg])
                )
                .map_err(|_| unknown())?;
            }
            if let Some(subs) = &p.sub_signature {
                self.check_param_custom_traits(subs)?;
            }
        }
        Ok(())
    }

    pub(super) fn exec_make_lambda_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
        cc_idx: Option<u32>,
        is_whatever_code: bool,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::SubDecl {
            params,
            param_defs,
            return_type,
            body,
            is_rw,
            is_raw,
            ..
        } = stmt
        {
            self.check_param_custom_traits(param_defs)?;
            let compiled_code = Self::resolve_closure_code(code, cc_idx);
            self.box_captured_lexicals(code, &compiled_code);
            let owned_captures = self.compute_owned_captures(&compiled_code);
            let authoritative_captures = self.compute_authoritative_captures(&compiled_code);
            let upvalues = self.capture_upvalues(code, &compiled_code);
            // Upvalue snapshot (single-store Slice E); see `capture_closure_env`.
            let mut env = self.capture_closure_env(code, &compiled_code);
            if let Some(rt) = return_type {
                env.insert("__mutsu_return_type".to_string(), Value::str(rt.clone()));
            }
            if is_whatever_code {
                env.insert(
                    "__mutsu_callable_type".to_string(),
                    Value::str_from("WhateverCode"),
                );
            }
            let cc_source_line = compiled_code
                .as_ref()
                .and_then(|cc| cc.source_line)
                .map(|l| l as u32)
                .or_else(|| self.current_source_line());
            let val = Value::sub_value(crate::gc::Gc::new(crate::value::SubData {
                package: Symbol::intern(&self.lexical_closure_package()),
                name: Symbol::intern(""),
                params: params.clone(),
                param_defs: param_defs.clone(),
                body: body.clone(),
                is_rw: *is_rw,
                is_raw: *is_raw,
                env,
                assumed_positional: Vec::new(),
                assumed_named: std::collections::HashMap::new(),
                id: crate::value::next_instance_id(),
                empty_sig: params.is_empty() && param_defs.is_empty(),
                // A pointy block (`-> $x {...}`) is a `Block`, not a `Sub` — mark it
                // so `.WHAT`/`.^name`/smartmatch report `Block`. Named anonymous subs
                // (`sub {...}`) have `is_pointy_block == false` and stay `Sub`.
                is_bare_block: compiled_code.as_ref().is_some_and(|cc| cc.is_pointy_block),
                owned_captures,
                authoritative_captures,
                upvalues,
                compiled_code,
                deprecated_message: None,
                source_line: cc_source_line,
                source_file: self.current_source_file(),
            }));
            self.stack.push(val);
            Ok(())
        } else {
            Err(RuntimeError::new("MakeLambda expects SubDecl"))
        }
    }

    pub(super) fn exec_make_block_closure_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
        cc_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::Block(body) = stmt {
            let compiled_code = Self::resolve_closure_code(code, cc_idx);
            self.box_captured_lexicals(code, &compiled_code);
            let owned_captures = self.compute_owned_captures(&compiled_code);
            let authoritative_captures = self.compute_authoritative_captures(&compiled_code);
            let upvalues = self.capture_upvalues(code, &compiled_code);
            let cc_source_line = compiled_code
                .as_ref()
                .and_then(|cc| cc.source_line)
                .map(|l| l as u32)
                .or_else(|| self.current_source_line());
            let val = Value::sub_value(crate::gc::Gc::new(crate::value::SubData {
                package: Symbol::intern(&self.lexical_closure_package()),
                name: Symbol::intern(""),
                params: vec![],
                param_defs: Vec::new(),
                body: body.clone(),
                is_rw: false,
                is_raw: false,
                // Upvalue snapshot (single-store Slice E); see capture_closure_env.
                env: self.capture_closure_env(code, &compiled_code),
                assumed_positional: Vec::new(),
                assumed_named: std::collections::HashMap::new(),
                id: crate::value::next_instance_id(),
                empty_sig: false,
                is_bare_block: true,
                owned_captures,
                authoritative_captures,
                upvalues,
                compiled_code,
                deprecated_message: None,
                source_line: cc_source_line,
                source_file: self.current_source_file(),
            }));
            self.stack.push(val);
            Ok(())
        } else {
            Err(RuntimeError::new("MakeBlockClosure expects Block"))
        }
    }

    pub(super) fn exec_register_sub_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::SubDecl {
            name,
            name_expr,
            params,
            param_defs,
            return_type,
            associativity,
            signature_alternates,
            body,
            multi,
            is_rw,
            is_raw,
            is_export,
            export_tags,
            is_test_assertion,
            supersede,
            custom_traits,
            ..
        } = stmt
        {
            let resolved_name = if let Some(expr) = name_expr {
                self.vm_eval_block_value(&[Stmt::Expr(expr.clone())])?
                    .to_string_value()
            } else {
                name.resolve()
            };
            self.check_param_custom_traits(param_defs)?;
            // Compile-time declaration fingerprint for this site (absent for a
            // runtime-resolved `name_expr` sub), enabling the idempotent
            // re-registration fast path inside `register_sub_decl_fp`.
            let site_fp = code.sub_fingerprints.get(&idx).copied();
            let outcome = self.loan_env_for(|i| {
                i.register_sub_decl_fp(
                    &resolved_name,
                    params,
                    param_defs,
                    return_type.as_ref(),
                    associativity.as_ref(),
                    body,
                    *multi,
                    *is_rw,
                    *is_raw,
                    *is_test_assertion,
                    *supersede,
                    custom_traits,
                    site_fp,
                )
            })?;
            // An idempotent re-registration of an already-installed identical sub
            // leaves the registry untouched, so none of the install bookkeeping
            // below (cache invalidation, `&`-param shadow tracking, export, native
            // descriptor, signature alternates) needs to re-run — they were all
            // done by the first installation and persist.
            if outcome == crate::runtime::registration_sub::SubRegisterOutcome::Installed {
                // If this sub carries the `is native(...)` trait, record its C-FFI
                // descriptor so calls route through NativeCall instead of the body.
                if custom_traits.iter().any(|(t, _)| t == "native") {
                    self.register_native_call_sub(
                        &resolved_name,
                        param_defs,
                        return_type.as_ref(),
                        custom_traits,
                    )?;
                }
                self.fn_resolve_gen += 1;
                self.method_resolve_cache.clear();
                self.last_method_resolve = None;
                self.fast_method_cache.clear();
                self.native_ctor_plan_cache.clear();
                self.multi_resolve_cache.clear();
                self.multi_type_cacheable.clear();
                self.func_multi_resolve_cache.clear();
                self.func_multi_type_cacheable.clear();
                self.dispatch_multi_candidate.clear();
                // Record `&`-sigil parameter names so calls to a same-named routine
                // inside this sub bypass the name-keyed light-call caches (the param
                // can shadow a package sub of the same name).
                for pd in param_defs {
                    if let Some(bare) = pd.name.strip_prefix('&')
                        && !bare.is_empty()
                    {
                        // Records both plain names (`foo`) and operator categories
                        // (`infix:<@@>`); both can shadow a same-named package routine.
                        self.amp_param_shadowed_names.insert(Symbol::intern(bare));
                    }
                }
                if *is_export && !self.suppress_exports {
                    let pkg = self.current_package().to_string();
                    self.register_exported_sub(
                        pkg.clone(),
                        resolved_name.clone(),
                        export_tags.clone(),
                    );
                    // If a custom `is` trait mixed a role into this routine, the
                    // resulting Mixin lives in the lexical env as `&name` but would
                    // be dropped when the module scope exits. Capture it so `import`
                    // can restore the trait-modified value.
                    let code_var_key = format!("&{}", resolved_name);
                    if let Some(val) = self.env().get(&code_var_key)
                        && matches!(val.view(), ValueView::Mixin(..))
                    {
                        let val = val.clone();
                        self.record_exported_sub_value(pkg, resolved_name.clone(), val);
                    }
                }
                if !signature_alternates.is_empty() {
                    // Remember that this name was declared with signature alternates,
                    // whose `state` cell is shared across all alternates. A resolved
                    // per-candidate `FunctionDef` carries no alternate marker, so the
                    // OTF-dispatch gate consults this set to keep state-bearing
                    // candidates of such a name on the interpreter (which honors the
                    // shared cell). See `multi_candidate_state_forces_interpreter`.
                    self.multi_alternate_signature_names
                        .insert(Symbol::intern(&resolved_name));
                }
                for (alt_params, alt_param_defs) in signature_alternates {
                    self.loan_env_for(|i| {
                        i.register_sub_decl(
                            &resolved_name,
                            alt_params,
                            alt_param_defs,
                            return_type.as_ref(),
                            associativity.as_ref(),
                            body,
                            *multi,
                            *is_rw,
                            *is_raw,
                            *is_test_assertion,
                            *supersede,
                            custom_traits,
                        )
                    })?;
                }
            }
            // An `our sub` declared in a bare block closes over the block's `my`
            // lexicals, but a registry routine has no per-sub closure env and the
            // block scope is dropped on exit. When this `RegisterSub` runs in SOURCE
            // ORDER (after the `my $a = ...` that the sub captures — `RegisterSub` is
            // emitted both hoisted at block top AND in place), the captured local is
            // already a boxed shared cell in `env`. Persist those cells into
            // `escaped_our_lexical_cells` so a call made AFTER the block reads the
            // live value. Keyed to the sub's own declaration (not the box site), so
            // an unrelated sibling-block `my $a` cannot pollute the map; the hoisted
            // top-of-block registration runs before the box and finds no cell, so it
            // correctly persists nothing (a call before the block reads undefined).
            if custom_traits.iter().any(|(t, _)| t == "__our_scoped")
                && !self.escaping_our_lexical_names.is_empty()
            {
                // Record the sub itself: the cell resolution
                // (`escaping_our_read`/`escaping_our_write_cell`) fires only while
                // the innermost named routine frame is one of these subs, so a
                // plain `my sub` sharing a captured variable's name keeps using
                // its own live env capture.
                self.escaped_our_sub_names.insert(resolved_name.clone());
                let names: Vec<String> = self.escaping_our_lexical_names.iter().cloned().collect();
                for name in names {
                    if let Some(cell) = self.env().get(&name).cloned()
                        && cell.is_container_ref()
                    {
                        self.escaped_our_lexical_cells.insert(name, cell);
                    }
                }
            }
            // A sub declared inside a BLOCK scope is lexical: the block-exit
            // routine-registry restore drops its registration, so a closure
            // that escapes the block (Cro's RequestParser declares
            // `my sub fresh-message` in a `supply` block and calls it from a
            // `whenever`) could no longer resolve the name. Store the real Sub
            // value under `&name` in env — the closure's captured env carries
            // it out, and the call fallback (`&name` in env, Sub/WeakSub arm)
            // dispatches it after the registry miss. Runs on the idempotent
            // re-registration path too: env was rolled back at the previous
            // block exit even though the registry entry was reused.
            // Not inside an EVAL: an EVAL'd compilation unit also runs with a
            // raised block-scope depth, but a `sub` it declares is lexical to
            // that unit and must NOT stay callable afterwards
            // (`EVAL q|sub zzz9 {…}|; zzz9()` dies in raku —
            // `t/undeclared-routine-compile-time.t`).
            let in_eval = self
                .env()
                .get("__mutsu_in_eval")
                .is_some_and(|v| v.truthy());
            if self.block_scope_depth() > 0
                && !in_eval
                // An exported sub is part of its module's interface, installed
                // by the export machinery; the escape hatch is only for a
                // genuinely block-lexical one. Registering it here also made
                // the module-load env diff carry the reserved key, which broke
                // `require M <quux>`'s missing-symbol detection
                // (roast/S11-modules/require.t 11).
                && !*is_export
                // A prelude-injected helper (`cglobal`, `nativecast`,
                // `nativesizeof`, ... — see `inject_nativecall_subs_prelude`)
                // is an ambient GLOBAL routine, not a block-lexical one: every
                // compunit that so much as mentions NativeCall carries its own
                // identical copy, and only the first registration wins. Taking
                // the escape hatch stored one env-captured copy per module and
                // let the *last* module loaded answer a later `cglobal` call
                // with its own closure env, so DBIish's mysql driver probed the
                // library through the SQLite driver's scope and died with
                // "Cannot load native library 'libmariadb.so.0'" where it
                // should have soft-failed. Same reasoning as the `is_export`
                // gate above: an interface routine is not lexical to a block.
                && !custom_traits
                    .iter()
                    .any(|(t, _)| t == crate::runtime::PRELUDE_SUB_TRAIT)
                && !*multi
                && name_expr.is_none()
                && !resolved_name.contains("::")
                && !resolved_name.contains(':')
            {
                let sub_val = Value::make_sub(
                    Symbol::intern(&self.lexical_closure_package()),
                    Symbol::intern(&resolved_name),
                    params.clone(),
                    param_defs.clone(),
                    body.clone(),
                    *is_rw,
                    self.env().clone(),
                );
                // A RESERVED key, not the plain `&name`: while the block is
                // still live the registry entry is authoritative (it is what
                // carries `state` variables and wrap chains), and a plain
                // `&name` would be consulted ahead of it by the bareword and
                // call paths. Only the post-block-exit fallbacks read this.
                self.env_mut().insert(
                    format!("{}{resolved_name}", crate::env::BLOCK_LEXICAL_SUB_PREFIX),
                    sub_val,
                );
            }
            // Note: we intentionally do NOT push the Sub onto the stack or
            // store it in env here (beyond the block-lexical escape hatch
            // above). The interpreter's trailing_sub_value mechanism handles
            // returning the Sub when it's the last statement of a block.
            // Pushing would interfere with stack depth tracking.
            Ok(())
        } else {
            Err(RuntimeError::new("RegisterSub expects SubDecl"))
        }
    }

    /// Build and store the NativeCall descriptor for an `is native(...)` sub.
    /// The library name comes from the `native` trait argument, the C symbol
    /// from an optional `is symbol('...')` trait (defaulting to the sub name),
    /// and the C signature from the parameter / return type constraints.
    pub(crate) fn register_native_call_sub(
        &mut self,
        name: &str,
        param_defs: &[crate::ast::ParamDef],
        return_type: Option<&String>,
        custom_traits: &[(String, Option<crate::ast::Expr>)],
    ) -> Result<(), RuntimeError> {
        self.register_native_call_routine(name, None, param_defs, return_type, custom_traits)
    }

    /// The same, for an `is native(...)` **method**.
    ///
    /// A native method's invocant is its first C argument — `method
    /// mysql_query(MYSQL:D: Str $sql --> int32)` is `mysql_query(MYSQL*, const
    /// char*)` — so the descriptor gains a leading pointer parameter that the
    /// declared signature does not spell. This is how `DBDish::mysql::Native`
    /// declares its entire surface, and (unlike `DBDish::SQLite`, which uses
    /// plain subs) nothing in that driver runs without it.
    ///
    /// The descriptor is keyed by `<class>.<method>` under both the class's
    /// declared name and its short one, because a `nativecast`ed handle carries
    /// the short name while the declaration is package-qualified.
    pub(crate) fn register_native_call_method(
        &mut self,
        class_name: &str,
        name: &str,
        param_defs: &[crate::ast::ParamDef],
        return_type: Option<&String>,
        custom_traits: &[(String, Option<crate::ast::Expr>)],
    ) -> Result<(), RuntimeError> {
        self.register_native_call_routine(
            name,
            Some(class_name),
            param_defs,
            return_type,
            custom_traits,
        )
    }

    /// The key a native method's descriptor is stored and looked up under.
    pub(crate) fn native_method_key(class_name: &str, method: &str) -> String {
        format!("{class_name}.{method}")
    }

    fn register_native_call_routine(
        &mut self,
        name: &str,
        invocant_class: Option<&str>,
        param_defs: &[crate::ast::ParamDef],
        return_type: Option<&String>,
        custom_traits: &[(String, Option<crate::ast::Expr>)],
    ) -> Result<(), RuntimeError> {
        use crate::runtime::nativecall::{CType, NativeCallSpec, ParamSpec};

        // Evaluate a trait's argument expression to a String, if present.
        // A native library name can be supplied dynamically as a code object:
        // `is native(&ssl-lib)` means "call `ssl-lib()` at bind time and use its
        // return value as the library name" (Rakudo semantics — the OpenSSL /
        // IO::Socket::SSL bindings resolve `libssl.so.3` etc. this way). So when
        // the trait argument evaluates to a Callable, invoke it with no
        // arguments and stringify the result instead of stringifying the code
        // object itself.
        let mut eval_trait_str = |trait_name: &str| -> Result<Option<String>, RuntimeError> {
            for (t, arg) in custom_traits {
                if t == trait_name {
                    return Ok(match arg {
                        Some(expr) => {
                            let val = self.vm_eval_block_value(&[Stmt::Expr(expr.clone())])?;
                            let resolved = if matches!(val.view(), ValueView::Sub(..)) {
                                self.vm_call_sub_value(val, Vec::new(), false)?
                            } else {
                                val
                            };
                            // An UNDEFINED library is "no library" — Rakudo's
                            // `guess_library_name` maps it to a NULL handle, i.e.
                            // this process's own symbol space. It is written
                            // deliberately: `DBDish::mysql::Native` declares
                            // `constant LIB = Rakudo::Internals.IS-WIN ?? 'mysql' !! Str`
                            // and every one of its subs `is native(LIB)`, having
                            // dlopened the client library itself first.
                            // Stringifying the type object instead produced the
                            // nonsense name `lib(Str).so`.
                            if !crate::runtime::types::value_is_defined(&resolved) {
                                None
                            } else {
                                Some(resolved.to_string_value())
                            }
                        }
                        None => None,
                    });
                }
            }
            Ok(None)
        };

        let library = eval_trait_str("native")?;
        let symbol = eval_trait_str("symbol")?.unwrap_or_else(|| name.to_string());

        // Map each parameter's type constraint to a C type. An unmapped /
        // missing type means we cannot marshal it — skip native registration so
        // the failure surfaces clearly rather than mis-calling.
        let mut params = Vec::with_capacity(param_defs.len() + 1);
        // A native method receives its invocant as the first C argument whether
        // or not the signature spells it: `method PQstatus(--> int32)` on a
        // CPointer class is `PQstatus(PGconn*)` (DBDish::Pg declares its whole
        // surface this way). An explicit invocant (`MYSQL:D:`) arrives as a
        // leading `is_invocant` parameter; synthesize the pointer slot when the
        // signature leaves it implicit.
        if invocant_class.is_some() && !param_defs.first().is_some_and(|pd| pd.is_invocant) {
            params.push(ParamSpec {
                ct: CType::Pointer,
                is_rw: false,
                elem: None,
            });
        }
        for pd in param_defs {
            // A method's invocant is its first C argument, passed by pointer —
            // a handle's address for `:D:`, NULL for `:U:` (`MYSQL.mysql_init`
            // deliberately calls `mysql_init(NULL)`). The parser already hands
            // it over as a leading `is_invocant` parameter, so it needs no
            // synthesis here — only the right C type, which its declared
            // constraint (`MYSQL:D`, smiley and all) would not map to.
            if pd.is_invocant {
                params.push(ParamSpec {
                    ct: CType::Pointer,
                    is_rw: false,
                    elem: None,
                });
                continue;
            }
            let Some(tc) = pd.type_constraint.as_deref() else {
                return Ok(());
            };
            // A definedness smiley is not part of the C type: `Blob:D $dest`
            // marshals as `Blob`. Left attached, the name missed every scalar
            // mapping and fell through to the opaque-CStruct branch — the Buf
            // was passed as an address-less handle, i.e. NULL, and the callee
            // (NativeHelpers::Blob's `memcpy(Blob:D $dest, ...)`) wrote to it.
            let tc = tc
                .strip_suffix(":D")
                .or_else(|| tc.strip_suffix(":U"))
                .or_else(|| tc.strip_suffix(":_"))
                .unwrap_or(tc);
            // `constant my_bool = int8;` — follow the alias to the type it names.
            let resolved_tc = self.resolve_native_type_alias(tc);
            let tc = resolved_tc.as_str();
            let is_rw = pd.traits.iter().any(|t| t == "rw");
            // `CArray[T]` — a contiguous C buffer whose element type T is
            // marshalled per-element. Unrecognized element types skip native
            // registration (so the failure surfaces clearly).
            if let Some(inner) = tc.strip_prefix("CArray[").and_then(|s| s.strip_suffix(']')) {
                let inner = self.resolve_native_type_alias(inner);
                let Some(elem) = CType::from_type_name(&inner) else {
                    return Ok(());
                };
                params.push(ParamSpec {
                    ct: CType::CArray,
                    is_rw,
                    elem: Some(elem),
                });
                continue;
            }
            // An unparameterized `CArray` parameter — OpenSSL declares
            // `RSA_sign(int32, Blob, int32, Blob, CArray, OpaquePointer)`, whose
            // fifth argument is the `unsigned int*` signature-length slot. The
            // element type is not in the signature, so it is read from the
            // argument's container metadata at call time. Without this the name
            // fell through to the CStruct branch and was passed as an opaque
            // `void*` — i.e. NULL, since a `CArray` carries no address — and the
            // callee wrote through it.
            if tc == "CArray" {
                params.push(ParamSpec {
                    ct: CType::CArray,
                    is_rw,
                    elem: None,
                });
                continue;
            }
            let ct = match CType::from_type_name(tc) {
                Some(ct) => ct,
                // A user-declared `is repr('CStruct')` class (an opaque native
                // handle, e.g. `SSL_CTX`) is passed by pointer. Recognize it by
                // its type-name shape (a class name, so it lacks a mapped scalar
                // C type) and marshal it as a `void*` — `pointer_address` reads
                // the address the instance carries. A genuinely-unmarshallable
                // type (a lowercase / unqualified non-class name) still skips
                // native registration so the failure surfaces clearly.
                None if self.is_native_struct_type(tc) => CType::Pointer,
                None => return Ok(()),
            };
            params.push(ParamSpec {
                ct,
                is_rw,
                elem: None,
            });
        }

        let mut ret_struct: Option<String> = None;
        let ret = match return_type {
            None => CType::Void,
            // A returned `CArray[T]` has no length to reify into a Raku array,
            // so it is surfaced as the raw `Pointer` it carries.
            Some(rt) if rt.starts_with("CArray[") => CType::Pointer,
            Some(rt) => match CType::from_type_name(&self.resolve_native_type_alias(rt)) {
                Some(ct) => ct,
                // A CStruct return (opaque native handle): wrap the returned
                // pointer in an instance of the declared class so it round-trips
                // as that handle type (`ret_struct` carries the class name).
                None if self.is_native_struct_type(rt) => {
                    ret_struct = Some(self.registered_native_class_name(rt));
                    CType::Pointer
                }
                None => return Ok(()),
            },
        };

        let spec = NativeCallSpec {
            library,
            symbol,
            params,
            ret,
            ret_struct,
            entry: None,
        };
        if let Some(class_name) = invocant_class {
            // Both spellings: the declaration is package-qualified while a
            // `nativecast`ed handle carries only the short class name.
            let short = Self::native_struct_class_name(class_name);
            if short != class_name {
                self.native_call_specs
                    .insert(Self::native_method_key(&short, name), spec.clone());
            }
            self.native_call_specs
                .insert(Self::native_method_key(class_name, name), spec);
            return Ok(());
        }
        // Key the descriptor under the sub's short name. An `our sub` declared
        // inside a `module`/`package` (e.g. `OpenSSL::Method::TLS_client_method`)
        // is also called by its package-qualified name, and the callsite looks
        // the descriptor up by exactly the name it wrote — so register the
        // qualified name too. Without this, a qualified call misses the native
        // path and runs the stub `{ * }` body instead.
        let pkg = self.current_package();
        if pkg != "GLOBAL" {
            self.native_call_specs
                .insert(format!("{pkg}::{name}"), spec.clone());
        }
        self.native_call_specs.insert(name.to_string(), spec);
        Ok(())
    }

    /// A native parameter / return type name that is not one of the mapped
    /// scalar C types is treated as an opaque native handle (a pointer) when it
    /// is a `is repr('CStruct')` class — or, failing an exact registry match,
    /// when it has the shape of a class name: it starts with an uppercase letter
    /// or is package-qualified (`Foo::Bar`). This matches Rakudo NativeCall,
    /// where a `CStruct` type used directly is passed by pointer. The registry
    /// check catches lowercase CStruct classes (e.g. `evp_cipher_st`) that the
    /// shape heuristic would miss; the heuristic catches structs declared in
    /// another compilation unit not visible in this registry. A lowercase,
    /// unqualified, non-CStruct name (a likely typo'd scalar type) is rejected so
    /// a real mistake still surfaces rather than being silently mis-marshalled.
    fn is_native_struct_type(&self, name: &str) -> bool {
        let short = Self::native_struct_class_name(name);
        {
            let reg = self.registry();
            if reg.cstruct_classes.contains(name) || reg.cstruct_classes.contains(&short) {
                return true;
            }
        }
        name.contains("::") || name.starts_with(|c: char| c.is_ascii_uppercase())
    }

    /// The class name to tag a returned CStruct instance with: the last
    /// component of a package-qualified name (`OpenSSL::Method::SSL_METHOD` ->
    /// `SSL_METHOD`), matching how the class is registered by its short name.
    fn native_struct_class_name(name: &str) -> String {
        name.rsplit("::").next().unwrap_or(name).to_string()
    }

    /// The class name a returned native handle should be tagged with so that
    /// ordinary method dispatch on it works: the name the class is actually
    /// *registered* under. A class declared inside a `unit module` is
    /// registered package-qualified (`DBDish::Pg::Native::PGresult`) while the
    /// return type is spelled short (`--> PGresult`); resolving through the
    /// env — the same lookup `.^name` performs — recovers the registered name.
    /// Falls back to the short name for a class not visible here (declared in
    /// another compilation unit), preserving the previous behavior.
    fn registered_native_class_name(&mut self, name: &str) -> String {
        if let Some(ValueView::Package(sym)) = self.env().get(name).map(Value::view) {
            let resolved = sym.resolve().to_string();
            if self.registry().classes.contains_key(&resolved) {
                return resolved;
            }
        }
        Self::native_struct_class_name(name)
    }

    /// Follow a `constant` type alias to the type it names.
    ///
    /// A C binding routinely spells its platform types as constants:
    /// `DBDish::mysql::Native` declares `constant my_bool = int8;` and returns
    /// `my_bool` from most of the `MYSQL_STMT` surface. The constant holds the
    /// aliased *type object*, so read it back out of the environment and use
    /// its name. Without this the signature type is unmappable and the whole
    /// declaration silently skips native registration — the method then stays
    /// the stub `{ * }` body and the call fails with "No such method".
    ///
    /// Bounded: an alias chain longer than a few links is treated as no alias.
    ///
    /// A CStruct *field* is spelled the same way, so `cstruct_layout` follows
    /// the alias too — `MYSQL_BIND` declares `has intptr $.length`.
    pub(crate) fn resolve_native_type_alias(&self, name: &str) -> String {
        self.resolve_native_type_alias_for_owner(name, "")
    }

    /// [`Self::resolve_native_type_alias`] with a fallback anchor: when the
    /// live env no longer holds the constant (the module was loaded by a frame
    /// that has since returned — a `require` inside a method), the alias is
    /// looked up in `owner`'s module scope (`module_scope_lexicals`, walked up
    /// the `::` chain). `owner` is the declaration the alias was spelled in,
    /// e.g. the CStruct class whose field is being laid out.
    pub(crate) fn resolve_native_type_alias_for_owner(&self, name: &str, owner: &str) -> String {
        use crate::runtime::nativecall::CType;
        let mut current = name.to_string();
        for _ in 0..4 {
            if CType::from_type_name(&current).is_some() {
                return current;
            }
            let value = self.get_env_with_main_alias(&current).or_else(|| {
                if owner.is_empty() {
                    None
                } else {
                    self.module_scope_lexical_for_owner(owner, &current)
                        .cloned()
                }
            });
            let Some(value) = value else {
                break;
            };
            let ValueView::Package(target) = value.view() else {
                break;
            };
            let target = target.as_str().to_string();
            if target == current {
                break;
            }
            current = target;
        }
        current
    }

    pub(super) fn exec_register_token_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        match stmt {
            Stmt::TokenDecl {
                name,
                params,
                param_defs,
                body,
                multi,
                ..
            }
            | Stmt::RuleDecl {
                name,
                params,
                param_defs,
                body,
                multi,
            } => {
                self.register_token_decl(&name.resolve(), params, param_defs, body, *multi);
                Ok(())
            }
            _ => Err(RuntimeError::new(
                "RegisterToken expects TokenDecl/RuleDecl",
            )),
        }
    }

    pub(super) fn exec_register_proto_sub_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::ProtoDecl {
            name,
            params,
            param_defs,
            body,
            is_export,
            custom_traits,
            is_method,
            is_our,
            ..
        } = stmt
        {
            let name_str = name.resolve();
            // A `proto method`/`proto submethod` (`is_method`) is a *method*-level
            // proto: its `{*}` dispatches over the type's multi-method candidates
            // via the class method table, not the package-level proto-sub table.
            // Registering it as a package proto sub is not only unnecessary but
            // breaks role composition: the role body's `RegisterProtoSub` runs once
            // when the role is declared and again when a class does the role, so the
            // second registration hits the already-present `GLOBAL::<name>` proto and
            // wrongly raises `X::Redeclaration` (lizmat's `Enumify` proto+multi
            // pattern, SBOM::CycloneDX). Skip the package-level registration for
            // method protos; the method-table path already handles them.
            if !*is_method {
                self.register_proto_decl(&name_str, params, param_defs, body, *is_our)?;
            }
            if *is_export {
                self.register_proto_decl_as_global(&name_str, params, param_defs, body)?;
                // Record the export so consumers/MAIN-dispatch see the whole multi
                // family. A `proto … is export` exports its candidates too (raku),
                // e.g. zef's `proto MAIN(|) is export` over `multi sub MAIN(…)`.
                if !self.suppress_exports {
                    let pkg = self.current_package().to_string();
                    self.register_exported_sub(pkg, name_str.clone(), Vec::new());
                }
            }
            // Apply custom trait_mod:<is> for each non-builtin trait (only if defined)
            if !custom_traits.is_empty() {
                let has_trait_mod =
                    self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>");
                for trait_name in custom_traits.iter().filter(|t| {
                    !t.starts_with("__")
                        && *t != "default"
                        && !t.starts_with("DEPRECATED")
                        && *t != "deep"
                }) {
                    if !has_trait_mod {
                        return Err(RuntimeError::new(format!(
                            "Can't use unknown trait 'is' -> '{}' in sub declaration.",
                            trait_name
                        )));
                    }
                    let sub_val = Value::make_sub(
                        Symbol::intern(&self.current_package()),
                        Symbol::intern(&name_str),
                        params.clone(),
                        param_defs.clone(),
                        body.clone(),
                        false,
                        self.clone_env(),
                    );
                    let named_arg = Value::pair(trait_name.clone(), Value::TRUE);
                    let result = loan_env!(
                        self,
                        call_function("trait_mod:<is>", vec![sub_val, named_arg])
                    )?;
                    // If the trait_mod returned a modified sub (e.g. with CALL-ME mixed in),
                    // store it in the env so function dispatch can find it.
                    if matches!(result.view(), ValueView::Mixin(..)) {
                        self.env_mut().insert(format!("&{}", name), result);
                    }
                }
            }
            Ok(())
        } else {
            Err(RuntimeError::new("RegisterProtoSub expects ProtoDecl"))
        }
    }

    pub(super) fn exec_register_proto_token_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[idx as usize];
        if let Stmt::ProtoToken { name } = stmt {
            self.register_proto_token_decl(&name.resolve());
            Ok(())
        } else {
            Err(RuntimeError::new("RegisterProtoToken expects ProtoToken"))
        }
    }
}
