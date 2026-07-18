use super::methods_signature_errors::{
    make_method_not_found_error, make_private_permission_error, make_private_unqualified_error,
};
use super::*;
use crate::symbol::Symbol;
use crate::value::AttrMap;
use crate::value::ValueView;

impl Interpreter {
    /// Dispatch method calls for Instance values and handle all fallback paths.
    ///
    /// This is called from `call_method_with_values` after the main method name
    /// match and enum/promise/channel/mixin dispatch.
    pub(super) fn dispatch_instance_and_fallback(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // Instance dispatch
        if let ValueView::Instance {
            class_name,
            attributes,
            id: target_id,
        } = target.view()
        {
            // A per-attribute container descriptor produced by `Attribute.container`
            // (tagged with `__mutsu_attr_container_owner`). `.VAR` returns the
            // descriptor itself so a subsequent `does Role(...)` can be recorded
            // against the owning attribute (see `exec_does_op`); every other method
            // delegates to a plain empty container of the attribute's sigil so
            // introspection (`.container.shape`, ...) keeps working.
            if attributes
                .as_map()
                .contains_key("__mutsu_attr_container_owner")
            {
                if method == "VAR" {
                    return Ok(target.clone());
                }
                let sigil = attributes
                    .as_map()
                    .get("__mutsu_attr_container_sigil")
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|| "$".to_string());
                let base = match sigil.as_str() {
                    "@" => Value::array(Vec::new()),
                    "%" => Value::hash(std::collections::HashMap::new()),
                    _ => Value::NIL,
                };
                return self.call_method_with_values(base, method, args);
            }
            // `Duration` does `Real`: delegate numeric methods (e.g.
            // `(now - now).abs`) to its underlying numeric value. `.Str` / `.gist`
            // / `.raku` and other identity methods keep Duration's own behaviour.
            if class_name.resolve() == "Duration"
                && matches!(
                    method,
                    "abs"
                        | "floor"
                        | "ceiling"
                        | "round"
                        | "truncate"
                        | "sign"
                        | "Int"
                        | "Rat"
                        | "FatRat"
                        | "Real"
                        | "sqrt"
                )
            {
                let num = crate::runtime::utils::coerce_to_numeric(target.clone());
                return self.call_method_with_values(num, method, args);
            }
            if let Some(private_rest) = method.strip_prefix('!') {
                let caller_class = self
                    .method_class_stack
                    .last()
                    .cloned()
                    .or_else(|| Some(self.current_package().to_string()));
                // An unqualified external private call (`$o!meth` where `$o` is
                // not `self`) must name the defining package — Raku reports
                // X::Method::Private::Unqualified rather than the Permission
                // error used for a qualified-but-untrusted call.
                let was_qualified = private_rest.contains("::");
                // Resolve: owner-qualified (!Owner::method) or unqualified (!method)
                let (pm_name, resolved) =
                    if let Some((owner_class, pm_name)) = private_rest.split_once("::") {
                        let caller_allowed = caller_class.as_deref() == Some(owner_class)
                            || self.registry().class_trusts.get(owner_class).is_some_and(
                                |trusted| {
                                    caller_class
                                        .as_ref()
                                        .is_some_and(|caller| trusted.contains(caller))
                                },
                            );
                        if !caller_allowed {
                            return Err(make_private_permission_error(
                                pm_name,
                                owner_class,
                                caller_class.as_deref().unwrap_or("GLOBAL"),
                            ));
                        }
                        (
                            pm_name,
                            self.resolve_private_method_with_owner(
                                &class_name.resolve(),
                                owner_class,
                                pm_name,
                                &args,
                            ),
                        )
                    } else {
                        (
                            private_rest,
                            self.resolve_private_method_any_owner(
                                &class_name.resolve(),
                                private_rest,
                                &args,
                            ),
                        )
                    };
                if let Some((resolved_owner, method_def)) = resolved {
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
                        if was_qualified {
                            return Err(make_private_permission_error(
                                pm_name,
                                &class_name.resolve(),
                                caller_class.as_deref().unwrap_or("GLOBAL"),
                            ));
                        }
                        return Err(make_private_unqualified_error(pm_name));
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
                    return Ok(result);
                }
                // Private method not found — fall back to private attribute access.
                // In Raku, `$obj!Owner::attr` accesses the private attribute `$!attr`
                // directly when no explicit private method is defined.
                if args.is_empty()
                    && let Some(val) = attributes.as_map().get(pm_name)
                {
                    return Ok(val.clone());
                }
                return Err(make_method_not_found_error(
                    pm_name,
                    &class_name.resolve(),
                    true,
                ));
            }

            if class_name == "IterationBuffer" {
                let mut items = match attributes
                    .as_map()
                    .get("__mutsu_iterationbuffer_items")
                    .map(Value::view)
                {
                    Some(ValueView::Array(values, ..)) => values.to_vec(),
                    Some(ValueView::Seq(values)) | Some(ValueView::Slip(values)) => values.to_vec(),
                    _ => Vec::new(),
                };
                let iterationbuffer_values = |value: &Value| -> Vec<Value> {
                    match value.view() {
                        ValueView::Instance {
                            class_name,
                            attributes,
                            ..
                        } if class_name == "IterationBuffer" => {
                            match attributes
                                .as_map()
                                .get("__mutsu_iterationbuffer_items")
                                .map(Value::view)
                            {
                                Some(ValueView::Array(values, ..)) => values.to_vec(),
                                Some(ValueView::Seq(values)) | Some(ValueView::Slip(values)) => {
                                    values.to_vec()
                                }
                                _ => Vec::new(),
                            }
                        }
                        _ => Self::value_to_list(value),
                    }
                };
                let update_items = |new_items: Vec<Value>| {
                    let mut updated_attrs = attributes.to_map();
                    updated_attrs.insert(
                        "__mutsu_iterationbuffer_items".to_string(),
                        Value::real_array(new_items),
                    );
                    Value::write_back_sharing(&attributes, class_name, updated_attrs, target_id)
                };
                match method {
                    "elems" if args.is_empty() => return Ok(Value::int(items.len() as i64)),
                    "AT-POS" if args.len() == 1 => {
                        let index = crate::runtime::to_int(&args[0]);
                        if index >= 0
                            && let Some(value) = items.get(index as usize)
                        {
                            return Ok(value.clone());
                        }
                        return Ok(Value::package(Symbol::intern("Mu")));
                    }
                    "BIND-POS" if args.len() == 2 => {
                        let index = crate::runtime::to_int(&args[0]);
                        if index >= 0 {
                            let idx = index as usize;
                            if idx >= items.len() {
                                items.resize(idx + 1, Value::package(Symbol::intern("Mu")));
                            }
                            items[idx] = args[1].clone();
                        }
                        let bound = args[1].clone();
                        update_items(items);
                        return Ok(bound);
                    }
                    "push" if !args.is_empty() => {
                        items.extend(args.iter().cloned());
                        let result = args.last().cloned().unwrap_or(Value::NIL);
                        update_items(items);
                        return Ok(result);
                    }
                    "unshift" if !args.is_empty() => {
                        let mut prepended = args.clone();
                        prepended.extend(items);
                        let result = args.last().cloned().unwrap_or(Value::NIL);
                        update_items(prepended);
                        return Ok(result);
                    }
                    "List" if args.is_empty() => return Ok(Value::array(items)),
                    "Slip" if args.is_empty() => return Ok(Value::slip(items)),
                    "Seq" if args.is_empty() => return Ok(Value::seq(items)),
                    "append" if args.len() == 1 => {
                        items.extend(iterationbuffer_values(&args[0]));
                        return Ok(update_items(items));
                    }
                    "prepend" if args.len() == 1 => {
                        let mut prepended = iterationbuffer_values(&args[0]);
                        prepended.extend(items);
                        return Ok(update_items(prepended));
                    }
                    "clear" if args.is_empty() => {
                        update_items(Vec::new());
                        return Ok(Value::NIL);
                    }
                    _ => {}
                }
            }

            if class_name == "Attribute" {
                match method {
                    "set_build" => {
                        let build = args.first().cloned().ok_or_else(|| {
                            RuntimeError::new("Attribute.set_build expects a build callback")
                        })?;
                        let map = attributes.as_map();
                        let owner = map
                            .get("__mutsu_attr_owner")
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let attr_name = map
                            .get("__mutsu_attr_name")
                            .or_else(|| map.get("name"))
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        if owner.is_empty() || attr_name.is_empty() {
                            return Err(RuntimeError::new(
                                "Attribute.set_build missing owner or attribute name",
                            ));
                        }
                        self.registry_mut()
                            .attribute_build_overrides
                            .insert((owner, attr_name), build);
                        // A build override disqualifies the class from the native
                        // default constructor — drop any cached plan for it.
                        self.native_ctor_plan_cache.clear();
                        return Ok(target.clone());
                    }
                    "name" => {
                        return Ok(attributes
                            .as_map()
                            .get("name")
                            .cloned()
                            .unwrap_or(Value::NIL));
                    }
                    "type" => {
                        return Ok(attributes
                            .as_map()
                            .get("type")
                            .cloned()
                            .unwrap_or(Value::package(crate::symbol::Symbol::intern("Mu"))));
                    }
                    "has_accessor" => {
                        return Ok(attributes
                            .as_map()
                            .get("has_accessor")
                            .cloned()
                            .unwrap_or(Value::FALSE));
                    }
                    "set_rw" => {
                        // Mark the attribute as read-write. The generated accessor
                        // for a `has $.x is rw` is already rw; for an attribute that
                        // a custom `trait_mod:<is>` makes rw, record it on the meta
                        // so `.rw`/`.readonly` introspection reflects the change.
                        attributes.insert("is_rw".to_string(), Value::TRUE);
                        return Ok(target.clone());
                    }
                    "rw" => {
                        return Ok(attributes
                            .as_map()
                            .get("is_rw")
                            .cloned()
                            .unwrap_or(Value::FALSE));
                    }
                    "readonly" => {
                        let is_rw = attributes
                            .as_map()
                            .get("is_rw")
                            .map(|v| v.truthy())
                            .unwrap_or(false);
                        return Ok(Value::truth(!is_rw));
                    }
                    "build" => {
                        if let Some(build_val) = attributes.as_map().get("build") {
                            return Ok(build_val.clone());
                        }
                        if attributes
                            .as_map()
                            .get("__mutsu_has_build")
                            .map(|v| v.truthy())
                            .unwrap_or(false)
                        {
                            return Ok(Value::TRUE);
                        }
                        return Ok(Value::NIL);
                    }
                    "get_value" => {
                        let obj = args.first().ok_or_else(|| {
                            RuntimeError::new("Attribute.get_value expects an object argument")
                        })?;
                        let attr_name = attributes
                            .as_map()
                            .get("__mutsu_attr_name")
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        if let ValueView::Instance {
                            attributes: obj_attrs,
                            ..
                        } = obj.view()
                        {
                            return Ok(obj_attrs
                                .as_map()
                                .get(&attr_name)
                                .cloned()
                                .unwrap_or(Value::NIL));
                        }
                        return Ok(Value::NIL);
                    }
                    "set_value" => {
                        if args.len() < 2 {
                            return Err(RuntimeError::new(
                                "Attribute.set_value needs an object and a value",
                            ));
                        }
                        let new_val = args[1].clone();
                        let attr_name = attributes
                            .as_map()
                            .get("__mutsu_attr_name")
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        if let ValueView::Instance {
                            attributes: obj_attrs,
                            ..
                        } = args[0].view()
                        {
                            let mut updated = obj_attrs.to_map();
                            updated.insert(attr_name, new_val);
                            obj_attrs.commit_attrs(updated);
                        }
                        return Ok(Value::NIL);
                    }
                    "gist" | "Str" => {
                        let type_name = attributes
                            .as_map()
                            .get("type")
                            .map(|v| match v.view() {
                                ValueView::Package(name) => name.resolve(),
                                _ => v.to_string_value(),
                            })
                            .unwrap_or_else(|| "Mu".to_string());
                        let name = attributes
                            .as_map()
                            .get("name")
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        if method == "gist" {
                            return Ok(Value::str(format!("{} {}", type_name, name)));
                        }
                        return Ok(Value::str(name));
                    }
                    "raku" => {
                        let is_bootstrap = attributes
                            .as_map()
                            .get("__mutsu_is_bootstrapattr")
                            .is_some_and(|v| v.truthy());
                        if is_bootstrap {
                            return Ok(Value::str("BOOTSTRAPATTR.new".to_string()));
                        }
                        return Ok(Value::str("Attribute.new".to_string()));
                    }
                    "package" => {
                        let owner = attributes
                            .as_map()
                            .get("__mutsu_attr_owner")
                            .map(|v| v.to_string_value())
                            .unwrap_or_else(|| "Any".to_string());
                        return Ok(Value::package(crate::symbol::Symbol::intern(&owner)));
                    }
                    "container" => {
                        // Return a per-attribute container descriptor tagged with
                        // the owning class + attribute name so that a custom
                        // `trait_mod:<is>` can mix a role into it via
                        // `$a.container.VAR does Role(...)`. The recorded mixin is
                        // applied to each instance's attribute value at
                        // construction (see `apply_attribute_does_role_mixins`).
                        let map = attributes.as_map();
                        let sigil = map
                            .get("sigil")
                            .map(|v| v.to_string_value())
                            .unwrap_or_else(|| "$".to_string());
                        let owner = map
                            .get("__mutsu_attr_owner")
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let attr_name = map
                            .get("__mutsu_attr_name")
                            .or_else(|| map.get("name"))
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let mut cmeta = HashMap::new();
                        cmeta.insert(
                            "__mutsu_attr_container_owner".to_string(),
                            Value::str(owner),
                        );
                        cmeta.insert(
                            "__mutsu_attr_container_name".to_string(),
                            Value::str(attr_name),
                        );
                        cmeta.insert(
                            "__mutsu_attr_container_sigil".to_string(),
                            Value::str(sigil),
                        );
                        return Ok(Value::make_instance(Symbol::intern("Scalar"), cmeta));
                    }
                    _ => {}
                }
            }

            // IO::Spec::CurUpDir ACCEPTS
            if class_name.resolve() == "IO::Spec::CurUpDir" && method == "ACCEPTS" {
                let arg = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                // curupdir rejects "." and "..", accepts everything else
                let result = arg != "." && arg != "..";
                return Ok(Value::truth(result));
            }

            // IO::Spec methods — delegate to Package-based handler
            if class_name == "IO::Spec" || class_name.resolve().starts_with("IO::Spec::") {
                let pkg = Value::package(class_name);
                return self.call_method_with_values(pkg, method, args);
            }
            // Distribution::Path / ::Hash / ::Installation
            if class_name.resolve().starts_with("Distribution::")
                && let Some(result) = self.dispatch_distribution_method(
                    &class_name.resolve(),
                    &(attributes).as_map(),
                    method,
                    args.clone(),
                )
            {
                return result;
            }
            // CompUnit::Repository base methods shared by every repository kind
            // (FileSystem / Installation / ...). `repo-chain` walks the
            // `next-repo` links starting at self; mutsu's repositories are not
            // chained (each `$*REPO` is a standalone repo), so the chain is just
            // `(self,)` unless a `next-repo` attribute has been set. Without this,
            // `$*REPO.repo-chain` (zef's `list-installed`) died with
            // "No such method 'repo-chain'".
            if class_name.resolve().starts_with("CompUnit::Repository") {
                match method {
                    "repo-chain" => {
                        let mut chain = vec![target.clone()];
                        let mut cursor = attributes.as_map().get("next-repo").cloned();
                        while let Some(next) = cursor {
                            if !next.truthy() {
                                break;
                            }
                            if let ValueView::Instance { attributes, .. } = next.view() {
                                cursor = attributes.as_map().get("next-repo").cloned();
                            } else {
                                cursor = None;
                            }
                            chain.push(next);
                        }
                        return Ok(Value::array(chain));
                    }
                    "next-repo" => {
                        return Ok(attributes
                            .as_map()
                            .get("next-repo")
                            .cloned()
                            .unwrap_or(Value::NIL));
                    }
                    _ => {}
                }
            }
            if class_name == "CompUnit::Repository::Installation"
                && let Some(result) = self.dispatch_cur_installation_method(
                    &(attributes).as_map(),
                    method,
                    args.clone(),
                )
            {
                return result;
            }
            if class_name == "CompUnit::DependencySpecification" {
                match method {
                    "short-name" => {
                        return Ok(attributes
                            .as_map()
                            .get("short-name")
                            .cloned()
                            .unwrap_or(Value::NIL));
                    }
                    "auth-matcher" => {
                        return Ok(attributes
                            .as_map()
                            .get("auth-matcher")
                            .cloned()
                            .unwrap_or(Value::TRUE));
                    }
                    "version-matcher" => {
                        return Ok(attributes
                            .as_map()
                            .get("version-matcher")
                            .cloned()
                            .unwrap_or(Value::TRUE));
                    }
                    "api-matcher" => {
                        return Ok(attributes
                            .as_map()
                            .get("api-matcher")
                            .cloned()
                            .unwrap_or(Value::TRUE));
                    }
                    "Str" | "gist" => {
                        return Ok(attributes
                            .as_map()
                            .get("short-name")
                            .cloned()
                            .unwrap_or(Value::NIL));
                    }
                    _ => {}
                }
            }
            if class_name == "CompUnit::Repository::FileSystem" {
                match method {
                    // mutsu loads modules from source (precompilation is a
                    // parse-cache detail); expose the repository object the
                    // CompUnit API promises so precomp-aware callers
                    // (roast's Test::Compile) can thread it through `need`.
                    "precomp-repository" => {
                        return Ok(Value::make_instance(
                            Symbol::intern("CompUnit::PrecompilationRepository::Default"),
                            HashMap::new(),
                        ));
                    }
                    "candidates" => {
                        let prefix = attributes
                            .as_map()
                            .get("prefix")
                            .map(Value::to_string_value)
                            .unwrap_or_default();
                        let depspec = args.first().cloned().unwrap_or(Value::NIL);
                        return self.cur_fs_candidates(&prefix, &depspec);
                    }
                    "install" => {
                        return Err(RuntimeError::new("Cannot install on CUR::FileSystem"));
                    }
                    "need" => {
                        // A depspec may arrive either as the native `CompUnitDepSpec`
                        // value (short-name only) or as a full
                        // `CompUnit::DependencySpecification` instance carrying
                        // auth/api/version matchers (zef's plugin loader builds the
                        // latter). Extract the short-name from either form; the
                        // FileSystem repo resolves purely by name.
                        let depspec = args.first().cloned().unwrap_or(Value::NIL);
                        let (short_name_str, ..) = self.extract_depspec_fields(&depspec);
                        if short_name_str.is_empty() {
                            return Ok(Value::NIL);
                        }
                        let prefix = attributes
                            .as_map()
                            .get("prefix")
                            .map(Value::to_string_value)
                            .unwrap_or_default();
                        let canonical_prefix = std::fs::canonicalize(&prefix)
                            .unwrap_or_else(|_| std::path::PathBuf::from(&prefix))
                            .to_string_lossy()
                            .to_string();
                        // Check cache first
                        let cache_key =
                            format!("__mutsu_compunit::{}::{}", canonical_prefix, short_name_str);
                        if let Some(existing) = self.env.get(&cache_key).cloned() {
                            return Ok(existing);
                        }
                        // Try to find the module file in the prefix directory
                        let relative = short_name_str.replace("::", "/");
                        let mut found_path = None;
                        for ext in [".rakumod", ".pm6", ".raku", ".pm"] {
                            let candidate = std::path::Path::new(&canonical_prefix)
                                .join(format!("{relative}{ext}"));
                            if candidate.exists() {
                                found_path = Some(candidate);
                                break;
                            }
                        }
                        // If not found in prefix, try the standard module resolution
                        if found_path.is_none() {
                            found_path = self.resolve_module_path(&short_name_str).map(|(p, _)| p);
                        }
                        let Some(source_path) = found_path else {
                            return Ok(Value::NIL);
                        };
                        let repo_precomp_enabled = attributes
                            .as_map()
                            .get("__mutsu_precomp_enabled")
                            .is_none_or(Value::truthy);
                        // Load the module, using precompilation cache when available.
                        // Explicitly constructed FileSystem repositories default to
                        // precomp-disabled behavior.
                        let (stmts, precompiled) = if repo_precomp_enabled {
                            self.parse_module_source(&short_name_str, &source_path)?
                        } else {
                            let saved = self.precomp_enabled;
                            self.precomp_enabled = false;
                            let parsed = self.parse_module_source(&short_name_str, &source_path);
                            self.precomp_enabled = saved;
                            parsed?
                        };
                        let compile_time_only = !stmts.is_empty()
                            && stmts
                                .iter()
                                .all(|stmt| matches!(stmt, crate::ast::Stmt::Use { .. }));
                        let non_version_use_count = if compile_time_only {
                            stmts
                                .iter()
                                .filter_map(|stmt| match stmt {
                                    crate::ast::Stmt::Use { module, .. } => Some(module.as_str()),
                                    _ => None,
                                })
                                .filter(|module| *module != "v6")
                                .count()
                        } else {
                            0
                        };
                        let skip_runtime = compile_time_only && non_version_use_count > 1;
                        if !skip_runtime {
                            self.run_block(&stmts)?;
                        }
                        self.loaded_modules.insert(short_name_str.clone());
                        let mut attrs = HashMap::new();
                        attrs.insert("from".to_string(), Value::str_from("Raku"));
                        attrs.insert("short-name".to_string(), Value::str(short_name_str));
                        attrs.insert(
                            "precompiled".to_string(),
                            Value::truth(precompiled && repo_precomp_enabled),
                        );
                        let compunit = Value::make_instance(Symbol::intern("CompUnit"), attrs);
                        self.env.insert(cache_key, compunit.clone());
                        return Ok(compunit);
                    }
                    _ => {}
                }
            }
            if class_name == "CompUnit" {
                match method {
                    "short-name" => {
                        return Ok(attributes
                            .as_map()
                            .get("short-name")
                            .cloned()
                            .unwrap_or(Value::NIL));
                    }
                    "name" => {
                        return Ok(attributes
                            .as_map()
                            .get("short-name")
                            .cloned()
                            .unwrap_or(Value::NIL));
                    }
                    "version" => {
                        return Ok(attributes
                            .as_map()
                            .get("version")
                            .cloned()
                            .unwrap_or(Value::NIL));
                    }
                    // Rakudo exposes a handle object that can answer .globalish-package.
                    // For mutsu, the handle carries the symbols this CompUnit loaded so
                    // they can later be merged into GLOBAL via `merge-symbols`.
                    "handle" => {
                        let mut handle_attrs = HashMap::new();
                        if let Some(syms) = attributes.as_map().get("globalish-symbols") {
                            handle_attrs.insert("globalish-symbols".to_string(), syms.clone());
                        }
                        return Ok(Value::make_instance(
                            Symbol::intern("CompUnit::Handle"),
                            handle_attrs,
                        ));
                    }
                    "globalish-package" => {
                        return Ok(self
                            .make_globalish_package(attributes.as_map().get("globalish-symbols")));
                    }
                    _ => {}
                }
            }
            if class_name == "CompUnit::Handle" && method == "globalish-package" {
                return Ok(
                    self.make_globalish_package(attributes.as_map().get("globalish-symbols"))
                );
            }
            if class_name == "Stash" && method == "merge-symbols" {
                let pkg = args.first().cloned().unwrap_or(Value::NIL);
                return Ok(self.merge_global_symbols(&pkg));
            }
            if method == "can" {
                let method_name = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                if method_name.is_empty() {
                    return Ok(Value::array(Vec::new()));
                }
                let results = self.collect_can_methods(&target, &method_name);
                return Ok(Value::array(results));
            }
            if class_name == "Proc::Async" {
                if matches!(
                    method,
                    "start"
                        | "kill"
                        | "write"
                        | "close-stdin"
                        | "bind-stdin"
                        | "bind-stdout"
                        | "bind-stderr"
                        | "ready"
                        | "print"
                        | "put"
                        | "say"
                        | "stdout"
                        | "stderr"
                        | "Supply"
                ) {
                    let (result, updated) = self.call_native_instance_method_mut(
                        &class_name.resolve(),
                        attributes.to_map(),
                        method,
                        args,
                    )?;
                    attributes.commit_attrs(updated);
                    return Ok(result);
                }
                if matches!(
                    method,
                    "command" | "started" | "w" | "pid" | "stdout" | "stderr" | "Supply"
                ) {
                    return self.call_native_instance_method(
                        &class_name.resolve(),
                        &(attributes).as_map(),
                        method,
                        args,
                    );
                }
            }
            if class_name == "IO::CatHandle" && self.is_native_method(&class_name.resolve(), method)
            {
                // Lazy `.lines` (no `$limit`/`:close`) and `.handles` return a
                // lazy list backed by the live cat (sharing its attribute cell),
                // so mid-iteration `.chomp`/`.nl-in`/`.encoding` changes apply and
                // `.path`/on-switch track the current handle (Rakudo semantics).
                if let Some(lazy) = Self::cathandle_lazy_method(&target, method, &args) {
                    return Ok(lazy);
                }
                // Every IO::CatHandle method advances internal read state, so route
                // through the mutable path and commit the updated attributes back to
                // the receiver's shared cell.
                let (result, updated) = self.call_native_instance_method_mut(
                    &class_name.resolve(),
                    attributes.to_map(),
                    method,
                    args,
                )?;
                attributes.commit_attrs(updated);
                return Ok(result);
            }
            if self.is_native_method(&class_name.resolve(), method) {
                return self.call_native_instance_method(
                    &class_name.resolve(),
                    &(attributes).as_map(),
                    method,
                    args,
                );
            }
            if method == "isa" {
                let isa_arg = args.first().cloned().unwrap_or(Value::NIL);
                let target_name = match isa_arg.view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Str(name) => name.to_string(),
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => isa_arg.to_string_value(),
                };
                // isa only matches classes, not roles. If the target is a role
                // that has NOT been punned to a class, return False (use .does
                // for role checking).
                if self.registry().roles.contains_key(&target_name)
                    && !self.registry().classes.contains_key(&target_name)
                {
                    return Ok(Value::FALSE);
                }
                return Ok(Value::truth(
                    self.class_mro(&class_name.resolve())
                        .contains(&crate::symbol::Symbol::intern(&target_name)),
                ));
            }
            // `X::AdHoc.payload` returns the value passed to `die` (mutsu stores
            // it under `payload`). An X::AdHoc synthesized from an internal
            // `RuntimeError` — a native dlopen failure, a parse error inside
            // EVAL, … — carries only a `message` attr (no `payload`), so fall
            // back to that string so `.payload.starts-with(...)` / `.payload.Str`
            // keep working (raku: `X::AdHoc` made from `die "msg"` has
            // `.payload eq "msg"`).
            if method == "payload" && args.is_empty() && class_name.resolve() == "X::AdHoc" {
                let m = attributes.as_map();
                if let Some(p) = m.get("payload") {
                    return Ok(p.clone());
                }
                if let Some(msg) = m.get("message") {
                    return Ok(Value::str(msg.to_string_value()));
                }
            }
            // An Exception instance stringifies (.Str/.gist/~) and reports via its
            // `.message` (raku: Exception.Str and .gist both return .message). Detect
            // it via the MRO so a user `class E is Exception` (whose name is not
            // `X::*`/`*Exception`) is covered too. A user-defined `gist`/`Str`/
            // `message` already won dispatch above, so reaching here is the default.
            if args.is_empty() && matches!(method, "gist" | "Str" | "Stringy" | "message") {
                let cn = class_name.resolve();
                let is_exception = cn == "Exception"
                    || cn.starts_with("X::")
                    || cn.starts_with("CX::")
                    || cn.ends_with("Exception")
                    || self
                        .class_mro(&cn)
                        .iter()
                        .any(|p| p == "Exception" || p == "Failure");
                if is_exception {
                    // For .Str/.gist, prefer a user-defined `message` method (it may
                    // interpolate attributes); `message` itself only reaches here when
                    // no user method exists, so fall to the stored attr / formatted msg.
                    if method != "message" && self.has_user_method(&cn, "message") {
                        return self.call_method_with_values(target.clone(), "message", vec![]);
                    }
                    if let Some(msg) = attributes.as_map().get("message") {
                        return Ok(Value::str(msg.to_string_value()));
                    }
                    if let Some(formatted) =
                        crate::builtins::exception_message::format_exception_message(
                            &cn,
                            &(attributes).as_map(),
                        )
                    {
                        return Ok(Value::str(formatted));
                    }
                }
            }
            // Default `.gist` of a user instance matches its `.raku` (raku:
            // `say F.new(:z(5))` → `F.new(z => 5)`), unless the class defines its
            // own `gist`. Without this, a defined instance gisted as `F()` (the
            // type-object form), e.g. `say $obj`, `$obj.=meth` round-trips.
            // A class that does Real/Numeric/Stringy keeps its coercion-based
            // gist (`class Plain does Real { method Bridge {...} }` gists as its
            // number, not `Plain.new(...)`), so fall through for those. The
            // numeric coercion is also recognized via a `Bridge` method (mirrors
            // the native-bypass check), since `does Real` is not always visible
            // to `does_check`.
            let has_coercion_gist = target.does_check("Real")
                || target.does_check("Numeric")
                || target.does_check("Stringy")
                || matches!(target.view(), ValueView::Instance { class_name, .. }
                    if self.has_user_method(&class_name.resolve(), "Bridge"));
            // Callable instances (Method/Sub/…) gist by name (`foo`), not as
            // `Method.new(...)`, so keep their built-in gist.
            let is_callable_instance = matches!(target.view(), ValueView::Instance { class_name, .. }
                if matches!(class_name.resolve().as_str(),
                    "Method" | "Submethod" | "Sub" | "Routine"
                        | "Block" | "Code" | "WhateverCode" | "Callable"));
            let gist_default = method == "gist" && !has_coercion_gist && !is_callable_instance;
            if (method == "raku" || method == "perl" || gist_default)
                && args.is_empty()
                && !self.has_user_method(&class_name.resolve(), method)
            {
                // An `is Array` subclass instance gists/rakus as its backing
                // array (`Vector.new(1,2,3).gist` → `[1 2 3]`), not the generic
                // `Class.new`. Delegate to the storage array's own repr method.
                if let Some(storage) = attributes.as_map().get("__mutsu_array_storage").cloned() {
                    return self.call_method_with_values(storage, method, vec![]);
                }
                if class_name == Symbol::intern("ObjAt")
                    || class_name == Symbol::intern("ValueObjAt")
                {
                    let which = attributes
                        .as_map()
                        .get("WHICH")
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    // `.gist` (like `.Str`) shows the bare WHICH (`Int|42`);
                    // only `.raku`/`.perl` show the `ValueObjAt.new("Int|42")`
                    // constructor form.
                    if method == "gist" {
                        return Ok(Value::str(which));
                    }
                    return Ok(Value::str(format!(
                        "{}.new(\"{}\")",
                        class_name.resolve(),
                        which
                    )));
                }
                // X::AdHoc's raku-visible attribute is `payload` (mutsu stores the
                // die string under `message`/`payload`); render it as
                // `X::AdHoc.new(payload => "...")` rather than the bare `.new`.
                if class_name.resolve() == "X::AdHoc"
                    && (method == "raku" || method == "perl")
                    && let attr_map = attributes.as_map()
                    && let Some(payload) = attr_map
                        .get("payload")
                        .or_else(|| attr_map.get("message"))
                        .cloned()
                {
                    let payload_raku = self
                        .call_method_with_values(payload.clone(), "raku", vec![])
                        .map(|v| v.to_string_value())
                        .unwrap_or_else(|_| payload.to_string_value());
                    return Ok(Value::str(format!(
                        "X::AdHoc.new(payload => {payload_raku})"
                    )));
                }
                // Collect public attributes for .raku representation
                let class_key = class_name.resolve();
                let display_name = crate::value::user_facing_type_name(&class_key);
                let public_attrs =
                    self.collect_public_raku_attrs(&class_key, &(attributes).as_map());
                if public_attrs.is_empty() {
                    return Ok(Value::str(format!("{}.new", display_name)));
                }
                return Ok(Value::str(format!(
                    "{}.new({})",
                    display_name,
                    public_attrs.join(", ")
                )));
            }
            if method == "name" && args.is_empty() {
                return Ok(attributes
                    .as_map()
                    .get("name")
                    .cloned()
                    .unwrap_or(Value::NIL));
            }
            if method == "clone" {
                let mut attrs: AttrMap = attributes.to_map();
                // A slot promoted to a `ContainerRef` cell (a `:=`-bound
                // attribute) must not leak its cell into the clone: snapshot
                // the inner value so a write to the clone's attribute stays
                // invisible to the original (raku clones get fresh containers).
                for v in attrs.values_mut() {
                    if matches!(v.view(), ValueView::ContainerRef(_)) {
                        let taken = std::mem::replace(v, Value::NIL);
                        *v = taken.deref_container();
                    }
                }
                // Build sigil map from class attributes to coerce values properly
                let class_attrs_info = self.collect_class_attributes(&class_name.resolve());
                let sigil_map: HashMap<String, char> = class_attrs_info
                    .iter()
                    .map(|(name, _, _, _, _, sigil, _)| (name.clone(), *sigil))
                    .collect();
                let cn = class_name.resolve();
                for arg in &args {
                    if let ValueView::Pair(key, boxed) = arg.view()
                        && self.is_attribute_buildable(&cn, key)
                    {
                        let sigil = sigil_map.get(key.as_str()).copied().unwrap_or('$');
                        let coerced = Self::coerce_attr_value_by_sigil(boxed.clone(), sigil);
                        attrs.insert(key.clone(), coerced);
                    }
                }
                return Ok(Value::make_instance(class_name, attrs));
            }
            if method == "Bool"
                && args.is_empty()
                && ((target.does_check("Real") || target.does_check("Numeric"))
                    || self.has_user_method(&class_name.resolve(), "Bridge"))
                && let Ok(coerced) = self
                    .call_method_with_values(target.clone(), "Numeric", vec![])
                    .or_else(|_| self.call_method_with_values(target.clone(), "Bridge", vec![]))
            {
                return Ok(Value::truth(coerced.truthy()));
            }
            if method == "Bridge"
                && args.is_empty()
                && target.does_check("Real")
                && !self.has_user_method(&class_name.resolve(), "Bridge")
            {
                if let Ok(coerced) = self.call_method_with_values(target.clone(), "Numeric", vec![])
                    && coerced != target
                {
                    return Ok(coerced);
                }
                if let Ok(coerced) = self.call_method_with_values(target.clone(), "Num", vec![])
                    && coerced != target
                {
                    return Ok(coerced);
                }
            }
            if method == "Bridge"
                && args.is_empty()
                && self.has_user_method(&class_name.resolve(), "Num")
                && !self.has_user_method(&class_name.resolve(), "Bridge")
                && let Ok(coerced) = self.call_method_with_values(target.clone(), "Num", vec![])
                && coerced != target
            {
                return Ok(coerced);
            }
            if method == "log"
                && args.len() == 1
                && self.has_user_method(&class_name.resolve(), "Bridge")
                && let Ok(bridged) = self.call_method_with_values(target.clone(), "Bridge", vec![])
            {
                let base = if let Some(arg) = args.first() {
                    if matches!(arg.view(), ValueView::Instance { class_name, .. }
                        if self.has_user_method(&class_name.resolve(), "Bridge"))
                    {
                        self.call_method_with_values(arg.clone(), "Numeric", vec![])
                            .or_else(|_| {
                                self.call_method_with_values(arg.clone(), "Bridge", vec![])
                            })
                            .unwrap_or_else(|_| arg.clone())
                    } else {
                        arg.clone()
                    }
                } else {
                    Value::NIL
                };
                return self.call_method_with_values(bridged, "log", vec![base]);
            }
            // User-defined methods take priority over auto-generated accessors
            // only when they win the per-MRO-level race: a child class's
            // accessor shadows a parent's explicit method, and a method that
            // comes ONLY from a composed role does not shadow the class's own
            // attribute accessor at the same level (class entities are
            // prioritized over role entities). When the accessor wins, fall
            // through to the accessor block below.
            let cn_resolved = class_name.resolve();
            let accessor_wins = matches!(
                self.resolve_user_method_or_accessor(&cn_resolved, method),
                Some(UserMethodOrAccessor::Accessor)
            );
            if !accessor_wins && self.has_user_method(&cn_resolved, method) {
                let (result, updated) = self.run_instance_method(
                    &class_name.resolve(),
                    attributes.to_map(),
                    method,
                    args,
                    Some(target.clone()),
                )?;
                attributes.commit_attrs(updated.clone());
                // Auto-FETCH if the method returned a Proxy
                if !self.in_lvalue_assignment
                    && let ValueView::Proxy { fetcher, .. } = result.view()
                {
                    return self.proxy_fetch(
                        fetcher,
                        None,
                        &class_name.resolve(),
                        &updated,
                        target_id,
                    );
                }
                return Ok(result);
            }
            // Fallback: auto-generated accessor for public attributes.
            if args.is_empty() {
                let cn = class_name.resolve();
                let class_attrs = self.collect_class_attributes(&cn);
                // For a *user-declared* class the collected public-attribute list is
                // authoritative: a `.name` accessor resolves ONLY for a declared
                // public `has $.name`. An undeclared name (e.g. an unknown named arg
                // `.new` accepted and stored) is NOT an accessor and falls through to
                // X::Method::NotFound (Rakudo: `class C {}; C.new(x=>3).x` dies). A
                // *built-in* class (exception types, ...) keeps its attributes only
                // in the stored map (not collected), so still read them.
                if class_attrs.is_empty() {
                    if !self.user_declared_classes.contains(&cn)
                        && let Some(val) = attributes.as_map().get(method)
                    {
                        // Check for deprecated attribute accessor
                        if let Some(msg) = self.class_attribute_deprecated(&cn, method) {
                            self.check_deprecation_for_method(method, &cn, &msg);
                        }
                        return Ok(val.clone());
                    }
                } else {
                    for (attr_name, is_public, _, _, _, sigil, _) in &class_attrs {
                        if *is_public && attr_name == method {
                            // Check for deprecated attribute accessor
                            if let Some(msg) = self.class_attribute_deprecated(&cn, method) {
                                self.check_deprecation_for_method(method, &cn, &msg);
                            }
                            let val = attributes
                                .as_map()
                                .get(method)
                                .cloned()
                                .unwrap_or(Value::NIL);
                            // For typed @/% attributes, register type metadata on the
                            // returned value so push/insert type enforcement works.
                            if (*sigil == '@' || *sigil == '%')
                                && !val.is_nil()
                                && self.container_type_metadata(&val).is_none()
                                && let Some(tc) =
                                    self.get_attr_type_constraint(&cn, method.as_ref())
                            {
                                let info = crate::runtime::ContainerTypeInfo {
                                    value_type: tc.clone(),
                                    key_type: None,
                                    declared_type: Some(tc.clone()),
                                };
                                return Ok(self.tag_container_metadata(val, info));
                            }
                            return Ok(val);
                        }
                    }
                }
            }
            // Enum-as-role dispatch: if the class `does` an enum, check variant methods
            if args.is_empty()
                && let Some(enum_names) = self
                    .registry()
                    .class_enum_roles
                    .get(&class_name.resolve())
                    .cloned()
            {
                for enum_name in &enum_names {
                    if let Some(variants) = self.registry().enum_types.get(enum_name).cloned()
                        && variants.iter().any(|(vname, _)| vname == method)
                    {
                        // Get the stored enum value from the instance attribute
                        let map = attributes.as_map();
                        let stored = map.get(enum_name);
                        if let Some(ValueView::Enum { key, .. }) = stored.map(Value::view) {
                            return Ok(Value::truth(key.resolve() == method));
                        }
                        // No enum value set
                        return Ok(Value::FALSE);
                    }
                }
            }
        }

        // For user-defined numeric/real-like objects, delegate unknown methods through
        // their coercion bridge so default Real behavior is available.
        if matches!(target.view(), ValueView::Instance { class_name, .. }
            if (target.does_check("Real") || target.does_check("Numeric"))
                || self.has_user_method(&class_name.resolve(), "Bridge")
                || self.has_user_method(&class_name.resolve(), "Numeric"))
        {
            if matches!(method, "Bridge" | "Real")
                && let Ok(coerced) = self.call_method_with_values(target.clone(), "Numeric", vec![])
                && coerced != target
            {
                return Ok(coerced);
            }
            // When the object has its OWN `Str` method, stringification must not
            // be delegated through the numeric bridge: `.Stringy` (and `.Str`)
            // should use that custom stringification, not the numeric value
            // (e.g. HTTP::Status: `method Numeric { $!code }; method Str
            // { $!title }` → `~$status` / `"$status"` is the title, not the
            // code). A class that `does Real` with no custom `Str` keeps
            // bridging `.Str`/`.gist` to its number (e.g. `class P does Real
            // { method Bridge { $!n.Num } }` → `P.new(n=>7).gist` is "7").
            let has_user_str = matches!(target.view(), ValueView::Instance { class_name, .. }
                if self.has_user_method(&class_name.resolve(), "Str"));
            let is_numeric_coercion = matches!(method, "Numeric" | "Real" | "Bridge");
            let is_own_stringify = has_user_str && matches!(method, "Str" | "Stringy");
            if !is_numeric_coercion
                && !is_own_stringify
                && let Ok(coerced) = self
                    .call_method_with_values(target.clone(), "Numeric", vec![])
                    .or_else(|_| self.call_method_with_values(target.clone(), "Bridge", vec![]))
                && coerced != target
                && let Ok(result) = {
                    let mut delegated_args = Vec::with_capacity(args.len());
                    for arg in &args {
                        let coerced_arg = if matches!(arg.view(), ValueView::Instance { class_name, .. }
                            if self.has_user_method(&class_name.resolve(), "Bridge")
                                || arg.does_check("Real")
                                || arg.does_check("Numeric"))
                        {
                            self.call_method_with_values(arg.clone(), "Numeric", vec![])
                                .or_else(|_| {
                                    self.call_method_with_values(arg.clone(), "Bridge", vec![])
                                })
                                .unwrap_or_else(|_| arg.clone())
                        } else {
                            arg.clone()
                        };
                        delegated_args.push(coerced_arg);
                    }
                    let method_sym = crate::symbol::Symbol::intern(method);
                    if delegated_args.is_empty()
                        && let Some(result) =
                            crate::builtins::native_method_0arg(&coerced, method_sym)
                    {
                        result
                    } else if delegated_args.len() == 1
                        && let Some(result) = crate::builtins::native_method_1arg(
                            &coerced,
                            method_sym,
                            &delegated_args[0],
                        )
                    {
                        result
                    } else if delegated_args.len() == 2
                        && let Some(result) = crate::builtins::native_method_2arg(
                            &coerced,
                            method_sym,
                            &delegated_args[0],
                            &delegated_args[1],
                        )
                    {
                        result
                    } else {
                        self.call_method_with_values(coerced, method, delegated_args)
                    }
                }
            {
                return Ok(result);
            }
        }

        // Package (type object) dispatch -- private method call
        if let ValueView::Package(name) = target.view() {
            let normalized_method: String = method
                .chars()
                .map(|ch| {
                    if ch.is_ascii_alphanumeric() || ch == '_' {
                        ch
                    } else {
                        '_'
                    }
                })
                .collect();
            if name == "Instant"
                && normalized_method == "from_posix"
                && let Some(result) = Self::try_native_builtin_class_method(name, method, &args)
            {
                // Shared with the VM's native class-method fast path.
                return result;
            }
            if name == "Supply" && method == "interval" {
                let seconds = args.first().map_or(1.0, |value| {
                    if let Some(i) = value.as_int() {
                        i as f64
                    } else if let Some(n) = value.as_num() {
                        n
                    } else {
                        value.to_string_value().parse::<f64>().unwrap_or(1.0)
                    }
                });
                let period_secs = if seconds.is_finite() && seconds > 0.0 {
                    seconds
                } else {
                    0.001 // minimum timer resolution
                };
                // Second argument: initial delay before first emission (default 0)
                let initial_delay = args.get(1).map_or(0.0, |value| {
                    if let Some(i) = value.as_int() {
                        i as f64
                    } else if let Some(n) = value.as_num() {
                        n
                    } else {
                        value.to_string_value().parse::<f64>().unwrap_or(0.0)
                    }
                });
                let initial_delay = if initial_delay.is_finite() && initial_delay >= 0.0 {
                    initial_delay
                } else {
                    0.0
                };

                // Check for :scheduler named argument
                let scheduler = Self::named_value(&args, "scheduler");

                if let Some(sched) = scheduler {
                    // Scheduler-driven Supply.interval:
                    // Store scheduler info; the scheduler will drive emissions
                    // when progress-by is called.
                    let mut attrs = HashMap::new();
                    attrs.insert("values".to_string(), Value::array(Vec::new()));
                    attrs.insert("taps".to_string(), Value::array(Vec::new()));
                    attrs.insert("live".to_string(), Value::FALSE);
                    attrs.insert("scheduler".to_string(), sched);
                    attrs.insert("scheduler_interval".to_string(), Value::num(period_secs));
                    attrs.insert("scheduler_delay".to_string(), Value::num(initial_delay));
                    return Ok(Value::make_instance(Symbol::intern("Supply"), attrs));
                }

                let supply_id = super::native_methods::next_supply_id();
                let (tx, rx) = super::native_methods::supply_channel::supply_event_channel();
                if let Ok(mut map) = super::native_methods::supply_channel_map_pub().lock() {
                    map.insert(supply_id, rx);
                }

                // Ticks are driven by the process-wide shared interval timer
                // (one deadline-heap thread), not a sleep-loop thread per
                // interval instance. The entry dies when the receiver drops.
                super::native_methods::interval_timer::register_interval(
                    std::time::Duration::from_secs_f64(period_secs),
                    std::time::Duration::from_secs_f64(initial_delay),
                    tx,
                );

                let mut attrs = HashMap::new();
                attrs.insert("values".to_string(), Value::array(Vec::new()));
                attrs.insert("taps".to_string(), Value::array(Vec::new()));
                attrs.insert("supply_id".to_string(), Value::int(supply_id as i64));
                attrs.insert("live".to_string(), Value::FALSE);
                return Ok(Value::make_instance(Symbol::intern("Supply"), attrs));
            }
            if let Some(private_rest) = method.strip_prefix('!') {
                let caller_class = self
                    .method_class_stack
                    .last()
                    .cloned()
                    .or_else(|| Some(self.current_package().to_string()));
                let (pm_name, resolved) = if let Some((owner_class, pm_name)) =
                    private_rest.split_once("::")
                {
                    let caller_allowed =
                        caller_class.as_deref() == Some(owner_class)
                            || self.registry().class_trusts.get(owner_class).is_some_and(
                                |trusted| {
                                    caller_class
                                        .as_ref()
                                        .is_some_and(|caller| trusted.contains(caller))
                                },
                            );
                    if !caller_allowed {
                        return Err(make_private_permission_error(
                            pm_name,
                            owner_class,
                            caller_class.as_deref().unwrap_or("GLOBAL"),
                        ));
                    }
                    (
                        pm_name,
                        self.resolve_private_method_with_owner(
                            &name.resolve(),
                            owner_class,
                            pm_name,
                            &args,
                        ),
                    )
                } else {
                    (
                        private_rest,
                        self.resolve_private_method_any_owner(&name.resolve(), private_rest, &args),
                    )
                };
                if let Some((resolved_owner, method_def)) = resolved {
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
                            &name.resolve(),
                            caller_class.as_deref().unwrap_or("GLOBAL"),
                        ));
                    }
                    let attrs = AttrMap::new();
                    let (result, _updated) = self.run_resolved_method_compiled_or_treewalk(
                        &name.resolve(),
                        &resolved_owner,
                        pm_name,
                        method_def,
                        attrs,
                        args,
                        Some(target.clone()),
                    )?;
                    return Ok(result);
                }
                return Err(make_method_not_found_error(pm_name, &name.resolve(), true));
            }
            // Package (type object) dispatch -- check user-defined methods
            if self.has_user_method(&name.resolve(), method) {
                let attrs = AttrMap::new();
                let (result, _updated) =
                    self.run_instance_method(&name.resolve(), attrs, method, args, None)?;
                return Ok(result);
            }
        }

        // Value-type dispatch for user-defined methods (e.g. `augment class Array/Hash/List`).
        // Non-instance values still need to find methods declared on their type object.
        if !matches!(
            target.view(),
            ValueView::Instance { .. } | ValueView::Package(_)
        ) {
            let class_name = crate::runtime::utils::value_type_name(&target);
            let dispatch_class = if self.has_user_method(class_name, method) {
                Some(class_name)
            } else if matches!(target.view(), ValueView::Array(_, kind) if !kind.is_itemized())
                && self.has_user_method("Array", method)
            {
                // @-sigiled values are list-like internally, but augmenting Array methods
                // should still apply to them.
                Some("Array")
            } else if matches!(
                target.view(),
                ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
            ) {
                // Callables follow the MRO Sub -> Routine -> Block -> Code -> Callable.
                // A method augmented onto any ancestor (e.g. `augment class Block`)
                // must still be found when invoked on a concrete Sub/Block value.
                ["Routine", "Block", "Code", "Callable"]
                    .into_iter()
                    .find(|ancestor| self.has_user_method(ancestor, method))
            } else {
                None
            };
            if let Some(dispatch_class) = dispatch_class {
                let attrs = AttrMap::new();
                let (result, _updated) = self.run_instance_method(
                    dispatch_class,
                    attrs,
                    method,
                    args,
                    Some(target.clone()),
                )?;
                return Ok(result);
            }
        }

        if let ValueView::Package(type_name) = target.view() {
            match (type_name.resolve().as_str(), method) {
                ("CompUnit", "handle") => {
                    return Ok(Value::make_instance(
                        Symbol::intern("CompUnit::Handle"),
                        HashMap::new(),
                    ));
                }
                ("CompUnit", "globalish-package") | ("CompUnit::Handle", "globalish-package") => {
                    return Ok(Value::package(Symbol::intern("GLOBAL")));
                }
                _ => {}
            }
        }

        // .can for Package values
        if method == "can"
            && !args.is_empty()
            && let ValueView::Package(class_name) = target.view()
        {
            let method_name = args[0].to_string_value();
            if (self.class_has_method(&class_name.resolve(), &method_name)
                || self.has_user_method(&class_name.resolve(), &method_name))
                && let Some(val) = self.classhow_find_method(&target, &method_name)
            {
                return Ok(Value::array(vec![val]));
            }
            return Ok(Value::array(Vec::new()));
        }

        // Wildcard delegation (`handles *`) and FALLBACK method dispatch.
        // Dispatch order: wildcard delegation -> FALLBACK -> built-in fallbacks -> error.
        {
            let fallback_class = match target.view() {
                ValueView::Instance { class_name, .. } => Some(class_name),
                ValueView::Package(name) => Some(name),
                _ => None,
            };
            if let Some(ref class_name) = fallback_class {
                // Try wildcard delegation: forward to delegate attribute's object
                let wildcard_attrs = self.collect_wildcard_handles(&class_name.resolve());
                if let ValueView::Instance { attributes, .. } = target.view() {
                    for attr_var in &wildcard_attrs {
                        // Check for regex delegation pattern: "attr_var:regex:pattern"
                        if let Some(regex_idx) = attr_var.find(":regex:") {
                            let real_attr = &attr_var[..regex_idx];
                            let pattern = &attr_var[regex_idx + ":regex:".len()..];
                            let attr_key =
                                real_attr.trim_start_matches('!').trim_start_matches('.');
                            // Check if method name matches the regex pattern. Clone
                            // the delegate out *in its own statement* so the
                            // attribute read guard is released before the
                            // (re-entrant) method call below — a let-chain
                            // condition keeps its temporaries (the guard) alive for
                            // the whole `if` body, which would deadlock when the
                            // callee writes back to this same instance's cell.
                            let matches = fancy_regex::Regex::new(pattern)
                                .map(|re| re.is_match(method).unwrap_or(false))
                                .unwrap_or(false);
                            let delegate = if matches {
                                attributes.as_map().get(attr_key).cloned()
                            } else {
                                None
                            };
                            if let Some(delegate) = delegate {
                                match self.call_method_with_values(delegate, method, args.clone()) {
                                    Ok(val) => return Ok(val),
                                    Err(_) => continue,
                                }
                            }
                            continue;
                        }
                        let attr_key = attr_var.trim_start_matches('!').trim_start_matches('.');
                        // Clone out before calling (release the read guard first).
                        let delegate = attributes.as_map().get(attr_key).cloned();
                        if let Some(delegate) = delegate {
                            // Try calling the method on the delegate; if it succeeds, return
                            match self.call_method_with_values(delegate, method, args.clone()) {
                                Ok(val) => return Ok(val),
                                Err(_) => continue, // delegate doesn't handle it either
                            }
                        }
                    }
                }

                // Try user-defined FALLBACK method
                if method != "FALLBACK" && self.has_user_method(&class_name.resolve(), "FALLBACK") {
                    let mut fallback_args = vec![Value::str(method.to_string())];
                    fallback_args.extend(args);
                    return self.call_method_with_values(target, "FALLBACK", fallback_args);
                }
            }
        }

        // Class-level attribute access (our $.x / my $.x) — works on both
        // type objects (Foo.bar) and instances ($obj.bar).
        // A per-instance attribute with the same name hides the class-level one.
        {
            let class_name_for_lookup = match target.view() {
                ValueView::Package(name) => Some(name.resolve()),
                ValueView::Instance { class_name, .. } => Some(class_name.resolve()),
                _ => None,
            };
            if let Some(cn) = class_name_for_lookup
                && args.is_empty()
                && self.has_class_level_attr(&cn, method)
                && !self.has_public_accessor(&cn, method)
                && let Some(val) = self.get_class_level_attr(&cn, method)
            {
                return Ok(val);
            }
        }

        if let Some(callable) = self.env.get(&format!("&{}", method)).cloned()
            && matches!(
                callable.view(),
                ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
            )
        {
            return self.call_sub_value(callable, args, true);
        }

        // Fallback methods
        match method {
            "DUMP" if args.is_empty() => match target.view() {
                ValueView::Package(name) => Ok(Value::str(format!("{}()", name))),
                _ => Ok(Value::str(target.to_string_value())),
            },
            "gist" if args.is_empty() => match target.view() {
                ValueView::Package(name) => {
                    if crate::value::is_internal_anon_type_name(&name.resolve()) {
                        return Ok(Value::str_from("()"));
                    }
                    let resolved = name.resolve();
                    let short = resolved.split("::").last().unwrap_or(&resolved);
                    Ok(Value::str(format!("({})", short)))
                }
                _ => Ok(Value::str(target.to_string_value())),
            },
            "WHERE" if args.is_empty() => {
                let type_obj_name = match target.view() {
                    ValueView::Package(name) => Some(name.resolve()),
                    ValueView::Str(name) => Some(name.to_string()),
                    _ => None,
                };
                if let Some(name) = type_obj_name {
                    if !self.registry().roles.contains_key(&name) {
                        return Err(RuntimeError::new(format!(
                            "X::Method::NotFound: Unknown method value dispatch (fallback disabled): {}",
                            method
                        )));
                    }
                    Ok(Value::str(format!("{}|type-object", name)))
                } else {
                    Err(RuntimeError::new(format!(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): {}",
                        method
                    )))
                }
            }
            "raku" | "perl" if args.is_empty() => match target.view() {
                ValueView::Package(name) => Ok(Value::str(name.resolve())),
                ValueView::Junction { kind, values } => {
                    let kind_name = match kind {
                        JunctionKind::Any => "any",
                        JunctionKind::All => "all",
                        JunctionKind::One => "one",
                        JunctionKind::None => "none",
                    };
                    let mut parts = Vec::with_capacity(values.len());
                    for value in values.iter() {
                        if let ValueView::Instance {
                            class_name,
                            attributes,
                            ..
                        } = value.view()
                            && self
                                .resolve_method_with_owner(&class_name.resolve(), "raku", &[])
                                .is_some()
                        {
                            let attrs_map = attributes.to_map();
                            if let Ok((rendered, _)) = self.run_instance_method(
                                &class_name.resolve(),
                                attrs_map,
                                "raku",
                                Vec::new(),
                                None,
                            ) {
                                parts.push(rendered.to_string_value());
                                continue;
                            }
                        }
                        let rendered = self
                            .call_method_with_values(value.clone(), "raku", Vec::new())
                            .unwrap_or_else(|_| Value::str(value.to_string_value()));
                        parts.push(rendered.to_string_value());
                    }
                    Ok(Value::str(format!("{}({})", kind_name, parts.join(", "))))
                }
                _ => Ok(Value::str(target.to_string_value())),
            },
            "name" if args.is_empty() => match target.view() {
                ValueView::Routine { name, .. } => {
                    Ok(Value::str(format_operator_name(&name.resolve())))
                }
                // `.name` on a *type object* whose class declares its own public
                // attribute `$.name` resolves to that accessor, and reading an
                // instance attribute off a type object is an error (raku). Only
                // divert when the class actually has a `name` attribute; otherwise
                // keep the type-name introspection behaviour.
                ValueView::Package(name) if self.has_public_accessor(&name.resolve(), "name") => {
                    Err(RuntimeError::new(format!(
                        "Cannot look up attributes in a {} type object. Did you forget a '.new'?",
                        name.resolve()
                    )))
                }
                ValueView::Package(name) => Ok(Value::str(name.resolve())),
                ValueView::Str(name) => Ok(Value::str_arc(name.clone())),
                ValueView::Sub(data) => Ok(Value::str(format_operator_name(&data.name.resolve()))),
                _ => Ok(Value::NIL),
            },
            "package" if args.is_empty() => match target.view() {
                ValueView::Sub(data) => Ok(Value::package(data.package)),
                ValueView::Routine { package, .. } => Ok(Value::package(package)),
                _ => Ok(Value::NIL),
            },
            "isa" if args.len() == 1 && matches!(target.view(), ValueView::Package(_)) => {
                let pkg_name = match target.view() {
                    ValueView::Package(name) => name.resolve(),
                    _ => unreachable!(),
                };
                let isa_arg = args.first().cloned().unwrap_or(Value::NIL);
                let target_name = match isa_arg.view() {
                    ValueView::Package(name) => name.resolve(),
                    ValueView::Str(name) => name.to_string(),
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    _ => isa_arg.to_string_value(),
                };
                if pkg_name == target_name {
                    return Ok(Value::TRUE);
                }
                if let Some(mut base) = self
                    .registry()
                    .subsets
                    .get(&pkg_name)
                    .map(|s| s.base.clone())
                {
                    loop {
                        if base == target_name {
                            return Ok(Value::TRUE);
                        }
                        let Some(parent_base) =
                            self.registry().subsets.get(&base).map(|s| s.base.clone())
                        else {
                            break;
                        };
                        if parent_base == base {
                            break;
                        }
                        base = parent_base;
                    }
                }
                Ok(Value::truth(
                    self.class_mro(&pkg_name)
                        .contains(&crate::symbol::Symbol::intern(&target_name)),
                ))
            }
            "REPR" if args.is_empty() => {
                // CustomType/CustomTypeInstance report their stored REPR; every
                // other type is P6opaque.
                if let Some(repr) = target.custom_repr() {
                    Ok(Value::str_from(repr))
                } else {
                    Ok(Value::str_from("P6opaque"))
                }
            }
            "Str" | "Stringy" if args.is_empty() => match target.view() {
                ValueView::Package(_) => Ok(Value::str(String::new())),
                ValueView::Instance { class_name, .. } => {
                    // When Stringy is requested but only Str is user-defined (or
                    // vice versa), delegate to the available user method so that
                    // custom stringification works through `~$obj`.
                    let alt = if method == "Stringy" {
                        "Str"
                    } else {
                        "Stringy"
                    };
                    if self.has_user_method(&class_name.resolve(), alt) {
                        self.call_method_with_values(target, alt, vec![])
                    } else {
                        Ok(Value::str(target.to_string_value()))
                    }
                }
                _ => Ok(Value::str(target.to_string_value())),
            },
            "Numeric" | "Real" if args.is_empty() => {
                let num_name = match target.view() {
                    ValueView::Package(name) => Some(name.resolve()),
                    ValueView::Str(name) => Some(name.to_string()),
                    _ => None,
                };
                if let Some(name) = num_name {
                    // A bare type object (no user `.Numeric`/`.Real`, and not one of
                    // the concrete numeric types handled on the fast path) inherits
                    // Mu's coercion: warn "uninitialized ... in numeric context" and
                    // return 0. Roles keep their existing silent 0 for back-compat.
                    if self.registry().roles.contains_key(&name) {
                        Ok(Value::int(0))
                    } else {
                        let msg = format!(
                            "Use of uninitialized value of type {} in numeric context",
                            name
                        );
                        Err(RuntimeError::warn_signal_with_resume(msg, Value::int(0)))
                    }
                } else {
                    Err(RuntimeError::new(format!(
                        "X::Method::NotFound: Unknown method value dispatch (fallback disabled): {}",
                        method
                    )))
                }
            }
            "EVAL" if args.is_empty() => {
                // .EVAL works on strings, Buf, and numeric types
                self.call_function("EVAL", vec![target])
            }
            // Metamodel::*HOW methods
            "new_type" if matches!(target.view(), ValueView::Package(n) if n.resolve().starts_with("Metamodel::")) =>
            {
                // Metamodel::PackageHOW.new_type(name => 'Foo')
                // Returns a type object (Package) with the given name
                // and registers an empty class so .new works on it.
                let name = args
                    .iter()
                    .find_map(|a| {
                        if let ValueView::Pair(k, v) = a.view() {
                            if k == "name" {
                                Some(v.to_string_value())
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    })
                    .unwrap_or_else(|| "Anon".to_string());
                // Register an empty class definition so that .new and other
                // class operations work on this dynamically created type.
                if !self.registry().classes.contains_key(&name) {
                    self.registry_mut()
                        .classes
                        .insert(name.clone(), Default::default());
                }
                Ok(Value::package(Symbol::intern(&name)))
            }
            // Metamodel::Primitives static methods
            _ if matches!(target.view(), ValueView::Package(n) if n == "Metamodel::Primitives") => {
                self.metamodel_primitives_dispatch(method, args)
            }
            _ => {
                // Method calls on callables compose by applying the method to the
                // callable's return value, e.g. `(*-*).abs`.
                if matches!(target.view(), ValueView::Sub(_) | ValueView::WeakSub(_)) {
                    use crate::ast::{Expr, Stmt};
                    use std::sync::atomic::{AtomicU64, Ordering};

                    static COMPOSE_METHOD_ID: AtomicU64 = AtomicU64::new(2_000_000);

                    let callable = match target.view() {
                        ValueView::Sub(data) => Value::sub_value(data.clone()),
                        ValueView::WeakSub(weak) => weak
                            .upgrade()
                            .map(Value::sub_value)
                            .ok_or_else(|| RuntimeError::new("Callable has been freed"))?,
                        _ => Value::NIL,
                    };
                    let params = match callable.view() {
                        ValueView::Sub(data) if !data.params.is_empty() => data.params.clone(),
                        _ => vec!["_".to_string()],
                    };

                    let mut env = crate::env::Env::new();
                    env.insert("__method_compose_target__".to_string(), callable);
                    let call_args = params.iter().cloned().map(Expr::Var).collect();
                    let method_args = args.into_iter().map(Expr::Literal).collect();
                    let body = vec![Stmt::Expr(Expr::MethodCall {
                        target: Box::new(Expr::Call {
                            name: Symbol::intern("__method_compose_target__"),
                            args: call_args,
                        }),
                        name: Symbol::intern(method),
                        args: method_args,
                        modifier: None,
                        quoted: false,
                    })];
                    let id = COMPOSE_METHOD_ID.fetch_add(1, Ordering::Relaxed);
                    return Ok(Value::make_sub_with_id(
                        Symbol::intern(""),
                        Symbol::intern(&format!("<composed-method:{}>", method)),
                        params,
                        Vec::new(),
                        body,
                        false,
                        env,
                        id,
                    ));
                }

                // Before giving up, check if this is a mutating array method
                // and we have a pending Proxy subclass attribute reference.
                if matches!(
                    method,
                    "push" | "pop" | "shift" | "unshift" | "append" | "prepend"
                ) && matches!(target.view(), ValueView::Array(..))
                    && let Some((attrs_ref, attr_name)) = self.pending_proxy_subclass_attr.take()
                {
                    return self.proxy_subclass_array_mutate(&attrs_ref, &attr_name, method, &args);
                }

                // `.wrap` on a Method object obtained from `.^methods(:local)`
                // (a "Method" instance carrying its owning class via the
                // `__mutsu_lookup_*` attributes): register a class-keyed wrap
                // chain, mirroring `.wrap` on a `^lookup`/`^find_method` Sub, so
                // the wrapper takes effect for later dispatch of that method
                // (`advent2011-day14` AOP `compose` wraps every local method).
                if method == "wrap"
                    && let ValueView::Instance {
                        class_name,
                        attributes,
                        ..
                    } = target.view()
                    && class_name == "Method"
                    && let Some(wrapper) = args.first().cloned()
                {
                    let am = attributes.as_map();
                    if let (
                        Some(ValueView::Str(cls)),
                        Some(ValueView::Str(meth)),
                        Some(ValueView::Int(idx)),
                    ) = (
                        am.get("__mutsu_lookup_class").map(Value::view),
                        am.get("__mutsu_lookup_method").map(Value::view),
                        am.get("__mutsu_lookup_candidate_idx").map(Value::view),
                    ) {
                        self.wrap_handle_counter += 1;
                        let handle_id = self.wrap_handle_counter;
                        let key = (cls.to_string(), meth.to_string(), idx as usize);
                        self.method_wrap_chains
                            .entry(key)
                            .or_default()
                            .push((handle_id, wrapper));
                        let mut wh = std::collections::HashMap::new();
                        wh.insert("handle-id".to_string(), Value::int(handle_id as i64));
                        wh.insert("wrapped-sub".to_string(), target.clone());
                        return Ok(Value::make_instance(
                            Symbol::intern("Routine::WrapHandle"),
                            wh,
                        ));
                    }
                }
                // Metamodel method fallback (`.^add_fallback`): before failing,
                // consult any dynamic fallbacks registered for this value's class.
                if let Some(result) = self.try_method_fallback(&target, method, &args) {
                    return result;
                }
                // A user HOW subclass (`class MyHOW is Metamodel::ClassHOW`)
                // inherits the native ClassHOW meta-methods (`methods`, `wrap`
                // sits on Method, `add_method`, ...): when the user class does
                // NOT override the method, dispatch it to the native ClassHOW
                // implementation. The invocant is the meta-object; the native
                // impl operates on its first arg (the type object), so pass the
                // call args straight through (e.g. `self.methods($obj, :local)`
                // in a custom `compose` — `advent2011-day14` AOP).
                if let ValueView::Instance { class_name, .. } = target.view()
                    && Self::is_classhow_method(method)
                    && self.is_metamodel_how_class(&class_name.resolve())
                {
                    return self.dispatch_classhow_method(method, args.to_vec());
                }
                // A grammar token/rule called as an instance method
                // (`G.new.tok`, URI's `IETF::RFC_Grammar::URI.new()
                // .TOP-non-empty`): rakudo runs the token against an
                // empty-string cursor and returns the resulting Match cursor —
                // falsy when the token cannot match the empty string. Run
                // `subparse("", rule => method)`; a failed subparse yields a
                // falsy value that smartmatches False like a failed cursor.
                if args.is_empty()
                    && let ValueView::Instance { class_name, .. } = target.view()
                {
                    let cn = class_name.resolve();
                    let qualified = format!("{}::{}", cn, method);
                    if self
                        .resolve_token_defs(&qualified)
                        .map(|d| !d.is_empty())
                        .unwrap_or(false)
                    {
                        return self.dispatch_package_parse(
                            &cn,
                            "subparse",
                            &[
                                Value::str_from(""),
                                Value::pair("rule".to_string(), Value::str(method.to_string())),
                            ],
                        );
                    }
                }
                let type_name = match target.view() {
                    ValueView::Instance { class_name, .. } => class_name.resolve(),
                    ValueView::Package(name) => name.resolve(),
                    _ => crate::runtime::utils::value_type_name(&target).to_string(),
                };
                Err(make_method_not_found_error(method, &type_name, false))
            }
        }
    }

    /// Consult metamodel method fallbacks registered via `.^add_fallback` for a
    /// value whose method lookup just failed. Returns `Some(result)` when a
    /// fallback's condition matched (and its calculated method body was invoked),
    /// or `None` when no fallback applies (the caller then raises NotFound).
    fn try_method_fallback(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if self.method_fallbacks.is_empty() {
            return None;
        }
        let type_name = match target.view() {
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            ValueView::Package(name) => name.resolve(),
            _ => crate::runtime::utils::value_type_name(target).to_string(),
        };
        // Consult the class itself first, then its ancestors (MRO).
        let mut classes = vec![type_name.clone()];
        classes.extend(
            self.class_mro(&type_name)
                .iter()
                .map(|s| s.resolve())
                .filter(|c| c != &type_name),
        );
        let name_val = Value::str(method.to_string());
        for cn in classes {
            let Some(fallbacks) = self.method_fallbacks.get(&cn).cloned() else {
                continue;
            };
            for (condition, calculator) in fallbacks {
                let cond = match self.call_sub_value(
                    condition,
                    vec![target.clone(), name_val.clone()],
                    false,
                ) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                if !cond.truthy() {
                    continue;
                }
                // The calculator returns the method body (a Callable) for this
                // name; invoke it with the invocant (and any call args).
                let method_code = match self.call_sub_value(
                    calculator,
                    vec![target.clone(), name_val.clone()],
                    false,
                ) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
                let mut call_args = vec![target.clone()];
                call_args.extend_from_slice(args);
                return Some(self.call_sub_value(method_code, call_args, false));
            }
        }
        None
    }

    /// Mutate an array attribute in a Proxy subclass's shared storage.
    pub(crate) fn proxy_subclass_array_mutate(
        &mut self,
        attrs_ref: &Arc<std::sync::Mutex<HashMap<String, Value>>>,
        attr_name: &str,
        method: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut attrs = attrs_ref.lock().unwrap();
        let arr = attrs
            .entry(attr_name.to_string())
            .or_insert_with(|| Value::real_array(Vec::new()));
        if let Some(result) = arr.with_array_mut(|items, _| -> Result<Value, RuntimeError> {
            let items = crate::value::gc_data_mut(items);
            match method {
                "push" => {
                    for arg in args {
                        items.push(arg.clone());
                    }
                }
                "pop" => {
                    if items.is_empty() {
                        return Ok(make_empty_array_failure("pop"));
                    }
                    return Ok(items.pop().unwrap_or(Value::NIL));
                }
                "shift" => {
                    return Ok(if items.is_empty() {
                        make_empty_array_failure("shift")
                    } else {
                        items.remove(0)
                    });
                }
                "unshift" => {
                    for (i, arg) in args.iter().enumerate() {
                        items.insert(i, arg.clone());
                    }
                }
                "append" => {
                    items.extend(flatten_append_args(args.to_vec()));
                }
                "prepend" => {
                    let mut new_items: Vec<Value> = Vec::new();
                    for arg in args {
                        match arg.view() {
                            ValueView::Array(vals, _) => new_items.extend(vals.iter().cloned()),
                            _ => new_items.push(arg.clone()),
                        }
                    }
                    new_items.append(items);
                    *items = crate::value::ArrayData::new(new_items);
                }
                _ => {}
            }
            Ok(Value::real_array(items.clone().items))
        }) {
            result
        } else {
            Err(RuntimeError::new(format!(
                "Cannot call '{}' on non-Array attribute",
                method
            )))
        }
    }

    pub(super) fn dispatch_are(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let values = Self::value_to_list(&target);
        match args {
            [] => {
                if values.is_empty() {
                    return Ok(Value::NIL);
                }
                let candidates = self.are_candidate_type_names(&values);
                for candidate in candidates {
                    if values
                        .iter()
                        .all(|value| self.are_value_matches_type(value, &candidate))
                    {
                        return Ok(Value::package(Symbol::intern(&candidate)));
                    }
                }
                Ok(Value::package(Symbol::intern("Any")))
            }
            [expected] => {
                let expected_type = self.are_expected_type_name(expected);
                for (idx, value) in values.iter().enumerate() {
                    if !self.are_value_matches_type(value, &expected_type) {
                        let actual = Self::are_actual_type_name(value);
                        let message = if values.len() == 1 {
                            format!("Expected '{}' but got '{}'", expected_type, actual)
                        } else {
                            format!(
                                "Expected '{}' but got '{}' in element {}",
                                expected_type, actual, idx
                            )
                        };
                        let mut ex_attrs = std::collections::HashMap::new();
                        ex_attrs.insert("message".to_string(), Value::str(message));
                        let ex = Value::make_instance(Symbol::intern("X::AdHoc"), ex_attrs);
                        let mut failure_attrs = std::collections::HashMap::new();
                        failure_attrs.insert("exception".to_string(), ex);
                        return Ok(Value::make_instance(
                            Symbol::intern("Failure"),
                            failure_attrs,
                        ));
                    }
                }
                Ok(Value::TRUE)
            }
            _ => Err(RuntimeError::new(
                "Method 'are' accepts zero or one argument",
            )),
        }
    }

    fn are_candidate_type_names(&mut self, values: &[Value]) -> Vec<String> {
        let mut names = Vec::new();
        for value in values {
            for candidate in self.are_specific_candidate_type_names(value) {
                if !names.contains(&candidate) {
                    names.push(candidate);
                }
            }
        }
        for fallback in ["Dateish", "Real", "Numeric", "Cool", "Any"] {
            let fallback = fallback.to_string();
            if !names.contains(&fallback) {
                names.push(fallback);
            }
        }
        names
    }

    fn are_specific_candidate_type_names(&mut self, value: &Value) -> Vec<String> {
        match value.view() {
            ValueView::Package(name) => vec![name.resolve()],
            ValueView::Instance { class_name, .. } => self
                .class_mro(&class_name.resolve())
                .iter()
                .map(|s| s.resolve())
                .collect(),
            _ => vec![crate::runtime::utils::value_type_name(value).to_string()],
        }
    }

    fn are_expected_type_name(&self, value: &Value) -> String {
        match value.view() {
            ValueView::Package(name) => name.resolve(),
            ValueView::Str(name) => name.to_string(),
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            _ => value.to_string_value(),
        }
    }

    fn are_value_matches_type(&mut self, value: &Value, expected_type: &str) -> bool {
        if expected_type == "Cool" {
            // `Cool` in are() should accept list/type-object values except clearly non-Cool ones.
            if let ValueView::Instance { class_name, .. } = value.view() {
                let cls = class_name.resolve();
                if cls == "Date" || cls == "DateTime" || cls == "Mu" {
                    return false;
                }
            }
            if let ValueView::Package(name) = value.view() {
                let cls = name.resolve();
                if cls == "Date" || cls == "DateTime" || cls == "Mu" {
                    return false;
                }
            }
            if matches!(
                value.view(),
                ValueView::Array(_, _) | ValueView::Seq(_) | ValueView::Slip(_)
            ) {
                return true;
            }
        }
        self.type_matches_value(expected_type, value)
    }

    fn are_actual_type_name(value: &Value) -> String {
        match value.view() {
            ValueView::Package(name) => name.resolve(),
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            _ => crate::runtime::utils::value_type_name(value).to_string(),
        }
    }

    /// Collect public attribute representations for `.raku` output.
    /// Returns a list of `"name => value.raku"` strings for public attributes.
    fn collect_public_raku_attrs(&mut self, class_name: &str, attributes: &AttrMap) -> Vec<String> {
        let class_attrs = self.collect_class_attributes(class_name);
        let mut parts = Vec::new();
        for (attr_name, is_public, _default, _is_rw, _is_required, _sigil, _where) in &class_attrs {
            if !is_public {
                continue;
            }
            if let Some(val) = attributes.get(attr_name) {
                let rendered = self
                    .call_method_with_values(val.clone(), "raku", vec![])
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|_| val.to_string_value());
                parts.push(format!("{} => {}", attr_name, rendered));
            }
        }
        parts
    }
}

/// Format an operator name for display.
///
/// Raku uses different delimiters depending on the operator symbol:
/// - If the symbol contains characters special inside `\u{ab}\u{bb}` quoting
///   (`$`, `@`, `%`, `&`, `{`, `\`, `\u{ab}`, `\u{bb}`), use `<>` delimiters
///   and backslash-escape any `<` or `>` in the symbol.
/// - Otherwise, if the symbol contains `<` or `>`, use `\u{ab}\u{bb}` delimiters.
/// - Otherwise, use `<>` delimiters as-is.
fn format_operator_name(name: &str) -> String {
    // Check if this is a categorical name like "infix:<...>", "prefix:<...>", etc.
    let Some(colon_pos) = name.find(":<") else {
        return name.to_string();
    };
    let category = &name[..colon_pos];
    let rest = &name[colon_pos + 2..];
    // The rest should end with '>'
    let Some(symbol) = rest.strip_suffix('>') else {
        return name.to_string();
    };

    // Characters that are special inside «» quoting and force <> delimiters
    let needs_angle_delims = symbol
        .chars()
        .any(|c| matches!(c, '$' | '@' | '%' | '&' | '{' | '\\' | '\u{ab}' | '\u{bb}'));
    let has_angle = symbol.contains('<') || symbol.contains('>');

    if needs_angle_delims {
        // Must use <> delimiters; backslash-escape < and > in the symbol
        if has_angle {
            let mut escaped = String::with_capacity(symbol.len() + 4);
            for ch in symbol.chars() {
                if ch == '<' || ch == '>' {
                    escaped.push('\\');
                }
                escaped.push(ch);
            }
            format!("{}:<{}>", category, escaped)
        } else {
            // No angle brackets to escape, just use <> as-is
            name.to_string()
        }
    } else if has_angle {
        // Use french quote delimiters
        format!("{}:\u{ab}{}\u{bb}", category, symbol)
    } else {
        name.to_string()
    }
}
