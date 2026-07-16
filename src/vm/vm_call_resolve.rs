use super::*;

impl Interpreter {
    pub(super) fn find_compiled_function<'a>(
        &mut self,
        compiled_fns: &'a CompiledFns,
        name: &str,
        args: &[Value],
    ) -> Option<&'a CompiledFunction> {
        // Pseudo-package names need interpreter's special resolution
        if self.is_interpreter_handled_function(name) {
            return None;
        }
        self.find_compiled_function_inner(compiled_fns, name, args)
    }

    /// Get the cached package for a function, if available.
    pub(super) fn cached_fn_package(&self, name: &str, arity: usize) -> Option<String> {
        // Don't attempt type-based cache lookup for package; fall through to full resolution.
        // The cache key includes type signature which we don't have here.
        let _ = (name, arity);
        None
    }

    fn find_compiled_function_inner<'a>(
        &mut self,
        compiled_fns: &'a CompiledFns,
        name: &str,
        args: &[Value],
    ) -> Option<&'a CompiledFunction> {
        let arity = args.len();
        let name_sym = Symbol::intern(name);
        // Build a type signature for cache key to handle multi dispatch correctly
        let type_sig: Vec<String> = args
            .iter()
            .map(|v| runtime::value_type_name(v).to_string())
            .collect();
        let cache_key = (name_sym, arity, type_sig.clone());
        // Check the resolution cache first to avoid expensive resolve_function_with_types.
        // Skip cache for multi functions since subset type dispatch depends on values.
        let use_cache = !self.has_multi_candidates_cached(name);
        if use_cache && self.fn_resolve_cache_gen == self.fn_resolve_gen {
            if let Some((cached_key, cached_fp, _)) = self.fn_resolve_cache.get(&cache_key)
                && let Some(cf) = compiled_fns.get(cached_key)
                && cf.fingerprint == *cached_fp
            {
                return Some(cf);
            }
        } else if self.fn_resolve_cache_gen != self.fn_resolve_gen {
            self.fn_resolve_cache.clear();
            self.fn_resolve_cache_gen = self.fn_resolve_gen;
        }
        let resolved_def = loan_env!(self, resolve_function_with_types(name, args));
        let expected_fingerprint = resolved_def.as_ref().map(|def| {
            crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body)
        });
        // If runtime resolution fails, avoid reusing stale compiled cache entries.
        // This can happen across repeated EVAL calls that redefine the same routine name.
        let expected_fingerprint = expected_fingerprint?;
        let def_arity = resolved_def
            .as_ref()
            .map(|def| {
                def.param_defs
                    .iter()
                    .filter(|pd| !pd.named && !pd.slurpy)
                    .count()
            })
            .unwrap_or(arity);
        let matches_resolved = |cf: &CompiledFunction| cf.fingerprint == expected_fingerprint;
        // Probe a candidate key string. The map is keyed by `Symbol`; every real
        // key was interned at compile time, so `Symbol::lookup` (no interning)
        // finds it, and a candidate that turns out not to exist never grows the
        // global symbol table. Returns the matched key's `Symbol`.
        let probe = |key: &str| -> Option<Symbol> {
            let sym = Symbol::lookup(key)?;
            compiled_fns
                .get(&sym)
                .filter(|cf| matches_resolved(cf))
                .map(|_| sym)
        };
        let pkg = self.current_package();
        let type_sig: Vec<String> = args
            .iter()
            .map(|v| runtime::value_type_name(v).to_string())
            .collect();
        // Try all key patterns and remember which one matched for caching
        let mut found_key: Option<Symbol>;
        if name.contains("::") {
            found_key = probe(&format!("{name}/{arity}:{}", type_sig.join(",")))
                .or_else(|| probe(&format!("{name}/{}#{:x}", arity, expected_fingerprint)))
                .or_else(|| probe(&format!("{name}/{arity}")))
                .or_else(|| probe(name));
            // If not found directly, try qualifying with the current package
            // when the prefix package is visible in the current scope.
            if found_key.is_none() && pkg != "GLOBAL" {
                let prefix_visible = if let Some((pkg_prefix, _)) = name.rsplit_once("::") {
                    self.env().get(pkg_prefix).is_some()
                        || self
                            .env()
                            .get(&format!("{}::{}", pkg, pkg_prefix))
                            .is_some()
                } else {
                    false
                };
                if prefix_visible {
                    let qname = format!("{}::{}", pkg, name);
                    found_key = probe(&format!("{qname}/{arity}:{}", type_sig.join(",")))
                        .or_else(|| probe(&format!("{qname}/{}#{:x}", arity, expected_fingerprint)))
                        .or_else(|| probe(&format!("{qname}/{arity}")))
                        .or_else(|| probe(&qname));
                }
            }
        } else {
            let pos_arity = || {
                args.iter()
                    .filter(|a| !matches!(a.view(), ValueView::Pair(..)))
                    .count()
            };
            found_key = probe(&format!("{}::{}/{}:{}", pkg, name, arity, type_sig.join(",")))
                .or_else(|| {
                    probe(&format!(
                        "{}::{}/{}#{:x}",
                        pkg, name, arity, expected_fingerprint
                    ))
                })
                .or_else(|| probe(&format!("{}::{}/{}", pkg, name, arity)));
            if found_key.is_none() {
                // `key_simple` (`Pkg::name`) and the positional-only-arity key are
                // probed but their match is intentionally *not* kept here: the
                // original control flow gates the global fallback on their result
                // and then discards it (the `else { found_key = None }` below).
                // Behaviour-preserving — the def-arity fallback re-resolves.
                let simple_or_pos = probe(&format!("{}::{}", pkg, name)).or_else(|| {
                    let pos = pos_arity();
                    if pos != arity {
                        probe(&format!(
                            "{}::{}/{}#{:x}",
                            pkg, name, pos, expected_fingerprint
                        ))
                    } else {
                        None
                    }
                });
                found_key = if simple_or_pos.is_none() && pkg != "GLOBAL" {
                    probe(&format!(
                        "GLOBAL::{}/{}#{:x}",
                        name, arity, expected_fingerprint
                    ))
                    .or_else(|| probe(&format!("GLOBAL::{}", name)))
                    .or_else(|| {
                        // Try with positional-only arity (excluding Pair named args)
                        let pos = pos_arity();
                        if pos != arity {
                            probe(&format!(
                                "GLOBAL::{}/{}#{:x}",
                                name, pos, expected_fingerprint
                            ))
                        } else {
                            None
                        }
                    })
                } else {
                    None
                };
            }
        }
        // Fallback: when call arity differs from definition arity (e.g. optional
        // params), try the definition's param count to find the compiled function.
        if found_key.is_none() && def_arity != arity {
            found_key = probe(&format!(
                "{}::{}/{}#{:x}",
                pkg, name, def_arity, expected_fingerprint
            ))
            .or_else(|| {
                if pkg != "GLOBAL" {
                    probe(&format!(
                        "GLOBAL::{}/{}#{:x}",
                        name, def_arity, expected_fingerprint
                    ))
                } else {
                    None
                }
            });
        }
        if let Some(key) = found_key {
            // Cache the resolution result for future lookups
            let cached_pkg = resolved_def
                .map(|def| def.package.resolve())
                .unwrap_or_else(|| self.current_package().to_string());
            if use_cache {
                self.fn_resolve_cache
                    .insert(cache_key, (key, expected_fingerprint, cached_pkg));
            }
            compiled_fns.get(&key)
        } else {
            None
        }
    }
}
