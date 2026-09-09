use super::*;
use crate::runtime::MultiCompiledKey;

impl Interpreter {
    pub(super) fn find_compiled_function<'a>(
        &mut self,
        compiled_fns: &'a CompiledFns,
        name: &str,
        args: &[Value],
    ) -> Option<&'a Arc<CompiledFunction>> {
        self.find_compiled_function_memo(compiled_fns, name, args, &mut None)
    }

    /// [`Self::find_compiled_function`], handing back the multi resolution it
    /// performed on the way.
    ///
    /// `dispatch_func_call_inner` needs the same winner immediately afterwards
    /// when this returns `None` for a `multi` name, and used to resolve it a
    /// second time — rebuilding `multi_arg_type_keys` (a `Symbol` per argument
    /// property) for an answer already in hand. `memo` is filled only when the
    /// resolution came from the type-keyed path, which by construction cannot
    /// depend on anything about the call that changed in between; see
    /// [`Interpreter::resolve_function_multi_cached_keyed`] (#7573).
    pub(super) fn find_compiled_function_memo<'a>(
        &mut self,
        compiled_fns: &'a CompiledFns,
        name: &str,
        args: &[Value],
        memo: &mut Option<Arc<crate::ast::FunctionDef>>,
    ) -> Option<&'a Arc<CompiledFunction>> {
        // Pseudo-package names need interpreter's special resolution
        if self.is_interpreter_handled_function(name) {
            return None;
        }
        self.find_compiled_function_inner(compiled_fns, name, args, memo)
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
        memo: &mut Option<Arc<crate::ast::FunctionDef>>,
    ) -> Option<&'a Arc<CompiledFunction>> {
        let arity = args.len();
        let name_sym = Symbol::intern(name);
        // ONE type signature, shared by both resolution-cache keys and by the
        // compiled-key probes further down. The probes used to build their own
        // copy, so every call allocated a `String` per argument twice over; the
        // names are `&'static str` to begin with, so neither copy needs to own
        // one at all. `find_compiled_function_inner` was 40% of a multi call's
        // retired instructions (#7573).
        let type_sig: Vec<&'static str> = args.iter().map(runtime::value_type_name).collect();
        // Check the resolution cache first to avoid expensive resolve_function_with_types.
        // Skip cache for multi functions since subset type dispatch depends on values.
        let is_multi = self.has_multi_candidates_cached(name);
        if self.fn_resolve_cache_gen != self.fn_resolve_gen {
            self.fn_resolve_cache.clear();
            self.multi_compiled_key_cache.clear();
            self.fn_resolve_cache_gen = self.fn_resolve_gen;
        }
        let cache_key = (!is_multi).then(|| {
            (
                name_sym,
                self.current_package_sym(),
                arity,
                type_sig.clone(),
            )
        });
        if let Some(cache_key) = &cache_key
            && let Some((cached_key, cached_fp, _)) = self.fn_resolve_cache.get(cache_key)
            && let Some(cf) = compiled_fns.get(cached_key)
            && cf.fingerprint == *cached_fp
        {
            return Some(cf);
        }
        // Same sound multi-resolution cache the interpreter's dispatch uses:
        // `cache_key` above deliberately withholds the *compiled-key* cache from
        // a multi name (its winner depends on argument types), but the
        // resolution itself is still cacheable whenever the candidates are
        // type+arity deterministic, and for a `multi` this call was otherwise a
        // full candidate walk on every single dispatch.
        let (resolved_def, type_keyed) =
            loan_env!(self, resolve_function_multi_cached_keyed(name, args));
        if type_keyed {
            memo.clone_from(&resolved_def);
        }
        let expected_fingerprint = resolved_def.as_ref().map(|def| def.body_fingerprint());
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
        // Positional arity (the probe chain builds keys from it as well as from
        // the raw arity, and it is part of the multi memo key below).
        let pos_arity = args.iter().filter(|a| !a.is_string_pair_value()).count();
        // The multi memo (#7573). A `multi` is excluded from `fn_resolve_cache`
        // above, so without this every call re-ran the whole `format!` probe
        // chain below — ~15 heap-allocated key strings and as many
        // `Symbol::lookup`s — even though for the common shape (the winning
        // candidate lives outside the caller's `compiled_fns`) they all fail and
        // the answer is a constant `None`. Keyed by the resolved winner's
        // fingerprint plus everything else the chain reads, so a hit reproduces
        // the probe result exactly; a positive hit is still re-validated against
        // the table and falls through to a fresh probe if it has gone stale.
        let multi_memo_key = (is_multi && !name.contains("::")).then(|| MultiCompiledKey {
            name: name_sym,
            pkg: self.current_package_sym(),
            lexical_pkg: self
                .routine_stack()
                .last()
                .and_then(|frame| frame.lexical_package),
            arity,
            pos_arity,
            fingerprint: expected_fingerprint,
            type_sig: type_sig.clone(),
        });
        if let Some(memo_key) = &multi_memo_key
            && let Some(hit) = self.multi_compiled_key_cache.get(memo_key).copied()
        {
            match hit {
                None => return None,
                Some(key) => {
                    if let Some(cf) = compiled_fns
                        .get(&key)
                        .filter(|cf| cf.fingerprint == expected_fingerprint)
                    {
                        return Some(cf);
                    }
                    // Stale entry: re-probe below rather than answering `None`.
                }
            }
        }
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
            // Innermost package first, then each enclosing package, then GLOBAL:
            // a method of `NL::Searcher` calling a bare name must reach `NL`'s
            // compiled routine (see `bare_name_packages`). The GLOBAL fallback
            // below stays as it was — the walk only fills in the packages
            // between the current one and GLOBAL, which used to be skipped.
            found_key = self.bare_name_packages().iter().find_map(|p| {
                probe(&format!("{}::{}/{}:{}", p, name, arity, type_sig.join(",")))
                    .or_else(|| {
                        probe(&format!(
                            "{}::{}/{}#{:x}",
                            p, name, arity, expected_fingerprint
                        ))
                    })
                    .or_else(|| probe(&format!("{}::{}/{}", p, name, arity)))
            });
            if found_key.is_none() {
                // `key_simple` (`Pkg::name`) and the positional-only-arity key are
                // probed but their match is intentionally *not* kept here: the
                // original control flow gates the global fallback on their result
                // and then discards it (the `else { found_key = None }` below).
                // Behaviour-preserving — the def-arity fallback re-resolves.
                let simple_or_pos = probe(&format!("{}::{}", pkg, name)).or_else(|| {
                    if pos_arity != arity {
                        probe(&format!(
                            "{}::{}/{}#{:x}",
                            pkg, name, pos_arity, expected_fingerprint
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
                        if pos_arity != arity {
                            probe(&format!(
                                "GLOBAL::{}/{}#{:x}",
                                name, pos_arity, expected_fingerprint
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
        if let Some(memo_key) = multi_memo_key {
            self.multi_compiled_key_cache.insert(memo_key, found_key);
        }
        if let Some(key) = found_key {
            // Cache the resolution result for future lookups
            let cached_pkg = resolved_def
                .map(|def| def.package.resolve())
                .unwrap_or_else(|| self.current_package().to_string());
            if let Some(cache_key) = cache_key {
                self.fn_resolve_cache
                    .insert(cache_key, (key, expected_fingerprint, cached_pkg));
            }
            compiled_fns.get(&key)
        } else {
            None
        }
    }
}
