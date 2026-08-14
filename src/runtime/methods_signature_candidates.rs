//! Signature/candidate introspection: named-param keys, call-arg matching,
//! candidate routine lookup, and arity/count Value builders.
use super::*;
use crate::value::signature::{
    SubSignatureKey, cache_sub_signature, cached_sub_signature, make_signature_value_with_owner,
    param_defs_to_sig_info,
};

impl Interpreter {
    pub(super) fn collect_named_param_keys(
        param_defs: &[ParamDef],
        out: &mut std::collections::HashSet<String>,
    ) {
        for pd in param_defs {
            if pd.named {
                if pd.name == "__subsig__" {
                    if let Some(key) = &pd.type_constraint {
                        out.insert(key.clone());
                    }
                } else {
                    out.insert(pd.name.clone());
                }
            }
            if let Some(sub) = &pd.sub_signature {
                Self::collect_named_param_keys(sub, out);
            }
        }
    }

    pub(super) fn has_named_slurpy_param(param_defs: &[ParamDef]) -> bool {
        for pd in param_defs {
            if pd.slurpy && pd.name.starts_with('%') {
                return true;
            }
            if let Some(sub) = &pd.sub_signature
                && Self::has_named_slurpy_param(sub)
            {
                return true;
            }
        }
        false
    }

    pub(super) fn capture_to_call_args(value: &Value) -> Vec<Value> {
        match value.view() {
            ValueView::Capture { positional, named } => {
                let mut args = positional.to_vec();
                for (k, v) in named.iter() {
                    args.push(Value::pair(k.clone(), v.clone()));
                }
                args
            }
            _ => vec![value.clone()],
        }
    }

    pub(super) fn varref_parts(value: &Value) -> Option<(String, Value)> {
        let (name, inner, _) = value.as_varref()?;
        Some((name.resolve(), inner.clone()))
    }

    pub(super) fn var_target_from_meta_value(value: &Value) -> Option<String> {
        match value.view() {
            ValueView::Mixin(inner, _) => Self::var_target_from_meta_value(inner),
            ValueView::Instance { attributes, .. } => {
                match attributes
                    .as_map()
                    .get("__mutsu_var_target")
                    .map(Value::view)
                {
                    Some(ValueView::Str(name)) => Some(name.to_string()),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub(super) fn candidate_matches_call_args(
        &mut self,
        candidate: &Value,
        args: &[Value],
    ) -> bool {
        match candidate.view() {
            ValueView::Sub(data) => {
                if data.empty_sig && !args.is_empty() {
                    return false;
                }
                if data.param_defs.is_empty() && !data.params.is_empty() {
                    if args.iter().any(|arg| {
                        matches!(
                            arg.view(),
                            ValueView::Pair(key, _) if key != "__mutsu_test_callsite_line"
                        )
                    }) {
                        return false;
                    }
                    let positional = args
                        .iter()
                        .filter(|arg| {
                            !matches!(arg.view(), ValueView::Pair(..) | ValueView::ValuePair(..))
                        })
                        .count();
                    return positional == data.params.len();
                }
                self.method_args_match(args, &data.param_defs)
            }
            ValueView::WeakSub(weak) => weak
                .upgrade()
                .is_some_and(|strong| self.method_args_match(args, &strong.param_defs)),
            ValueView::Routine { name, .. } => self
                .resolve_function_with_types(&name.resolve(), args)
                .is_some(),
            _ => false,
        }
    }

    pub(super) fn routine_candidate_subs(&self, package: &str, name: &str) -> Vec<Value> {
        let exact_local = format!("{package}::{name}");
        let exact_global = format!("GLOBAL::{name}");
        let prefix_local = format!("{package}::{name}/");
        let prefix_global = format!("GLOBAL::{name}/");
        let mut candidates = Vec::new();
        let registry = self.registry();
        for (key, def) in &registry.functions {
            let key_s = key.resolve();
            if key_s == exact_local
                || key_s == exact_global
                || key_s.starts_with(&prefix_local)
                || key_s.starts_with(&prefix_global)
            {
                candidates.push(def);
            }
        }
        // Rakudo returns `.candidates` in DECLARATION order. Each candidate is
        // registered TWICE — once by the forward-declaration/hoist pre-pass,
        // once by the in-sequence pass that runs when execution reaches the
        // statement — and the second registration cannot reuse the hoisted
        // registry key (it is keyed by mangled type signature, e.g.
        // `GLOBAL::mm/1:Int`, which the hoist pass already occupied), so it
        // falls back to a `__m{N}`-suffixed key. That leaves TWO registry rows
        // per candidate with the SAME body (`body_fingerprint`) but DIFFERENT
        // `decl_order` stamps. The scan above visits the registry's `HashMap`
        // in bucket order, which is arbitrary and unstable against unrelated
        // statements elsewhere in the file — so naively keeping "whichever row
        // is seen first" per body fingerprint reproduced that instability.
        //
        // Fix: sort ALL rows (both copies of every candidate) by `decl_order`
        // first, then dedupe by body fingerprint keeping the smallest
        // `decl_order` — always the hoist-pass row, since hoisting walks the
        // block's statements top-to-bottom (`Compiler::hoist_sub_decls`) and so
        // stamps candidates in true declaration order, chronologically before
        // any in-sequence stamp. This mirrors the established `decl_order`
        // min-per-key dedup pattern already used for token/grammar proto
        // candidates (`token_key_decl_order`, `sort_sym_keys_by_decl_order` in
        // `resolution.rs`). See
        // todo/tickets/multi-candidates-declaration-order.md.
        candidates.sort_by_key(|def| def.decl_order);
        let mut seen = std::collections::HashSet::new();
        let mut defs = Vec::new();
        for def in candidates {
            let fp = def.body_fingerprint();
            if seen.insert(fp) {
                defs.push(def);
            }
        }
        defs.into_iter()
            .enumerate()
            .map(|(multi_idx, def)| {
                let mut env = self.env.clone();
                // Store the multi index for doc comment lookup
                env.insert(
                    "__mutsu_multi_index".to_string(),
                    Value::int(multi_idx as i64),
                );
                Value::make_sub_for_routine(
                    def.package,
                    def.name,
                    def.params.clone(),
                    def.param_defs.clone(),
                    def.body.clone(),
                    def.is_rw,
                    env,
                    def.compiled.clone(),
                )
            })
            .collect()
    }

    /// The stable identity `sub_signature_value` caches a materialized
    /// `Signature` Value under. `SubData::id` is NOT stable for this purpose:
    /// a bareword lookup like `&f` (or `.candidates`, or a method MRO lookup)
    /// builds a brand new `SubData` -- a fresh id -- on every evaluation, even
    /// for the same declared sub/candidate (verified via `.WHERE` returning a
    /// different address each time). What IS stable across those rebuilds is
    /// the `Arc<CompiledFunction>` / `Arc<CompiledCode>` such a rebuild clones
    /// from the registry's own `FunctionDef` (an `Arc::clone`, so the pointee
    /// is the exact same allocation) -- and, crucially, that Arc is per
    /// DECLARATION, so it also distinguishes between different `multi`
    /// candidates that happen to share a name (a name-only key does not: it
    /// collapsed every candidate's signature onto whichever was materialized
    /// first). Only a callable with neither (e.g. an `.assuming()` wrapper,
    /// which intentionally builds a fresh identity per call) falls back to
    /// its own `id`, which IS stable for repeated reads of that one wrapper
    /// value.
    fn sub_signature_cache_key(data: &crate::value::SubData) -> SubSignatureKey {
        if let Some(cr) = &data.compiled_routine {
            SubSignatureKey::from_routine(cr.clone())
        } else if let Some(cc) = &data.compiled_code {
            SubSignatureKey::from_code(cc.clone())
        } else {
            SubSignatureKey::from_id(data.id)
        }
    }

    pub(super) fn sub_signature_value(&self, data: &crate::value::SubData) -> Value {
        // `.assuming(...)` clones the primed sub's `SubData` verbatim --
        // `id`, `compiled_routine`, `compiled_code` all unchanged -- and only
        // mutates `assumed_positional`/`assumed_named` on the clone (see the
        // `"assuming"` arm above). So two differently-primed wrappers of the
        // SAME declaration share every field `sub_signature_cache_key` reads,
        // and would collide on one cache entry despite having different
        // effective signatures. Bypass the cache for a primed sub entirely;
        // it is a one-off wrapper value, not something repeated `.signature`
        // reads on the same identity are expected to return a stable object
        // for.
        let is_primed = !data.assumed_positional.is_empty() || !data.assumed_named.is_empty();
        let cache_key = (!is_primed).then(|| Self::sub_signature_cache_key(data));
        if let Some(key) = &cache_key
            && let Some(cached) = cached_sub_signature(key)
        {
            return cached;
        }
        let param_defs =
            Self::assumed_signature_param_defs(data, &data.assumed_positional, &data.assumed_named)
                .unwrap_or_else(|| {
                    if !data.params.is_empty() {
                        data.params
                            .iter()
                            .map(|name| ParamDef {
                                name: name.clone(),
                                default: None,
                                multi_invocant: true,
                                required: false,
                                named: false,
                                slurpy: false,
                                double_slurpy: false,
                                onearg: false,
                                sigilless: false,
                                type_constraint: None,
                                literal_value: None,
                                sub_signature: None,
                                where_constraint: None,
                                traits: Vec::new(),
                                optional_marker: false,
                                outer_sub_signature: None,
                                code_signature: None,
                                is_invocant: false,
                                shape_constraints: None,
                                block_param: false,
                            })
                            .collect()
                    } else {
                        let (use_positional, use_named) =
                            crate::method_signature_shared::auto_signature_uses(&data.body);
                        let mut defs = Vec::new();
                        // A *bare* block `{ ... }` has an implicit `$_` parameter
                        // (default from the outer topic). A pointy block `-> { ... }`
                        // is also a `Block` (`is_bare_block` is set for the `.WHAT`),
                        // but its signature is explicit — an empty `-> {}` takes no
                        // arguments, so it must NOT gain the implicit `$_` (which
                        // would render its signature as `($$_?)` and break
                        // `.raku`/`.gist` round-tripping).
                        let is_pointy = data
                            .compiled_code
                            .as_ref()
                            .is_some_and(|cc| cc.is_pointy_block);
                        if data.is_bare_block && !is_pointy && !use_positional {
                            defs.push(ParamDef {
                                name: "$_".to_string(),
                                default: Some(Expr::Var("$_".to_string())),
                                multi_invocant: true,
                                required: false,
                                named: false,
                                slurpy: false,
                                double_slurpy: false,
                                onearg: false,
                                sigilless: false,
                                type_constraint: None,
                                literal_value: None,
                                sub_signature: None,
                                where_constraint: None,
                                traits: Vec::new(),
                                optional_marker: true,
                                outer_sub_signature: None,
                                code_signature: None,
                                is_invocant: false,
                                shape_constraints: None,
                                block_param: true,
                            });
                        }
                        if use_positional {
                            defs.push(ParamDef {
                                name: "@_".to_string(),
                                default: None,
                                multi_invocant: true,
                                required: false,
                                named: false,
                                slurpy: true,
                                double_slurpy: false,
                                onearg: false,
                                sigilless: false,
                                type_constraint: None,
                                literal_value: None,
                                sub_signature: None,
                                where_constraint: None,
                                traits: Vec::new(),
                                optional_marker: false,
                                outer_sub_signature: None,
                                code_signature: None,
                                is_invocant: false,
                                shape_constraints: None,
                                block_param: false,
                            });
                        }
                        if use_named {
                            defs.push(ParamDef {
                                name: "%_".to_string(),
                                default: None,
                                multi_invocant: true,
                                required: false,
                                named: false,
                                slurpy: true,
                                double_slurpy: false,
                                onearg: false,
                                sigilless: false,
                                type_constraint: None,
                                literal_value: None,
                                sub_signature: None,
                                where_constraint: None,
                                traits: Vec::new(),
                                optional_marker: false,
                                outer_sub_signature: None,
                                code_signature: None,
                                is_invocant: false,
                                shape_constraints: None,
                                block_param: false,
                            });
                        }
                        defs
                    }
                });
        let return_type = data
            .env
            .get("__mutsu_return_type")
            .and_then(|v| match v.view() {
                ValueView::Str(s) => Some(s.to_string()),
                _ => None,
            });
        let info = param_defs_to_sig_info(&param_defs, return_type);
        // Build the owner sub key for parameter doc comment lookup.
        // Must match the key format used by collect_doc_comments:
        // - Subs use "&name" prefix
        // - Methods use "ClassName::name" format
        let owner_key = if !data.name.is_empty() {
            let name = data.name.resolve();
            // Check if the sub is a method (has a non-GLOBAL package context
            // and uses Class::method format in doc comments)
            let pkg = data.package.resolve();
            if !pkg.is_empty() && pkg != "GLOBAL" {
                Some(format!("{}::{}", pkg, name))
            } else {
                Some(format!("&{}", name))
            }
        } else {
            None
        };
        let signature = make_signature_value_with_owner(info, owner_key, Some(self));
        if let Some(key) = cache_key {
            cache_sub_signature(key, signature.clone());
        }
        signature
    }

    pub(super) fn signature_required_positional_count(
        info: &crate::value::signature::SigInfo,
    ) -> i64 {
        info.params
            .iter()
            .filter(|p| !p.named && !p.slurpy && !p.has_default && !p.optional_marker)
            .count() as i64
    }

    fn signature_positional_count(info: &crate::value::signature::SigInfo) -> Option<i64> {
        let mut count = 0i64;
        for p in &info.params {
            if p.named || (p.slurpy && p.sigil == '%') {
                continue;
            }
            if p.slurpy {
                return None;
            }
            count += 1;
        }
        Some(count)
    }

    pub(super) fn signature_count_value(info: &crate::value::signature::SigInfo) -> Value {
        match Self::signature_positional_count(info) {
            Some(count) => Value::int(count),
            None => Value::num(f64::INFINITY),
        }
    }

    pub(super) fn candidate_arity_value(infos: &[crate::value::signature::SigInfo]) -> Value {
        let arity = infos
            .iter()
            .map(Self::signature_required_positional_count)
            .min()
            .unwrap_or(0);
        Value::int(arity)
    }

    pub(super) fn candidate_count_value(infos: &[crate::value::signature::SigInfo]) -> Value {
        let mut max_count = 0i64;
        for info in infos {
            match Self::signature_positional_count(info) {
                Some(count) => {
                    if count > max_count {
                        max_count = count;
                    }
                }
                None => return Value::num(f64::INFINITY),
            }
        }
        Value::int(max_count)
    }
}
