//! Signature/candidate introspection: named-param keys, call-arg matching,
//! candidate routine lookup, and arity/count Value builders.
use super::*;
use crate::value::signature::{make_signature_value_with_owner, param_defs_to_sig_info};

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
        match value {
            Value::Capture { positional, named } => {
                let mut args = (**positional).clone();
                for (k, v) in named.iter() {
                    args.push(Value::Pair(k.clone(), Box::new(v.clone())));
                }
                args
            }
            other => vec![other.clone()],
        }
    }

    pub(super) fn varref_parts(value: &Value) -> Option<(String, Value)> {
        if let Value::Capture { positional, named } = value
            && positional.is_empty()
            && let Some(Value::Str(name)) = named.get("__mutsu_varref_name")
            && let Some(inner) = named.get("__mutsu_varref_value")
        {
            return Some((name.to_string(), inner.clone()));
        }
        None
    }

    pub(super) fn var_target_from_meta_value(value: &Value) -> Option<String> {
        match value {
            Value::Mixin(inner, _) => Self::var_target_from_meta_value(inner),
            Value::Instance { attributes, .. } => {
                match attributes.as_map().get("__mutsu_var_target") {
                    Some(Value::Str(name)) => Some(name.to_string()),
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
        match candidate {
            Value::Sub(data) => {
                if data.empty_sig && !args.is_empty() {
                    return false;
                }
                if data.param_defs.is_empty() && !data.params.is_empty() {
                    if args.iter().any(|arg| {
                        matches!(
                            arg,
                            Value::Pair(key, _) if key != "__mutsu_test_callsite_line"
                        )
                    }) {
                        return false;
                    }
                    let positional = args
                        .iter()
                        .filter(|arg| !matches!(arg, Value::Pair(_, _) | Value::ValuePair(_, _)))
                        .count();
                    return positional == data.params.len();
                }
                self.method_args_match(args, &data.param_defs)
            }
            Value::WeakSub(weak) => weak
                .upgrade()
                .is_some_and(|strong| self.method_args_match(args, &strong.param_defs)),
            Value::Routine { name, .. } => self
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
        let mut seen = std::collections::HashSet::new();
        let mut out = Vec::new();
        let mut multi_idx = 0usize;
        for (key, def) in &self.registry().functions {
            let key_s = key.resolve();
            if key_s == exact_local
                || key_s == exact_global
                || key_s.starts_with(&prefix_local)
                || key_s.starts_with(&prefix_global)
            {
                let fp =
                    crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body);
                if seen.insert(fp) {
                    let mut env = self.env.clone();
                    // Store the multi index for doc comment lookup
                    env.insert(
                        "__mutsu_multi_index".to_string(),
                        Value::Int(multi_idx as i64),
                    );
                    out.push(Value::make_sub(
                        def.package,
                        def.name,
                        def.params.clone(),
                        def.param_defs.clone(),
                        def.body.clone(),
                        def.is_rw,
                        env,
                    ));
                    multi_idx += 1;
                }
            }
        }
        out
    }

    pub(super) fn sub_signature_value(&self, data: &crate::value::SubData) -> Value {
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
                            })
                            .collect()
                    } else {
                        let (use_positional, use_named) = Self::auto_signature_uses(&data.body);
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
                            });
                        }
                        defs
                    }
                });
        let return_type = data.env.get("__mutsu_return_type").and_then(|v| match v {
            Value::Str(s) => Some(s.to_string()),
            _ => None,
        });
        let info = param_defs_to_sig_info(&param_defs, return_type);
        // Build the owner sub key for parameter doc comment lookup.
        // Must match the key format used by collect_doc_comments:
        // - Subs use "&name" prefix
        // - Methods use "ClassName::name" format
        let sub_key = if !data.name.is_empty() {
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
        make_signature_value_with_owner(info, sub_key)
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
            Some(count) => Value::Int(count),
            None => Value::Num(f64::INFINITY),
        }
    }

    pub(super) fn candidate_arity_value(infos: &[crate::value::signature::SigInfo]) -> Value {
        let arity = infos
            .iter()
            .map(Self::signature_required_positional_count)
            .min()
            .unwrap_or(0);
        Value::Int(arity)
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
                None => return Value::Num(f64::INFINITY),
            }
        }
        Value::Int(max_count)
    }
}
