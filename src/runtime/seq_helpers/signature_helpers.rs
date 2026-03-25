use super::super::*;
use crate::symbol::Symbol;
use crate::value::signature::{SigInfo, SigParam};
use std::collections::HashMap;

impl Interpreter {
    pub(in crate::runtime) fn parse_parametric_spec(spec: &str) -> (String, Vec<String>) {
        if let Some((base, rest)) = spec.split_once('[') {
            let inner = rest.strip_suffix(']').unwrap_or(rest);
            (base.to_string(), split_balanced_comma_list(inner))
        } else {
            (spec.to_string(), Vec::new())
        }
    }

    pub(in crate::runtime) fn role_parent_args_for(
        &self,
        role_name: &str,
        role_args: &[Value],
        target_base: &str,
    ) -> Option<Vec<Value>> {
        let candidate = self.role_candidates.get(role_name).and_then(|candidates| {
            candidates
                .iter()
                .find(|c| c.type_params.len() == role_args.len())
                .cloned()
        })?;
        let candidate_param_names = candidate.type_params;
        let param_map: HashMap<String, Value> = candidate_param_names
            .into_iter()
            .zip(role_args.iter().cloned())
            .collect();
        for parent in candidate.parents {
            let resolved_parent = if let Some(v) = param_map.get(&parent) {
                match v {
                    Value::Package(name) => name.resolve(),
                    other => other
                        .to_string_value()
                        .trim_start_matches('(')
                        .trim_end_matches(')')
                        .to_string(),
                }
            } else {
                parent.clone()
            };
            let (parent_base, parent_arg_specs) = Self::parse_parametric_spec(&resolved_parent);
            if parent_base != target_base {
                continue;
            }
            let resolved = parent_arg_specs
                .iter()
                .map(|spec| {
                    param_map
                        .get(spec)
                        .cloned()
                        .or_else(|| param_map.get(spec.trim_start_matches("::")).cloned())
                        .unwrap_or_else(|| {
                            if let Ok(i) = spec.parse::<i64>() {
                                Value::Int(i)
                            } else {
                                Value::Package(Symbol::intern(spec))
                            }
                        })
                })
                .collect();
            return Some(resolved);
        }
        None
    }

    pub(in crate::runtime) fn signature_capture_like(
        value: &Value,
    ) -> Option<(Vec<Value>, HashMap<String, Value>)> {
        match value {
            Value::Capture { positional, named } => Some((positional.clone(), named.clone())),
            Value::Hash(map) => Some((Vec::new(), (**map).clone())),
            Value::Set(items, _) => Some((
                Vec::new(),
                items
                    .iter()
                    .map(|k| (k.clone(), Value::Bool(true)))
                    .collect(),
            )),
            Value::Bag(items, _) => Some((
                Vec::new(),
                items
                    .iter()
                    .map(|(k, v)| (k.clone(), Value::Int(*v)))
                    .collect(),
            )),
            Value::Mix(items, _) => Some((
                Vec::new(),
                items
                    .iter()
                    .map(|(k, v)| (k.clone(), Value::Num(*v)))
                    .collect(),
            )),
            Value::Rat(n, d) | Value::FatRat(n, d) => {
                let mut named = HashMap::new();
                named.insert("numerator".to_string(), Value::Int(*n));
                named.insert("denominator".to_string(), Value::Int(*d));
                Some((Vec::new(), named))
            }
            _ if value.as_list_items().is_some() => {
                let mut positional = Vec::new();
                let mut named = HashMap::new();
                for item in value.as_list_items().unwrap().iter() {
                    if let Value::Pair(k, v) = item {
                        named.insert(k.clone(), *v.clone());
                    } else {
                        positional.push(item.clone());
                    }
                }
                Some((positional, named))
            }
            Value::Pair(k, v) => {
                let mut named = HashMap::new();
                named.insert(k.clone(), *v.clone());
                Some((Vec::new(), named))
            }
            Value::Instance { attributes, .. } => Some((Vec::new(), (**attributes).clone())),
            Value::Mixin(inner, _) => Self::signature_capture_like(inner),
            _ => None,
        }
    }

    pub(in crate::runtime) fn signature_where_ok(
        &mut self,
        candidate: &Value,
        where_expr: &Expr,
    ) -> bool {
        let saved = self.env.clone();
        self.env.insert("_".to_string(), candidate.clone());
        let ok = match where_expr {
            Expr::AnonSub { body, .. } => self
                .eval_block_value(body)
                .map(|v| v.truthy())
                .unwrap_or(false),
            expr => self
                .eval_block_value(&[Stmt::Expr(expr.clone())])
                .map(|v| self.smart_match(candidate, &v))
                .unwrap_or(false),
        };
        self.env = saved;
        ok
    }

    pub(in crate::runtime) fn sig_param_optional(p: &SigParam) -> bool {
        p.has_default || p.optional_marker
    }

    pub(in crate::runtime) fn sig_param_matches_value(
        &mut self,
        candidate: &Value,
        param: &SigParam,
    ) -> bool {
        if let Some(constraint) = &param.type_constraint
            && !self.type_matches_value(constraint, candidate)
        {
            return false;
        }
        if let Some(where_expr) = &param.where_constraint
            && !self.signature_where_ok(candidate, where_expr)
        {
            return false;
        }
        if let Some(sub_params) = &param.sub_signature {
            let sub = SigInfo {
                params: sub_params.clone(),
                return_type: None,
            };
            if !self.signature_accepts_value(candidate, &sub) {
                return false;
            }
        }
        true
    }

    pub(in crate::runtime) fn signature_accepts_value(
        &mut self,
        left: &Value,
        signature: &SigInfo,
    ) -> bool {
        let Some((positional, named)) = Self::signature_capture_like(left) else {
            return false;
        };

        let mut pos_idx = 0usize;
        let mut consumed_named: HashSet<String> = HashSet::new();
        let has_capture = signature.params.iter().any(|p| p.is_capture);
        let has_slurpy_positional = signature
            .params
            .iter()
            .any(|p| p.slurpy && !(p.named || p.sigil == '%'));
        let has_slurpy_named = signature
            .params
            .iter()
            .any(|p| p.slurpy && (p.named || p.sigil == '%'));

        if has_capture {
            return true;
        }

        for param in &signature.params {
            if param.is_capture {
                return true;
            }

            let is_named_space = param.named || (param.slurpy && param.sigil == '%');
            if is_named_space {
                if param.slurpy {
                    if let Some(constraint) = &param.type_constraint {
                        for (key, value) in &named {
                            if consumed_named.contains(key) {
                                continue;
                            }
                            if !self.type_matches_value(constraint, value) {
                                return false;
                            }
                        }
                    }
                    consumed_named.extend(named.keys().cloned());
                    continue;
                }

                let mut candidate = named.get(&param.name).cloned();
                if candidate.is_none() {
                    candidate = self
                        .call_method_with_values(left.clone(), &param.name, Vec::new())
                        .ok();
                }
                let Some(candidate) = candidate else {
                    if Self::sig_param_optional(param) {
                        continue;
                    }
                    return false;
                };
                consumed_named.insert(param.name.clone());
                if !self.sig_param_matches_value(&candidate, param) {
                    return false;
                }
                continue;
            }

            if param.slurpy {
                if let Some(constraint) = &param.type_constraint {
                    for value in &positional[pos_idx..] {
                        if !self.type_matches_value(constraint, value) {
                            return false;
                        }
                    }
                }
                pos_idx = positional.len();
                continue;
            }

            let Some(candidate) = positional.get(pos_idx).cloned() else {
                if Self::sig_param_optional(param) {
                    continue;
                }
                return false;
            };
            pos_idx += 1;
            if !self.sig_param_matches_value(&candidate, param) {
                return false;
            }
        }

        if !has_slurpy_positional && pos_idx < positional.len() {
            return false;
        }
        if !has_slurpy_named
            && named
                .keys()
                .any(|key| !consumed_named.contains(key.as_str()))
        {
            return false;
        }
        true
    }
}
